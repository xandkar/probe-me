#lang racket

(require (prefix-in sendmail: net/sendmail)
         (prefix-in url: net/url)
         racket/date
         racket/logging
         (prefix-in os: racket/os))

(module+ test
  (require rackunit))


(define (kvl/c k v) (listof (cons/c k v)))

(define headers? (kvl/c string? string?))

(define query? (kvl/c symbol? (or/c #f string?)))

(define req-id? (cons/c 'req-id string?))

(struct Req (meth path-str path query proto headers from) #:transparent)

(define Req/c (struct/dc Req
                         [meth string?]
                         [path-str string?]
                         [path (listof string?)]
                         [query query?]
                         [proto string?]
                         [headers headers?]
                         [from string?]))

(define req-handler? (-> input-port? output-port? Req/c void?))

(struct Ok (data) #:transparent)
(struct Error (data) #:transparent)

(define (Result/c α β)
  (or/c (struct/dc Ok [data α])
        (struct/dc Error [data β])))


(define phrases (hash 200 "OK"
                      400 "Bad Request"
                      404 "Not Found"
                      405 "Method Not Allowed"
                      500 "Internal Server Error"))

(define targets (make-hash))
(define emails  (make-hash))
(define history (make-hash))

(define/contract req-id-next
  (-> req-id?)
  (let ([start-time (current-inexact-milliseconds)]
        [pid (os:getpid)]
        [counter 0])
    (λ ()
       (set! counter (+ 1 counter))
       (cons 'req-id (format "req:~a-~a-~a" start-time pid counter)))))

(define current-req-id (make-parameter #f))

(define/contract (logger-start level)
  (-> log-level/c void?)
  (define logger (make-logger #f #f level #f))
  (define log-receiver (make-log-receiver logger level))
  (current-logger logger)
  (define (log-writer)
    (parameterize ([date-display-format 'iso-8601])
      (let loop ()
        (match-define (vector level msg req-id topic) (sync log-receiver))
        (eprintf "~a [~a]~a ~a~n"
                 (date->string (current-date) #t)
                 level
                 (match req-id
                   [(cons 'req-id id) (format " [~a]" id)]
                   [_ ""])
                 msg)
        (loop))))
  (void (thread log-writer)))

(define (log-debug fmt . args)
  (log-message (current-logger)
               'debug
               (apply format (cons fmt args))
               (current-req-id)
               #f))

(define/contract (reply op code [body ""])
  (->* (output-port? (integer-in 100 599)) (string?) void?)
  (log-debug "begin response: ~a ~a" code body)
  (display-lines
    (list (format "HTTP/1.0 ~a ~a" code (hash-ref phrases code ""))
          "Server: probe-me.xandkar"
          "Content-Type: text/plain"
          ""
          body)
    op
    #:separator "\r\n")
  (log-debug "end response"))

(define/contract (read-headers ip)
  (-> input-port? headers?)
  (define (r headers)
    (match (read-line ip 'return-linefeed)
      [eof #:when (eof-object? eof) headers]
      [line
        (match (regexp-match #px"^([^\\s]+)\\s+(.*)$" line)
          [#f headers]
          [(list _ k v) (r (cons (cons k v) headers))])]))
  (r '()))

(define (str->url s)
  (with-handlers*
    ([exn:fail? (λ (e) #f)])
    (url:string->url s)))

(define/contract (read-req ip from)
  (-> input-port?
      string?
      (Result/c Req/c
                (or/c (cons/c 'unsupported-protocol-version
                              string?)
                      'invalid-path
                      'invalid-req-line
                      'eof)))
  (define req-line (read-line ip 'return-linefeed))
  (log-debug "req line: ~v" req-line)
  (if (eof-object? req-line)
      (Error 'eof)
      (match (string-split (string-downcase req-line) #rx" +")
        ; XXX Let path handlers worry about method validity?
        [(list meth path-str (and proto (or "http/1.0" "http/1.1")))
         (let ([url (str->url path-str)])
           (if url
               (let ([path (map url:path/param-path (url:url-path url))]
                     [query (url:url-query url)])
                 (log-debug "query: ~s" query)
                 (Ok (Req meth
                          path-str
                          path
                          query
                          proto
                          (read-headers ip)
                          from)))
               (Error 'invalid-path)))]
        [(list _ _ proto)
         (Error (cons 'unsupported-protocol-version proto))]
        [_
          (Error 'invalid-req-line)])))

(module+ test
  (define (read-req-str str from)
    (read-req (open-input-string str) from))

  (check-equal?
    (let* ([key 'key-but-no-val]
           [req-line (format "GET http://path?~a HTTP/1.0\r\n" key)])
      (dict-ref (Req-query (Ok-data (read-req-str req-line "localhost"))) key))
    #f))

(define/contract (read-line/timeout ip timeout)
  (-> input-port? (and/c real? (not/c negative?)) (or/c #f string?))
  (define line #f)
  (sync/timeout timeout (thread (λ () (set! line (read-line ip 'any)))))
  line)

(define/contract (string-drop-control-chars s)
  (-> string? string?)
  (list->string (filter (not/c char-iso-control?) (string->list s))))

(define/contract (service-line-normalize str)
  (-> string? string?)
  (string-drop-control-chars str))

(define/contract (probe addr port-num)
  (-> string? number? (or/c boolean? string?))
  (define up? #f)
  (define timeout-connect 5)  ; TODO Option
  (define timeout-read    1)  ; TODO Option
  (sync/timeout
    timeout-connect
    (thread (λ ()
               (with-handlers
                 ([exn:fail:network? (λ (_) (void))])
                 (define-values (ip op) (tcp-connect addr port-num))
                 (log-debug "probe connection succeeded to ~a:~a" addr port-num)
                 (set! up? #t)
                 (define service-line (read-line/timeout ip timeout-read))
                 (if service-line
                     (begin
                       (set! up? (service-line-normalize service-line))
                       (log-debug
                         "probe service banner read succeeded from ~a:~a"
                         addr
                         port-num))
                     (log-debug
                       "probe service banner read failed from ~a:~a"
                       addr
                       port-num))
                 (close-input-port ip)
                 (close-output-port op)))))
  (unless up?
    (log-debug "probe connection failed to ~a:~a" addr port-num))
  up?)

(define/contract (handle-probe ip op req)
  req-handler?
  (define addr (Req-from req))
  (define port-num (string->number (car (Req-path req))))
  (if (and port-num (port-number? port-num))
      (match (Req-meth req)
        ["get" (reply op
                      200
                      (format "~a ~a ~a"
                              addr
                              port-num
                              (match (probe addr port-num)
                                [#f "down"]
                                [#t "up"]
                                [service-line (format "up ~a" service-line)])))]
        [_ (reply op 405)])
      (reply op 400 (format "Invalid port number: ~v" port-num))))

(define/contract (handle-register ip op req)
  req-handler?
  (define addr-email (dict-ref (Req-query req) 'register))
  (define addr-target (Req-from req))
  (define port (string->number (car (Req-path req))))
  (when addr-email
    hash-set! emails addr-target addr-email)
  (hash-update! targets addr-target (λ (ports) (set-add ports port)) (set))
  (define resp-body
    (string-join (map number->string (set->list (hash-ref targets addr-target))) "\n"))
  (reply op 200 resp-body))

(define/contract (handle-history ip op req)
  req-handler?
  (define addr (Req-from req))
  (define port (string->number (car (Req-path req))))
  (define addr-hist (hash-ref history (cons addr port) '()))
  (define resp-body
    (string-join
      (map (match-lambda
             [(list t s b) (format "~a ~a ~a" t s b)]
             [(list t s  ) (format "~a ~a"    t s)])
           addr-hist)
      "\n"))
  (reply op 200 resp-body))

(define/contract (route req)
  (-> Req/c (or/c #f req-handler?))
  (match req
    [(struct* Req ([path (list n)] [query q])) #:when (regexp-match? #rx"^[0-9]+$" n)
     (match q
       ['() handle-probe]
       [(list (cons 'register _)) handle-register]
       ['((history  . #f)) handle-history]
       [_ #f])]
    [_ #f]))

(define/contract (dispatch ip op client-addr)
  (-> input-port? output-port? string? void?)
  (define req-read-result (read-req ip client-addr))
  (log-debug "request read result: ~s" req-read-result)
  (match req-read-result
    [(Error 'eof) (void)]
    [(Error _) (reply op 400)]
    [(Ok req)
     (match (route req)
       [#f (reply op 404)]
       [handler (with-handlers
                  ([any/c (λ (e)
                             (log-debug "handler crash: ~a" e)
                             (reply op 500 ""))])
                  (handler ip op req))])]))

(define/contract (accept listener)
  (-> tcp-listener? void?)
  (parameterize ([current-req-id (req-id-next)])
    (define acceptor-custodian (make-custodian))
    (custodian-limit-memory acceptor-custodian
                            (* (request-mem-limit-mb) 1024 1024))
    (define completed (make-channel))
    (define timed-out (make-channel))
    (define-values (t0 handler-thread)
      (parameterize ([current-custodian acceptor-custodian])
        (define-values (ip op) (tcp-accept listener))
        (values (current-inexact-milliseconds)
                (thread (λ ()
                           (match-define-values
                             (_ _ client-addr client-port)
                             (tcp-addresses ip #t))
                           (log-debug "BEGIN: connected to ~a:~a"
                                      client-addr
                                      client-port)
                           (dispatch ip op client-addr)
                           (close-input-port ip)
                           (close-output-port op)
                           (channel-put completed 'completed))))))
    (thread (λ ()
               (sleep (request-timeout))
               (channel-put timed-out 'timed-out)))
    (thread (λ ()
               (define result (sync completed timed-out))
               (define t1 (current-inexact-milliseconds))
               (log-debug "END: ~a in ~a seconds"
                          result
                          (real->decimal-string (/ (- t1 t0) 1000) 3))
               (custodian-shutdown-all acceptor-custodian)
               (kill-thread handler-thread)))
    (void)))

(define (execute-background-probes)
  (define (probe-and-record addr ports)
    (set-for-each
      ports
      (λ (port)
         (define status
           (match (probe addr port)
             [#f     '(down "")]
             [#t     '(up "")]
             [banner `(up ,banner)]))
         (when (equal? 'down (car status))
           (define addr-email (hash-ref emails addr #f))
           (when addr-email
             (thread (λ ()
                        (sendmail:send-mail-message
                          "probe-me <noreply@probe-me>"
                          (format "Your host:port is down: ~a:~a [EOM]" addr port)
                          '(,addr-email)
                          '()
                          '()
                          '())))))
         (define time (current-inexact-milliseconds))
         (define curr (cons time status))
         (hash-update! history (cons addr port) (λ (prev) (cons curr prev)) '()))))
  (hash-for-each targets probe-and-record))

(define/contract (serve hostname port-num max-allow-wait reuse-port?)
  (-> string? listen-port-number? exact-nonnegative-integer? boolean? void?)
  (define server-custodian (make-custodian))
  (parameterize ([current-custodian server-custodian])
    (define listener (tcp-listen port-num max-allow-wait reuse-port? hostname))
    (log-info
      "Listening on ~a:~a. max-allow-wait: ~a, reuse-port?: ~a, request-timeout: ~a, request-mem-limit-mb: ~a"
      hostname
      port-num
      max-allow-wait
      reuse-port?
      (request-timeout)
      (request-mem-limit-mb))
    ; TODO Supervisors: restarts and restart rate limits.
    (sync (thread (λ () (let loop ()
                          (accept listener)
                          (loop))))
          (thread (λ () (let loop ()
                          (execute-background-probes)
                          (sleep (* 15 60)) ; TODO Interval parameter/option
                          (loop)))))))

(define request-timeout (make-parameter 5))
(define request-mem-limit-mb (make-parameter 1))

(module+ main
  (let ([port-num       8080]
        [max-allow-wait 5]
        [reuse-port?    #t]
        [hostname       "0.0.0.0"])

    (command-line
      #:program
      "probe-me"

      #:once-each
      [("--host")
       host-name-or-address "Hostname or address to listen on."
       (set! hostname host-name-or-address)]
      [("-p" "--port")
       integer-from-0-to-65535 "TCP port to listen on."
       (set! port-num (string->number integer-from-0-to-65535))]
      [("--max-allow-wait")
       nonnegative-integer
       "Maximum number of client connections that can be waiting for acceptance."
       (set! max-allow-wait (string->number nonnegative-integer))]
      [("--request-timeout")
       nonnegative-number "Seconds before terminating the request handler."
       (request-timeout (string->number nonnegative-number))]
      [("--request-mem-limit")
       nonnegative-integer "Maximum memory allowed per request, in MB."
       (request-mem-limit-mb (string->number nonnegative-integer))]

      #:once-any
      [("--reuse-port")
       "Create a listener even if the port is involved in a TIME_WAIT state. (default)"
       (set! reuse-port? #t)]
      [("--no-reuse-port")
       "Do NOT create a listener if the port is involved in a TIME_WAIT state."
       (set! reuse-port? #f)]

      #:args ()
      (logger-start 'debug)
      (serve hostname
             port-num
             max-allow-wait
             reuse-port?))))
