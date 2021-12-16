#lang racket

(require (prefix-in url: net/url)
         (prefix-in os: racket/os))


(define (kvl/c k v) (listof (cons/c k v)))

(define headers? (kvl/c string? string?))

(define query? (kvl/c symbol? string?))

(struct Req (meth path-str path query proto headers from req-id) #:transparent)

(define Req/c (struct/dc Req
                         [meth string?]
                         [path-str string?]
                         [path (listof string?)]
                         [query query?]
                         [proto string?]
                         [headers headers?]
                         [from string?]
                         [req-id string?]))

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

(define/contract req-id-next
  (-> string?)
  (let ([start-time (current-inexact-milliseconds)]
        [pid (os:getpid)]
        ; XXX -1 assumes initial call will come immediately after next's definition:
        [counter -1])
    (λ ()
       (set! counter (+ 1 counter))
       (format "req:~a-~a-~a" start-time pid counter))))

(define current-req-id (make-parameter (req-id-next)))

(define/contract (reply op code [body ""])
  (->* (output-port? (integer-in 100 599)) (string?) void?)
  (eprintf "[~a] response: ~a ~a~n" (current-req-id) code body)
  (display-lines
    (list (format "HTTP/1.0 ~a ~a" code (hash-ref phrases code ""))
          "Server: probe-me.xandkar"
          "Content-Type: text/plain"
          ""
          body)
    op
    #:separator "\r\n"))

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
  (define req-id (current-req-id))
  (define req-line (read-line ip 'return-linefeed))
  (eprintf "[~a] req line: ~s~n" req-id req-line)
  (if (eof-object? req-line)
      (Error 'eof)
      (match (string-split (string-downcase req-line) #rx" +")
        ; XXX Let path handlers worry about method validity?
        [(list meth path-str (and proto (or "http/1.0" "http/1.1")))
         (let ([url (str->url path-str)])
           (if url
               (let ([path (map url:path/param-path (url:url-path url))]
                     [query (url:url-query url)])
                 (eprintf "[~a] query: ~s~n" req-id query)
                 (Ok (Req meth
                          path-str
                          path
                          query
                          proto
                          (read-headers ip)
                          from
                          req-id)))
               (Error 'invalid-path)))]
        [(list _ _ proto)
         (Error (cons 'unsupported-protocol-version proto))]
        [_
          (Error 'invalid-req-line)])))

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
  (define req-id (current-req-id))
  (define up? #f)
  (define timeout-connect 5)  ; TODO Option
  (define timeout-read    1)  ; TODO Option
  (sync/timeout
    timeout-connect
    (thread (λ ()
               (with-handlers
                 ([exn:fail:network? (λ (_) (void))])
                 (define-values (ip op) (tcp-connect addr port-num))
                 (eprintf "[~a] probe connection succeeded to ~a:~a~n" req-id addr port-num)
                 (set! up? #t)
                 (define service-line (read-line/timeout ip timeout-read))
                 (if service-line
                     (begin
                       (set! up? (service-line-normalize service-line))
                       (eprintf
                         "[~a] probe service banner read succeeded from ~a:~a~n"
                         req-id
                         addr
                         port-num))
                     (eprintf
                       "[~a] probe service banner read failed from ~a:~a~n"
                       req-id
                       addr
                       port-num))
                 (close-input-port ip)
                 (close-output-port op)))))
  (unless up?
    (eprintf "[~a] probe connection failed to ~a:~a~n" req-id addr port-num))
  up?)

(define/contract (handle-probe ip op req)
  req-handler?
  (define req-id (current-req-id))
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

(define/contract (route path)
  (-> (listof string?) (or/c #f req-handler?))
  (match path
    [(list n) #:when (regexp-match? #rx"^[0-9]+$" n) handle-probe]
    [_ #f]))

(define/contract (dispatch ip op client-addr)
  (-> input-port? output-port? string? void?)
  (define req-id (current-req-id))
  (define req-read-result (read-req ip client-addr))
  (eprintf "[~a] request read result: ~s~n" req-id req-read-result)
  (match req-read-result
    [(Error 'eof) (void)]
    [(Error _) (reply op 400)]
    [(Ok req)
     (match (route (Req-path req))
      [#f (reply op 404)]
      [handler (with-handlers
                 ([any/c (λ (e)
                            (eprintf "[~a] handler crash: ~a~n" req-id e)
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
                           (define req-id (current-req-id))
                           (match-define-values
                             (_ _ client-addr client-port)
                             (tcp-addresses ip #t))
                           (eprintf "[~a] BEGIN: connected to ~a:~a~n"
                                    req-id
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
               (eprintf "[~a] END: ~a in ~a seconds~n"
                        (current-req-id)
                        result
                        (real->decimal-string (/ (- t1 t0) 1000) 3))
               (custodian-shutdown-all acceptor-custodian)
               (kill-thread handler-thread)))
    (void)))

(define/contract (serve hostname port-num max-allow-wait reuse-port?)
  (-> string? listen-port-number? exact-nonnegative-integer? boolean? void?)
  (define server-custodian (make-custodian))
  (parameterize ([current-custodian server-custodian])
    (define listener (tcp-listen port-num max-allow-wait reuse-port? hostname))
    (eprintf
      "Listening on ~a:~a. max-allow-wait: ~a, reuse-port?: ~a, request-timeout: ~a, request-mem-limit-mb: ~a~n"
      hostname
      port-num
      max-allow-wait
      reuse-port?
      (request-timeout)
      (request-mem-limit-mb))
    (define server
      (thread (λ () (let loop ()
                      (accept listener)
                      (loop)))))
    (thread-wait server)))

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
      (serve hostname
             port-num
             max-allow-wait
             reuse-port?))))
