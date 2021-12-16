#lang racket

;; TODO Can req-id be stashed in some runtime location? parameter? thread tree?

(require (prefix-in url: net/url))

(require (prefix-in req-id: "req-id.rkt"))

(define headers? (listof (cons/c string? string?)))

(struct Req (meth path proto headers from req-id) #:transparent)

(define Req/c (struct/dc Req
                         [meth string?]
                         [path (listof string?)]
                         [proto string?]
                         [headers headers?]
                         [from string?]
                         [req-id string?]))

(define req-handler? (-> input-port? output-port? Req/c void?))

(define phrases (hash 200 "OK"
                      400 "Bad Request"
                      404 "Not Found"
                      405 "Method Not Allowed"
                      500 "Internal Server Error"))

(define/contract (reply req-id op code [body ""])
  (->* (string? output-port? (integer-in 100 599)) (string?) void?)
  (eprintf "[~a] response: ~a ~a~n" req-id code body)
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

(define/contract (read-req ip from req-id)
  (-> input-port? string? string? (or/c #f Req/c))
  (define req-line (read-line ip 'return-linefeed))
  (cond [(eof-object? req-line)
         #f]
        [(string? req-line)
         (match (string-split req-line #rx" +")
           [(list meth path proto)
            ; TODO Handle string->url exceptions
            (let ([path (filter (λ (s) (not (string=? "" s)))
                                (map url:path/param-path
                                     (url:url-path (url:string->url path))))])
              (Req meth
                   path
                   proto
                   (read-headers ip)
                   from
                   req-id))]
           [_
             ; Invalid req line
             #f])]
        [else
          (eprintf "[~a] WARN: req-line neither EOF nor string: ~v" req-id req-line)
          #f]))

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

(define/contract (probe addr port-num req-id)
  (-> string? number? string? (or/c boolean? string?))
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
  (define req-id (Req-req-id req))
  (define addr (Req-from req))
  (define port-num (string->number (car (Req-path req))))
  (if (and port-num (port-number? port-num))
      (match (Req-meth req)
        ["GET" (reply req-id
                      op
                      200
                      (format "~a ~a ~a"
                              addr
                              port-num
                              (match (probe addr port-num (Req-req-id req))
                                [#f "down"]
                                [#t "up"]
                                [service-line (format "up ~a" service-line)])))]
        [_ (reply req-id op 405)])
      (reply req-id op 400 (format "Invalid port number: ~v" port-num))))

(define/contract (route path)
  (-> (listof string?) (or/c #f req-handler?))
  (match path
    [(list n) #:when (regexp-match? #rx"^[0-9]+$" n) handle-probe]
    [_ #f]))

(define/contract (dispatch ip op client-addr req-id)
  (-> input-port? output-port? string? string? void?)
  (define req (read-req ip client-addr req-id))
  (eprintf "[~a] request: ~s~n" req-id req)
  (match req
    [#f (reply req-id op 400)]
    [req (match (route (Req-path req))
           [#f (reply req-id op 404)]
           [handler (with-handlers
                      ([any/c (λ (e)
                                 (eprintf "[~a] handler crash: ~a~n" req-id e)
                                 (reply req-id op 500 ""))])
                      (handler ip op req))])]))

(define/contract (accept-and-dispatch listener req-id-init)
  (-> tcp-listener? string? void?)
  (define acceptor-custodian (make-custodian))
  (custodian-limit-memory acceptor-custodian
                          (* (request-mem-limit-mb) 1024 1024))
  (parameterize ([current-custodian acceptor-custodian])
    (define-values (ip op) (tcp-accept listener))
    (match-define-values (_ _ client-addr client-port) (tcp-addresses ip #t))
    (define req-id (req-id:next req-id-init client-addr client-port))
    (eprintf "[~a] connection from ~a:~a~n"
             req-id
             client-addr
             client-port)
    (thread (λ ()
               (dispatch ip op client-addr req-id)
               (close-input-port ip)
               (close-output-port op))))
  (thread (λ ()
             (sleep (request-timeout))
             (custodian-shutdown-all acceptor-custodian)))
  (void))

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
      (let ([req-id-init (req-id:init port-num)])
        (thread (λ () (let loop ()
                        (accept-and-dispatch listener req-id-init)
                        (loop))))))
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
