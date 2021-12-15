#lang racket

;; TODO Can req-id be stashed in some runtime location? parameter? thread tree?

(require net/url) ; TODO prefix-in

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
            (Req meth
                 (map path/param-path (url-path (string->url path))) ; TODO Handle exn
                 proto
                 (read-headers ip)
                 from
                 req-id)]
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
  (eprintf "[~a] request: ~a~n" req-id req)
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
  (define mem-limit 50)
  (define timeout 10)
  (custodian-limit-memory acceptor-custodian (* mem-limit 1024 1024))
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
             (sleep timeout)
             (custodian-shutdown-all acceptor-custodian)))
  (void))

(define/contract (serve port-num)
  (-> listen-port-number? void?)
  (define server-custodian (make-custodian))
  (parameterize ([current-custodian server-custodian])
    ; Maximum number of client connections that can be waiting for acceptance:
    (define max-allow-wait 5)
    ; create a listener even if the port is involved in a TIME_WAIT state?
    (define reuse? #t)
    (define listener (tcp-listen port-num max-allow-wait reuse?))
    (define server
      (let ([req-id-init (req-id:init port-num)])
        (thread (λ () (let loop ()
                        (accept-and-dispatch listener req-id-init)
                        (loop))))))
    (thread-wait server)))

(module+ main
  ; TODO CLI opts
  (serve 8080))
