#lang racket

(require net/url)

(define headers? (listof (cons/c string? string?)))

(struct Req (meth path proto headers from) #:transparent)

(define Req/c (struct/dc Req
                         [meth string?]
                         [path (listof string?)]
                         [proto string?]
                         [headers headers?]
                         [from string?]))

(define phrases (hash 200 "OK"
                      400 "Bad Request"
                      500 "Internal Server Error"))

(define/contract (reply op code body)
  (-> output-port? (integer-in 100 599) string? void?)
  (display-lines
    (list (format "HTTP/1.0 ~a ~a" code (hash-ref phrases code ""))
          "Server: probeme.xandkar"
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

(define/contract (read-req ip from)
  (-> input-port? string? (or/c #f Req/c))
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
                 from)]
           [_
             ; Invalid req line
             #f])]
        [else
          (eprintf "WARN: req-line neither EOF nor string: ~v" req-line)
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

(define/contract (probe addr port-num)
  (-> string? number? (or/c boolean? string?))
  (define up? #f)
  (define timeout-connect 5)  ; TODO Option
  (define timeout-read    1)  ; TODO Option
  (sync/timeout
    timeout-connect
    (thread (λ ()
               (with-handlers
                 ([exn:fail:network?
                    (λ (_)
                       (eprintf "connection failed to ~a ~a~n" addr port-num))])
                 (define-values (ip op) (tcp-connect addr port-num))
                 (eprintf "connection succeeded to ~a ~a~n" addr port-num)
                 (set! up? #t)
                 (define service-line (read-line/timeout ip timeout-read))
                 (when service-line
                   (set! up? (service-line-normalize service-line)))
                 (close-input-port ip)
                 (close-output-port op)))))
  up?)

(define/contract (handle ip op)
  (-> input-port? output-port? void?)
  (define default-target-port-num 80)
  (define-values
    (server-addr server-port-num client-addr client-port-num)
    (tcp-addresses ip #t))
  (eprintf "tcp-addresses: server:~a:~a client:~a:~a~n"
           server-addr server-port-num
           client-addr client-port-num)
  (define req (read-req ip client-addr))
  (when req
    (define target-port-num
      (match (Req-path req)
        [(or '() '("")) default-target-port-num]
        [(list* port-num-str _)
         (eprintf "port-num-str ~v~n" port-num-str)
         (string->number port-num-str)]))
    (if (and target-port-num
             (>= target-port-num 1)
             (<= target-port-num 65535))
        (let ([probe-status
                (match (probe client-addr target-port-num)
                  [#f "down"]
                  [#t "up"]
                  [service-line (format "up ~a" service-line)])])
          (reply op 200 (format "~a ~a ~a" client-addr target-port-num probe-status)))
        (reply op 400 (format "Expected: number 1-65535. Received: ~v" (car (Req-path req)))))))

(define/contract (accept-and-handle listener)
  (-> tcp-listener? void?)
  (define acceptor-custodian (make-custodian))
  (define mem-limit 50)
  (define timeout 10)
  (custodian-limit-memory acceptor-custodian (* mem-limit 1024 1024))
  (parameterize ([current-custodian acceptor-custodian])
    (define-values (ip op) (tcp-accept listener))
    (thread (λ ()
               (with-handlers
                 ([any/c
                    (λ (e)
                       (eprintf "handler crash: ~a~n" e)
                       (reply op 500 ""))])
                 (handle ip op))
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
      (thread (λ () (let loop ()
                      (accept-and-handle listener)
                      (loop)))))
    (thread-wait server)))

(module+ main
  ; TODO CLI opts
  (serve 8080))
