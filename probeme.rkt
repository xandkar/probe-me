#lang racket

(struct Req (meth path proto headers from) #:transparent)

(define (read-headers ip)
  (define (r headers)
    (match (read-line ip 'return-linefeed)
      [eof #:when (eof-object? eof) headers]
      [line
        (match (regexp-match #px"^([^\\s]+)\\s+(.*)$" line)
          [#f headers]
          [(list _ k v) (r (cons (cons k v) headers))])]))
  (r '()))

(define (read-req ip from)
  (define req-line (read-line ip 'return-linefeed))
  (match (string-split req-line #rx" +")
    [(list meth path proto)
     (Req meth path proto (read-headers ip) from)]
    [_
      ; Invalid req line
      #f]))

(define/contract (probe addr port-num)
  (-> string? number? boolean?)
  (define up? #f)
  (sync/timeout
    5
    (thread (λ ()
               (with-handlers
                 ([exn:fail:network?
                    (λ (_)
                       (eprintf "connection failed to ~a ~a~n" addr port-num))])
                 (define-values (ip op) (tcp-connect addr port-num))
                 (eprintf "connection succeeded to ~a ~a~n" addr port-num)
                 (close-input-port ip)
                 (close-output-port op)
                 (set! up? #t)))))
  up?)

(define (handle ip op)
  (define target-port-num 80)  ; TODO Grap from URL path
  (define-values (addr-server addr-client) (tcp-addresses ip))
  (eprintf "tcp-addresses: server:~v client:~v~n" addr-server addr-client)
  (define req (read-req ip addr-client))
  (display "HTTP/1.0 200 OK" op)
  (display "\r\n" op)
  (display "Server: probeme.xandkar\r\nContent-Type: text/plain" op)
  (display "\r\n" op)
  (display "\r\n" op)
  (display (if (probe addr-client target-port-num) "up" "down") op)
  (display "\r\n" op)
  )

(define (accept-and-handle listener)
  (define acceptor-custodian (make-custodian))
  (define mem-limit 50)
  (define timeout 10)
  (custodian-limit-memory acceptor-custodian (* mem-limit 1024 1024))
  (parameterize ([current-custodian acceptor-custodian])
    (define-values (ip op) (tcp-accept listener))
    (thread (λ ()
               (handle ip op)
               (close-input-port ip)
               (close-output-port op))))
  (thread (λ ()
             (sleep timeout)
             (custodian-shutdown-all acceptor-custodian))))

(define (serve port-num)
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
