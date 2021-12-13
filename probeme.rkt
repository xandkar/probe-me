#lang racket

(struct Req (meth path proto headers) #:transparent)

(define (read-headers ip)
  (define (r headers)
    (match (read-line ip 'return-linefeed)
      [eof #:when (eof-object? eof) headers]
      [line
        (match (regexp-match #px"^([^\\s]+)\\s+(.*)$" line)
          [#f headers]
          [(list _ k v) (r (cons (cons k v) headers))])]))
  (r '()))

(define (read-req ip)
  (define req-line (read-line ip 'return-linefeed))
  (match (string-split req-line #rx" +")
    [(list meth path proto)
     (Req meth path proto (read-headers ip))]
    [_
      ; Invalid req line
      #f]))

(define (handle ip op)
  (define req (read-req ip))
  (display "HTTP/1.0 200 OK" op)
  (display "\r\n" op)
  (display "Server: probeme.xandkar\r\nContent-Type: text/plain" op)
  (display "\r\n" op)
  (display "\r\n" op)
  (display (pretty-format req) op)
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
