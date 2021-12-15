#lang racket

(provide init next)

(require (prefix-in os: racket/os))

(define/contract (init server-port)
  (-> port-number? string?)
  (format "reqid-~a-~a-~a" (os:gethostname) server-port (os:getpid)))

(define/contract (next init client-addr client-port)
  (-> string? string? port-number? string?)
  (format "~a-~a-~a-~a-~a"
          init
          (current-inexact-milliseconds)
          (current-inexact-monotonic-milliseconds)
          client-addr
          client-port))
