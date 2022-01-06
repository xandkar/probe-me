#lang racket/signature

;; TODO IO errors

(contracted
  [conn?     (-> any/c boolean?)]
  [list-keys (-> conn? #:bucket string? (listof string?))]

  ; blob
  [store     (-> conn? #:bucket string? #:key string? #:val string? void?)]
  [fetch     (-> conn? #:bucket string? #:key string? (or/c #f string?))]

  ; list
  [store*    (-> conn? #:bucket string? #:key string? #:vals (listof string?) void?)]
  [fetch*    (-> conn? #:bucket string? #:key string? (listof string?))]

  ; log
  [append    (-> conn? #:bucket string? #:key string? #:val string? void?)])
