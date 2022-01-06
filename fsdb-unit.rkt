#lang racket/unit

(require racket/file)

(require "db-sig.rkt")

(import)
(export db^)

(define conn? path-string?)

(define (store dir #:bucket b #:key k #:val v)
  (define path (build-path dir b k))
  (make-parent-directory* path)
  (display-to-file v path #:exists 'replace))

(define (store* dir #:bucket b #:key k #:vals vs)
  (define path (build-path dir b k))
  (make-parent-directory* path)
  (display-lines-to-file vs path #:exists 'replace))

(define (fetch dir #:bucket b #:key k)
  (define path (build-path dir b k))
  (if (file-exists? path)
      (file->string path)
      #f))

(define (fetch* dir #:bucket b #:key k)
  (define path (build-path dir b k))
  (if (file-exists? path)
      (file->lines path)
      '()))

(define (list-keys dir #:bucket b)
  (define path (build-path dir b))
  (make-directory* path)
  (map path->string (directory-list path)))

(define (append dir #:bucket b #:key k #:val v)
  (define path (build-path dir b k))
  (make-parent-directory* path)
  (display-lines-to-file (list v) path #:exists 'append))
