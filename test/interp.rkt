#lang racket

(provide run-interp
         run-interp/io
         interp-test-suite
         interp-io-test-suite)

(require "../interp.rkt"
         "../interp-io.rkt"
         "../parse.rkt"

         "tests.rkt")

(define (run-interp e)
  (interp (parse e)))

(define (run-interp/io e in)
  (interp/io (parse e) in))

(define interp-test-suite (make-test-suite "interp" run-interp))
(define interp-io-test-suite (make-io-test-suite "interp/io" run-interp/io))

(module+ test
  (run-tests interp-test-suite)
  (run-tests interp-io-test-suite))
