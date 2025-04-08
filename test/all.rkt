#lang racket

(require "compile.rkt"
         "interp.rkt"
         "tests.rkt")

(module+ test
  (run-tests compile-test-suite)
  (run-tests compile-io-test-suite)
  (run-tests interp-test-suite)
  (run-tests interp-io-test-suite))
