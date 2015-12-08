#lang racket

(require rackunit
         rackunit/text-ui
         (rename-in "./helpers.rkt" [tests helpers-tests])
         (rename-in "./assembling/helpers.rkt" [tests assembling-helpers-tests]))

(module+ main
  (run-tests
    (test-suite
      "Complete tests"
      helpers-tests
      assembling-helpers-tests)))
