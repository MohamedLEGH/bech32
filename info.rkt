#lang info
(define collection "base58check")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/base58check.scrbl" ())))
(define pkg-desc "Package to encode and decode base58check encoding")
(define version "0.0.1")
(define pkg-authors '(Mohamed Amine LEGHERABA))
(define license 'MIT)
