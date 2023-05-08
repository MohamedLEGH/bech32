#lang info
(define collection "bech32")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/bech32.scrbl" ())))
(define pkg-desc "Package to encode and decode bech32 encoding")
(define version "0.0.1")
(define pkg-authors '(Mohamed Amine LEGHERABA))
(define license 'MIT)
