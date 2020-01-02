#lang info

(define collection "regraph")
(define version "1.4")

;; Packaging information

(define pkg-desc "A pure-Racket e-graph implementations")
(define pkg-authors
  '("Alex Sanchez-Stern"
    "Pavel Panchekha"))

;; Dependencies
(define deps '(("base" #:version "7.0")))
(define build-deps '("rackunit-lib"))
