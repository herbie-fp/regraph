#lang racket
;; code extracted and simplified from Herbie v1.4 master April 25 2020
(require math/flonum math/base math/special-functions math/bigfloat racket/hash)


(provide eval-application)

(define (cbrt x) (expt x (/ 1 3)))
(define (exp2 x) (expt 2 x))
(define (log10 x) (log x 10))
(define (copysign x y) (if (>= y 0) (abs x) (- (abs x))))
(define (fdim x y) (max (- x y) 0))
(define (fma x y z) (bigfloat->flonum (bf+ (bf* (bf x) (bf y)) (bf z))))
(define (fmax x y) (cond ((nan? x) y) ((nan? y) x) (else (max x y))))
(define (fmin x y) (cond ((nan? x) y) ((nan? y) x) (else (min x y))))
(define (logb x) (floor (log (abs x) 2)))

(define real-fns
  (list + - * / acos acosh asin asinh atan atanh cbrt copysign cos cosh erf erfc exp exp2
        fdim floor fma fmax fmin log log10 logb remainder round sin sinh sqrt tan tanh))

(define (no-complex fun)
  (λ xs
     (define res (apply fun xs))
     (if (real? res)
       res
       +nan.0)))

(define real-operations
  (for/hash ([fn real-fns])
    (values (object-name fn) (no-complex fn))))


(define (from-bigfloat bff)
  (λ args (bigfloat->flonum (apply bff (map bf args)))))

(define (bffmod x mod)
  (bf- x (bf* (bftruncate (bf/ x mod)) mod)))

(define from-bf-operations
  (hash 'expm1 (from-bigfloat bfexpm1)
        'fmod (from-bigfloat bffmod)
        'hypot (from-bigfloat bfhypot)
        'j0 (from-bigfloat bfbesj0)
        'j1 (from-bigfloat bfbesj1)
        'log1p (from-bigfloat bflog1p)
        'log2 (from-bigfloat bflog2)
        'y0 (from-bigfloat bfbesy0)
        'y1 (from-bigfloat bfbesy1)))


(define complex-operations
  (hash '+.c + '-.c - '*.c * '/.c / 'exp.c exp 'log.c log 'pow.c expt 'sqrt.c sqrt
        'complex make-rectangular 're real-part 'im imag-part 'conj conjugate 'neg.c -))


(define ((comparator test) . args)
  (for/and ([left args] [right (cdr args)])
    (test left right)))

(define (if-fn test if-true if-false) (if test if-true if-false))
(define (and-fn . as) (andmap identity as))
(define (or-fn  . as) (ormap identity as))

(define (!=-fn . args)
  (not (check-duplicates args =)))

(define other-operations
  (hash 'ceil ceiling
        'atan2 (no-complex atan)
        'fabs abs
        'lgamma log-gamma
        'pow (no-complex expt)
        'rint round
        'tgamma gamma
        'trunc truncate
        '== (comparator =)
        '!= !=-fn
        '< (comparator <)
        '> (comparator >)
        '<= (comparator <=)
        '>= (comparator >=)
        'if if-fn
        'not not
        'and and-fn
        'or or-fn))

(define operations
  (hash-union complex-operations real-operations from-bf-operations other-operations))

(define (val-to-type val)
  (match val [#t 'TRUE] [#f 'FALSE] [(? real?) val]))

(define (exact-noncomplex-value? val)
  (and (not (and (complex? val) (not (real? val))))
       (exact? val)))
 
(define (eval-application op . args)
  (if (and (not (null? args)) (andmap (conjoin number? exact?) args))
      (with-handlers ([exn:fail:contract:divide-by-zero? (const #f)])
        (define res (apply (hash-ref operations op) args))
        (and (exact-noncomplex-value? res)
             (val-to-type res)))
      false))
