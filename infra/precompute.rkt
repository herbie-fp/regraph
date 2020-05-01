#lang racket
(require math/bigfloat)

;; code extracted and simplified from Herbie v1.4 master April 25 2020
(provide eval-application)

(require math/flonum)
(require math/base)
(require math/special-functions)

(define ((comparator test) . args)
  (for/and ([left args] [right (cdr args)])
    (test left right)))

(define (from-bigfloat bff)
  (λ args (bigfloat->flonum (apply bff (map bf args)))))

(define value? (or/c bigfloat? boolean?))

(define type-dict (make-hash))
(define-syntax-rule (define-type name val? bigval?)
  (hash-set! type-dict 'name (cons val? bigval?)))
(define-type real real? bigfloat?)
(define-type bool boolean? boolean?)
(define-type complex (conjoin complex? (negate real?)) boolean?) ;; removed bigcomplex from this case

(define (type? x) (hash-has-key? type-dict x))

(define (val-to-type val)
  (cond
    [(boolean? val)
     (if val 'TRUE 'FALSE)]
    [(real? val)
     val]
    [else (error "Unknown type" val)]))

(define (table-ref tbl key field)
  (match-let ([(cons header rows) tbl])
    (for/first ([(field-name type) (in-dict header)]
                [value (in-list (hash-ref rows key))]
                #:when (equal? field-name field))
      value)))

(define-syntax-rule (define-table name [field type] ...)
  (define name (cons (list (cons 'field type) ...) (make-hash))))

(define (table-set! tbl key fields)
  (match-let ([(cons header rows) tbl])
    (define row (for/list ([(hkey htype) (in-dict header)]) (dict-ref fields hkey)))
    (hash-set! rows key row)))


(define (operator-info operator field) (table-ref operators operator field))

(define (unconstrained-argument-number-> from/c to/c)
  (unconstrained-domain-> to/c))

(define-table operators
  [args  (listof (or/c '* natural-number/c))]
  [fl    (unconstrained-argument-number-> value? value?)]
  [nonffi (unconstrained-argument-number-> value? value?)]
  [type  (hash/c (or/c '* natural-number/c) (listof (list/c (or/c (listof type?) (list/c '* type?)) type?)))]
  [->c/double (unconstrained-argument-number-> string? string?)]
  [->c/mpfr   (unconstrained-argument-number-> string? string?)]
  [->tex      (unconstrained-argument-number-> string? string?)])


(define-syntax-rule (define-operator (operator atypes ...) rtype [key value] ...)
  (let ([type (hash (length '(atypes ...)) (list (list '(atypes ...) 'rtype)))]
        [args (list (length '(atypes ...)))])
    (table-set! operators 'operator
                (make-hash (list (cons 'type type) (cons 'args args) (cons 'key value) ...)))))

(define (no-complex fun)
  (λ xs
     (define res (apply fun xs))
     (if (real? res)
       res
       +nan.0)))

(define (default-nonffi . args)
  (raise
   (make-exn:fail:unsupported
    (format "couldn't find ~a and no default implementation defined" 'operator)
    (current-continuation-marks))))

(define-operator (+ real real) real 
  [fl +] 
  [->c/double (curry format "~a + ~a")]
  [->c/mpfr (curry format "mpfr_add(~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "~a + ~a")]
  [nonffi +])

(define-operator (+.c complex complex) complex
  [fl +]  [ival #f]
  [->c/double (curry format "~a + ~a")]
  [->c/mpfr (const "/* ERROR: no complex support in C */")]
  [->tex (curry format "~a + ~a")]
  [nonffi +])

(define-operator (- real [real]) real
  ;; Override the normal argument handling because - can be unary
  [args '(1 2)] [type (hash 1 '(((real) real)) 2 '(((real real) real)))]
  [fl -] 
  [->c/double (λ (x [y #f]) (if y (format "~a - ~a" x y) (format "-~a" x)))]
  [->c/mpfr (λ (out x [y #f]) (if y (format "mpfr_sub(~a, ~a, ~a, MPFR_RNDN)" out x y) (format "mpfr_neg(~a, ~a, MPFR_RNDN)" out x)))]
  [->tex (λ (x [y #f]) (if y (format "~a - ~a" x y) (format "-~a" x)))]
  [nonffi -])

(define-operator (neg.c complex) complex
  [fl -]  [ival #f]
  [->c/double (curry format "-~a")]
  [->c/mpfr (const "/* ERROR: no complex support in C */")]
  [->tex (curry format "-~a")]
  [nonffi -])

(define-operator (-.c complex complex) complex
  [fl -] [ival #f]
  [->c/double (λ (x [y #f]) (if y (format "~a - ~a" x y) (format "-~a" x)))]
  [->c/mpfr (const "/* ERROR: no complex support in C */")]
  [->tex (λ (x [y #f]) (if y (format "~a - ~a" x y) (format "-~a" x)))]
  [nonffi -])

(define-operator (* real real) real
  [fl *]
  [->c/double (curry format "~a * ~a")]
  [->c/mpfr (curry format "mpfr_mul(~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "~a \\cdot ~a")]
  [nonffi *])

(define-operator (*.c complex complex) complex
  [fl *]  [ival #f]
  [->c/double (curry format "~a * ~a")]
  [->c/mpfr (const "/* ERROR: no complex support in C */")]
  [->tex (curry format "~a \\cdot ~a")]
  [nonffi *])

(define-operator (/ real real) real
  [fl /]
  [->c/double (curry format "~a / ~a")]
  [->c/mpfr (curry format "mpfr_div(~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\frac{~a}{~a}")]
  [nonffi /])

(define-operator (/.c complex complex) complex
  [fl /] 
  [->c/double (curry format "~a / ~a")]
  [->c/mpfr (const "/* ERROR: no complex support in C */")]
  [->tex (curry format "\\frac{~a}{~a}")]
  [nonffi /])

(require ffi/unsafe)
(define *unknown-d-ops* (make-parameter '()))
(define *unknown-f-ops* (make-parameter '()))

(define-syntax (define-operator/libm stx)
  (syntax-case stx (real libm)
    [(_ (operator real ...) real [libm id_d id_f] [key value] ...)
     (let ([num-args (length (cdr (syntax-e (cadr (syntax-e stx)))))])
       #`(begin
           (define (fallback . args)
             
             (apply (operator-info 'operator 'nonffi) args))
           (define double-proc (get-ffi-obj 'id_d #f (_fun #,@(build-list num-args (λ (_) #'_double)) -> _double)
                                            (lambda () (*unknown-d-ops* (cons 'operator (*unknown-d-ops*))) fallback)))
           
           (define-operator (operator #,@(build-list num-args (λ (_) #'real))) real
             [fl (λ args (apply double-proc args))]
             [key value] ...)))]))

(define-operator/libm (acos real) real
  [libm acos acosf]  
  [->c/double (curry format "acos(~a)")]
  [->c/mpfr (curry format "mpfr_acos(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\cos^{-1} ~a")]
  [nonffi (no-complex acos)])

(define-operator/libm (acosh real) real
  [libm acosh acoshf]  
  [->c/double (curry format "acosh(~a)")]
  [->c/mpfr (curry format "mpfr_acosh(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\cosh^{-1} ~a")]
  [nonffi (no-complex acosh)])

(define-operator/libm (asin real) real
  [libm asin asinf]  
  [->c/double (curry format "asin(~a)")]
  [->c/mpfr (curry format "mpfr_asin(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\sin^{-1} ~a")]
  [nonffi (no-complex asin)])

(define-operator/libm (asinh real) real
  [libm asinh asinhf]  
  [->c/double (curry format "asinh(~a)")]
  [->c/mpfr (curry format "mpfr_asinh(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\sinh^{-1} ~a")]
  [nonffi (no-complex asinh)])

(define-operator/libm (atan real) real
  [libm atan atanf]  
  [->c/double (curry format "atan(~a)")]
  [->c/mpfr (curry format "mpfr_atan(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\tan^{-1} ~a")]
  [nonffi (no-complex atan)])

(define-operator/libm (atan2 real real) real
  [libm atan2 atan2f] 
  [->c/double (curry format "atan2(~a, ~a)")]
  [->c/mpfr (curry format "mpfr_atan2(~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\tan^{-1}_* \\frac{~a}{~a}")]
  [nonffi (no-complex atan)])

(define-operator/libm (atanh real) real
  [libm atanh atanhf]  
  [->c/double (curry format "atanh(~a)")]
  [->c/mpfr (curry format "mpfr_atanh(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\tanh^{-1} ~a")]
  [nonffi (no-complex atanh)])

(define-operator/libm (cbrt real) real
  [libm cbrt cbrtf]  
  [->c/double (curry format "cbrt(~a)")]
  [->c/mpfr (curry format "mpfr_cbrt(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\sqrt[3]{~a}")]
  [nonffi (no-complex (λ (x) (expt x (/ 1 3))))])

(define-operator/libm (ceil real) real
  [libm ceil ceilf]  
  [->c/double (curry format "ceil(~a)")]
  [->c/mpfr (curry format "mpfr_ceil(~a, ~a)")]
  [->tex (curry format "\\left\\lceil~a\\right\\rceil")]
  [nonffi ceiling])

(define-operator/libm (copysign real real) real
  [libm copysign copysignf]  
  [->c/double (curry format "copysign(~a, ~a)")]
  [->c/mpfr (curry format "mpfr_copysign(~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\mathsf{copysign}\\left(~a, ~a\\right)")]
  [nonffi (λ (x y) (if (>= y 0) (abs x) (- (abs x))))])

(define-operator/libm (cos real) real
  [libm cos cosf]  
  [->c/double (curry format "cos(~a)")]
  [->c/mpfr (curry format "mpfr_cos(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\cos ~a")]
  [nonffi cos])

(define-operator/libm (cosh real) real
  [libm cosh coshf]  
  [->c/double (curry format "cosh(~a)")]
  [->c/mpfr (curry format "mpfr_cosh(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\cosh ~a")]
  [nonffi cosh])

(define-operator/libm (erf real) real
  [libm erf erff]  
  [->c/double (curry format "erf(~a)")]
  [->c/mpfr (curry format "mpfr_erf(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\mathsf{erf}\\left(~a\\right)")]
  [nonffi (no-complex erf)])

(define-operator/libm (erfc real) real
  [libm erfc erfcf]  
  [->c/double (curry format "erfc(~a)")]
  [->c/mpfr (curry format "mpfr_erfc(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\mathsf{erfc}\\left(~a\\right)")]
  [nonffi erfc])

(define-operator/libm (exp real) real
  [libm exp expf]  
  [->c/double (curry format "exp(~a)")]
  [->c/mpfr (curry format "mpfr_exp(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "e^{~a}")]
  [nonffi exp])

(define-operator (exp.c complex) complex
  [fl exp] 
  [->c/double (curry format "exp(~a)")]
  [->c/mpfr (const "/* ERROR: no complex support in C */")]
  [->tex (curry format "e^{~a}")]
  [nonffi exp])

(define-operator/libm (exp2 real) real
  [libm exp2 exp2f] 
  [->c/double (curry format "exp2(~a)")]
  [->c/mpfr (curry format "mpfr_exp2(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "2^{~a}")]
  [nonffi (no-complex (λ (x) (expt 2 x)))])

(define-operator/libm (expm1 real) real
  [libm expm1 expm1f]
  [->c/double (curry format "expm1(~a)")]
  [->c/mpfr (curry format "mpfr_expm1(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\mathsf{expm1}\\left(~a\\right)")]
  [nonffi (from-bigfloat bfexpm1)])

(define-operator/libm (fabs real) real
  [libm fabs fabsf]  
  [->c/double (curry format "fabs(~a)")]
  [->c/mpfr (curry format "mpfr_abs(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\left|~a\\right|")]
  [nonffi abs])

(define-operator/libm (fdim real real) real
  [libm fdim fdimf]  
  [->c/double (curry format "fdim(~a, ~a)")]
  [->c/mpfr (curry format "mpfr_dim(~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\mathsf{fdim}\\left(~a, ~a\\right)")]
  [nonffi (λ (x y) (max (- x y) 0))])

(define-operator/libm (floor real) real
  [libm floor floorf]  
  [->c/double (curry format "floor(~a)")]
  [->c/mpfr (curry format "mpfr_floor(~a, ~a)")]
  [->tex (curry format "\\left\\lfloor~a\\right\\rfloor")]
  [nonffi (λ (x) (floor x))])

(define-operator/libm (fma real real real) real
  [libm fma fmaf]  
  [->c/double (curry format "fma(~a, ~a, ~a)")]
  [->c/mpfr (curry format "mpfr_fma(~a, ~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\mathsf{fma}\\left(~a, ~a, ~a\\right)")]
  [nonffi (λ (x y z) (bigfloat->flonum (bf+ (bf* (bf x) (bf y)) (bf z))))])

(define-operator/libm (fmax real real) real
  [libm fmax fmaxf]  
  [->c/double (curry format "fmax(~a, ~a)")]
  [->c/mpfr (curry format "mpfr_fmax(~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\mathsf{max}\\left(~a, ~a\\right)")]
  [nonffi (λ (x y) (cond  [(nan? x) y] [(nan? y) x] [else (max x y)]))])

(define-operator/libm (fmin real real) real
  [libm fmin fminf]  
  [->c/double (curry format "fmin(~a, ~a)")]
  [->c/mpfr (curry format "mpfr_fmin(~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\mathsf{min}\\left(~a, ~a\\right)")]
  [nonffi (λ (x y) (cond  [(nan? x) y] [(nan? y) x] [else (min x y)]))])

(define (bffmod x mod)
  (bf- x (bf* (bftruncate (bf/ x mod)) mod)))

(define-operator/libm (fmod real real) real
  [libm fmod fmodf]  
  [->c/double (curry format "fmod(~a, ~a)")]
  [->c/mpfr (curry format "mpfr_fmod(~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "~a \\bmod ~a")]
  [nonffi (from-bigfloat bffmod)])

(define-operator/libm (hypot real real) real
  [libm hypot hypotf]  
  [->c/double (curry format "hypot(~a, ~a)")]
  [->c/mpfr (curry format "mpfr_hypot(~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\mathsf{hypot}\\left(~a, ~a\\right)")]
  [nonffi (from-bigfloat bfhypot)])

(define-operator/libm (j0 real) real
  [libm j0 j0f] [bf bfbesj0] [ival #f]
  [->c/double (curry format "j0(~a)")]
  [->c/mpfr (curry format "mpfr_j0(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "j_0\\left(~a\\right)")]
  [nonffi (from-bigfloat bfbesj0)])

(define-operator/libm (j1 real) real
  [libm j1 j1f] [bf bfbesj1] [ival #f]
  [->c/double (curry format "j1(~a)")]
  [->c/mpfr (curry format "mpfr_j1(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "j_1\\left(~a\\right)")]
  [nonffi (from-bigfloat bfbesj1)])

(define-operator/libm (lgamma real) real
  [libm lgamma lgammaf] [bf bflog-gamma] [ival #f]
  [->c/double (curry format "lgamma(~a)")]
  [->c/mpfr (curry format "mpfr_lngamma(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\mathsf{lgamma} \\left( ~a \\right)")]
  [nonffi log-gamma])

(define-operator/libm (log real) real
  [libm log logf]  
  [->c/double (curry format "log(~a)")]
  [->c/mpfr (curry format "mpfr_log(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\log ~a")]
  [nonffi (no-complex log)])

(define-operator (log.c complex) complex
  [fl log] 
  [->c/double (curry format "log(~a)")]
  [->c/mpfr (const "/* ERROR: no complex support in C */")]
  [->tex (curry format "\\log ~a")]
  [nonffi log])

(define-operator/libm (log10 real) real
  [libm log10 log10f] 
  [->c/double (curry format "log10(~a)")]
  [->c/mpfr (curry format "mpfr_log10(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\log_{10} ~a")]
  [nonffi (no-complex (λ (x) (log x 10)))])

(define-operator/libm (log1p real) real
  [libm log1p log1pf] 
  [->c/double (curry format "log1p(~a)")]
  [->c/mpfr (curry format "mpfr_log1p(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\mathsf{log1p}\\left(~a\\right)")]
  [nonffi (from-bigfloat bflog1p)])

(define-operator/libm (log2 real) real
  [libm log2 log2f] 
  [->c/double (curry format "log2(~a)")]
  [->c/mpfr (curry format "mpfr_log2(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\log_{2} ~a")]
  [nonffi (from-bigfloat bflog2)])

(define (bflogb x)
  (bffloor (bflog2 (bfabs x))))

(define-operator/libm (logb real) real
  [libm logb logbf]  
  [->c/double (curry format "logb(~a)")]
  [->c/mpfr (curry format "mpfr_set_si(~a, mpfr_get_exp(~a), MPFR_RNDN)")]
  [->tex (curry format "\\log_{b} ~a")]
  [nonffi (λ (x) (floor (bigfloat->flonum (bflog2 (bf (abs x))))))])

(define-operator/libm (pow real real) real
  [libm pow powf]  
  [->c/double (curry format "pow(~a, ~a)")]
  [->c/mpfr (curry format "mpfr_pow(~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "{~a}^{~a}")]
  [nonffi (no-complex expt)])

(define-operator (pow.c complex complex) complex
  [fl expt] 
  [->c/double (curry format "pow(~a, ~a)")]
  [->c/mpfr (const "/* ERROR: no complex support in C */")]
  [->tex (curry format "{~a}^{~a}")]
  [nonffi expt])

(define (bfremainder x mod)
  (bf- x (bf* (bfround (bf/ x mod)) mod)))

(define-operator/libm (remainder real real) real
  [libm remainder remainderf]   
  [->c/double (curry format "remainder(~a, ~a)")]
  [->c/mpfr (curry format "mpfr_remainder(~a, ~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "~a \\mathsf{rem} ~a")]
  [nonffi remainder])

(define-operator/libm (rint real) real
  [libm rint rintf]  
  [->c/double (curry format "rint(~a)")]
  [->c/mpfr (curry format "mpfr_rint(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\mathsf{rint} ~a")]
  [nonffi round])

(define-operator/libm (round real) real
  [libm round roundf]  
  [->c/double (curry format "round(~a)")]
  [->c/mpfr (curry format "mpfr_round(~a, ~a)")]
  [->tex (curry format "\\mathsf{round} ~a")]
  [nonffi round])

(define-operator/libm (sin real) real
  [libm sin sinf]  
  [->c/double (curry format "sin(~a)")]
  [->c/mpfr (curry format "mpfr_sin(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\sin ~a")]
  [nonffi sin])

(define-operator/libm (sinh real) real
  [libm sinh sinhf]  
  [->c/double (curry format "sinh(~a)")]
  [->c/mpfr (curry format "mpfr_sinh(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\sinh ~a")]
  [nonffi sinh])

(define-operator/libm (sqrt real) real
  [libm sqrt sqrtf]  
  [->c/double (curry format "sqrt(~a)")]
  [->c/mpfr (curry format "mpfr_sqrt(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\sqrt{~a}")]
  [nonffi (no-complex sqrt)])

(define-operator (sqrt.c complex) complex
  [fl sqrt] 
  [->c/double (curry format "sqrt(~a)")]
  [->c/mpfr (const "/* ERROR: no complex support in C */")]
  [->tex (curry format "\\sqrt{~a}")]
  [nonffi sqrt])

(define-operator/libm (tan real) real
  [libm tan tanf]  
  [->c/double (curry format "tan(~a)")]
  [->c/mpfr (curry format "mpfr_tan(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\tan ~a")]
  [nonffi tan])

(define-operator/libm (tanh real) real
  [libm tanh tanhf]  
  [->c/double (curry format "tanh(~a)")]
  [->c/mpfr (curry format "mpfr_tanh(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\tanh ~a")]
  [nonffi tanh])

(define-operator/libm (tgamma real) real
  [libm tgamma tgammaf]  [ival #f]
  [->c/double (curry format "tgamma(~a)")]
  [->c/mpfr (curry format "mpfr_gamma(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "\\Gamma\\left(~a\\right)")]
  [nonffi gamma])

(define-operator/libm (trunc real) real
  [libm trunc truncf]  
  [->c/double (curry format "trunc(~a)")]
  [->c/mpfr (curry format "mpfr_trunc(~a, ~a)")]
  [->tex (curry format "\\mathsf{trunc}\\left(~a\\right)")]
  [nonffi truncate])

(define-operator/libm (y0 real) real
  [libm y0 y0f] [bf bfbesy0] [ival #f] 
  [->c/double (curry format "y0(~a)")]
  [->c/mpfr (curry format "mpfr_y0(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "y_0\\left(~a\\right)")]
  [nonffi (from-bigfloat bfbesy0)])

(define-operator/libm (y1 real) real
  [libm y1 y1f] [bf bfbesy1] [ival #f]
  [->c/double (curry format "y1(~a)")]
  [->c/mpfr (curry format "mpfr_y1(~a, ~a, MPFR_RNDN)")]
  [->tex (curry format "y_1\\left(~a\\right)")]
  [nonffi (from-bigfloat bfbesy1)])

(define (if-fn test if-true if-false) (if test if-true if-false))
(define (and-fn . as) (andmap identity as))
(define (or-fn  . as) (ormap identity as))

(define (!=-fn . args)
  (not (check-duplicates args =)))

(define (bf!=-fn . args)
  (not (check-duplicates args bf=)))

(define-operator (if bool real real) real ; types not used, special cased in type checker
  [fl if-fn] [bf if-fn] 
  [->c/double (curry format "~a ? ~a : ~a")]
  [->c/mpfr
   (λ (out c a b)
     (format "if (mpfr_get_si(~a, MPFR_RNDN)) { mpfr_set(~a, ~a, MPFR_RNDN); } else { mpfr_set(~a, ~a, MPFR_RNDN); }" c out a out b))]
  [->tex (curry format "~a ? ~a : ~a")]
  [nonffi if-fn])

(define ((infix-joiner x) . args)
  (string-join args x))

(define-operator (== real real) bool
  ; Override number of arguments
  [type #hash((* . (((* real) bool))))] [args '(*)]
  [fl (comparator =)] 
  [->c/double (curry format "~a == ~a")]
  [->c/mpfr (curry format "mpfr_set_si(~a, mpfr_cmp(~a, ~a) == 0, MPFR_RNDN)")] ; TODO: cannot handle variary =
  [->tex (infix-joiner " = ")]
  [nonffi (comparator =)])

(define-operator (complex real real) complex
  ; Override number of arguments
  [fl make-rectangular]  [ival #f]
  [->c/double (const "/* ERROR: no complex support in C */")]
  [->c/mpfr (const "/* ERROR: no complex support in C */")]
  [->tex (curry format "~a + ~a i")]
  [nonffi make-rectangular])

(define-operator (re complex) real
  ; Override number of arguments
  [fl real-part] 
  [->c/double (const "/* ERROR: no complex support in C */")]
  [->c/mpfr (const "/* ERROR: no complex support in C */")]
  [->tex (curry format "\\Re(~a)")]
  [nonffi real-part])

(define-operator (im complex) real
  ; Override number of arguments
  [fl imag-part]
  [->c/double (const "/* ERROR: no complex support in C */")]
  [->c/mpfr (const "/* ERROR: no complex support in C */")]
  [->tex (curry format "\\Im(~a)")]
  [nonffi imag-part])

(define-operator (conj complex) complex
  ; Override number of arguments
  [fl conjugate] 
  [->c/double (const "/* ERROR: no complex support in C */")]
  [->c/mpfr (const "/* ERROR: no complex support in C */")]
  [->tex (curry format "\\overline{~a}")]
  [nonffi conjugate])

(define-operator (!= real real) bool
  ; Override number of arguments
  [type #hash((* . (((* real) bool))))] [args '(*)]
  [fl !=-fn] 
  [->c/double (curry format "~a != ~a")]
  [->c/mpfr (curry format "mpfr_set_si(~a, mpfr_cmp(~a, ~a) != 0, MPFR_RNDN)")] ; TODO: cannot handle variary !=
  [->tex (infix-joiner " \\ne ")]
  [nonffi !=-fn])

(define-operator (< real real) bool
  ; Override number of arguments
  [type #hash((* . (((* real) bool))))] [args '(*)]
  [fl (comparator <)] 
  [->c/double (curry format "~a < ~a")]
  [->c/mpfr (curry format "mpfr_set_si(~a, mpfr_cmp(~a, ~a) < 0, MPFR_RNDN)")] ; TODO: cannot handle variary <
  [->tex (infix-joiner " \\lt ")]
  [nonffi (comparator <)])

(define-operator (> real real) bool
  ; Override number of arguments
  [type #hash((* . (((* real) bool))))] [args '(*)]
  [fl (comparator >)] 
  [->c/double (curry format "~a > ~a")]
  [->c/mpfr (curry format "mpfr_set_si(~a, mpfr_cmp(~a, ~a) > 0, MPFR_RNDN)")] ; TODO: cannot handle variary >
  [->tex (infix-joiner " \\gt ")]
  [nonffi (comparator >)])

(define-operator (<= real real) bool
  ; Override number of arguments
  [type #hash((* . (((* real) bool))))] [args '(*)]
  [fl (comparator <=)] 
  [->c/double (curry format "~a <= ~a")]
  [->c/mpfr (curry format "mpfr_set_si(~a, mpfr_cmp(~a, ~a) <= 0, MPFR_RNDN)")] ; TODO: cannot handle variary <=
  [->tex (infix-joiner " \\le ")]
  [nonffi (comparator <=)])

(define-operator (>= real real) bool
  ; Override number of arguments
  [type #hash((* . (((* real) bool))))] [args '(*)]
  [fl (comparator >=)] 
  [->c/double (curry format "~a >= ~a")]
  [->c/mpfr (curry format "mpfr_set_si(~a, mpfr_cmp(~a, ~a) >= 0, MPFR_RNDN)")] ; TODO: cannot handle variary >=
  [->tex (infix-joiner " \\ge ")]
  [nonffi (comparator >=)])

(define-operator (not bool) bool
  [fl not]  
  [->c/double (curry format "!~a")]
  [->c/mpfr (curry format "mpfr_set_si(~a, !mpfr_get_si(~a, MPFR_RNDN), MPFR_RNDN)")]
  [->tex (curry format "\\neg ~a")]
  [nonffi not])

(define-operator (and bool bool) bool
  ; Override number of arguments
  [type #hash((* . (((* bool) bool))))] [args '(*)]
  [fl and-fn]  
  [->c/double (curry format "~a && ~a")]
  [->c/mpfr (curry format "mpfr_set_si(~a, mpfr_get_si(~a, MPFR_RNDN) && mpfr_get_si(~a, MPFR_RNDN), MPFR_RNDN)")]
  [->tex (infix-joiner " \\land ")]
  [nonffi and-fn])

(define-operator (or bool bool) bool
  ; Override number of arguments
  [type #hash((* . (((* bool) bool))))] [args '(*)]
  [fl or-fn]  
  [->c/double (curry format "~a || ~a")]
  [->c/mpfr (curry format "mpfr_set_si(~a, mpfr_get_si(~a, MPFR_RNDN) || mpfr_get_si(~a, MPFR_RNDN), MPFR_RNDN)")]
  [->tex (infix-joiner " \\lor ")]
  [nonffi or-fn])

(match-let ([(cons header rows) operators])
  (println (hash-keys rows)))
    
(define (exact-noncomplex-value? val)
  (and (not (and (complex? val) (not (real? val))))
       (exact? val)))

 
(define (eval-application op . args)
  (if (and (not (null? args)) (andmap (conjoin number? exact?) args))
      (with-handlers ([exn:fail:contract:divide-by-zero? (const #f)])
        (define fn (operator-info op 'nonffi))
        (define res (apply fn args))
        
        (and (exact-noncomplex-value? res)
             (val-to-type res)))
      false))
