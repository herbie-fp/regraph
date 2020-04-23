#lang racket
(require (only-in xml write-xexpr))
(require racket/date)
(module+ test (require rackunit))

(define (total-time row)
  (~r (first row) #:precision '(= 1)))
(define (congruence-time row)
  (~r (+ (second row) (third row)) #:precision '(= 1)))
(define (congruence-detail row)
  (format " (~a + ~a)" (~r (second row) #:precision '(= 1)) (~r (third row) #:precision '(= 1))))
(define (search-time row)
  (~r (fourth row) #:precision '(= 1)))

(define (average-rows rows)
  (map (lambda (ele) (exact->inexact (/ ele (length rows))))
       (apply map + rows)))

(define (average-table table)
  (define total-len
    (apply +
           (for/list ([(key data) table])
             (length data))))
  (map
   (lambda (sum) (exact->inexact (/ sum total-len)))
   (apply map +
          (for/list ([(key data) table])
            (apply map + data)))))

(module+ test
  (define test-table (make-hash `((a (1 2 3) (2 4 9) (1 1 1)) (b (10 10 10)))))
  (println test-table)
  (check-equal? (average-table test-table) (list (exact->inexact 14/4)
                                                 (exact->inexact 17/4)
                                                 (exact->inexact 23/4))))


(define (data-table c1-name data-um data-rb)
  `(table
    (thead
     (tr (th ,c1-name) (th ([colspan "2"]) "Total (ms)") (th ([colspan "2"]) "Congruence (ms)") (th ([colspan "2"]) "Search (ms)"))
     (tr ([class "right"]) (th) (th "UM") (th "RB") (th "UM") (th "RB") (th "UM") (th "RB")))
    (tbody
     ,@(for/list ([(benchname um-rows) data-um])
         (define rb-rows (hash-ref data-rb benchname))
         (define um (average-rows um-rows))
         (define rb (average-rows rb-rows))
         `(tr (th ,benchname)
              (td ,(total-time um)) (td ,(total-time rb))
              (td ,(congruence-time um))
              (td (span ([title ,(congruence-detail rb)]))
                  ,(congruence-time rb))
              (td ,(search-time um)) (td ,(search-time rb)))))))

(define (make-html port data-um data-rb counts-same?)
  (define averaged-um (average-table data-um))
  (define averaged-rb (average-table data-rb))
  (define congruence-speedup (/ (+ (second averaged-um) (third averaged-um))
                                (+ (second averaged-rb) (third averaged-rb))))
  (define searching-speedup (/ (fourth averaged-um) (fourth averaged-rb)))
  (define overall-speedup (/ (first averaged-um) (first averaged-rb)))

  (write-xexpr
   `(html
     (head
      (meta ([charset "utf-8"]))
      (link ([rel "stylesheet"] [href "index.css"]))
      (title "Regraph evaluation for " ,(date->string (current-date))))
     (body
      (h1  "Regraph evaluation for " ,(date->string (current-date)))
      ,(if counts-same?
           `(p "Match counts correct!")
           `(p
             ([class "error-text"])
             "Match counts differed!"))
      (p "Rebuilding is " (strong ,(~r (* (- overall-speedup 1) 100) #:precision '(= 2)) "%") " faster,"
         " with " (strong ,(~r congruence-speedup #:precision '(= 2)) "×") " faster congruence closure"
         " and " (strong ,(~r (* (- searching-speedup 1) 100) #:precision '(= 2)) "%") " faster searching.")

      ,(data-table "Suite" data-um data-rb)
      
      (figure
       (img ([src "search-time.png"]))(img ([src "total-time.png"]))
       (img ([src "congruence-closure-time.png"])))
      ))
   port))

(define (match-counts-same? port1 port2)
  (define l1 (read port1))
  (define l2 (read port2))
  (if
   (equal? eof l1)
   (equal? eof l2)
   (and (equal? l1 l2) (match-counts-same? port1 port2))))


(define (read-file port)
  (for/list ([line (in-port read-line port)])
    (let ([line (string-split (string-trim line) ",")])
      (cons (first line) (map string->number (rest line))))))

;; table is benchmark name -> all data for that benchmark
(define (make-hash-list associations)
  (define table (make-hash))
  (for ([row associations])
    (hash-update! table (first row)
                  (lambda (existing) (cons (rest row) existing))
                  (list (rest row))))
  table)

(module+ main
  (command-line 
   #:args (table-um table-rb match-counts-um match-counts-rb output)
   (call-with-output-file
     output #:exists 'replace
     (λ (p)
       (make-html p
                  (make-hash-list (call-with-input-file table-um read-file))
                  (make-hash-list (call-with-input-file table-rb read-file))
                  (match-counts-same? (open-input-file match-counts-um)
                                      (open-input-file match-counts-rb)))))))
