#lang racket

; calculate sum of a list
(define (sumList lis)
  (if (empty? lis) 0 (+ (car lis) (sumList (cdr lis)))))

(define (sumListr lis)
  (foldl + 0 lis))

; calculate mean  of a list
(define (mean lis)
  (/ (sumList lis) (length lis)))

;calculate median of a list
(define (median lis)
(define lis2(sort lis <))
  (define len(length lis2))
  (if (even? len)
      (/ (+ (list-ref lis2 (quotient len 2))
            (list-ref lis2 (quotient (- len 1) 2)))
         2)
      (list-ref lis2 (quotient len 2))))

;calculate mode of a list

;base case - empty list ==> return 0
(define (countOccurrences atm lis)
  (cond
    [(empty? lis) 0]
    [(equal? atm(car lis)) (+ 1 (countOccurrences atm (cdr lis)))]
    [else (countOccurrences atm (cdr lis))]))

(define (countAll lis1 lis2)
  (if (= (length lis1) 1) (list (cons (car lis1) (countOccurrences (car lis1) lis2)))
      (append (list (cons (car lis1) (countOccurrences (car lis1) lis2))) (countAll (cdr lis1) lis2))))

(define (pairorder a b)
  (> (cdr a) (cdr b)))

(define (mode lis)
 (let
    [
    (listcounts (countAll (remove-duplicates lis) lis))
    ]
   (car (car (sort listcounts pairorder)))))
 
    
;Calling the functions
(sumList '(1 2 3 3 4 5 5 5 6))
(mean '(1 2 3 3 4 5 5 5 6))
(median '(1 2 3 3 4 5 5 5 6 7))
(mode '(1 2 3 3 4 5 5 5 6 7))


