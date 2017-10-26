#lang racket

;;A)
(define (ins_beg lis1 lis2)
  (cons lis1 lis2))

>(ins_beg 'a '(b c d))
>(ins_beg '(a b) '(b c d))

;;B)
(define (ns_end lis1 lis2)
  (cons lis2 lis1))

>(ns_end 'a '(b c d))
>(ns_end '(a b) '(b c d))

;;C)
 (define (cout_top_level  ls)
  (if (null? ls)
  0
  (+ 1 (cout_top_level  (cdr ls)))))

>(cout_top_level  '(2 1 3 6 6 3 5 7))

;;D)
(define (count_instances x ls)
  (if (null? ls)
   0
     (if (equal? x (car ls))
	(+ 1 (count_instances x (cdr ls)))
	(count_instances x (cdr ls)))))

>(count_instances  '3 '(1 3 6 6 3 3 1))

;;E)
(define (count_instances_tr x ls)
   (count_instances_tr_1 x ls 0))

(define (count_instances_tr_1 x ls num)
 (if (null? ls) num
    (if (equal? x (car ls))
	 (count_instances_tr_1 x (cdr ls)(+ 1 num))
	(count_instances_tr_1 x (cdr ls) num))))

>(count_instances_tr  '3 '(1 3 6 6 3 3 1))

;;F)
(define (count_instances_deep x ls)
   (count_instances_deep_1 x ls 0))

(define (count_instances_deep_1 x ls num)
  (cond[ (empty? ls) 0]
    [(equal? x (car ls))  (count_instances_deep_1 x (cdr ls)(+ 1 num))]
     [   ((list? (car ls)) ((count_instances_deep_1 x (car ls) (+ 1 num)) (count_instances_deep_1 x (cdr ls)(+ 1 num))))]
    [else (count_instances_deep_1 x (cdr ls) num)]))

>(count_instances_deep '3 '((5 3) (3 4) 7 (1 8) 3 (5 0 3 3 7 9 3 5 7 3)))









