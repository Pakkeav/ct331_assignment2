#lang racket


(define tree '(((()1())5(()8()))10((()16())18((()19())51()))))

;A)
(define (sort tree)
   (cond[(not(empty? tree))(sort (car tree))
     (printf" ~a "(cadr tree))
     (sort (caddr tree))]))

>(sort tree)

;B)
(define (present item tree)
  (cond[(empty? tree)#f]
       [(equal? item(cadr tree))#t] 
         [(< item (cadr tree))(present item (car tree))]
          [(> item (cadr tree))(present item (caddr tree))]))

>(present 11 tree)

;C)
(define (insert item tree)
  (cond[(empty? tree)(list empty item empty)]
         [(< item (cadr tree))(list (insert item (car tree)) (cadr tree) (caddr tree))]
         [(= item (cadr tree))tree]
         [(> item (cadr tree))(list (car tree) (cadr tree) (insert item (caddr tree)))]))

>(insert 2 tree)


;D)
(define (insert_list list tree )
    (insert_list (insert (car list) tree)( cdr list)))

>(insert_list '(2 44 5 33) tree)