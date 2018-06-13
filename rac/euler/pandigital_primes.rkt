;;;;;;;;;;;;;;;;;;;; The following problem is from Project Euler.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; PROBLEM AS STATED
#lang racket


;We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is also prime.

;What is the largest n-digit pandigital prime that exists?


;;;;;;;;;;;;;;;;;;;; https://projecteuler.net/problem=41
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (prime? n)
  (define (inner n i); = has-a-factor?
    (cond
      ; end search negative when i * i is bigger than n
      ((> i (/ n i)) #f)
      ; found a factor
      ((= (modulo n i) 0) #t)
      ; keep looking
      (else (inner n (+ i 2)))))
  ; is 2 or neither divisible by 2 nor divisible by an odd number 
  (or (= n 2) (nor (inner n 3) (= (modulo n 2) 0))))


(define (number->list n acc)
  (let ([rem  (remainder n 10)])
    (if (= n 0)
        acc
        (number->list (/ (- n rem) 10) (cons rem acc)))))

(define (list->number li)
  (letrec ([inner
            (lambda (li string-acc)
              (cond
                ((null? li) (string->number string-acc))
                (else (inner (cdr li) (string-append string-acc (number->string (car li)))))))])
    (inner li "")))


(define (qsort li)
  (if (< (length li) 2)
      li
      (append
       (qsort (filter (lambda (x)
                        (< x (car li))) li))
       (filter (lambda (x)
                 (= x (car li))) li)
       (qsort (filter
               (lambda (x)
                 (> x (car li))) li)))))

(define (next-exists? li)
  (> (length li) 1))

(define (next li)
  (car (cdr li)))

(define (conscar li1 li2)
  (cons (car li1) li2))


(define (swapped li left-pos right-pos)
  (append (take li left-pos); before left
          (list (list-ref li right-pos));left swapped with right
          ; up to right-pos
          (take (drop li (add1 left-pos)) (- right-pos left-pos 1))
          (list (list-ref li left-pos)); right swapped with left
          (drop li (add1 right-pos)) ; the rest after right
          ))

(define (reverse-before li pos)
  (letrec ([inner
            (lambda (li pos acc)
              (if (= pos 0)
                  (append acc li)
                  (inner (cdr li) (sub1 pos) (conscar li acc))))])
    (inner li pos empty)))


; lexicograph/find next larger permutation of number
(define (next-permutation n)
  (letrec (
           [original-li
            (reverse (number->list n empty))]

           [piv
            (lambda (li pos)
              (cond
                ((not (next-exists? li)) 'none)
                ((< (next li) (car li)) pos)
                (else (piv (cdr li) (add1 pos)))))]

           [pivot-pos (piv original-li 1)]
           [pivot-val (list-ref original-li pivot-pos)]

           [successor; 'inversion' before pivot proves this exists
            (lambda (li pos)
              (if (> (car li) pivot-val)
                  pos
                  (successor (cdr li) (add1 pos))))]
           [successor-pos (successor original-li 0)]
           )
    (list->number
     (reverse
      (reverse-before
       (swapped original-li successor-pos pivot-pos) pivot-pos)))))

; prev permutation the ugly copy paste way
(define (prev-permutation n)
  (letrec (
           [original-li
            (reverse (number->list n empty))]

           [piv
            (lambda (li pos)
              (cond
                ((not (next-exists? li)) 'none)
                ((> (next li) (car li)) pos)
                (else (piv (cdr li) (add1 pos)))))]

           [pivot-pos (piv original-li 1)]
           [pivot-val (list-ref original-li pivot-pos)]

           [successor; 'inversion' before pivot proves this exists
            (lambda (li pos)
              (if (< (car li) pivot-val)
                  pos
                  (successor (cdr li) (add1 pos))))]
           [successor-pos (successor original-li 0)]
           )
    (list->number
     (reverse
      (reverse-before
       (swapped original-li successor-pos pivot-pos) pivot-pos)))
    ))

  

(define (find-n n); please sort n largest to smallest
  (if (prime? n)
      n
      (find-n (prev-permutation  n))))

; 3210 -> 3201 -> 3120 - > 3102 -> 3021
;(prev-permutation 3021); -> 3012

(find-n 7654321); it turns out that a larger number doesn't exist
