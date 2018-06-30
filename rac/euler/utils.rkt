#lang racket


; functions which have may well be used across multiple solutions

(provide prime?)

; deprecated
(provide number->list)
(provide list->number)

(provide integer->list)
(provide list->integer)

(provide number-cat)

(provide qsort)
(provide next-permutation)
(provide prev-permutation)
(provide factor)

(provide two-combs)
(provide memoize)

(define (memoize func)
  (let ([result-ht (make-hash)])
    (lambda args ; this is the rest-id pattern
      (when (not (hash-has-key? result-ht args))
        (hash-set! result-ht args (apply func args)))
      (hash-ref result-ht args))))

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

; this seems reasonably fast
(define (factor n)
  ;(display n) (newline) (newline)
  (letrec (
           [inner ; this misses twos
            (lambda (n i)
              ;(display n) (newline)
              (cond
                ((> i (/ n i)) (if (> n 1) (list n) '()))
                ((= (modulo n i) 0) (cons i (factor (/ n i))))
                (else (inner n (+ i 2)))))]
           [twos ; get all the two factors
            (lambda (n)
              (if
               (= (modulo n 2) 0)
               (cons 2 (twos (/ n 2)))
               '()))]
           ; we'll need n after dividing out all those twos
           [n-after-twos (/ n (apply * (twos n)))]
           )
    (append (twos n) (inner n-after-twos 3))))

(define (integer->list n)
  (number->list n '()))

(define (list->integer li)
  (list->number li))

;; external use depracated
;; (number->list 654 '()) -> '(6 5 4) ; Ineed to refactor this really
;; no need to expose acc
;; TODO make it return correct value for exactly 0
(define (number->list n acc)
  (let ([rem  (remainder n 10)])
    (if (= n 0)
        acc
        (number->list (/ (- n rem) 10) (cons rem acc)))))

;; to be depracated
(define (list->number li)
  (letrec ([inner
            (lambda (li string-acc)
              (cond
                ((null? li) (string->number string-acc))
                (else (inner (cdr li) (string-append string-acc (number->string (car li)))))))])
    (inner li "")))

(define (number-cat li)
  (string->number (apply string-append (map number->string li))))

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

(define (next-exists? li) ; helper function
  (> (length li) 1))

(define (next li) ; helper function
  (car (cdr li)))

(define (conscar li1 li2) ; helper function
  (cons (car li1) li2))


(define (swapped li left-pos right-pos) ; helper function
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



;; lexicograph/find next larger permutation of number
;; integer -> integer
(define (next-permutation n)
  (call/cc
   (lambda (return)
     (letrec (
              [original-li
               (reverse (integer->list n))]
              
              [piv
               (lambda (li pos)
                 (cond
                   ((not (next-exists? li)) (return #f))
                   ((< (next li) (car li)) pos)
                   (else (piv (cdr li) (add1 pos)))))]
              
              [pivot-pos (piv original-li 1)] ; initial position is from 1
              
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
          (swapped original-li successor-pos pivot-pos) pivot-pos)))))))


;; 0123 ->  9876 (must keep 0 otherwise big trouble with leading 9s 0s)
;; e.g. 908 -> 91 -> 8
;; probably needs to store as string until permuted
;; **** FIXME ****
;;(define (inverse-digits n)
;;  (list->integer
;;   (map
;;    (lambda (x) (- 9 x))
;;    (integer->list n))))

;; invert the digits
(define (prev-permutation n)
  (call/cc
   (lambda (return)
     (letrec (
              [original-li
               (reverse (integer->list n))]
              
              [piv
               (lambda (li pos)
                 (cond
                   ((not (next-exists? li)) (return #f))
                   ((> (next li) (car li)) pos)
                   (else (piv (cdr li) (add1 pos)))))]
              
              [pivot-pos (piv original-li 1)] ; initial position is from 1
              
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
          (swapped original-li successor-pos pivot-pos) pivot-pos)))))))

(define (slice li from to)
  (letrec
      (; gets function returning at index from provided list 
       [make-index-mapping 
        (lambda (li)
          (lambda (i)
            (list-ref li i)))])
    (map (make-index-mapping li) (range from to))))

;; why write general solution
(define (two-combs li)
  (filter
   identity
   (append-map (λ (x)
                 (map (λ (y)
                        (if (= x y) #f
                            (list x y))) li)) li)))

;; general solutions, but not optimized
(define (all-combs li)
  (cond
    [(null? li) '(())]
    [else
     (append
      (map (curry cons (car li)) (all-combs (cdr li)))
      (all-combs (cdr li)))]))

(define (n-combs li n)
  (filter (λ (x) (= (length x) n)) (all-combs li)))
