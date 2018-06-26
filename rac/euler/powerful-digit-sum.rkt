#lang racket

(require "utils.rkt")
;; A googol (10^100) is a massive number: one followed by one-hundred zeros; 100^100 is almost unimaginably large: one followed by two-hundred zeros. Despite their size, the sum of the digits in each number is only 1.

;; Considering natural numbers of the form, ab, where a, b < 100, what is the maximum digital sum?

;; first we'll have a look at 99 ^ 99 to see what kind of number that is

;; It's probably reducing from there to get the most nines

;(expt 99 99); - which is huge
;﻿; 369729637649726772657187905628805440595668764281741102430259972423552570455277523421410650010128232727940978889548326540119429996769494359451621570193644014418071060667659301384999779999159200499899

(define (digital-sum n)
  (apply + (integer->list n)))

;(digital-sum (expt 99 99)); - 936

;; ok we can look at these numbers going down

;(argmax cadr (map (λ (x) (list x (digital-sum (expt x 99)))) (range 1 100))); 955
;(argmax cadr (map (λ (x) (list x (digital-sum (expt x 98)))) (range 1 100))); 970
;(argmax cadr (map (λ (x) (list x (digital-sum (expt x 97)))) (range 1 100))); 945
;(argmax cadr (map (λ (x) (list x (digital-sum (expt x 96)))) (range 1 100))); 928
(argmax cadr (map (λ (x) (list x (digital-sum (expt x 95)))) (range 1 100))); 99^95 972
;(argmax cadr (map (λ (x) (list x (digital-sum (expt x 94)))) (range 1 100))); 909
;(argmax cadr (map (λ (x) (list x (digital-sum (expt x 93)))) (range 1 100))); 872
;(argmax cadr (map (λ (x) (list x (digital-sum (expt x 92)))) (range 1 100))); 883
;(argmax cadr (map (λ (x) (list x (digital-sum (expt x 91)))) (range 1 100))); 860
;(argmax cadr (map (λ (x) (list x (digital-sum (expt x 90)))) (range 1 100))); 883
;(argmax cadr (map (λ (x) (list x (digital-sum (expt x 89)))) (range 1 100))); 848
;(argmax cadr (map (λ (x) (list x (digital-sum (expt x 88)))) (range 1 100)))
;(argmax cadr (map (λ (x) (list x (digital-sum (expt x 87)))) (range 1 100)))
;(argmax cadr (map (λ (x) (list x (digital-sum (expt x 86)))) (range 1 100)))
;(argmax cadr (map (λ (x) (list x (digital-sum (expt x 85)))) (range 1 100)))
;(argmax cadr (map (λ (x) (list x (digital-sum (expt x 84)))) (range 1 100)))
;(argmax cadr (map (λ (x) (list x (digital-sum (expt x 83)))) (range 1 100)))
;(argmax cadr (map (λ (x) (list x (digital-sum (expt x 82)))) (range 1 100)))
;(argmax cadr (map (λ (x) (list x (digital-sum (expt x 81)))) (range 1 100)))
(argmax cadr (map (λ (x) (list x (digital-sum (expt x 80)))) (range 1 100)))
(argmax cadr (map (λ (x) (list x (digital-sum (expt x 79)))) (range 1 100)))
(argmax cadr (map (λ (x) (list x (digital-sum (expt x 78)))) (range 1 100)))
(argmax cadr (map (λ (x) (list x (digital-sum (expt x 77)))) (range 1 100)))
(argmax cadr (map (λ (x) (list x (digital-sum (expt x 76)))) (range 1 100)))
(argmax cadr (map (λ (x) (list x (digital-sum (expt x 75)))) (range 1 100)))
(argmax cadr (map (λ (x) (list x (digital-sum (expt x 74)))) (range 1 100)))


; ok that was fun but

(argmax car (map (λ (y) (argmax car (map (λ (x) (list (digital-sum (expt x y)) x y)) (range 1 100)))) (range 1 100)))
