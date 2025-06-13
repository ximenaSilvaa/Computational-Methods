;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Functional Programming Racket|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; DEFINE FUNCTIONS AND VARIABLES
(define standard-person "Horatiu")
(define x 10)

(define persone-name standard-person)

(define m-in-km 1000)

(check-expect (sum 1 2) 3)
(check-within (sum 1 2) 3 0.1)
(define (sum x y)
  (+ x y))

(define(km->m km)
  (* km m-in-km))

; this is an exact number (= #i3 3)
(= #i3 3)


;BASIC FUNCTIONS
;raises 3 to the 2
(expt 3 2)
(max 2 3)
(min 2 1)
(floor 1.02)
(ceiling 1.9)

;PREDICATE
(equal? "five" "five")
(number? 3)
(string? "HI")

;ARRAY CONS
(define a (cons 'b empty))

(define students (cons 'Mike (cons 'Bob empty)))
(first (rest students))
(empty? students)

;LIST
;you cant at things to a list un less is with cons (nested lists)
(define Y (list 'A 'B 'C))
(first Y)
(append a (list 'Y))

;STRUCT
;encapsulating all the data = struct = structure
(define-struct card (type value))
(define demo (make-card 'hearts 7))
(card-type demo)
(card-value demo)

(define (create-card type value)
  (make-card type value))

;; Pure structural recursion
;; Acumulative rescursion
;; Generative recursion

;; sum1: (listof Num) -> Num
;; Recursively sums all the numbers in a list
;; 'cond' is a conditional expression, similar to if-else if-else chains in other languages.
;; It checks each condition from top to bottom and executes the corresponding expression for the first true condition.
(define (sum1 lon)
  (cond
    [(empty? lon) 0] ; if the list is empty, return 0 (base case)
    [else (+ (first lon) (sum1 (rest lon)))])) ; otherwise, add the first number to the sum of the rest

;

