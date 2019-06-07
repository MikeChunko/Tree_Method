#lang racket

;; Michael Chunko
;; mchunko
;; I pledge that I have abided by the Stevens Honor System

;; This will serve as a way of replicating the tree method for logic in Scheme (eopl)
;; Note: a is represented as (a #t) and (not a) is represented as (a #f)
;; Also, for the purposes of this, we will use a different notation for boolean operators so as not to conflict with Scheme
;; The redefinitions follow in the format (original, redefinition)
;; and        : '_and
;; or         : '_or
;; not        : '_not
;; if-then    : if
;; if-only-if : iff
;; A is true  : '(A #t)
;; A is false : '(A #f)
;; Note: All procedures (except if, iff, (A #t), (A #f)) must be defined using list (for consistency with the tick operator)
;; EG: If you wanted to test for
;;       prop 1: A is false
;;       prop 2: A or B
;;       prop 3: C -> D     (this one isn't used in the argument at all, it's just here for an example of how to use 'if' and 'iff'
;;       conclusion: B
;; Enter it as: (tree-method-starter (list '(B #t) '(A #f) (list '_or '(A #t) '(B #t)) (if '(C #t) '(D #t))))   (btw this returns #t, as this is true)
;; (Yes I know this way of entering is annoying, but its what works
;; Since this is so hard to understand, if you're having a hard time with it shoot me an email and I'll try to clear things up


;; Define tree-method-starter
;; Serves as the base for the tree method
;; Takes in a list of propositions where the conclusion is assumed to be the first
;; Returns #t if the statements are a tautology
;; Returns #f otherwise
(define (tree-method-starter propositions)
  (not (tree-method (cons (list '_not (car propositions)) (cdr propositions)) '())))

(define (tree-method propositions knowns)
  (cond
    [(null? propositions)
      (check-knowns knowns (make-set knowns))]
    [else
      (evaluate-propositions (car propositions) (cdr propositions) knowns)]))


;; Define evaluate-propositions
;; The meat-and-bones of the tree method
;; This turns propositions into a series of knowns (lone variables)
;; Note that 'if' and 'iff' are seperate functions and not evaluated here
(define (evaluate-propositions prop propositions knowns)
  (cond
    [(equal? (car prop) '_and) ;; and
     (evaluate-propositions (cadr prop) (append propositions (list (caddr prop))) knowns)]
    [(equal? (car prop) '_or)  ;; or
     (or (evaluate-propositions (cadr prop) propositions knowns)
          (evaluate-propositions (caddr prop) propositions knowns))]
    [(equal? (car prop) '_not) ;; not
     (evaluate-propositions (evaluate-not (cadr prop)) propositions knowns)]
    [else                    ;; normal variable
     (tree-method propositions (append knowns (list prop)))]))


;; Define evaluate-not
;; This function serves just to evaluate the interior of a 'not' propositions
;; This seperate function is needed since 'not's are somewhat hard to evaluate
(define (evaluate-not prop)
  (cond
    [(equal? (car prop) '_and) ;; and
     (list '_or (evaluate-not (cadr prop)) (evaluate-not (caddr prop)))]
    [(equal? (car prop) '_or)  ;; or
     (list '_and (evaluate-not (cadr prop)) (evaluate-not (caddr prop)))]
    [(equal? (car prop) '_not) ;; not
     (cadr prop)]
    [else                    ;; normal variable
     (list (car prop) (not (cadr prop)))]))


;; Define if
;; Wrapper function for if-then
;; Needed so that I only have to make 'not' work for 'and', 'or', and lone variables
(define (if p q)
  (list '_or (list '_not p) q))


;; Define iff
;; Wrapper for if-and-only-if
;; Needed so that I only have to make 'not' work for 'and', 'or', and lone variables
(define (iff p q)
  (list '_and (if p q) (if q p)))


;; Define check-knowns
;; Checks if there are any contradictions in the list of knowns
;; (exists an a such that a in knowns and (not a) in knowns
;; Return #f if there are conflicts
;; Return #t if there are no conflicts
(define (check-knowns original knowns)
  (cond
    [(null? knowns)
     (error (make-set original))] ;; original is needed so that the full list of knowns can be thrown
    [(and (element? (list (caar knowns) #t) knowns) (element? (list (caar knowns) #f) knowns))
     #f]
    [else
     (check-knowns original (cdr knowns))]))


;; Define make-set
;; Taken from lab_relations_2
(define (make-set list-of-items)
  (cond
    [(null? list-of-items)  ;An empty list can have no duplicates,
     '()]                   ;so just return an empty list.
    [(element? (car list-of-items) (cdr list-of-items))
     (make-set (cdr list-of-items))]
    [else
     (cons (car list-of-items) (make-set (cdr list-of-items)))]))


;; Define element?
;; Taken from lab_relations_2
(define (element? item list-of-items)
  (cond
    [(null? list-of-items)                 ;Is our "set" empty?
      #f]                                  ;If empty, not an element!
    [(equal? item (car list-of-items))     ;Is our item first in list?
     #t]                                   ;Yes?  Then it's an element!
    [else
     (element? item (cdr list-of-items))]));No? Check the rest.