;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname m02-status-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))

(require spd/tags)

(@assignment lectures/m02-status)

(@cwl dana28cl) ;replace ??? with your cwl

#|

;; Is this function correct?? (ignore the fact that it is commented out)

(define (can-vote? s)
  (= s 1))




;; How about now? Is it correct now?

(@htdf can-vote?)
(@signature Natural -> Boolean)
;; produces true if a person with given status is eligible to vote
(check-expect (can-vote? 0) false)
(check-expect (can-vote? 1) true) programmer here did not fully explain what
;; information 0 and 1 stand for, so it could be confusing reading this function

(@template-origin Natural)

(define (can-vote? s)
  (= s 1))


;; What's the problem?

|#
(@problem 1)
;; adult and minor
;; get the form of the information from the
;;Htdd --> fixed number of distinct items
;; adult is a single thing = this category is 1 thing, same for minor
; for cases like that we use "enumeration" (based on what the Htdd recipe says
; enumeration refers to data that is one category among many"

(@htdd Status)
; Status is one of: (this is the type comment)
; "minor" - 0
; "adult" - 1
;; interp. the legal age category of a citizen (make sure it's not long)
;; <examples> e redundant for enumerations>

(@dd-template-rules one-of ; 2 subclasses
                    atomic-distinct; "minor"
                    atomic-distinct; "adult"
                    )

(define (fn-for-status- s)
  (cond [(string=? s "minor") (...)]
        [(string=? s "adult") (...)]))

(@htdf can-vote?)
(@signature Status -> Boolean)
;; produces true if a person with given status is eligible to vote

;(define (can-vote? s) false); stub

(check-expect (can-vote? "minor") false)
(check-expect (can-vote? "adult") true)

(@template-origin Status)

(@template
 (define (can-vote? s)
   (cond [(string=? s "minor") (...)]
         [(string=? s "adult") (...)])))

(define (can-vote? s)
  (cond [(string=? s "minor") false]
        [(string=? s "adult") true]))


