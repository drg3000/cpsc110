;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname pset-02-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #t)))
;; DO NOT PUT ANYTHING PERSONALLY IDENTIFYING BEYOND YOUR CWL IN THIS FILE.
;; YOUR CWLs WILL BE SUFFICIENT TO IDENTIFY YOU AND, IF YOU HAVE ONE, YOUR
;; PARTNER.
;;
(require spd/tags)
(require 2htdp/image)

(@assignment psets/pset-02);Do not edit or remove this tag

;; If you are:
;;   - A 110 or 107 student replace the first set of '???'s with your cwl.
;;     For problem sets, If you have a partner, please replace the second
;;     set of '???'s with their cwl.  Remember this, it is what you will
;;     do with these @cwl annotations for the whole course.
;;   - A UBC Extended Learning student, replace the first set of ??? with
;;     your email address as confirmed in the email you received from
;;     extended learning.  The handin password is also in that email.
;;     Remember this, it is what you will do with these @cwl annotations
;;     for the whole course.
;;   
(@cwl dana28cl)

(@problem 1)
;;
;; The following function design may have errors in it.  Please fix the error
;; or errors that you find.  Any changes you make should preserve the existing
;; design intent.  NOTE THAT when fixing an error in existing code the best
;; practice is to change as little of the code as possible.  That is because
;; in most organizations someone will have to review any changes you make,
;; and changing only what you really mean to change will make that person's
;; job easier.  Remember always that the goal is to be kind to your fellow
;; programmers.
;;
;; First uncomment the entire function design, and then fix the error.
;; If you are unable to find and fix the error, leave it commented out.
;; Note that when fixing a bug in an existing program your goal should
;; be to have a light touch.  If there is a small simple way to fix
;; something then do it the small and simple way.
;;
;; Your solution must include @htdf, @signature, and @template-origin tags.

(@htdf stack)
(@signature Image Image -> Image)
;;stack images, widest on bottom, first on bottom if same width

(check-expect (stack (rectangle 10 5 "solid" "blue")
                     (rectangle 20 9 "solid" "red"))
              (above (rectangle 10 5 "solid" "blue")
                     (rectangle 20 9 "solid" "red")))

(check-expect (stack (rectangle 20 5 "solid" "blue")
                     (rectangle 20 9 "solid" "red"))
              (above (rectangle 20 9 "solid" "red")
                     (rectangle 20 5 "solid" "blue")))

(check-expect (stack (rectangle 20 5 "solid" "blue")
                     (rectangle 10 9 "solid" "red"))
              (above (rectangle 10 9 "solid" "red")
                     (rectangle 20 5 "solid" "blue")))
;
;(define (stack i1 i2) i1)   ;stub
;
(@template-origin Image)

(@template
 (define (stack i1 i2)
   (... i1 i2)))


(define (stack i1 i2)
  (if (>= (image-width i1) (image-width i2))
      (above i2 i1)
      (above i1 i2)))

(@problem 2)
;;
;; The following data design may have errors in it. Please fix the error or
;; errors that you find.  Any changes you make should preserve the existing
;; design intent. Again, change only the parts of the code you really need
;; to change.
;;
;; Your solution must include @htdd tag, @dd-template-rules tag, and a NOT
;; commented fn-for-site template.
;; 


(@htdd Site)
;; Site is one of:
;; - "Vancouver"
;; - "Okanagan"
;; - "Robson"
;; - "CDM"
;; interp. a UBC site
;; <Examples are redundant for enumerations>

(@dd-template-rules one-of           ; 4 cases
                    atomic-distinct  ; "Vancouver"
                    atomic-distinct  ; "Okanagan"
                    atomic-distinct  ; "Robson"
                    atomic-distinct) ; "CDM" 

(define (fn-for-site s)
  (cond [(string=? s "Vancouver" ) (...)]
        [(string=? s "Okanagan") (...)]
        [(string=? s "Robson") (...)]
        [(string=? s "CDM") (...)]))

        
(@problem 3)
;; 
;; Consider the following data definition for Likert ratings.
;; 

(@htdd Rating)
;; Rating is one of:
;;  - "n/a"
;;  - Natural
;; interp. a Likert rating on a survey, either not applicable or rating
;;         1 is least happy, 5 is most happy, 3 is neither happy nor unhappy
;; CONSTRAINT: if a natural, is always in [1, 5]
(define R1 "n/a")  ;didn't vote
(define R2 5)      ;happiness with UBC!

(@dd-template-rules one-of               ;2 cases
                    atomic-distinct      ; "n/a"
                    atomic-non-distinct) ; Number

(define (fn-for-rating r)
  (cond [(and (string? r) (string=? r "n/a")) (...)]
        [(number? r) (... r)]  
        [else
         (... r)]))
         

;;
;; Ali wants to work with ratings, but first needs a function that
;; consumes a rating and produces true if the rating reflects a
;; clear opinion either way. Design this function, call it preference?
;;

(@htdf preference?)
(@signature Rating -> Boolean)
;; Produces true if given rating is a Natural in [1,5] showing happiness level 

(check-expect (preference? 1) true)
(check-expect (preference? 4) true)
(check-expect (preference? 0) false)
(check-expect (preference? "n/a") false)
(check-expect (preference? "") false)
(check-expect (preference? 6) false)

;(define (preference r) false); stub 


(@template-origin Rating)

(@template (define (preference? r)
  (cond [(and (string? r) (string=? r "n/a")) (...)]
        [(number? r) (... r)]  
        [else
         (... r)])))
            
(define (preference? r)
  (cond [(and (string? r) (string=? r "n/a")) false]
        [(and (number? r) (> r 0) (<= r 5)) true]
        [else false]))

(preference? 5)


(@problem 4)
;;
;; Write the dd-template-rules tag and template for the following
;; type comment.  Your rules and template must be in the same order
;; as the type comment.
;;

(@htdd Altitude)
;; Altitude is one of:
;;  - "pre-launch"
;;  - Number
;;  - "post-flight"
;; interp. Altitude of rocket. Before launch, in meters above launch
;;         pad, after flight has ended.
;; CONSTRAINT: when a number is > 0

(define A0 "pre-launch")
(define A1 37.5)
(define A2 "post-flight")

(@dd-template-rules one-of               ; 3 cases
                    atomic-distinct      ; "pre-launch"
                    atomic-non-distinct  ; Number > 0
                    atomic-distinct)     ; "post-flight" 

(define (fn-for-altitude a)
  (cond [(and (string? a) (string=? a "pre-launch")) (...)]
        [(number? a) (... a)]
        [else (...)]))
       

(@problem 5)
;;
;; Design a function that consumes an Altitude and produces true
;; if the rocket is actually inflight.  Call the function inflight?
;;

(@htdf inflight?)
(@signature Altitude -> Boolean)
;; Produces true if given Altitude is Number > 0, meaning rocket is inflight

(check-expect (inflight? 95.7) true)
(check-expect (inflight? "pre-launch") false)
(check-expect (inflight? "post-flight") false)
;(check-expect (inflight? "") false)
(check-expect (inflight? 0) false)
(check-expect (inflight? 7000.3) true)
(check-expect (inflight? 1) true)

;(define (inflight? a) false); stub

(@template-origin Altitude)

(@template
 (define (inflight? a)
  (cond [(and (string? a) (string=? a "pre-launch")) (...)]
        [(number? a) (... a)]
        [else (...) ] )))

(define (inflight? a)
  (cond [(and (string? a) (string=? a "pre-launch")) false]
        [(and (number? a) (> a 0)) true]
        [else false]))



(inflight? "")
(inflight? 6543.2)
