;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname m02-grade-standing-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)

(@assignment lectures/m02-grade-standing)

(@cwl dana28cl)

;;
;; YOU MUST CONSULT THE m02-grade-standing-documents.rkt
;; starter file which has documents you will need.  But do all your
;; actual work in this starter file.
;;
;; We want to design a program to operate on submitted grades. For now 
;; JUST FOCUS ON THE % GRADE AND STANDING.  AFTER designing a representation
;; for this information, you must design two functions:
;;
;;  - A fn that consumes grade/standing and produces true if
;;    the grade/standing is >= 90.  Call this function excellent?
;;
;;  - A fn that consumes grade/standing and produces strings
;;    like  "90%"  "75%" "P" "F". Call this function grade->string


(@problem 1)
;; Data definition goes here:
;; examples: 100, 85, 51, H, P, F, T
;; natural - constrained [0, 100]
;; strings
;; because it's natural (non-disctinct) and specific strings "H, P..." then
;; itemization - one-of-several classes

;; GradeStanding is one of\:
;; - Natural 
;; - "H"
;; - "P"
;; - "F"
;; - "T"
;; interp. a percept grade OR a standing
;; CONSTRAINT if natural, is in [0, 100]
;; next examples - just how the data definition works
;;(define GS1 100); great
;;(define GS2 0) ; opps
;;(define GS3 "H"); good

(@htdd GradeStanding)

(@dd-template-rules one-of
                    atomic-non-distinct ; Natural, constrained [0,100]
                    atomic-distinct ; "H"
                    atomic-distinct ; "P"
                    atomic-distinct ; "F"
                    atomic-distinct ; "T"
                    )

(define (fn-for-gs gs)
  (cond [(number? gs) (...)]
        [(string=? gs "H") (...)]
        [(string=? gs "P") (...)]
        [(string=? gs "F") (...)]
        [else (...)]))

  
(@htdf excellent?)
(@signature GradeStanding -> Boolean)
;; produce true if percentage mark is 90 or greater

;(define (excellent? gs) false);stub

(check-expect (excellent? 91) true)
(check-expect (excellent? 90) true)
(check-expect (excellent? 89) false)
(check-expect (excellent? "H") false)
(check-expect (excellent? "P") false)
(check-expect (excellent? "F") false)
(check-expect (excellent? "T") false)

(@template-origin GradeStanding)

(@template
 (define (excellent? gs)
  (cond [(number? gs) (...)]
        [(string=? gs "H") (...)]
        [(string=? gs "P") (...)]
        [(string=? gs "F") (...)]
        [else (...)])))

(define (excellent? gs)
  (cond [(number? gs) (>= gs 90)]
        [(string=? gs "H") false]
        [(string=? gs "P") false]
        [(string=? gs "F") false]
        [else false]))
         

(@problem 2)
(@htdd GradeStanding)
(@htdf grade->string)
(@signature GradeStanding -> String)
;; takes a grade or standing and returns string of grade or standing
;(define (grade->string gs) "")

(check-expect (grade->string 90) "90%")
(check-expect (grade->string "T") "T")
(check-expect (grade->string 80) "80%")
(check-expect (grade->string "H") "H")
(check-expect (grade->string "P") "P")
(check-expect (grade->string "F") "F")


(@template-origin GradeStanding)

(@template
 (define (grade->string gs)
  (cond [(number? gs) (...)]
        [(string=? gs "H") (...)]
        [(string=? gs "P")(...)]
        [(string=? gs "F") (...)]
        [else (...)])))


(define (grade->string gs)
  (cond [(number? gs)
         (string-append (number->string gs) "%")]
        [(string=? gs "H") gs]
        [(string=? gs "P") gs]
        [(string=? gs "F") gs]
        [else gs]))

(grade->string 89)



