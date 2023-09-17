;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab-02-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)

(@assignment labs/lab-02)

;; If you are:
;;   - A 110 or 107 student replace the first set of '???'s with your cwl.
;;     Remember this, it is what you will do with these @cwl annotations
;;     for the whole course.
;;   - A UBC Extended Learning student, replace the first set of ??? with
;;     the your email address as confirmed in the email you received from
;;     extended learning.  The handin password is also in that email.
;;     Remember this, it is what you will do with these @cwl annotations
;;     for the whole course.
;;   
(@cwl dana28cl ???)

;; HtDF Lab, Problem 1

;; PROBLEM:
;;
;; Design a function called square? that consumes an image and determines 
;; whether the image's height is the same as the image's width.
(@problem 1)

(@htdf square?) ;!!!UNCOMMENT this line when you start on this function

(@htdf square?)
(@signature Image -> Boolean)
;; Checks image is a square, image-height = image-width returns true
;(define (square? i) false) ;stub

(check-expect (square? (rectangle 40 10 "solid" "green")) false)
(check-expect (square? (square 20 "solid" "blue")) true)
(check-expect (square? (circle 90 "solid" "red")) true)

(@template-origin Image)
(@template
 (define (square? i)
   (... i)))

(define (square? i)
   (= (image-height i) (image-width i))
       )

(square? (rectangle 3 13 "solid" "red"))
(square? (square 45 "solid" "red"))
(square? (circle 60 "solid" "yellow"))

;; HtDF Lab, Problem 2

;; PROBLEM:
;; 
;; Design a function named underline that consumes an image 
;; and produces that image underlined by a black line of the same width. 
; 
;;
;; For example, 
   ;; 
   ;;   (underline (circle 20 "solid" "green"))
   ;; 
   ;; should produce
   ;;
   ;;   (above (circle 20 "solid" "green")
   ;;          (rectangle 40 2 "solid" "black"))

(@problem 2)
(@htdf underline) ;!!!UNCOMMENT this line when you start on this function
(@signature Image -> Image)
;; Produces an image with given image and black line under it

;(define (underline i)empty-image) ;stub

(check-expect (underline (circle 20 "solid" "green"))
              (above (circle 20 "solid" "green")
                     (rectangle 40 2 "solid" "black")))

(check-expect (underline (triangle 40 "solid" "seagreen"))
              (above (triangle 40 "solid" "seagreen")
                     (rectangle 40 2 "solid" "black")))

(check-expect (underline (ellipse 60 20 "solid" "purple"))
              (above (ellipse 60 20 "solid" "purple")
                     (rectangle 60 2 "solid" "black")))

(@template-origin Image)
(@template
 (define (underline i)
   (... i)))

(define (underline i)
   (above i (rectangle (image-width i) 2 "solid" "black")))

(underline (circle 90 "solid" "pink"))
                                                         

   ;; HtDF Lab, Problem 3

   ;; PROBLEM:
   ;; 
   ;; A (much too) simple scheme for pluralizing words in English is to add an 
   ;; s at the end unless the word already ends in s.
   ;; 
   ;; Design a function that consumes a string, and adds s to the end unless 
   ;; the string already ends in s.

(@problem 3)
(@htdf pluralize) ;!!!UNCOMMENT this line when you start on this function

(@signature String -> String)
;; pluralizes a word if given word does not end in "s"

;(define (pluralize s) ""); stub

(check-expect (pluralize "balloon") "balloons")
(check-expect (pluralize "campus") "campus")
(check-expect (pluralize "") "")

(@template-origin String)
(@template
 (define (pluralize s)
   (... s)))

(define (pluralize s)
  (if (string=? s "")
      ""
      (if (string=?
           (substring s (- (string-length s) 1)) "s")
                 s
                 (string-append s "s"))))
(pluralize "banana")
(pluralize "cards")

   ;; HtDF Lab, Problem 4

   ;; PROBLEM:
   ;; 
   ;; Design a function called nth-char-equal? that consumes two strings 
   ;; and a natural and produces true if the strings both have length greater 
   ;; than n and have the same character at position n.
   ;; 
   ;; 
   ;; Note, the signature for such a function is:
   ;; 
   ;; (@signature String String Natural -> Boolean)
   ;; 
   ;; 
   ;; The tag and template for such a function are:
   ;; 
   ;; (@template-origin String)
   ;; 
   ;; (@template (define (nth-char-equal? s1 s2 n)
   ;;              (... s1 s2 n)))
(@problem 4)
(@htdf nth-char-equal?) ;!!!UNCOMMENT this line when you start on this function

(@signature String String Natural -> Boolean)
;;produces true if s1 and s2 have same string length and same char in position n

;(define (nth-char-equal? s1 s2 n) false); stub

(check-expect (nth-char-equal? "dog" "log" 2) true)
(check-expect (nth-char-equal? "dog" "log" 3) false)
(check-expect (nth-char-equal? "dog" "day" 2) false)
(check-expect (nth-char-equal? "dog" "apple" 2) false)
(check-expect (nth-char-equal? "dog" "" 2) false)
(check-expect (nth-char-equal? "hut" "mut" 5) false)


(@template-origin String)

(@template (define (nth-char-equal? s1 s2 n)
             (... s1 s2 n)))

(define
  (nth-char-equal? s1 s2 n)
             (and (> (string-length s1) n)
                  (> (string-length s2) n)
                  (string=? (substring s1 n (+ n 1))
                            (substring s2 n (+ n 1))))) 

(nth-char-equal? "dog" "nag" 1)











   