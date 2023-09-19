;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname m01-htdf-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require 2htdp/image)
(require spd/tags)

(@assignment lectures/m01-htdf)

(@cwl dana28cl) ;replace ??? by your CWL


;; *****************************************************************************
;; NOTE:
;;
;; Your AUTOGRADER RESULTS will be found at the following address:
;;
;;    https://cs110.students.cs.ubc.ca/handback/12345678/
;;
;; Where you must replace 12345678 with your UBC student number.  You will need
;; to login with your CWL and CWL password.
;;
;;  For lecture starters you can get some feedback from the autograder during
;;  lecture, provided you can submit a file that passes the Check Syntax button
;;  at the top right of the Dr Racket window.
;;
;;  Especially at first it can be hard to get all your ( ) and "  " properly
;;  balanced, and so you might not be able to pass the Check Syntax.  If that
;;  happens then first start by getting the gist of the problem right.  You
;;  can (and should) go back after lecture to work the problem until you
;;  get autograder feedback saying it is 100% correct.
;;
;;  But for problem sets, labs, and exams you should not submit your file if
;;  Check Syntax produces an error.  Fix the error first, then submit.
;; *****************************************************************************


(@problem 1)
;;
;; Design a function, called topple, that takes an image and rotates it 
;; by 90 degrees.
;;
(@htdf topple)
(@signature Image -> Image)
;; rotates given image by 90 degrees counter-clockwise

;(define (topple img)) empty-image) ; this is the stub

(check-expect (topple (rectangle 5 10 "solid" "yellow"))
                      (rotate 90 (rectangle 5 10 "solid" "yellow")))

(check-expect (topple (triangle 20 "solid" "red"))
              (rotate 90 (triangle 20 "solid" "red")))

(@template-origin Image) ; for module 1, type of first parameter

(@template
 (define (topple img); for module 1, always (... <first param>)
 (... img)))

(define (topple img)
  (rotate 90 img))

(ellipse 60 20 "solid" "green")

(topple (ellipse 60 20 "solid" "green"))
  
(@problem 2)
;;
;; Design a function that consumes the name of something and produces a
;; "checkbox line" image that allows someone to check off that item.  For 
;; example (checkbox-line "apples") would produce an image with a small
;; check box next to the word apples.
;;

(@htdf checkbox-line)
(@signature String -> Image)
; Produces checkbox next to given string

;(define (checkbox-line str) empty-image) ; stub

(check-expect (checkbox-line "apples")
              (beside (square 12 "outline" "black")
                      empty-image (text "apples" 12 "black")))

(check-expect (checkbox-line "")
              (beside (square 12 "outline" "black")
                      empty-image (text "" 12 "black")))


(@template-origin Image)

(@template
 (define (checkbox-line str)
   (... str)))

(define (checkbox-line str)
   (beside (square 12 "outline" "black") empty-image (text str 12 "black")))

;(checkbox-line "bananas")


(@problem 3)
;;
;; Design a function, that consumes an image and determines whether it is tall.
;;

(@htdf tall?)
(@signature Image -> Boolean)
; returns "tall" if given image's height is greater than image's width

;(define (tall? img) false);stub

(check-expect (tall? (rectangle 10 20 "solid" "red")) true)
(check-expect (tall? (ellipse 17 7 "solid" "red")) false)
(check-expect (tall? (rectangle 3 3 "solid" "red")) false)

(@template-origin Image)
(@template
 (define (tall? img)
(... img)))

(define (tall? img)
  (> (image-height img) (image-width img)))

(tall? (rectangle 15 3 "solid" "blue"))

(@problem 4)
;;
;; Design a function, called image>, that takes two images and determines 
;; whether the first is larger than the second.
;;
;; For this function we are giving you the signature template-origin
;; and template below
;;

(@htdf image>)
(@signature Image Image -> Boolean)
;; Compares are size of 2 images, returns true when area image1 > area image2 

;(define (image> i1 i2) false);stub

(check-expect (image> (rectangle 4 5 "solid" "red")
                      (rectangle 2 3 "solid" "red")) true)
(check-expect (image> (rectangle 1 2 "solid" "red")
                      (rectangle 5 6 "solid" "red")) false)
(check-expect (image> (square 2 "solid" "blue")
                      (square 2 "solid" "red")) false)

(@template-origin Image)

(@template
 (define (image> i1 i2)
   (... i1 i2)))


(define (image> i1 i2)
  (> (* (image-height i1)(image-width i1))
     (* (image-height i2)(image-width i2))))

(image> (square 10 "solid" "blue")(rectangle 2 3 "solid" "blue"))
(image> (square 10 "solid" "blue") (square 10 "solid" "yellow"))




