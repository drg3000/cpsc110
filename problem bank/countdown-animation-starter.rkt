;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname countdown-animation-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require spd/tags)

(@assignment bank/htdw-p1)
(@cwl dana28cl)

(@problem 1)
;; Design a world program that represents a countdown. The program should 
;; display the number of seconds remaining and should decrease at each 
;; clock tick. Upon reaching zero, it should stay there and not change.
;; 
;; To make your countdown progress at a reasonable speed, you can use the 
;; rate option to on-tick. If you say, for example, 
;; (on-tick advance-countdown 1) then big-bang will wait 1 second between 
;; calls to advance-countdown.
;;
;; Remember to follow the HtDW recipe! Be sure to do a proper domain 
;; analysis before starting to work on the code file.
;; 
;; Once you are finished the simple version of the program, you can improve
;; it by reseting the countdown to ten when you press the spacebar.


;(@htdw countdown-animation) ;
;countdown-animation.rkt
;world program for a countdown
;============================
; Constants

(define WIDTH 400)
(define HEIGHT WIDTH)

(define MTS (empty-scene WIDTH HEIGHT "midnight blue"))

(define CTR-X (/ WIDTH 2))
(define CTR-Y (/ HEIGHT 2))

;;============================
;; Data Definitions
(@htdd Counter-d)
;; Counter-d is a Number
;; interp. number of seconds remaining at start of clock ticks

(define counter-2min 120); countdown starts at 120 seconds
(define counter-1min 60); countodnw starts at 60 seconds

(@dd-template-rules atomic-non-distinct); Number

(define (fn-for-counter-d s); s for seconds
  (... s))

;;===========================
;; Functions

(@htdf main)
(@signature Counter-d -> Counter-d)
;; start the world with (main 120); initial state s at 120 seconds
;; 

(@template-origin htdw-main)

(define (main s)
  (big-bang s                   ; counter-d
            (on-tick   seconds-down)     ; counter-d-> counter-d
            (to-draw   render)))   ; counter-d -> Image
           


(@htdf seconds-down)
(@signature Counter-d -> Counter-d)
;; produce the next second in the countdown
;; !!!

(check-expect (seconds-down 0) 0)
(check-expect (seconds-down 10) 9)
(check-expect (seconds-down -10)0)

;(define (seconds-down s) 59); stub

(define (seconds-down s)
  (cond [(<= s 0) 0]
        [else (- s 1)]))

(@htdf render)
(@signature Counter-d -> Image)
;; render next number in the countdown
;; !!!

;(check-expect (render 10) (text "10" 24 "white"))
;(check-expect (render 0) (text "0" 24 "white"))



;(define (render s) empty-image);stub


(define (render s)
  (place-image (text (number->string s)  24 "white") CTR-X CTR-Y MTS))

