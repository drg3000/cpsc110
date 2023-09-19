;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname m03-spider-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require 2htdp/image)
(require 2htdp/universe)
(require spd/tags)

(@assignment lectures/m03-spider)

(@cwl dana28cl) ;replace ??? with your cwl


(@problem 1)
#|
PROBLEM:

Design a world program in which a spider starts at the top of the screen
and slowly drops down it. The spider should stop when it reaches the bottom
of the screen.

You can improve your spider by re-running the HtDW recipe to add these
features. 


  - Draw a line from the top of the screen to the spider, this is the thread 
    it is hanging from. You will need to use add-line for this. Look in the
    DrRacket help desk to see how add-line works.  [NOTE that adding this
    functionality will cause the autograder to complain, the autograder is
    just designed to grade the original problem.]
    
  - Arrange for pressing the space key to reset the spider to the top of 
    the screen.
|#


(@htdw Spider)
;; Program draws a spider starts at the top of the screen and slowly drops
;; down it. The spider stops when it reaches the bottomof the screen.


;; =================
;; Constants:

(define WIDTH  400) ; in pixels
(define HEIGHT 600) ;in pixels

(define CTR-X (/ WIDTH 2))

(define S-RADIUS 20)

(define TOP (+ 0 S-RADIUS))
(define BOT (- HEIGHT 1 S-RADIUS))


(define SPIDER-IMAGE (circle S-RADIUS "solid" "black"))

(define SPEED 3) ;pixels per tick

(define MTS (empty-scene WIDTH HEIGHT "orange" ))


;; =================
;; Data definitions:

(@htdd Spider)
;; Spider is a Number
;; Interp. the y coordinate of the centre of the spider
;;          0 at the top, y growns going down

(define MID (/ HEIGHT 2))

(@dd-template-rules atomic-non-distinct)

(define (fn-for-spider s)
  (... s))

;; =================
;; Functions:

(@htdf main)
(@signature Spider -> Spider)
;; start the world with (main TOP)
;; no tests needed for main

(@template-origin htdw-main)

(define (main s) ;s is for spider
  (big-bang s                   ; Spider
            (on-tick   tock)     ; Spider -> Spider
            (to-draw   render)))   ; Spider -> Image
            ;(stop-when ...)      ; Spider -> Boolean
            ;on-mouse  ...)      ; Spider Integer Integer MouseEvent -> Spider
            ;(on-key    ...)))    ; Spider KeyEvent -> Spider



(@htdf tock)
(@signature Spider -> Spider)
;; produces the next by addding sppek to y-coord of Spider, stopping at BOT 
;;

(@template-origin Spider)

(@template
 (define (tock s)
   (... s)))


(check-expect (tock TOP) (+ TOP SPEED))
(check-expect (tock (- BOT SPEED  1)) (- BOT 1))
(check-expect (tock (- BOT SPEED  0)) BOT)
(check-expect (tock (- BOT SPEED -1))  BOT)

;(define (tock s) s); stub can just take spider and
                   ;produce the same spider as dummy

(define (tock s)
  (if (>= (+ s SPEED) BOT); moving at speed, but going too far
      BOT                 ; stop at BOT (if above true)
      (+ s SPEED)))


(@htdf render)
(@signature Spider -> Image)
;; places SPIDER-IMAGE on MTS at CTR-X and spider's y

(check-expect (render TOP)
              (place-image SPIDER-IMAGE CTR-X TOP MTS))

(check-expect (render (/ HEIGHT 2))
              (place-image SPIDER-IMAGE CTR-X (/ HEIGHT 2) MTS))


;(define (render s) empty-image); stub

(@template-origin Spider)

(@template
 (define (render s)
   (... s)))

(define (render s)
  (place-image SPIDER-IMAGE CTR-X s MTS))
