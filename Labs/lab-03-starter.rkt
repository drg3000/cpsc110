;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab-03-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/image)
(require 2htdp/universe)

(@assignment labs/lab-03)

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

; Balloon popping

(@htdw Balloon)
;; CONSTANTS ==========================

(define WIDTH 500)
(define HEIGHT 500)
(define MTS (empty-scene WIDTH HEIGHT))

(define BALLOON-COLOR "red")
(define POP-IMAGE
  (overlay (text "POP!" 80 "black")
           (radial-star 30 (/ WIDTH 10) (/ WIDTH 2) "solid" "yellow")))

(define CTR-X (/ WIDTH 2))
(define CTR-Y (/ HEIGHT 2))

(define SPEED 2)

;; Do i need a starter balloon?
#;
(define BALLOON-START (circle 10 "solid" "red"))

(define MAX-SIZE (/ WIDTH 2))

;; DATA DEFINITIONS ============================
 
(@problem 1)

(@htdd Balloon)
; is Balloon just a number?
;; Balloon is one of:
;; - Natural (values > 0)
;; - False
;;Interp. Number = r, the radius of the balloon; radius grows, so r>0
;; false represents the popped balloon replacing number
;;examples
(define B1 10)
(define B2 60)
(define B3 99)
(define BP false)

(@dd-template-rules one-of             ; 2 cases
                    atomic-non-distinct; Natural
                    atomic-distinct)   ; false

(define (fn-for-balloon r)             ; r for radius
  (cond [(number? r) (... r)]
        [else (...)]))
           
                                         


;; FUNCTIONS ====================================
(@problem 2)
(@htdf main)
(@signature Balloon -> Balloon)  ;world state type
;; starts the world program with (main 1)
; no examples for main function

(@template-origin htdw-main)
(define (main r)
  (big-bang r               ; Balloon
    (on-tick tick)   ; Balloon -> Balloon
    (to-draw render) ; Balloon -> Image
    ;           (stop-when ...)  ; WS -> Boolean
    ;           (on-mouse ...)   ; WS Integer Integer MouseEvent -> WS
    ;           (on-key ...)     ; WS KeyEvent -> WS
    ))

(@problem 3)
(@htdf tick)
(@signature Balloon -> Balloon) 
;; produces the next balloon of size r + SPEED or just MAX-IMAGE if r=MAX-SIZE
;; !!!

(check-expect (tick 10) 12)
(check-expect (tick MAX-SIZE) false)
(check-expect (tick (- MAX-SIZE 1)) false)
(check-expect (tick (- MAX-SIZE SPEED)) MAX-SIZE)
(check-expect (tick (- MAX-SIZE 3)) MAX-SIZE)
(check-expect (tick false) false)
(check-expect (tick 0) 2)


;(define (tick r) r);stub

(@template-origin Balloon)

(@template
 (define (fn-for-balloon r)             ; r for radius
   (cond [(number? r) (... r)]
         [else (...)])))
           
(define (tick r)             ; r for radius
  (cond [(number? r) (if (<= (+ r SPEED) MAX-SIZE)
                         (+ r SPEED)
                         false)] 
        [else false])) 
            

  

(@problem 4)
(@htdf render)
(@signature Balloon -> Image) ;
;; renders image of balloon r as long as MAX-SIZE > r

(check-expect (render MAX-SIZE)
              (place-image
               (circle MAX-SIZE "solid" BALLOON-COLOR) CTR-X CTR-Y MTS))
(check-expect (render false) (place-image POP-IMAGE CTR-X CTR-Y MTS))
(check-expect (render 12) (place-image
                           (circle 12 "solid" BALLOON-COLOR) CTR-X CTR-Y MTS))
(check-expect (render 0) (place-image
                          (circle 0 "solid" BALLOON-COLOR) CTR-X CTR-Y MTS)) 



;(define (render r) empty-image); stub

(@template-origin Balloon)

(@template
 (define (render r)             ; r for radius
   (cond [(number? r) (... r)]
         [else (...)])))

(define (render r)             ; r for radius
  (cond [(number? r)
         (place-image (circle r "solid" BALLOON-COLOR) CTR-X CTR-Y MTS)]
        [else (place-image POP-IMAGE CTR-X CTR-Y MTS)])) 
