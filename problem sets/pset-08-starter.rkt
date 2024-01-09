;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname pset-08-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; DO NOT PUT ANYTHING PERSONALLY IDENTIFYING BEYOND YOUR CWL IN THIS FILE.
;; YOUR CWLs WILL BE SUFFICIENT TO IDENTIFY YOU AND, IF YOU HAVE ONE, YOUR 
;; PARTNER.
;;
(require spd/tags)

(@assignment psets/pset-08); Do not edit or remove this tag

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
(@cwl dana28cl ???)

;; These problems will be autograded.  NOTE that, for problems 1-5, all of the
;; following are required. Violating one or more will cause your solution to
;; receive 0 marks.
;;
;;   - Files must not have any errors when the Check Syntax button is pressed.
;;     Press Check Syntax and Run often, and correct any errors early.
;;
;;   - The function definition MUST call one or more built-in abstract
;;     functions.
;;
;;   - For maximum credit the function definition should use the most clear
;;     and expressive combination of abstract functions.  In particular, while
;;     it is possible to just use foldr for these problems that is not always
;;     correct. If what is happening is a filter, then it is not correct to 
;;     just implement filtering with foldr.
;;
;;   - The function definition MUST NOT be recursive.
;;
;;   - The function definition MUST NOT use any part of the recursive Natural
;;     template or the (listof X) template.
;;
;;       - it must not include (cond [(empty? ... anywhere
;;       - it must not include (cond [(zero? ... anywhere
;;       - it must not include (if (empty? ... anywhere
;;       - it must not include (if (zero? ... anywhere
;;
;;   - The result of the function must directly be the result of one of the
;;     built-in abstract functions. So, for example, the following is not
;;     a valid function body:
;;
;;        (define (foo x)
;;          (empty? (filter ...)))
;;
;;   - You MUST NOT change or comment out any check-expects, but you are free
;;     to add new ones.

(@problem 1)
;; 
;; Complete the design of a function called sum-squares that consumes a list
;; of natural numbers and produces the sum of squaring all of the naturals
;; in the list.
;;
;; For example: (sum-squares (list 5 2 4)) produces 45.
;;
;; Your function definition must use built-in abstract functions.
;; For full marks it must be a composition of exactly 2 built-in
;; abstract functions.
;;

(@htdf sum-squares)
(@signature (listof Natural) -> Natural)
; squares all the naturals in the list and produces the sum of whole list

(check-expect (sum-squares (list 5 2 4)) 45)

;(define (sum-squares lon) empty)

(@template-origin use-abstract-fn fn-composition)

(define (sum-squares lon)
  (foldr + 0 (map sqr lon)))


(@problem 2)
;;
;; Complete the design of the following function.
;;
;; Your function definition must use built-in abstract functions.
;; For full marks it must be a composition of exactly 2 built-in
;; abstract functions.
;;
;; NOTE: Looking up the function string-contains? in the help desk
;; might be helpful...

(@htdf all-a-contain?)
(@signature (listof String) String -> Boolean)
;; determine if all strings that begin with the letter 'a' contain word w
(check-expect (all-a-contain? empty "word") true)
(check-expect (all-a-contain? (list "bicycle" "car" "train" "walk")
                              "transportation")
              true)
(check-expect (all-a-contain? (list "assignment") "eight") false)
(check-expect (all-a-contain? (list "attend" "radio" "antenna" "listen" "study")
                              "ten") 
              true)
(check-expect (all-a-contain? (list "art" "bin" "action" "tin") "in") false)
(check-expect (all-a-contain? (list "art" "artery" "ban" "action" "tin")
                              "art")
              false)

;(define (all-a-contain? los w) false)  ;stub

(@template-origin use-abstract-fn fn-composition)

(define (all-a-contain? los w)
  (local [(define (first-a? str) (string=? (substring str 0 1) "a"))
          (define (contains-w? str) (string-contains? w str))]
  (andmap contains-w? (filter first-a? los))))
          

;;
;; Before completing problems 3, 4, and 5, please familiarize
;; yourself with the provided data definition for a Cat.
;; An example (listof Cat) is also provided.
;;

(@htdd Cat)
(define-struct cat (name color age))
;; Cat is (make-cat String Color Natural)
;; interp. a cat with a name, coat color, and age (in years)
(define C1 (make-cat "Whiskers" "brown" 13))
(define C2 (make-cat "Si"       "black"  4))
(define C3 (make-cat "Am"       "white"  4))
(define C4 (make-cat "Meow"     "brown"  7))
(define C5 (make-cat "Garfield" "orange" 8))
(define C6 (make-cat "Sassy"    "brown"  6))

(define CATS (list C1 C2 C3 C4 C5 C6))

(define (fn-for-cat c)
  (... (cat-name c)
       (cat-color c)
       (cat-age c)))


(@problem 3)
;; 
;; Complete the design of a function called brown-cat-names that consumes
;; a list of cats, and produces a list of the names of the cats that are
;; brown in color.
;;
;; For example: (brown-cat-names (list C1 C2 C3 C4 C5 C6))
;;    produces: (list "Whiskers" "Meow" "Sassy")
;;
;; Your function definition must use built-in abstract functions.
;; For full marks it must be a composition of exactly 2 built-in
;; abstract functions. 
;;
(@htdf brown-cat-names)
(@signature (listof Cat) -> (listof String))

(check-expect (brown-cat-names (list C1 C2 C3 C4 C5 C6))
              (list "Whiskers" "Meow" "Sassy"))

;(define (brown-cat-names c) empty)

(@template-origin use-abstract-fn fn-composition)

(define (brown-cat-names loc)
  (local [(define (brown? c) (string=? (cat-color c) "brown"))
          (define (get-name c) (cat-name c))]
    (map get-name (filter brown? loc))))


(@problem 4)
;; 
;; Design a function called count-short-names that consumes a list of cats and a
;; natural n, and produces a count of the number of cats with names less than n
;; characters long.
;;
;; For example: (count-short-names (list C1 C2 C3 C4 C5 C6) 5) produces 3, since
;;    "Si", "Am", and "Meow" all have names less than 5 characters long.
;;
;; Your function definition must use built-in abstract functions.
;; For full marks it must be a composition of exactly 3 built-in
;; abstract functions.
;; 
(@htdf count-short-names) ; uncomment this when you start problem 4
(@signature (listof Cat) Natural -> Natural)

(check-expect (count-short-names CATS 5) 3)
(check-expect (count-short-names (list C1 C5) 10) 2)

;(define (count-short-names loc n) 0)

(@template-origin use-abstract-fn fn-composition)

(define (count-short-names loc n)
  (local [(define (name-length c) (string-length (cat-name c)))
          (define (short-only num) (< num n))
          (define (count fn z) (add1 z)) ]
    (foldr count 0 (filter short-only (map name-length loc)))))

(@problem 5)
;;
;; Complete the design of the following function.
;;
;; Your function definition must use built-in abstract functions.
;; For full marks it must be a composition of exactly 3 built-in
;; abstract functions.
;;

(@htdf sum-age-c)
(@signature (listof Cat) Color -> Natural)
;; produce the sum of the ages of all cats coloured c
(check-expect (sum-age-c empty "black") 0)
(check-expect (sum-age-c (list C1) "black") 0)
(check-expect (sum-age-c (list C1) "brown") 13)
(check-expect (sum-age-c (list C1 C2 C3 C4 C5 C6) "black") 4)
(check-expect (sum-age-c (list C1 C2 C3 C4 C5 C6) "brown") (+ 13 7 6))

;(define (sum-age-c loc c) 0)    ;stub

(@template-origin use-abstract-fn fn-composition)


(define (sum-age-c loc col)
  (local[(define (same-color? c) (string=? (cat-color c) col))
         (define (get-ages c) (cat-age c))]
    (foldr + 0 (map get-ages (filter same-color? loc)))))

;;
;; Please read through the data definition  for Treasure that can be found
;; in a Scavenger Hunt.
;;

(@htdd Status)
;; Status is one of:
;; - "buried"
;; - "sunken"
;; - "locked"
;; interp. the status of an unopened treasure box
;; <examples are redundant for enumeration>

(@htdd Treasure)
(define-struct treasure (label amount difficulty status treasures))
;; Treasure is (make-treasure String Natural Natural Status (listof Treasure))
;; interp. a treasure box with a label name,
;;         the number of gold coins contained in the treasure box,
;;         a rating of difficulty to find and open the treasure box between 1
;;         and 5, where 1 is very easy to find and open and 5 is very difficult,
;;         the status of the treasure box before it was opened,
;;         and a list of other treasure boxes that opening the box leads to.

(define LOT0 empty)
(define T1 (make-treasure "E" 32 3 "buried" LOT0))
(define T2 (make-treasure "F" 10 2 "locked" LOT0))
(define LOT1 (cons T1 (cons T2 empty)))
(define T3 (make-treasure "B" 6 5 "locked" LOT1))      

(define T4 (make-treasure "J" 1 1 "sunken" LOT0))
(define LOT2 (cons T4 empty))
(define T5 (make-treasure "H" 17 2 "sunken" LOT2))
(define T6 (make-treasure "G" 52 3 "buried" LOT0))
(define T7 (make-treasure "I" 100 5 "locked" LOT0))
(define LOT3 (cons T6 (cons T5 (cons T7 empty))))
(define T8 (make-treasure "D" 21 1 "sunken" LOT3))

(define T9 (make-treasure "C" 41 4 "buried" LOT0))
(define LOT4 (cons T3 (cons T9 (cons T8 empty))))
(define T10 (make-treasure "A" 7 1 "locked" LOT4))

(@template-origin encapsulated Status Treasure (listof Treasure))
                        
(define (fn-for-treasure t)
  (local [(define (fn-for-status s)
            (cond [(string=? s "buried") (...)]
                  [(string=? s "sunken") (...)]
                  [(string=? s "locked") (...)]))

          (define (fn-for-treasure t)
            (... (treasure-label t)
                 (treasure-amount t)
                 (treasure-difficulty t)
                 (fn-for-status (treasure-status t))
                 (fn-for-lot (treasure-treasures t))))

          (define (fn-for-lot lot)
            (cond [(empty? lot) (...)]
                  [else
                   (... (fn-for-treasure (first lot))
                        (fn-for-lot (rest lot)))]))]
    
    (fn-for-treasure t)))

(@problem 6)
#|
Complete the design of the following abstract fold function for Treasure.
Note that we have already given you the actual function definition and the
template tag. You must complete the design with a signature, purpose,
function definition and the two following check-expects:

  - uses the fold function to produce a copy of T10
  - uses the fold function to count the total amount of gold in the T10
    tree
|#
(@htdf fold-treasure)
(@signature (String Natural Natural X Z -> X)(Y Z -> Z) X X X Z Treasure -> X)
;;abstract fold function for treasure

;copy
(check-expect (local[(define (c2 t rnr) (cons t rnr))]
                (fold-treasure
                 make-treasure c2 "buried" "sunken" "locked" empty T10))
              (make-treasure "A" 7 1 "locked" LOT4))
;count

(check-expect (local [(define (c1 label amount diff status rmr)
                        (+ amount rmr))]
                (fold-treasure c1 + "buried" "sunken" "locked" 0 T10))
              (+ 7 6 32 10 41 21 52 17 1 100))

(@template-origin encapsulated Status Treasure (listof Treasure))

(define (fold-treasure c1 c2 b1 b2 b3 b4 t)
  (local [(define (fn-for-status s)            ;;-> S ->X
            (cond [(string=? s "buried") b1]
                  [(string=? s "sunken") b2]
                  [(string=? s "locked") b3]))

          (define (fn-for-treasure t)          ;;-> T->Y
            (c1 (treasure-label t)
                (treasure-amount t)
                (treasure-difficulty t)
                (fn-for-status (treasure-status t))
                (fn-for-lot (treasure-treasures t))))

          (define (fn-for-lot lot)             ;;-> L-> Z
            (cond [(empty? lot) b4]
                  [else
                   (c2 (fn-for-treasure (first lot))
                       (fn-for-lot (rest lot)))]))]
    
    (fn-for-treasure t)))


(@problem 7)
;;
;; Design a function called more-than-n. more-than-n takes two arguments: a
;; Treasure (t) and a Natural (n) IN THAT ORDER. It produces a list of the label
;; names of treasures that contain an amount of gold larger than n. For this 
;; question, assume that the amount of gold in each box is magically multiplied
;; based on the status of the treasure box.
;;
;; Treasure boxes that are buried get their amount of gold multiplied by 2,
;; sunken treasure get a multiplier of 4, and locked chests get a multiplier
;; of 3. This means that if a treasure is defined as follows:
;;   - (define T1 (make-treasure "E" 32 3 "buried" empty))
;;   - T1 would effectively have 32 gold x 2 (since it's buried) = 64 gold.
;;
;; Your function definition must call the fold-treasure function you defined
;; in PROBLEM 6.

(@htdf more-than-n)
(@signature Treasure Natural -> (listof String)) 
                    
;;produces list of label names of treasures containing gold > n
;; Gold determined by status of treasure:
;; - "buried" treasure has gold * 2
;; - "sunken" treasure has gold * 4
;; - "locked" treasure has gold * 3

(check-expect (more-than-n T1 15) (list "E"))
(check-expect (more-than-n T10 100) (list "G" "I"))
(check-expect (more-than-n T10 300) empty)
(check-expect (more-than-n T8 30) (list "D" "G" "H" "I"))


;(define (more-than-n t n) empty)

(@template-origin use-abstract-fn Treasure)
 
(define (more-than-n t n)
  (local [(define (c1 name amount diff rstat rlot)
            (if (> (* amount rstat) n)
                (cons name rlot)
                rlot))]
(fold-treasure c1 append 2 4 3 empty t)))        
