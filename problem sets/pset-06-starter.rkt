;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname pset-06-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; DO NOT PUT ANYTHING PERSONALLY IDENTIFYING BEYOND YOUR CWL IN THIS FILE.
;; YOUR CWLs WILL BE SUFFICIENT TO IDENTIFY YOU AND, IF YOU HAVE ONE, YOUR 
;; PARTNER.
;;
(require spd/tags)

(@assignment psets/pset-06); Do not edit or remove this tag

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


(@problem 1)
;; Below is the start of a data definition called Course that represents limited
;; information about UBC courses.  Below there are only two example data.  
;; Please complete this definition by adding constants C110, C213, C313 and C317
;; which are representations of the descendent tree for 110, 213, 313 and 317.  
;; You can find the information you need at
;;  https://cs110.students.cs.ubc.ca/psets/pset-06-image.png
;;
;; NOTE 1: Use the information in the image above, rather than any other source.
;;         We are significantly simplying the information.
;;
;; NOTE 2: Do this very carefully, the autograder wants to see correct results
;;         from the functions you design to operate on this data.
;;
;; NOTE 3: The tree you will make for C110 will be a bit odd because 210 has 110
;;         as a pre-req, and both 213 and 221 have 210 as a pre-req, and313 has
;;         213 AND 221 as a pre-req, and 317 has 213 AND 221 as a pre-req. As a
;;         result, 313 and 317 will both show up twice in your descendent tree
;          for C110. This is okay for this problem set.
;; NOTE 4: Expect this step of the problem set to take you some time.


(@htdd Course ListOfCourse)
(define-struct course (number credits dependents))
;; Course is (make-course Natural Natural ListOfCourse)
;; interp. a course with a course number,
;;         the number of credits the course is worth, and a
;;         list of courses that list this course as a direct pre-requisite

;; ListOfCourse is one of:
;; - empty
;; - (cons Course ListOfCourse)
;; interp. a list of courses


(define LOC0 empty)

(define C100 (make-course 100 3 LOC0))

(define C107 (make-course 107 3 LOC0))
(define LOC2 (list C107))

(define C110 (make-course 110 4 LOC0))

(define C103 (make-course 103 3 LOC2))

(define C210 (make-course 210 4 (list C110)))

(define C110 (make-course 110 3 (list C210 C213 C221))) 


(define C121 (make-course 121 4 LOC0))


(define C213 (make-course 213 4 (list C121 C210)))
(define C221 (make-course 221 4 (list C210)))

(define LOC1 (list C213 C221))

(define C313 (make-course 313 3 LOC1))
(define C317 (make-course 317 3 LOC1))



(@template-origin Course)

(define (fn-for-course c)
  (... (course-number c)
       (course-credits c)
       (fn-for-loc (course-dependents c))))

(@template-origin ListOfCourse)

(define (fn-for-loc loc)
  (cond [(empty? loc) (...)]
        [else
         (... (fn-for-course (first loc))
              (fn-for-loc (rest loc)))]))

(@problem 2)
;;
;; Design a function that produces the list of all the course numbers in the
;; course's tree including the given course's number.
;;
;; Your @htdf tag and the rest of the design MUST have the definition for
;; the function that takes Course as an argument first. The function that
;; operates on a list must be second.  Marks will only be rewarded for
;; solutions that order the design this way.
;;

(@htdf course-reqs--course course-reqs--loc)
(@signature Course -> ListOfNatural)
(@signature ListOfCourse -> ListOfNatural)
;; Produces list of all course numbers in courses' tree,
;;including given course number

(check-expect (course-reqs--course C100) (cons 100 empty))
(check-expect (course-reqs--course C210) (list 210 110))
(check-expect (course-reqs--course C213) (list 213 121 210 110))
(check-expect (course-reqs--loc LOC1) (list 213 121 210 110 221 210 110))
(check-expect (course-reqs--loc LOC2) (list 107))

;(define (course-reqs--course c) C100);stub
;(define (course-reqs--loc loc) empty); stub

(@template-origin Course)
 
(define (course-reqs--course c)
  (cons (course-number c) (course-reqs--loc (course-dependents c))))

(@template-origin ListOfCourse)

(define (course-reqs--loc loc)
  (cond [(empty? loc) empty]
        [else
         (append (course-reqs--course (first loc))
                 (course-reqs--loc (rest loc)))]))



(@problem 3)
;;
;; Design a function that takes two arguments: a Course and a Natural, in that
;; order. It produces the list of courses in the tree that are worth that
;; many credits or more.
;;
;; Your @htdf tag and the rest of the design MUST have the definition for
;; the function that takes Course as an argument first. The function that
;; operates on a list must be second.  Marks will only be rewarded for
;; solutions that order the design this way.
;;

(@htdf credit-val--course credit-val--loc)
(@signature Course Natural -> ListOfCourse)
(@signature ListOfCourse Natural -> ListOfCourse)
;; Produces list of courses in the tree worth given credits or more

(check-expect (credit-val--loc LOC0 4) empty)
(check-expect (credit-val--course C210 3) (list 210 110))
(check-expect (credit-val--course C213 3) (list 213 121 210 110))
;(check-expect (credit-val--course C313 4) (list 213 121 210 110 221 210 110))
;(check-expect (credit-val--loc LOC1 3) (list 213 121 210 110 221 210 110))
(check-expect (credit-val--loc LOC2 3) (list 107))
;(check-expect (credit-val--loc LOC1 4) (list 213 121 210 110 221 210 110)) 


;(define (credit-val--course c cred ) empty)
;(define (credit-val--loc c cred) empty);stubs

(@template-origin Course)

(define (credit-val--course c cred)
  (if (>= (course-credits c) cred)
      (cons (course-number c) (credit-val--loc (course-dependents c) cred))
      (credit-val--loc (course-dependents c) cred)))

(@template-origin ListOfCourse)

(define (credit-val--loc loc cred)
  (cond [(empty? loc) empty]
        [else
         (append (credit-val--course (first loc) cred)
              (credit-val--loc (rest loc) cred))]))

(@problem 4)
;;
;; Design a function that produces the largest course number in the tree.
;;
;; Your @htdf tag and the rest of the design MUST have the definition for
;; the function that takes Course as an argument first. The function that
;; operates on a list must be second.  Marks will only be rewarded for
;; solutions that order the design this way.
;;

(@htdf largest--course largest--loc)
(@signature Course -> Natural)
(@signature ListOfCourse -> Natural)
;; produces the largest course number in tree

(check-expect (largest--course C100) 100)
(check-expect (largest--loc LOC1) 221)
(check-expect (largest--loc LOC2) 107)
(check-expect (largest--course C317) 317)
(check-expect (largest--course C103) 107)

;(define (largest--course c) 0)
;(define (largest--loc loc) 0);stubs

(@template-origin Course)

(define (largest--course c)
  (max (course-number c) (largest--loc (course-dependents c))))

(@template-origin ListOfCourse)

(define (largest--loc loc)
  (cond [(empty? loc) 0]
        [else
         (max (largest--course (first loc))
              (largest--loc (rest loc)))]))



(@problem 5)
;;
;; Design a function that takes two arguments: a Course and a Natural, in that
;; order. It produces the course in the tree with that course number. If it
;; can't find a course in the given tree with that course number, it signals
;; failure by producing false.
;;
;; Your @htdf tag and the rest of the design MUST have the definition for
;; the function that takes Course as an argument first. The function that
;; operates on a list must be second.  Marks will only be rewarded for
;; solutions that order the design this way.
;;

(@htdf find--course find--loc)
(@signature Course Natural -> Course or false)
(@signature ListOfCourse Natural -> Course or false)
;; searches course tree for given course
;; produces false if course does not exist in tree

(check-expect (find--course C210 110) C110)
(check-expect (find--course C313 221) C221)
(check-expect (find--loc LOC1 213) C213)
(check-expect (find--course C121 0110) false)
(check-expect (find--loc LOC0 110) false)
(check-expect (find--course C100 110) false)


;(define (find--course c cnum) false)
;(define (find--loc c cnum) false);stub

(@template-origin Course)

(define (find--course c cnum)
  (if (= (course-number c) cnum)
      c
       (find--loc (course-dependents c) cnum)))

(@template-origin ListOfCourse try-catch)

(define (find--loc loc cnum)
  (cond [(empty? loc) false]
        [else
         (if (not (false? (find--course (first loc) cnum)));first/false
             (find--course (first loc) cnum)
             (find--loc (rest loc) cnum))]))
            


