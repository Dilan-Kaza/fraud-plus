#lang racket

(provide make-test-suite
         make-io-test-suite
         run-tests)

(require rackunit
         rackunit/text-ui)

(define (make-test-suite name run-proc)
  (test-suite
   name

   ;; Abscond examples
   (test-suite
    "Abscond"
    (check-equal? (run-proc 7) 7)
    (check-equal? (run-proc -8) -8))

   ;; Blackmail examples
   (test-suite
    "Blackmail"
    (check-equal? (run-proc '(add1 (add1 7))) 9)
    (check-equal? (run-proc '(add1 (sub1 7))) 7))

   ;; Con examples
   (test-suite
    "Con"
    (check-equal? (run-proc '(if (zero? 0) 1 2)) 1)
    (check-equal? (run-proc '(if (zero? 1) 1 2)) 2)
    (check-equal? (run-proc '(if (zero? -7) 1 2)) 2)
    (check-equal? (run-proc '(if (zero? 0)
                                 (if (zero? 1) 1 2)
                                 7))
                  2)
    (check-equal? (run-proc '(if (zero? (if (zero? 0) 1 0))
                                 (if (zero? 1) 1 2)
                                 7))
                  7))

   ;; Dupe examples
   (test-suite
    "Dupe"
    (check-equal? (run-proc #t) #t)
    (check-equal? (run-proc #f) #f)
    (check-equal? (run-proc '(if #t 1 2)) 1)
    (check-equal? (run-proc '(if #f 1 2)) 2)
    (check-equal? (run-proc '(if 0 1 2)) 1)
    (check-equal? (run-proc '(if #t 3 4)) 3)
    (check-equal? (run-proc '(if #f 3 4)) 4)
    (check-equal? (run-proc '(if 0 3 4)) 3)
    (check-equal? (run-proc '(zero? 4)) #f)
    (check-equal? (run-proc '(zero? 0)) #t))

   ;; Dupe+ examples
   (test-suite
    "Dupe+"
    (check-equal? (run-proc '(not #t)) #f)
    (check-equal? (run-proc '(not #f)) #t)
    (check-equal? (run-proc '(not 7)) #f)
    (check-equal? (run-proc '(cond [else #t])) #t)
    (check-equal? (run-proc '(cond [(not #t) 2] [else 3])) 3)
    (check-equal? (run-proc '(cond [(if #t #t #f) 2] [else 3])) 2)
    (check-equal? (run-proc '(cond [(zero? 1) 2] [(if (not (zero? (sub1 2))) #t #f) 4] [else 3])) 4)
    (check-equal? (run-proc '(cond [#t 1] [else 2])) 1)
    (check-equal? (run-proc '(cond [1 1] [else 2])) 1)

    (check-equal? (run-proc '(case 2 [else 1])) 1)
    (check-equal? (run-proc '(case 2 [() 3] [else 1])) 1)
    (check-equal? (run-proc '(case 2 [(2) 3] [else 1])) 3)
    (check-equal? (run-proc '(case 4 [(2) 3] [else 1])) 1)
    (check-equal? (run-proc '(case 2 [(7 2) 3] [else 1])) 3)
    (check-equal? (run-proc '(case 4 [(7 2) 3] [else 1])) 1)
    (check-equal? (run-proc '(case 2 [(7 2 #t) 3] [else 1])) 3)
    (check-equal? (run-proc '(case 4 [(7 2 #t) 3] [else 1])) 1)
    (check-equal? (run-proc '(case #t [(7 2 #t) 3] [else 1])) 3)
    (check-equal? (run-proc '(case #f [(7 2 #t) 3] [else 1])) 1))

   ;; Dodger examples
   (test-suite
    "Dodger"
    (check-equal? (run-proc #\a) #\a)
    (check-equal? (run-proc #\b) #\b)
    (check-equal? (run-proc '(char? #\a)) #t)
    (check-equal? (run-proc '(char? #t)) #f)
    (check-equal? (run-proc '(char? 8)) #f)
    (check-equal? (run-proc '(char->integer #\a)) (char->integer #\a))
    (check-equal? (run-proc '(integer->char 955)) #\λ))

   ;; Evildoer examples
   (test-suite
    "Evildoer"
    (check-equal? (run-proc '(void)) (void))
    (check-equal? (run-proc '(begin 1 2)) 2)
    (check-equal? (run-proc '(eof-object? (void))) #f))

   ;; Extort examples
   (test-suite
    "Extort"
    (check-equal? (run-proc '(add1 #f)) 'err)
    (check-equal? (run-proc '(sub1 #f)) 'err)
    (check-equal? (run-proc '(zero? #f)) 'err)
    (check-equal? (run-proc '(char->integer #f)) 'err)
    (check-equal? (run-proc '(integer->char #f)) 'err)
    (check-equal? (run-proc '(integer->char -1)) 'err)
    (check-equal? (run-proc '(write-byte #f)) 'err)
    (check-equal? (run-proc '(write-byte -1)) 'err)
    (check-equal? (run-proc '(write-byte 256)) 'err)
    (check-equal? (run-proc '(begin (integer->char 97)
                                    (integer->char 98)))
                  #\b))

   ;; Fraud examples
   (test-suite
    "Fraud"
    (check-equal? (run-proc '(let ([x 7]) x)) 7)
    (check-equal? (run-proc '(let ([x 7]) 2)) 2)
    (check-equal? (run-proc '(let ([x 7]) (add1 x))) 8)
    (check-equal? (run-proc '(let ([x (add1 7)]) x)) 8)
    (check-equal? (run-proc '(let ([x 7]) (let ((y 2)) x))) 7)
    (check-equal? (run-proc '(let ([x 7]) (let ((x 2)) x))) 2)
    (check-equal? (run-proc '(let ([x 7]) (let ((x (add1 x))) x))) 8)
    (check-equal? (run-proc '(let ([x 0])
                               (if (zero? x) 7 8)))
                  7)
    (check-equal? (run-proc '(let ([x 1])
                               (add1 (if (zero? x) 7 8))))
                  9)
    (check-equal? (run-proc '(+ 3 4)) 7)
    (check-equal? (run-proc '(- 3 4)) -1)
    (check-equal? (run-proc '(+ (+ 2 1) 4)) 7)
    (check-equal? (run-proc '(+ (+ 2 1) (+ 2 2))) 7)
    (check-equal? (run-proc '(let ([x (+ 1 2)])
                               (let ([z (- 4 x)])
                                 (+ (+ x x) z))))
                  7)
    (check-equal? (run-proc '(= 5 5)) #t)
    (check-equal? (run-proc '(= 4 5)) #f)
    (check-equal? (run-proc '(= (add1 4) 5)) #t)
    (check-equal? (run-proc '(< 5 5)) #f)
    (check-equal? (run-proc '(< 4 5)) #t)
    (check-equal? (run-proc '(< (add1 4) 5)) #f))

   ;; Fraud+ examples
   (test-suite
    "Fraud+"
    (check-equal? (run-proc '(integer? 7)) #t)
    (check-equal? (run-proc '(integer? #f)) #f)
    (check-equal? (run-proc '(boolean? 7)) #f)
    (check-equal? (run-proc '(boolean? #f)) #t)
    (check-equal? (run-proc '(boolean? #t)) #t)
    (check-equal? (run-proc '(let () 7)) 7)
    (check-equal? (run-proc '(let ([x 1]) x)) 1)
    (check-equal? (run-proc '(let* () 7)) 7)
    (check-equal? (run-proc '(let* ([x 1]) x)) 1)
    (check-equal? (run-proc '(let ([x 1] [y 2]) (+ x y))) 3)
    (check-equal? (run-proc '(let ([x 1])
                               (let* ([x 2] [x x]) x)))
                  2)
    (check-equal? (run-proc '(+)) (+))
    (check-equal? (run-proc '(+ 5)) 5)
    (check-equal? (run-proc '(+ 1 2 3)) 6))

   ;; Student examples
   (test-suite
    "Student"
    ;; TODO: You may want to add more tests here.

    (check-equal? (run-proc '(- 5)) -5)
    (check-equal? (run-proc '(if (not (zero? (sub1 2))) #t #f)) #t)
    (check-equal? (run-proc '(if (if (not (zero? (sub1 2))) #t #f) 1 2)) 1)
    (check-equal? (run-proc '(let ([x (+ 1 2)]) (+ x x)))6)
    (check-equal? (run-proc '(let ([x (+ 1 2)]) x))3)
    (check-equal? (run-proc '(+ #f 1)) 'err)
    (check-equal? (run-proc '(+ 1 #f)) 'err)
    (check-equal? (run-proc '(abs #f)) 'err)
    (check-equal? (run-proc '(- #f)) 'err)
    (check-equal? (run-proc '(let ([x (abs #f)]) x)) 'err)
    (check-equal? (run-proc '(cond [(abs #f) 1] [else 1])) 'err)
    (check-equal? (run-proc '(cond [#t (abs #f)] [else 1])) 'err)
    (check-equal? (run-proc '(cond [#f 1] [else (abs #f)])) 'err)
    (check-equal? (run-proc '(case 2 [(#f 1) 3] [else (abs #f)])) 'err)
    (check-equal? (run-proc '(case 1 [(#f 1) (abs #f)] [else 2])) 'err)
    (check-equal? (run-proc '(case (abs #f) [(#f 1) 2] [else (write-byte 97)])) 'err)
    (check-equal? (run-proc '(let ([x 1] [y 2] [z (abs #f)]) x)) 'err)
    (check-equal? (run-proc '(let ([x 1] [y 2] [z 3]) (abs #f))) 'err)
    (check-equal? (run-proc '(let ([x #f] [y 2] [z 3]) (abs x))) 'err)
    (check-equal? (run-proc '(let* ([x 1] [y x] [z (abs #f)]) x)) 'err)
    (check-equal? (run-proc '(let* ([x 1] [y x] [z y]) (abs #f))) 'err)
    (check-equal? (run-proc '(let* ([x #f] [y x] [z y]) (abs z))) 'err)
    (check-equal? (run-proc '(not (abs #f))) 'err)
    (check-equal? (run-proc '(integer? (abs #f))) 'err)
    (check-equal? (run-proc '(boolean? (abs #f))) 'err)
    (check-equal? (run-proc '(abs (abs #f))) 'err)
    (check-equal? (run-proc '(- (abs #f))) 'err)
    

    )))

;; NOTE: [run/io-proc] takes two arguments: a quoted input expression and a
;; string containing the text that can be read by operations like [read-byte].
;; The result is a pair (built with [cons]) containing the result and a string
;; containing the output. This means that a properly formed check looks like:
;;
;;   (check-equal? (run/io-proc <program> <input>) (cons <result> <output>))
;;
;; For example:
;;
;;   (check-equal? (run/io-proc '(add1 (read-byte)) "a") (cons 98 ""))
(define (make-io-test-suite name run/io-proc)
  (test-suite
   name

   ;; Evildoer examples
   (test-suite
    "Evildoer"
    (check-equal? (run/io-proc 7 "") (cons 7 ""))
    (check-equal? (run/io-proc '(write-byte 97) "") (cons (void) "a"))
    (check-equal? (run/io-proc '(read-byte) "a") (cons 97 ""))
    (check-equal? (run/io-proc '(begin (write-byte 97) (read-byte)) "b")
                  (cons 98 "a"))
    (check-equal? (run/io-proc '(read-byte) "") (cons eof ""))
    (check-equal? (run/io-proc '(eof-object? (read-byte)) "") (cons #t ""))
    (check-equal? (run/io-proc '(eof-object? (read-byte)) "a") (cons #f ""))
    (check-equal? (run/io-proc '(begin (write-byte 97) (write-byte 98)) "")
                  (cons (void) "ab"))
    (check-equal? (run/io-proc '(peek-byte) "ab") (cons 97 ""))
    (check-equal? (run/io-proc '(begin (peek-byte) (read-byte)) "ab") (cons 97 ""))
    (check-equal? (run/io-proc '(read-byte) "†") (cons 226 "")))

   ;; Extort examples
   (test-suite
    "Extort"
    (check-equal? (run/io-proc '(write-byte #t) "") (cons 'err "")))

   ;; Fraud examples
   (test-suite
    "Fraud"
    (check-equal? (run/io-proc '(let ([x 97]) (write-byte x)) "") (cons (void) "a"))
    (check-equal? (run/io-proc '(let ([x 97])
                                  (begin (write-byte x)
                                         x))
                               "")
                  (cons 97 "a"))
    (check-equal? (run/io-proc '(let ([x 97]) (begin (read-byte) x)) "b")
                  (cons 97 ""))
    (check-equal? (run/io-proc '(let ([x 97]) (begin (peek-byte) x)) "b")
                  (cons 97 "")))

   ;; Student examples
   (test-suite
    "Student"
    ;; TODO: You may want to add more tests here.
    )))
