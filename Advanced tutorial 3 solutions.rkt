;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Advanced tutorial 3 solutions|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; A dictionary contains relationships between variable names and their values
;; We represent a dictionary as a list of "variable bindings" where each variable binding is
;; itself a list of the name of the variable and the value of the variable.  So the binding
;; (list 'a 1) says the variable a is 1.
(define dictionary
  (list
   ;; a has the value 1
   (list 'a 1)
   ;; b has the value 2
   (list 'b 2)
   ;; c has the value 3
   (list 'c 3)
   ;; name has the value "Ian
   (list 'name "Ian")
   ;; + has the same value that Racket has for +
   (list '+ +)
   ;; * has the same value that Racket has for *
   (list '* *)
   ;; Etc.
   (list '- -)
   (list '/ -)
   (list '= =)
   (list '> >)
   (list '< <)))

;; lookup: variable-name, dictionary -> any
;; Returns the value associated with the variable name in the dictionary
(define (lookup variable-name dict)
  (local [(define probe
            ;; Assq returns the sublist of dict that starts with variable-name
            ;; Or false if there isn't any sublist that starts with variable-name.
            (assq variable-name dict))]
    (if (list? probe)
        ;; Found it; return the value, which is the second element of the list
        (second probe)
        ;; Didn't find it; print an error message.
        (error "Unknown variable" variable-name))))

;; evaluate: any -> any
;; Runs the expression and returns its value
(define (evaluate exp)
  (if (list? exp)
      (evaluate-complex exp)
      (evaluate-primitive exp)))

;; evaluate-primitive: non-list -> any
;; Runs a primitive expression (a constant or a variable name)
(define (evaluate-primitive exp)
  (if (symbol? exp)
      (lookup exp dictionary)
      exp))

;; evaluate-complex: list -> any
;; Runs a complex expression and returns its value
(define (evaluate-complex exp)
  (evaluate-procedure-call exp))

;; evaluate-procedure-call: list -> any
;; Runs a procedure call and returns the result
(define (evaluate-procedure-call exp)
  (local [(define values-of-subexpressions
            (map evaluate exp))]
    (apply (first values-of-subexpressions)
           (rest values-of-subexpressions))))


(check-expect (evaluate '1)
              1)
(check-expect (evaluate 'a)
              1)
(check-expect (evaluate '(+ 1 1))
              2)
(check-expect (evaluate '(+ a (* b 2)))
              5)
(check-expect (evaluate '(= a 1))
              true)

