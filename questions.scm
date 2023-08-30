(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

;; Problem 15
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 15
  (define (helper n x s)
     (cond 
        ((null? s) n)
        (else 
            (define n (append n (list (list x (car s)))))
            (helper n (+ x 1) (cdr s))
        )
     )
  )
  (helper '() 0 s)
)
  ; END PROBLEM 15

;; Problem 16

;; Merge two lists LIST1 and LIST2 according to ORDERED? and return
;; the merged lists.
(define (merge ordered? list1 list2)
  ; BEGIN PROBLEM 16
  (define (helper ordered? list1 list2 merged_list)
    (cond 
      ((null? list1) 
        (if (null? list2) merged_list (append merged_list list2))
      )
      ((null? list2) 
        (if (null? list1) merged_list (append merged_list list1))
      )
      ((ordered? (car list1) (car list2))
        (define merged_list (append merged_list (list (car list1) (car list2))))
        (helper ordered? (cdr list1) (cdr list2) merged_list)
      )
      (else 
        (define merged_list (append merged_list (list (car list2) (car list1))))
        (helper ordered? (cdr list1) (cdr list2) merged_list)
      )
    )
  )
  (helper ordered? list1 list2 '())
)
  ; END PROBLEM 16

;; Optional Problem

;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN OPTIONAL PROBLEM
         'replace-this-line
         ; END OPTIONAL PROBLEM
         )
        ((quoted? expr)
         ; BEGIN OPTIONAL PROBLEM
         'replace-this-line
         ; END OPTIONAL PROBLEM
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN OPTIONAL PROBLEM
           'replace-this-line
           ; END OPTIONAL PROBLEM
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN OPTIONAL PROBLEM
           'replace-this-line
           ; END OPTIONAL PROBLEM
           ))
        (else
         ; BEGIN OPTIONAL PROBLEM
         'replace-this-line
         ; END OPTIONAL PROBLEM
         )))

; Some utility functions that you may find useful to implement for let-to-lambda

(define (zip pairs)
  'replace-this-line)
