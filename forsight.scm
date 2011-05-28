;;; forsight.scm
;;; by Justin Hamilton
;;; last updated May 27th 2011 (currently not done)
;;; released under BSD 2-clause license

;;; Global variables
(define *prompt* "forsight> ")
(define *dictionary* '(("+" add-f) ("-" sub-f) ("*" mul-f) ("/" div-f) 
		       ("mod" mod-f) ("/mod" mod-div-f) ("and" and-f) 
		       ("or" or-f) ("=" eq-f) (">" gt-f) ("<" lt-f) 
		       ("swap" swap-f) ("dup" dup-f)
		       ("over" over-f) ("drop" drop-f) ("dump" dump-f)
		       ("=0" eq-zero-f) (">0" gt-zero-f) ("<0" lt-zero-f)
		       ("." dot-f) (".s" dots-f) (";" pass-f) (":" colon-f)
		       ("exit" exit-f) ("" pass-f) ("spaces" spaces-f)
		       (".\"" string-f) ("false" false-f) ("true" true-f)
		       ("invert" invert-f)))


;;; Generic functions for ease-of-use
; returns the keys of a hash ((k v) (k v) ... (k v))
(define (hash-keys hash)
  (cond
   ((null? hash)
    (quote ()))
   (else
    (cons (car (car hash)) (hash-keys (cdr hash))))))

; returns the values of a hash
(define (hash-vals hash)
  (cond
   ((null? hash)
    (quote ()))
   (else
    (cons (car (cdr (car hash))) (hash-vals (cdr hash))))))

; returns true if hash has the supplied key
(define (has-key? key hash)
  (cond
   ((member key (hash-keys hash))
    #t)
   (else
    #f)))

; returns the value of the specified key in hash
(define (get-val key hash)
  (cond
   ((null? hash)
    '())
   ((equal? key (car (car hash)))
    (car (cdr (car hash))))
   (else
    (get-val key (cdr hash)))))

; returns a list of characters sans the character sep
(define (string-split sep)
  (lambda (str)
    (letrec ((split-iter
	      (lambda (str i last stop)
		(cond
		 ((eq? i stop)
		  (cons (substring str last stop) '()))
		 ((equal? (substring str i (+ i 1)) sep)
		  (cons (substring str last i) 
			(split-iter str (+ i 1) (+ i 1) stop)))
		 (else
		  (split-iter str (+ i 1) last stop))))))
      (split-iter str 0 0 (string-length str)))))

; splits a string into a list deliminated by spaces
(define space-split (string-split " "))

; returns the last element of a list
(define (last lat)
  (cond
   ((null? lat)
    '())
   ((null? (cdr lat))
    (car lat))
   (else
    (last (cdr lat)))))

; continue to read input until character is input
(define (read-until-maker char)
  (lambda (input)
    (cond
     ((null? input)
      '())
     ((equal? (last input) char)
      input)
     (else
      ((read-until-maker char) (append input (space-split (read-line))))))))

; reads input until a semi-colon in entered
(define read-until-scolon (read-until-maker ";"))

; returns the second element on the stack
(define (second stack)
  (car (cdr stack)))

; returns the stack sans the first two elements
(define (pop2 stack)
  (cdr (cdr stack)))

; performs a binary operation on the stack, pushing the result onto it
(define (binary-maker op)
  (lambda (stack)
    (cond
     ((< (length stack) 2)
      (display "error: stack underflow\n")
      stack)
     (else
      (cons (op (second stack) (car stack)) (pop2 stack))))))
    
; performs a binary logic operation on the stack, pushing the result
(define (bi-logical-maker op)
  (lambda (stack)
    (cond
     ((< (length stack) 2)
      (display "error: stack underflow\n")
      stack)
     ((op (car stack) (second stack))
      (cons -1 (pop2 stack)))
     (else
      (cons 0 (pop2 stack))))))

; performs and/or on the stack, pushing -1 if true, 0 otherwise
(define (and-or-maker op)
  (lambda (stack)
    (cond
     ((< (length stack) 2)
      (display "error: stack underflow\n")
      stack)
     (else
      (let* ((a (eq? 0 (car stack)))
	     (b (eq? 0 (second stack)))
	     (result (eval (cons op (cons a (cons b '()))))))
	(cond
	 (result
	  (cons -1 (pop2 stack)))
	 (else
	  (cons 0 (pop2 stack)))))))))

;;; Forth functions, most of these are defined for clarity
(define add-f (binary-maker +))
  
(define sub-f (binary-maker -))
  
(define mul-f (binary-maker *))
  
(define div-f (binary-maker quotient))

(define mod-f (binary-maker remainder))

(define (mod-div-f stack)
  (cons (car (mod-f stack)) (cons (car div-f stack)) (pop2 stack)))

(define and-f (and-or-maker 'and))
  
(define or-f (and-or-maker 'or))
  
(define eq-f (bi-logical-maker =))
  
; gt-f & lt-f are backwards due to the order of popping
(define gt-f (bi-logical-maker <))

(define lt-f (bi-logical-maker >))

(define (swap-f stack)
  (cons (second stack) (cons (car stack) (pop2 stack))))
  
(define (dup-f stack)
  (cond
   ((< (length stack) 1)
    (error-f stack))
   (else
    (cons (car stack) stack))))
  
(define (over-f stack)
  (cons (second stack) stack))
  
(define (drop-f stack)
  (cdr stack))
   
(define (eq-zero-f stack)
  (eq-f (cons 0 stack)))
  
(define (gt-zero-f stack)
  (gt-f (cons 0 stack)))
  
(define (lt-zero-f stack)
  (lt-f (cons 0 stack)))

(define (error-f stack)
  (display "error: stack underflow\n")
  stack)

(define (dot-f stack)
  (cond
   ((eq? 0 (length stack)) 
    (error-f stack))
   (else
    (display (car stack))
    (cdr stack))))

(define (dots-f stack)
  (display "<")
  (display (length stack))
  (display "> ")
  (display (reverse stack))
  stack)

(define (pass-f stack)
  stack)

(define (spaces-f stack)
  (cond
   ((= (car stack) 0)
    (cdr stack))
   (else
    (display " ")
    (spaces-f (cons (- (car stack) 1) (cdr stack))))))

(define (colon-f input stack)
  (set! *dictionary*
	(append *dictionary* (cons
			      (list (car input)
				    (lambda (x)
				    (eval-f (cdr input) x)))
			      '())))
  stack)

(define (exit-f stack)
  (exit))

(define (string-f input stack)
  (cond
   ((null? input)
    (string-f (read-line) stack))
   ((equal? (car input) "\"")
    (cond
     ((null? (cdr input))
      stack)
     (else
      (eval-f (cdr input) stack))))
   (else
    (display (car input))
    (display " ")
    (string-f (cdr input) stack))))

(define (true-f stack)
  (cons -1 stack))

(define (false-f stack)
  (cons 0 stack))

(define (invert-f stack)
  (cond
   ((eq? (car stack) 0)
    (cons -1 (cdr stack)))
   (else
    (cons 0 (cdr stack)))))

;;; the interpreter
(define (eval-f in-s eval-s)
  (cond
   ((null? in-s)
    (display " <ok>\n")
    eval-s)
   ((number? (car in-s))
    (eval-f (cdr in-s) (cons (car in-s) eval-s)))
   ((string->number (car in-s)) 
    (eval-f (cdr in-s) (cons (string->number (car in-s)) eval-s)))
   ((equal? ":" (car in-s))
    (colon-f (read-until-scolon (cdr in-s)) eval-s))
   ((equal? ".\"" (car in-s))
    (string-f (cdr in-s) eval-s))
   ((has-key? (car in-s) *dictionary*) 
    (let ((fun-f (get-val (car in-s) *dictionary*)))
      (cond
       ((symbol? fun-f)
	(eval-f (cdr in-s) ((eval fun-f) eval-s)))
       (else 
	(eval-f (cdr in-s) ((get-val (car in-s) *dictionary*) eval-s))))))
   (else 
    (display "error: unexpected character\n"))))

; the repl
(define (repl-f stack)
  (display *prompt*)
  (let* ((a (read-line))
        (stack-b (eval-f (space-split a) stack)))
    (repl-f stack-b)))

; main
(display "FORSIGHT 0.1 (27 May 2011)\n")
(repl-f '())

