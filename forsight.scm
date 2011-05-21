;;; forsight.scm
;;; by Justin Hamilton
;;; last updated May 20th 2011 (currently not done)
;;; released under BSD 2-clause license

;;; Global variables
(define *prompt* "forsight> ")
(define *dictionary* '(("+" add-f) ("-" sub-f) ("*" mul-f) ("/" div-f) 
		       ("mod" mod-f) ("/mod" mod-div-f) ("and" and-f) 
		       ("or" or-f) ("=" eq-f) (">" gt-f) ("<" lt-f) 
		       ("swap" swap-f) ("dup" dup-f)
		       ("over" over-f) ("drop" drop-f) ("dump" dump-f)
		       ("=0" eq-zero-f) (">0" gt-zero-f) ("<0" lt-zero-f)
		       ("." dot-f) (".s" dots-f) (";" scolon-f) (":" colon-f)))


;;; Generic functions for ease-of-use
; returns the keys of a hash ((k v) (k v) ... (k v))
(define (hash-keys hash)
  (cond
   ((null? hash) (quote ()))
   (else (cons (car (car hash)) (hash-keys (cdr hash))))))

; returns the values of a hash
(define (hash-vals hash)
  (cond
   ((null? hash) (quote ()))
   (else (cons (car (cdr (car hash))) (hash-vals (cdr hash))))))

; returns true if hash has the supplied key
(define (has-key? key hash)
  (cond
   ((member key (hash-keys hash)) #t)
   (else #f)))

; returns the value of the specified key in hash
(define (get-val key hash)
  (cond
   ((null? hash) (quote ()))
   ((equal? key (car (car hash))) (car (cdr (car hash))))
   (else (get-val key (cdr hash)))))

; returns a list of characters sans the character sep
(define (split-string sep str)
  (define (split-iter sep str i last stop)
    (cond
     ((eq? i stop) (cons (substring str last stop) '()))
     ((equal? (substring str i (+ i 1)) sep) 
      (cons (substring str last i) (split-iter sep str (+ i 1) (+ i 1) stop)))
     (else (split-iter sep str (+ i 1) last stop))))
  (split-iter sep str 0 0 (string-length str)))

; returns the second element on the stack
(define (second stack)
  (car (cdr stack)))

; returns the stack sans the first two elements
(define (pop2 stack)
  (cdr (cdr stack)))

; performs a binary operation on the stack, pushing the result onto it
(define (binary-f op stack)
  (cons (op (second stack) (car stack)) (pop2 stack)))
  
; performs a unary operation on the stack, pushing the result onto it
(define (unary-f op stack)
  (cons (op (car stack)) (cdr stack)))

; performs a unary logic operation on the stack, pushing the result
(define (logical-f op stack)
  (cond
   ((op (car stack)) (cons 1 (cdr stack)))
   (else (cons 0 (cdr stack)))))

; performs a binary logic operation on the stack, pushing the result
(define (bi-logical-f op stack)
  (cond
   ((op (car stack) (second stack)) (cons 1 (pop2 stack)))
   (else (cons 0 (pop2 stack)))))


;;; Forth functions, most of these are defined for clarity
(define (add-f stack)
  (binary-f + stack))
  
(define (sub-f stack)
  (binary-f - stack))
  
(define (mul-f stack)
  (binary-f * stack))
  
(define (div-f stack)
  (binary-f / stack))

(define (mod-div-f stack)
  (cons (car (mod-f stack)) (cons (car div-f stack)) (pop2 stack)))

(define (mod-f stack)
  (binary-f remainder stack))

(define (and-f stack)
  (cond
    ((and (car stack) (second stack)) (cons 1 (pop2 stack)))
(else (cons 0 (cdr stack)))))
  
(define (or-f stack)
  (cond
    ((or (car stack) (second stack)) (cons 1 (pop2 stack)))
(else (cons 0 (cdr stack)))))
  
(define (eq-f stack)
  (bi-logical-f equal? stack))
  
(define (gt-f stack)
  (bi-logical-f > stack))

(define (lt-f stack)
  (bi-logical-f < stack))
  
(define (swap-f stack)
  (cons (second stack) (cons (car stack) (pop2 stack))))
  
(define (dup-f stack)
  (cons (car stack) stack))
  
(define (over-f stack)
  (cons (second stack) stack))
  
(define (drop-f stack)
  (cdr stack))
   
(define (eq-zero-f stack)
  (eq-f (cons 0 stack)))
  
(define (gt-zero-f stack)
  (lt-f (cons 0 stack)))
  
(define (lt-zero-f stack)
  (gt-f (cons 0 stack)))

(define (dot-f stack)
  (cond
   ((eq? 0 (length stack)) 
    (display "error: stack underflow\n")
    stack)
   (else
    (display (car stack))
    (display " <ok>")
    (newline)
    (cdr stack))))

(define (dots-f stack)
  (display "<")
  (display (length stack))
  (display "> ")
  (display stack)
  (display " <ok>")
  (newline)
  stack)

(define (scolon-f stack)
  stack)

;;; the interpreter
(define (eval-f in-s eval-s dict)
  (cond
   ((null? in-s) 
    eval-s)
   ((string->number (car in-s)) 
    (eval-f (cdr in-s) (cons (string->number (car in-s)) eval-s) dict))
   ((has-key? (car in-s) dict) 
    (let ((fun-f (eval (get-val (car in-s) dict))))
	  (eval-f (cdr in-s) (fun-f eval-s) dict)))
   (else 
    (display "ERROR: UNEXPECTED CHARACTER"))))

(define (repl-f prompt dict stack)
  (display prompt)
  (let* ((a (read-line))
        (stack-b (eval-f (split-string " " a) stack dict)))
    (repl-f prompt dict stack-b)))

(define (repl-f-iter prompt dict stack)
  (display prompt)
  (let* ((a (read-line))
        (stack-b (eval-f (split-string " " a) stack dict)))
    (repl-f-iter prompt dict stack-b)))

(display "FORSIGHT 0.1 (20 Mar 2011)\n")
(repl-f *prompt* *dictionary* '())

