#lang racket
;;; forsight.rkt
;;; by Justin Hamilton
;;; June 21st, 2011
;;; see LICENSE for info

;;; General procedures
; like string to num, but returns the string if its not a number
(define (string-to-num str)
  (cond
   ((string->number str)
    (string->number str))
   (else 
    str)))

; breaks the input string into tokens, seperating at spaces
(define (tokenize str)
  (define (splitter str i last stop sep)
    (cond
      ((eq? i stop)
       (cons (string-to-num (substring str last stop)) '()))
      ((equal? (string-ref str i) sep)
       (cons (string-to-num (substring str last i)) (splitter str (+ i 1) (+ i 1) stop sep)))
      (else
        (splitter str (+ i 1) last stop sep))))
  (splitter str 0 0 (string-length str) #\space))

; returns all the elements of a list 'left' of a seperator
(define (l-split x sep)
  (cond
   ((null? x)
    '())
   ((equal? (first x) sep)
    '())
   (else
    (cons (first x) (l-split (rest x) sep)))))
   
; returns all the elements of a list 'right' of a seperator
(define (r-split x sep)
  (rest (member sep x)))


;;; Dictionary procedures
(define (dict-keys dict)
  (cond
    ((null? dict)
      '())
    (else
      (cons (first (first dict)) (dict-keys (rest dict))))))

(define (dict-vals dict)
  (cond
    ((null? dict)
      '())
    (else
     (cons (rest (first dict)) (dict-vals (rest dict))))))

(define (dict-key? key dict)
  (member key (dict-keys dict)))

(define (dict-get key dict)
  (cond
    ((null? dict)
      #f)
    ((equal? key (first (first dict)))
      (first (rest (first dict))))
    (else
      (dict-get key (rest dict)))))       

;;; Stack procedures
; pushes item onto stack
(define (push item stack)
  (cons item stack))

; pushes item1 followed by item2 onto the stack
(define (push-2 item1 item2 stack)
  (cons item2 (cons item1 stack)))

(define (pop stack)
  (rest stack))

(define (pop-2 stack)
  (rest (rest stack)))

(define (pop-3 stack)
  (rest (rest (rest stack))))

(define (big-enough? stack num)
  (>= (length stack) num))     


;;; Lambda procedures for composing abstractions
(define (mk-binary op)
  (lambda (stack)
    (cond
      ((big-enough? stack 2)
        (push 
         (op (second stack) (first stack)) 
         (pop-2 stack)))
      (else
       (push #f stack)))))  

; performs a binary logic operation on the stack, pushing the result
(define (mk-bilogical op)
  (lambda (stack)
    (cond
     ((big-enough? stack 2)
      (cond
        ((op (first stack) (second stack))
         (push -1 (pop-2 stack)))
        (else
         (push 0 (pop-2 stack)))))
     (else
      (push #f stack)))))

; performs and/or on the stack, pushing -1 if true, 0 otherwise
(define (mk-andor op)
  (lambda (stack)
    (cond
     ((< (length stack) 2)
      (push #f stack))
     (else
      (let* ((a (not (eq? 0 (car stack))))
	     (b (not (eq? 0 (second stack))))
	     (result (eval (cons op (cons a (cons b '()))) ns)))
	(cond
	 (result
	  (cons -1 (pop-2 stack)))
	 (else
	  (cons 0 (pop-2 stack)))))))))

;;; Forth procedures
(define add-f (mk-binary +))

(define sub-f (mk-binary -))

(define mul-f (mk-binary *))

(define div-f (mk-binary quotient))

(define mod-f (mk-binary remainder))

(define (dot-f stack)
  (cond
    ((big-enough? stack 1)
      (display (first stack))
      (display " ")
      (pop stack))
    (else
      (push #f stack))))

(define (dots-f stack)
  (display "<")
  (display (length stack))
  (display "> ")
  (display (reverse stack))
  (display " ")
  stack)

(define (bye-f stack)
  (display "goodbye!")
  (exit))

(define (dup-f stack)
  (push (first stack) stack))

(define and-f (mk-andor 'and))
  
(define or-f (mk-andor 'or))
  
(define eq-f (mk-bilogical =))
  
; gt-f & lt-f are backwards due to the order of popping
(define gt-f (mk-bilogical <))

(define lt-f (mk-bilogical >))

(define (swap-f stack)
  (cons (second stack) (push (first stack) (pop-2 stack))))

(define (over-f stack)
  (push (second stack) stack))
  
(define (drop-f stack)
  (rest stack))
   
(define (eq-zero-f stack)
  (eq-f (push 0 stack)))
  
(define (gt-zero-f stack)
  (gt-f (push 0 stack)))
  
(define (lt-zero-f stack)
  (lt-f (push 0 stack)))

(define (pass-f stack)
  stack)

(define (spaces-f stack)
  (cond
    ((big-enough? stack 1)
     (cond
       ((= (first stack) 0)
        (rest stack))
       (else
        (display " ")
        (spaces-f (push (- (first stack) 1) (rest stack))))))
    (else
     (push #f stack))))

(define (true-f stack)
  (push -1 stack))

(define (false-f stack)
  (push 0 stack))

(define (invert-f stack)
  (cond
    ((big-enough? stack 1)
     (cond
       ((eq? (car stack) 0)
        (cons -1 (cdr stack)))
       (else
        (cons 0 (cdr stack)))))
    (else
     (push #f stack))))

(define (rot-f stack)
  (cond
   ((< (length stack) 3)
    (push #f stack))
   (else
    (push (second stack)
	  (push (third stack)
		(push (first stack) (pop-3 stack)))))))

(define (print-f stack)
  (cond
    ((equal? (first stack) ")")
     (rest stack))
    (else
     (display (first stack))
     (display " ")
     (print-f (rest stack)))))


;;; Global variables
(define *prompt* "forsight> ")
(define *dictionary* (list (cons "+" add-f) (cons "-" sub-f) (cons "*" mul-f) 
			   (cons "/" div-f) (cons "." dot-f) (cons ".s" dots-f) 
			   (cons "bye" bye-f) (cons "dup" dup-f) (cons "rot" rot-f) 
			   (cons ".(" print-f) (cons ";" pass-f) (cons "" pass-f) 
			   (cons "and" and-f) (cons "or" or-f) (cons "true" true-f)
			   (cons "false" false-f) (cons "drop" drop-f) (cons "eq" eq-f)
			   (cons "<" lt-f) (cons ">" gt-f) (cons "swap" swap-f) 
			   (cons "over" over-f)      (cons "rot" rot-f)))

; the neccessity of this anchor was pointed out to me by offby1 on #racket
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))


;;; repl procedures
; repl takes a stack, and continually loops, passing the remaining data
; stack as input into the program coupled with user input
(define (repl-f d-stack)
  (display *prompt*)
  (let* ((input (read-line))
         (new-stack (interpret-f (tokenize input) d-stack)))
    (cond
      ((or (null? new-stack) (car new-stack))
       (display "<ok>\n")
       (repl-f new-stack))
      (else
       (display "?\n")
       (repl-f (cdr new-stack))))))

; compile allows the creation of new words, as well
; as strings, comparison operators, and use of the return stack
(define (string-f input)
  (define (string-iter input col)
    (cond
     ((null? input)
      (string-iter (tokenize (read-line)) col))
     ((member "\"" input)
      (list (append (l-split input "\"") col) (r-split input "\"")))
     (else
      (string-iter '() (append input col)))))
  (string-iter input '()))

; compile handles "compile-mode" (tenuously called, as no real compiling is
; done). I should clean a lot of this up when I have the chance. Also this
; currently pushes col in reverse, so I set the appending of string output
; to conform to this, although I could just as easily change it.
(define (compile-f input d-stack)
  (define (compile-iter input keyword col r-stack)    
    (cond
      ((null? input)
       (display "\n... ")
       (compile-iter (tokenize (read-line)) keyword col r-stack))
      ((equal? (first input) ";")
        (set! *dictionary* 
              (append (push (cons keyword col) *dictionary*)))
	(rest input))
      ((equal? (first input) ".\"")
       (let ((a (string-f (rest input))))
       (compile-iter (first (rest a)) keyword (append '(")") (reverse (first a)) '(".(") col) r-stack)))
      ((dict-has-key? *dictionary* (first input))
       (compile-iter (rest input) keyword (cons (dict-ref *dictionary* (first input)) col) r-stack))
      (else
       (compile-iter (rest input) keyword (push (first input) col) r-stack))))
;      ((or (symbol? (dict-ref *dictionary* (first input))) (not (dict-ref *dictionary* (first input))))
;       (compile-iter (rest input) keyword (push (first input) col) r-stack))
;      (else
;       (compile-iter (rest input) keyword (append (dict-ref *dictionary* (first input)) col) r-stack))))
  (compile-iter (rest input) (first input) '() '()))

(define (interpret-f input d-stack)
  (cond
    ((null? input)
     d-stack)
    ((and (not (null? d-stack)) (equal? (first d-stack) #f))
     d-stack)
    ((equal? ":" (first input))
     (interpret-f (compile-f (rest input) d-stack) d-stack))
    ((equal? ".(" (first input))
     (interpret-f (print-f (rest input)) d-stack))
    ((number? (first input))
     (interpret-f (rest input) (push (first input) d-stack)))
    ((procedure? (first input))
     (interpret-f (rest input) ((first input) d-stack)))
    ((list? (first input))
     (interpret-f (append (reverse (first input)) (rest input)) d-stack))
    ((dict-has-key? *dictionary* (first input))
     (let ([fun-f (dict-ref *dictionary* (first input))])
       (cond
	((procedure? fun-f)
	 (interpret-f (rest input) (fun-f d-stack)))
	(else
	 (interpret-f (append (reverse fun-f) (rest input)) d-stack)))))
    (else
     (cons #f (rest input)))))

;;; main
(display "FORSIGHT 0.2 (21 June 2011)\n")
(repl-f '())