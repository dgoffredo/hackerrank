#lang racket

; This is a naive recusive solution with memoization.

(require srfi/67) ; list-compare, min-compare

(define (memoized proc)
  "Return a procedure that has the same functional behavior as proc, but that
   stores calculated values in a hash table and returns stored values if they
   have been calculated already."
  (let ([memo (make-hash)])
    (lambda args
      (hash-ref! memo args (lambda () (apply proc args))))))

(define-syntax-rule (define-memoized (name args ...) body ...)
  (define name (memoized (lambda (args ...) body ...))))

(define-memoized (minimal-merge left right)
  "Return the lexicographically smallest merge of the specified lists of
   characters."
  (cond
    [(empty? left) right]
    [(empty? right) left]
    [else
      (match-let* ([(list x left-rest ...) left]
                   [(list y right-rest ...) right]
                   [from-left (lambda ()
                                (cons x (minimal-merge left-rest right)))]
                   [from-right (lambda ()
                                  (cons y (minimal-merge left right-rest)))])
        (cond
          [(char<? x y) (from-left)]
          [(char<? y x) (from-right)]
          [else (min-compare list-compare (from-left) (from-right))]))]))

(define (read-all-reversed port)
  "Return a list containing all of the data read from the specified port, but
   the list is backwards, e.g.
       (open-input-port \"foo bar 123\")
   would yield
       '(123 bar foo)"
  (let recur ([data '()] [datum (read port)])
    (if (eof-object? datum)
      data
      (recur (cons datum data) (read port)))))

(define (read-input port)
  "Break up input into a list where each element is a list of two elements,
   where each element is a list of the characters from one of the symbols,
   e.g.
       ignored a b c dee
   becomes
      '( ((#\\a) (#\\b)) ((#\\c) (#\\d #\\e #\\e)) )"
  (let recur ([pairs '()] [stacks (read-all-reversed port)])
    (match stacks
      ['() pairs]
      [(list _) pairs]
      [(list left right rest ...)
       (let* ([left (string->list (symbol->string left))]
              [right (string->list (symbol->string right))]
              [pairs (cons (list right left) pairs)])
         (recur pairs rest))])))
     
(for ([args (read-input (current-input-port))])
  (displayln (apply string (apply minimal-merge args))))