#lang racket

(require "in-diagonals.rkt"
         racket/generator)

(define (common-prefix-length-table left right)
  (define L (string-length left))
  (define R (string-length right))
  (define table (make-hash))

  (for ([coordinates (in-diagonals L R)])
    (match-let* ([(list i j) coordinates]
                 [i (- L i 1)]
                 [j (- R j 1)]
                 [left-char (string-ref left i)]
                 [right-char (string-ref right j)])
      (when (char=? left-char right-char)
        (hash-set! table (cons i j)
          (add1 (hash-ref table (cons (add1 i) (add1 j)) 0))))))

    table)

(define (in-minimal-merge left right)

  (define common-prefix-length
    (let ([table (common-prefix-length-table left right)])
      (lambda (i j)
        (hash-ref table (cons i j)))))

  (in-generator
    (let loop ([i 0] [j 0])

      (define (from-left)
        (yield (string-ref left i))
        (loop (add1 i) j))

      (define (from-right)
        (yield (string-ref right j))
        (loop i (add1 j)))

      (match (list (= i (string-length left)) (= j (string-length right)))
        ; done with left -> yield what remains of right
        [(list #t #f) (from-right)]
        ; done with right -> yield what remains of left
        [(list #f #t) (from-left)]
        ; done with both -> we're done
        [(list #t #t) (void)]
        ; done with neither -> look at the first char of each
        [(list #f #f)
         (match (list (string-ref left i) (string-ref right j))
           ; If the current chars are the same, look ahead past the common
           ; prefix. If one of the strings ends with the prefix, then take one
           ; from that string. Otherwise, pick from the string for which the
           ; character after the prefix is the least.  
           [(list char char)
            (let ([prefix-length (common-prefix-length i j)])
              (cond
                [(>= (+ i prefix-length) (string-length left))
                 (from-left)]
                [(>= (+ j prefix-length) (string-length right))
                 (from-right)]
                [else
                 (let ([left-char (string-ref left (+ i prefix-length))]
                       [right-char (string-ref right (+ j prefix-length))])
                   (if (char<? left-char right-char)
                     (from-left)
                     (from-right)))]))]
           ; if the current chars are different, then pick the lesser
           [(list left-char right-char)
            (if (char<? left-char right-char)
              (from-left)
              (from-right))])]))))

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
   where each element is a string derived from a symbol, e.g.
       ignored a b c dee
   becomes
      '( (\"a\" \"b\") (\"c\" \"dee\") )"
  (let recur ([pairs '()] [stacks (read-all-reversed port)])
    (match stacks
      ['() pairs]
      [(list _) pairs]
      [(list left right rest ...)
       (let* ([left (symbol->string left)]
              [right (symbol->string right)]
              [pairs (cons (list right left) pairs)])
         (recur pairs rest))])))

; for testing read-input
;
; (writeln (read-input (current-input-port)))    

(for ([args (read-input (current-input-port))])
  (for ([char (apply in-minimal-merge args)])
    (display char))
  (newline))
