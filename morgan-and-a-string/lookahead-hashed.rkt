#lang racket

(require racket/generator)

(define (common-prefix-length* left i right j memo-table)
  (or (hash-ref memo-table (cons i j) #f)
    (let loop ([count 0] [i i] [j j])
      (cond
        [(= i (string-length left)) count]
        [(= j (string-length right)) count]
        [(let ([left-char (string-ref left i)]
               [right-char (string-ref right j)])
           (if (char=? left-char right-char)
             (loop (add1 count) (add1 i) (add1 j))
             ; If there're not equal, then we've found the longest common
             ; prefix. Memoize counts for all of the suffixes of that prefix,
             ; and finally return the count.
             (begin
               (for ([k (in-range 1 (add1 count))])
                 (hash-set! memo-table (cons (- i k) (- j k)) k))
               count)))]))))

(define (bounded value max+1 fallback)
  (if (< value max+1)
    value
    fallback))

(define (in-minimal-merge left right)

  (define common-prefix-length
    (let ([table (make-hash)])
      (lambda (left i right j)
        (common-prefix-length* left i right j table))))

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
           ; prefix. If either of the strings ends with the prefix, then
           ; consider its "one after" to be the fist character of the prefix
           ; instead. Pick from the string for which the character after the
           ; prefix is the least.  
           [(list char char)
            (let ([prefix-length (common-prefix-length left i right j)])
              (let ([left-char (string-ref left
                                 (bounded (+ i prefix-length)
                                          (string-length left)
                                          i))]
                    [right-char (string-ref right
                                  (bounded (+ j prefix-length)
                                           (string-length right)
                                           j))])
                (if (char<? left-char right-char)
                  (from-left)
                  (from-right))))]
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
