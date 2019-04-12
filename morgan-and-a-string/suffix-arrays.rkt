#lang racket

; This solution uses two suffix arrays. One for the string L + R, and one for
; the string R + L. It uses the fact that, with the exception of a corner case,
; you can break ties by lexicographically comparing the suffixes of the tied
; character in each string, respectively. Suffix arrays make this comparison
; possible in constant time, once linear or (in this case, n log^2 n) time has
; been spent constructing the suffix arrays.

(require ; "suffix-array-prefix-doubling.rkt"
         "suffix-array-prefix-doubling-radix.rkt"
         racket/generator)

(define (in-minimal-merge left right)
  "Yield characters from the front of `left` and `right` such that the
   resulting merger is lexicographically least."
  (in-generator
    ; The corner case mentioned above is dealt with by appending to each string
    ; a character greater than any character otherwise in the strings. Since
    ; these strings consist of all uppercase letters, I use the character "[".
    (match-let* ([L (string-length left)]
                 [R (string-length right)]
                 [alphabet-min #\A] ; by assumption
                 [alphabet-max #\[] ; by assumption
                 [(cons _ ~saLR) ; inverse "suffix array left-right"
                  (suffix-array (string-append left right "[")
                                alphabet-min
                                alphabet-max
                                #:with-inverse #t)]
                 [(cons _ ~saRL) ; inverse "suffix array right-left"
                  (suffix-array (string-append right left "[")
                                alphabet-min
                                alphabet-max
                                #:with-inverse #t)])
      (let loop ([i 0] [j 0])
        ; i = L and j = R are end cases, so handle all the combinations of that
        ; first. Then the common case is the last one, where neither string is
        ; exhausted yet.
        (match (cons (= i L) (= j R))
          [(cons #t #t) (void)]
          [(cons #t #f)
           (yield (string-ref right j))
           (loop i (add1 j))]
          [(cons #f #t)
           (yield (string-ref left i))
           (loop (add1 i) j)]
          [(cons #f #f)
           (let ([left-char (string-ref left i)]
                 [right-char (string-ref right j)])
             (cond
               [(char<? left-char right-char)
                (yield left-char)
                (loop (add1 i) j)]
               [(char<? right-char left-char)
                (yield right-char)
                (loop i (add1 j))]
               [else
                ; TODO: explain (from the notes .txt file)
                (cond
                  [(< (vector-ref ~saLR i) (vector-ref ~saLR (+ L j)))
                   (yield left-char)
                   (loop (add1 i) j)]
                  [(< (vector-ref ~saRL j) (vector-ref ~saRL (+ R i)))
                   (yield right-char)
                   (loop i (add1 j))]
                  [(< (- L i) (- R j))
                   (yield left-char)
                   (loop (add1 i) j)]
                  [else
                   (yield right-char)
                   (loop i (add1 j))])]))])))))

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

(for ([args (read-input (current-input-port))])
  (for ([char (apply in-minimal-merge args)])
    (display char))
  (newline))
