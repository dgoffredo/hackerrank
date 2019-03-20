#lang racket

; This is a dynamic programming solution that tabulates intermediate results
; over the space (i, j) of indices into the two input strings.
;
; It seems to suck worse than the memoized versions.

(require "matrix.rkt"
         "in-diagonals.rkt"
         srfi/67) ; list-compare, min-compare

(define-syntax-rule (min-list lists ...)
  ; Return the lexicographically least list in lists.
  (min-compare list-compare lists ...))

(define (min-list-of-char left right)
  (min-list left right))

#;(define (min-list-of-char left right)
 (let recur ([left-suffix left] [right-suffix right])
   (match (cons left-suffix right-suffix)
     [(cons '() _) '()]
     [(cons _ '()) '()]
     [(cons (cons left-char left-rest) (cons right-char right-rest))
      (cond
        [(char<? left-char right-char) left]
        [(char<? right-char left-char) right]
        [(eq? left-rest right-rest)    left] ; arbitrary
        [else
          (recur left-rest right-rest)])])))

(define (intermediate-results-table left right)
  (let* ([L (string-length left)]
         [R (string-length right)]
         [table (matrix (add1 L) (add1 R))]) ; Extra row/column. See below.
    ; Populate the last row and last column with the appropriate "no choice"
    ; values.
    ; First, seed the bottom right corner with '() so that it can be the end
    ; of all the lists we'll be creating.
    (matrix-set! table L R '())

    ; the last column
    (for ([i (in-range (sub1 L) -1 -1)]) ; L-1, L-2, ..., 1, 0
      (let ([char (string-ref left i)]
            [suffix (table (add1 i) R)])
        (matrix-set! table i R (cons char suffix))))

    ; the last row
    (for ([j (in-range (sub1 R) -1 -1)]) ; R-1, R-2, ..., 1, 0
      (let ([char (string-ref right j)]
            [suffix (table L (add1 j))])
        (matrix-set! table L j (cons char suffix))))

    ; Traversing the table in diagonals beginning at the bottom right, fill in
    ; the value (cons cell).
    (for ([coordinates (in-diagonals L R)])
      (match-let* ([(list i j) coordinates] ; from (0, 0) diagonally
                   [i (- L 1 i)]            ; renaming i and j...
                   [j (- R 1 j)]            ; now from (L-1, R-1) diagonally
                   [left-char (string-ref left i)]
                   [right-char (string-ref right j)]  ;  ---------------------
                   [below (table (add1 i) j)]         ; |  @(i, j)   rightward
                   [rightward (table i (add1 j))])    ; |   below       ...
        (matrix-set! table i j
          (cond
            ; If the current character in the left string comes before the
            ; current character in the right string, then we take from the
            ; left string and carry on with its suffix and the same right
            ; string.
            [(char<? left-char right-char) (cons left-char below)]
            ; Vice versa if it's the current character in the right string
            ; that's smaller.
            [(char<? right-char left-char) (cons right-char rightward)]
            ; Otherwise, the current characters are the same, so cons either
            ; char (say, left-char) with whichever one of below and rightward
            ; is lexicographically less. 
            [else (cons left-char (min-list-of-char below rightward))]))))

    ; Return the table of intermediate results. The answer is element (0, 0).
    table))

(define (minimal-merge left right)
  (let* ([table (intermediate-results-table left right)]
         [chars (table 0 0)])
    (apply string chars)))

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
  "Break up input into a list where each element is a list of two strings,
   e.g.
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
  (displayln (apply minimal-merge args)))
