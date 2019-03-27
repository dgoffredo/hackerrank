#lang racket

(provide suffix-array)

(struct suffix
  (rank-high ; integer
   rank-low  ; integer
   string-index)
   #:transparent
   #:mutable)

(define (suffix<? left right)
  "Compare by rank-high and current-low, ignoring string-index."
  (match (cons left right)
    [(cons (suffix a0 a1 _) (suffix b0 b1 _))
     (if (= a0 b0)
       (< a1 b1)
       (< a0 b0))]))    

(define (suffix-array string #:with-inverse [with-inverse #f])
  "Return a vector that is the suffix array of `string`. If `with-inverse`,
   then return a pair containing the suffix array and another vector that maps
   every string index to the index in the suffix array of the suffix beginning
   at that character in the string, e.g. 

       (suffix-array \"banana\")
       ; -> #(6 5 3 1 0 4 2)

       (suffix-array \"banana\" #:with-inverse #t)
       ; -> (cons #(6 5 3 1 0 4 2) #(4 3 6 2 5 1 0))"

  (define n (string-length string))

  (match n
    [0 #(0)]   ; empty string has one suffix: the end
    [1 #(0 1)] ; string of length one has two suffixes: itself and the end
    [_         ; otherwise...
     (define (string-at index)
       (if (< index n)
         (char->integer (string-ref string index))
         -1)) ; -1 is less than any character value -- it indicates "end"

     ; This vector is what we'll be sorting over and over to calculate result.
     ; Start with the "ranks" being the first two characters of each suffix.
     (define suffixes
       (for/vector ([i (in-range (add1 n))])
         (suffix (string-at i) (string-at (add1 i)) i)))

     ; Sort the suffixes, initially by their first two characters.
     (vector-sort! suffixes suffix<?)

     ; As we sort `suffixes`, the location of the suffix in `suffixes` given
     ; the index of its starting character in `string` will change. Store that
     ; mapping here, which we'll update after each sort. 
     (define suffix-locations (make-vector (add1 n)))

     (define (update-suffix-locations!)
       (for ([(current-suffix i) (in-indexed suffixes)])
         (match current-suffix
           [(suffix _ _ string-index)
            ; beginning of suffix -> index of character beginning the suffix
            (vector-set! suffix-locations string-index i)])))

     ; Initial values for suffix-locations
     (update-suffix-locations!)

     ; Get the powers of 2 from [2, 2*n). This is what we'll loop over.
     (define lengths
       (reverse
         (let loop ([k 2] [lengths '()])
           (if (< k (* 2 n))
             (loop (* 2 k) (cons k lengths))
             lengths))))

     ; TODO: explain
     (for ([prefix-length (in-list lengths)])
       ; The rank-high is the number of suffixes whose rank is less than the
       ; current suffix's.
       (let loop ([i 0] [largest-suffix (suffix -1 -1 #f)] [current-rank 0])
         (when (<= i n)
           (let ([current-suffix (vector-ref suffixes i)])
             (if (suffix<? largest-suffix current-suffix)
               (let ([new-largest (struct-copy suffix current-suffix)])
                 (set-suffix-rank-high! current-suffix i)
                 (loop (add1 i) new-largest i))
               (begin
                 (set-suffix-rank-high! current-suffix current-rank)
                 (loop (add1 i) largest-suffix current-rank))))))

       ; The rank-low is the rank-high of the suffix that is prefix-length
       ; places away from the current suffix, in the input string. If that
       ; would be past the end, then rank-low is -1.
       (for ([current-suffix (in-vector suffixes)])
         (set-suffix-rank-low! current-suffix
           (let ([next-string-index
                  (+ (suffix-string-index current-suffix) prefix-length)])
             (if (>= next-string-index n)
               -1 ; the next suffix is beyond the end of the string
               (let* ([next-suffix-index
                       (vector-ref suffix-locations next-string-index)]
                      [next-suffix
                       (vector-ref suffixes next-suffix-index)])
                 (suffix-rank-high next-suffix))))))

       ; Now that we've defined the new ranks, sort by them.
       (vector-sort! suffixes suffix<?)

       ; Make sure that, given an index into the string, we can find the
       ; index of its suffix in suffixes.
       (update-suffix-locations!))

     (let ([result (vector-map suffix-string-index suffixes)])
       (if with-inverse
         ; If the caller said they want the "inverse," include locations.
         (cons result suffix-locations)
         ; Otherwise, just return the suffix array.
         result))]))

; for testing suffix-array
; (let ([string (vector-ref (current-command-line-arguments) 0)])
  ; (writeln (suffix-array string #:with-inverse #t)))
;   (writeln (suffix-array string)))
