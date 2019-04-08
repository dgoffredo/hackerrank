#lang racket

(define-syntax-rule (debug args ...)
  (void)) ; no debugging output
  ; (displayln (~a args ...))) ; debugging output

(define (vector-swap! vec i j)
  (let ([temp (vector-ref vec i)])
    (vector-set! vec i (vector-ref vec j))
    (vector-set! vec j temp)))

(define (vector-add1! vec i)
  (vector-set! vec i (add1 (vector-ref vec i))))

(define (vector-partition! elements
                           begin 
                           end 
                           bucket-offsets
                           [get-bucket identity])
  "Partition in place the slice `[begin, end)` within the `elements` vector.
   Use the `bucket-offsets` vector to keep track of at which offset from the
   beginning of the slice to swap an element in order put it in the
   corresponding bucket. Modify `bucket-offsets` in place so that the
   resulting vector contains the offsets of one-past-the-last of each
   respective bucket. Use the `get-bucket` procedure to transform an element
   to an index into `bucket-offsets`. Note that `bucket-offsets` contains
   offsets into the _slice_ of `elements`; so, to indicate index `begin` of
   `elements`, the pivot offset would be zero."
  ; This point is a little subtle: We want the original value of
  ; `bucket-offsets` preserved for use within this function, but we also want
  ; to modify `bucket-offsets` in place as we swap elements into their
  ; buckets. So, the output parameter is renamed `current-offsets`, and a copy
  ; of the initial value is given the name `bucket-offsets`.
  (unless (= begin end)
    (let ([current-offsets bucket-offsets]
          [bucket-offsets (vector-copy bucket-offsets)]
          [num-buckets (vector-length bucket-offsets)])
      (let loop ([i begin])
        (unless (= i end)
          (let* ([i-bucket (get-bucket (vector-ref elements i))]
                 [j (+ begin (vector-ref current-offsets i-bucket))])
            (debug i ", " j ": " (vector-ref elements i))
            (debug "with current-offsets: " current-offsets)
            (debug "with bucket-offsets: " bucket-offsets)
            (debug "and elements " elements)
            (cond
              ; This element is right where the next element in its bucket
              ; should be; so, good, increment that bucket position and
              ; continue.
              [(= i j)
               (vector-add1! current-offsets i-bucket)
               (debug "moving forward and incrementing")
               (loop (add1 i))
               ]
              ; This element is already in its bucket; so, good, just continue.
              [(and (>= i (+ begin (vector-ref bucket-offsets i-bucket)))
                    (or (= i-bucket (sub1 num-buckets))
                        (< i (+ begin
                                (vector-ref bucket-offsets (add1 i-bucket))))))
               (debug "just moving forward")
               (loop (add1 i))]
              ; This element is not in its bucket. Swap it into the current
              ; bucket, increment that bucket, and then reconsider the same
              ; position (i), since there's a different element there now.
              [else
               (debug "swapping " i " and " j ", and incrementing")
               (vector-add1! current-offsets i-bucket)
               (vector-swap! elements i j) 
               (loop i)])))))))

(define (vector-radix-sort! elements alphabet-min alphabet-max)
  "TODO: document"
  ; If there are no elements, then we're done.
  (unless (empty? elements)
    ; If there are elements, then it's ok to check the length of the first.
    (let* ([alphabet-size (add1 (- alphabet-max alphabet-min))]
           [num-digits (vector-length (vector-ref elements 0))])
      (let recur ([begin 0] [end (vector-length elements)] [digit-index 0])
        (debug
          "looking at indices [" begin ", " end ") at digit " digit-index)
        (debug "in " elements)
        ; If the subvector is empty or if we're out of digits, then done.
        (unless (or (= begin end) (= digit-index num-digits))
          (let ([alphabet-counts (make-vector alphabet-size 0)])
            ; count the number of elements in each bucket
            (for ([element (in-vector elements begin end)])
              (let ([digit-value
                     (- (vector-ref element digit-index) alphabet-min)])
                (vector-add1! alphabet-counts digit-value)))
            (debug "alphabet-counts: " alphabet-counts)
            ; calculate offset into subvector for each bucket 
            (let ([bucket-offsets
                   (build-vector alphabet-size
                     (let ([offset 0])
                       (lambda (i)
                         (cond
                           [(zero? i) 0]
                           [else
                            (set! offset
                              (+ offset (vector-ref alphabet-counts (sub1 i))))
                            offset]))))])
              (debug "bucket-offsets before is " bucket-offsets)
              ; Swap the elements into their respective buckets.
              (vector-partition!
                elements
                begin 
                end 
                bucket-offsets
                (lambda (element) ; get-bucket :: element -> bucket index
                  (- (vector-ref element digit-index) alphabet-min)))
              ; recur for each bucket. `bucket-offsets` now contains the
              ; one-past-the-last index offset for each bucket in `elements`.
              (debug "bucket-offsets after is " bucket-offsets)
              (for/fold ([bucket-begin begin])
                        ([(offset i) (in-indexed (in-vector bucket-offsets))])
                (let ([bucket-end (+ begin offset)])
                  (debug "begin " begin
                             " bucket-begin " bucket-begin
                             " i " i "/" (vector-length bucket-offsets)
                             " offset " offset
                             " bucket-end " bucket-end)
                  (recur bucket-begin bucket-end (add1 digit-index))
                  ; The end of this bucket is the beginning of the next.
                  bucket-end))
              ; Don't return any value. This is an in-place sort.
              (void))))))))

(define (radix-sort elements alphabet-min alphabet-max)
  "Return a sorted copy of the specified list of vectors of integers,
   `elements`, where each integer is between `alphabet-min` and `alphabet-max`,
   inclusive. The behavior is undefined unless all vectors in `elements` have
   the same length."
  (if (empty? elements)
    ; If there are no elements, then we're done.
    '()
    ; If there are elements, then it's ok to check the length of the first.
    (let* ([alphabet-size (add1 (- alphabet-max alphabet-min))]
           [num-digits (vector-length (first elements))])
      ; Bucket elements according to the value in the `digit-index`th place
      ; for each. Then recur on each bucket, with the `digit-index` increased.
      (let recur ([digit-index 0] [elements elements])
        (if (= digit-index num-digits)
          ; end case
          elements
          ; common case
          (let ([buckets (make-vector alphabet-size '())])
            ; Group elements into buckets by value at the `digit-index`th
            ; digit.
            (for ([element (in-list elements)])
              (let ([digit-value
                     (- (vector-ref element digit-index) alphabet-min)])
                (vector-set! buckets
                  digit-value
                  (cons element (vector-ref buckets digit-value)))))

            ; Recur on each bucket, but with the digit increased, and flatten
            ; the results.
            (append*
              (for/list ([bucket (in-vector buckets)])
                (recur (add1 digit-index) bucket)))))))))
