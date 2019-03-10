#lang racket

(provide matrix               ; constructor that returns a matrix
         matrix?              ; predicate: whether its argument is a matrix
         matrix-set!          ; set a value within the matrix
         matrix-row-count     ; get number of rows in the matrix
         matrix-column-count) ; get number of columns in the matrix

; Just for fun. Might end up using it as a lookup table if I figure out a good
; bottom-up method for this problem.

; A matrix is a procedure that contains a row-major two-dimensional composition
; of vectors. The resulting procedure (the matrix) supports four arities:

; ():          Return the underlying data (vector of row vectors).
; (i):         Return the i'th row vector.
; (i j):       Return the value in the i'th row and the j'th column.
; (i j value): Put value in the i'th row at the j'th column.

(define (matrix row-count column-count [initial-value #f])
  (let ([data (for/vector ([_ (in-range row-count)])
                (make-vector column-count initial-value))])
    (match-lambda*
      ['()              data]
      [(list i)         (vector-ref data i)]
      [(list i j)       (vector-ref (vector-ref data i) j)]
      [(list i j value) (vector-set! (vector-ref data i) j value)])))

; I'm a little uncomfortable with the setter, because it deviates from the
; Scheme convention of using an exclamation mark. So here's a conventional one.
(define (matrix-set! matrix i j value)
  (matrix i j value))

; I'm surprised that this isn't in the standard library.
(define (vector-find vec pred)
  (let loop ([i 0])
    (cond
      [(= i (vector-length vec)) #f]
      [(pred (vector-ref vec i)) i]
      [else (loop (+ i 1))])))

(define (matrix? datum)
  (with-handlers ([exn:fail? (lambda _ #f)])
    (let ([data (datum)])
      ; Invoking a matrix with no arguments returns its inner vector of
      ; vectors. If that works, then we additionally require that all of the
      ; inner vectors have the same length as each other.
      (and 
        ; vector of rows
        (vector? data)
        ; all of the rows are the same length
        (let ([len (vector-length (vector-ref data 0))])
          (not (vector-find data
                 (lambda (row)
                   (not (= len (vector-length row)))))))))))
           
(define (matrix-row-count matrix)
  (vector-length (matrix)))

(define (matrix-column-count matrix)
  (match (matrix)
    [(vector) #f ] ; If there are no rows, we don't know the number of columns.
    [(vector first-row _ ...) (vector-length first-row)]))
