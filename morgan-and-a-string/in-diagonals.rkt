#lang racket

(provide in-diagonals)

(require racket/generator) ; in-generator, yield

; For a matrix with dimensions (row-count, column-count),
; (in-diagonals row-count column-count) returns a sequence of coordinates,
; each expressed as (list row-index column-index), such that the entire matrix
; is covered in the following diagonal striping:
;
; The number inside the box indicates the index of the element in the sequence
; whose value is (list i j).
;
;                               (j)------->
;
;                   0       1       2       3       ...       column-count - 1
;                -----------------------------------   -----------------------
;       0        |  0       2       5       9
;                |
;       1        |  1       4       8      ...
;  (i)           |
;   |   2        |  3       7      ...
;   |            |
;   V   3        |  6      ...
;                |
;      ... 
;                |
; row-count - 1  |  ...
;
;

(define (in-diagonals row-count column-count)
  (in-generator
    ; diag is "which diagonal we're on," e.g. "diagonal 0" or "diagonal 2."
    (for ([diag (in-range (+ row-count column-count))])
      ; For a given diagnonal, figure out exactly where the diagonal begins.
      ; The beginning point moves down the first column and then across the
      ; last row (see ASCII diagram above).
      ; Once we know where the diagonal starts, then we can calculate how long
      ; it will be, and then we can yield the points of the diagonal, from
      ; bottom left to top right (as oriented in diagram).
      (let* ([start-column
              (if (< diag row-count)
                0
                (add1 (- diag row-count)))]
             [start-row
              (if (< diag row-count)
                diag
                (sub1 row-count))]
             [diag-length
              (min (add1 start-row) (- column-count start-column))])
        (for ([i (in-range diag-length)])
          (yield (list (- start-row i) (+ start-column i))))))))

; for testing
; (define row-count (read))
; (define column-count (read))
; (for ([element (in-diagonals row-count column-count)])
;   (displayln element))
