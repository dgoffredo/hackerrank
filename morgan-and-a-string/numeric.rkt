#lang racket

(provide integer->vector)

(require srfi/43) ; vector-reverse-copy

(define (integer->vector nonnegative-integer
                         num-bytes 
                         #:endianness [endianness 'big] 
                         #:bits-per-byte [bits-per-byte 8])
  "Return a vector containing the lowest 'num-bytes' of a byte-wise
   representation of 'nonnegative-integer'. The resulting bytes will be
   arranged according to the 'endianness'. Each byte is a nonnegative integer."
  (if (not (nonnegative-integer? nonnegative-integer))
    (raise-argument-error
      'nonnegative-integer "nonnegative-integer?" nonnegative-integer)
    (let* ([mask (sub1 (expt 2 bits-per-byte))] ; e.g. 0xFF
           [bytes-vector
            (for/vector ([i (in-range num-bytes)])
              (let* ([shift (- (* i bits-per-byte))]
                     [shifted (arithmetic-shift nonnegative-integer shift)])
                (bitwise-and shifted mask)))])
      (match endianness
        ['little bytes-vector]
        ['big    (vector-reverse-copy bytes-vector)]))))
