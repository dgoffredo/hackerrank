
"""brute force

The idea here is to find all 3x3 magic squares, and then take the minimum
"distance" (cost) between the input square and each magic one.
"""

import itertools
import sys

# A square is a list of lists of integers, where each inner list is a row,
# from top to bottom.  For example, if the square would be printed as
#
#     a b c
#     d e f
#     h i j
#
# then its representation would be
#
#     [[a, b, c],
#      [d, e, f],
#      [h, i, j]]


def permutation_to_square(numbers):
    return [numbers[0:3], numbers[3:6], numbers[6:9]]


def is_magic(square):
    """This function assumes that square's integers are already distinct."""
    ((a, b, c), (d, e, f), (h, i, j)) = square
    return a + b + c == \
           d + e + f == \
           h + i + j == \
           a + d + h == \
           b + e + i == \
           c + f + j == \
           a + e + j == \
           h + e + c


def difference(from_square, to_square):
    return sum(abs(from_square[i][j] - to_square[i][j]) \
               for i in range(3) for j in range(3))
    

magic_squares = []
for permutation in itertools.permutations(list(range(1, 10))):
    square = permutation_to_square(permutation)
    if is_magic(square):
        magic_squares.append(square)


def read_row(file):
    return [int(cell) for cell in file.readline().split()]


def read_square(file):
    row1 = read_row(file)
    row2 = read_row(file)
    row3 = read_row(file)
    return [row1, row2, row3]


square = read_square(sys.stdin)

print(min(difference(square, magic) for magic in magic_squares))