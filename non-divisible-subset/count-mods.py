
"""count-mods

This solution is based on the first comment (as of this writing) in the
"discussion" section of the problem.

The key observation is that the sum of two numbers is divisible by k if and
only if the sum of their remainders (mod k) is k or zero.

So, since we need find only the _size_ of the largest qualifying set, we can
reason about the counts of buckets by mod k.

We can calculate the remainder (mod k) of each element in the input in linear
time.

Remainder zero is a little special because there can be at most one number
in the solution that is divisible by k, so that's treated explicitly.
Similarly, the middle remainder for even k is special (at most one such number
can appear in the result set).
"""


from collections import defaultdict
import sys


def group_by_modulo(sequence, divisor):
    counts = defaultdict(int)
    for number in sequence:
        counts[number % divisor] += 1

    return counts


def largest_nondivisible_subset_size(sequence, divisor):
    by_modulo = group_by_modulo(sequence, divisor)
    max_size = 0

    # If there are numbers divisible by the divisor, include just one of them.
    if 0 in by_modulo:
        max_size += 1

    for remainder in range(1, divisor // 2 + 1):
        mate = divisor - remainder
        if mate == remainder:
            # Similar to the case with zero.
            if remainder in by_modulo:
                max_size += 1
        else:
            max_size += max(by_modulo[remainder], by_modulo[mate])

    return max_size


def read_row(file):
    return [int(word) for word in sys.stdin.readline().split()]


size, divisor = read_row(sys.stdin)
numbers = read_row(sys.stdin)

assert len(numbers) == size
numbers = set(numbers) # de-dupe

print(largest_nondivisible_subset_size(numbers, divisor))
