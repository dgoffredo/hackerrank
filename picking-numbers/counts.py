
"""counts

See how much of each number there are in the input, and check, for each number,
the counts of it with either of its successor (+1) or predecessor (-1) or just
itself.
"""

from collections import defaultdict
import sys


def histogram(sequence):
    counts = defaultdict(int)
    for item in sequence:
        counts[item] += 1

    return counts


def longest_qualifying(array):
    counts = histogram(array)
    longest = 0
    for number, count in list(counts.items()):
        longest = max(longest,
                      count,
                      count + counts[number + 1],
                      count + counts[number - 1])

    return longest


length = int(sys.stdin.readline().strip())
array = [int(word) for word in sys.stdin.readline().split()]
assert len(array) == length

print(longest_qualifying(array))