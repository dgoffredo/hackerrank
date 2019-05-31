
"""brute force

UPDATE: I misunderstood the problem. At first I got the idea right, but then
        was thrown off by the word "subarray."  This solution is thus wrong.
        Time to go back to the easier solution and ignore the use of the word
        "subarray."

Since the input is bounded to be small, this solution just examines each
subarray and finds the largest that satisfies the condition.
"""


import sys


def min_and_max(sequence):
    smallest = None
    largest = None
    for item in sequence:
        if smallest is None or item < smallest:
            smallest = item
        if largest is None or item > largest:
            largest = item

    return smallest, largest


def qualifies(subarray):
    smallest, largest = min_and_max(subarray)
    return largest - smallest <= 1


def longest_qualifying_subarray(array):
    i = 0
    j = 2
    i_best = i
    j_best = j

    while True:
        if qualifies(array[i:j]):
            if j - i > j_best - i_best:
                i_best = i
                j_best = j

            if j < len(array): # can expand forward
                j += 1
            else: # can't expand forward, so we're done
                break
        else:
            if j - i > j_best - i_best: # can shrink down
                i += 1
            elif j < len(array): # can slide forward
                i += 1
                j += 1
            else: # can't do either, so we're done
                break

    return array[i_best:j_best]


length = int(sys.stdin.readline().strip())
array = [int(word) for word in sys.stdin.readline().split()]
assert len(array) == length

print(longest_qualifying_subarray(array))
print(len(longest_qualifying_subarray(array)))