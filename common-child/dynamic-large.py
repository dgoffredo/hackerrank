
"""dynamic-large

This solution uses dynamic programming to calculate the answer to "what is the
length of the largest common subsequence between the two strings starting at or
ofter the i'th index of the first string and the j'th position of the second
string?"

It calculates the answer "bottom up" through the matrix of values (i, j).

Thus the time complexity of this solution is O(n**2) and the space complexity
is O(n**n), where n is the length of each of the input strings (they have the
same length).
"""


import collections
import sys


class LeakyDict:
    def __init__(self, max_size, default_value):
        self.max_size = max_size
        self.default_value = default_value
        self.data = {}
        self.keys = collections.deque()

    def __getitem__(self, key):
        return self.data.get(key, self.default_value)

    def __setitem__(self, key, value):
        if key in self.data:
            self.data[key] = value
            return

        if len(self.keys) == self.max_size:
            condemned = self.keys.popleft()
            del self.data[condemned]

        self.keys.append(key)
        self.data[key] = value


def length_of_longest_subsequence(left, right):
    assert len(left) == len(right)
    L = len(left)

    # L + 1 so that there's an extra row and column of zeroes as a boundary.
    grid = LeakyDict(max_size=3*L, default_value=0)

    for diag in range(2*L - 1):
        diag_length = diag + 1 if diag < L else 2*L - diag - 1
        i_start = min(diag, L - 1)
        j_start = 0 if diag < L else diag - L + 1
        for offset in range(diag_length):
            # starting from the "bottom right" (end), hence the "L - ... - 1"
            i = L - (i_start - offset) - 1
            j = L - (j_start + offset) - 1

            if left[i] == right[j]:
                grid[(i, j)] = max(grid[(i + 1, j)],
                                   grid[(i, j + 1)],
                                   grid[(i + 1, j + 1)] + 1)
            else:
                grid[(i, j)] = max(grid[(i + 1, j)],
                                   grid[(i, j + 1)])

    return grid[(0, 0)]


print(length_of_longest_subsequence(*sys.stdin.read().split()))
