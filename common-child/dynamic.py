
"""dynamic

This solution uses dynamic programming to calculate the answer to "what is the
length of the largest common subsequence between the two strings starting at or
ofter the i'th index of the first string and the j'th position of the second
string?"

It calculates the answer "bottom up" through the matrix of values (i, j), but
keeps in memory only the three most recent diagonals, since the others are not
needed.

Thus the time complexity of this solution is O(n**2) and the space complexity
is O(n), where n is the length of each of the input strings (they have the
same length).
"""

def length_of_longest_subsequence(left, right):
    assert len(left) == len(right)
    L = len(left)

    # The +2's are for the zeroes at the boundary.
    buffers = [[0] * (L + 2) for _ in range(3)]

    for diag in range(2*L - 1):
        diag_length = diag + 1 if diag < L else 2*L - diag - 1
        i_start = min(diag, L - 1)
        j_start = 0 if diag < L else diag - L + 1
        for offset in range(diag_length):
            # starting from the "bottom right" (end), hence the "L - ... - 1"
            i = L - (i_start - offset) - 1
            j = L - (j_start + offset) - 1
            curr = buffers[0]
            prev = buffers[1]
            prev_prev = buffers[2]
            # The offsets of cells within their diagonal is itself offset
            # relative to the offset in the current diagonal, depending on
            # which side of the grid we're on. These adjustments account for
            # this.
            """
            prev_fudge = -1 if i >= j else 0
            if (j, i) == (i + 1, j - 1):
                prev_prev_fudge = 0
            elif i >= j:
                prev_prev_fudge = -1
            else:
                prev_prev_fudge = 1
            """
            # I was thinking it's (i, j) but it's (j, L - i)
            prev_fudge = -1 if j >= L - i else 0
            if (L - i, j) == (j + 1, L - i - 1):
                prev_prev_fudge = 0
            elif j >= L - i:
                prev_prev_fudge = -1
            else:
                prev_prev_fudge = 1

            # Since the buffers are padded on each side by one zero (as a
            # boundary condition), it's useful here to talk about `offset + 1`
            # as the effective offset. Let's call it `k` for brevity.
            k = offset + 1
            print(locals())
            print('i:', i, 'j:', j,
                  'right:', prev[k + prev_fudge],
                  'below:', prev[k + 1 + prev_fudge],
                  'diagonally:', prev_prev[k + prev_prev_fudge])
            print()
            if left[i] == left[j]:
                curr[k] = max(prev[k + prev_fudge],
                              prev[k + 1 + prev_fudge],
                              prev_prev[k + prev_prev_fudge] + 1)
            else:
                curr[k] = max(prev[k + prev_fudge],
                              prev[k + 1 + prev_fudge])

        # storage role rotation
        temp = buffers[0]
        buffers[0] = buffers[1]
        buffers[1] = buffers[2]
        buffers[2] = temp

    return buffers[1][1] # TODO: explain

print(length_of_longest_subsequence('craby', 'rambo'))