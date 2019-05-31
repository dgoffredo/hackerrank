
"""solution

You break the integer n into a "multiple of the length of the string" part and
a "remainder" part.  Then the answer is the number of a's in the string times
the first part, plus the number of a's in the remainder prefix of the string.
"""


import sys


def div(dividend, divisor):
    quotient = dividend // divisor
    remainder = dividend % divisor
    return quotient, remainder


def count_in_prefix(string, prefix_length, letter):
    quotient, remainder = div(prefix_length, len(string))
    
    def count(in_what):
        return sum(char == letter for char in in_what)

    return quotient * count(string) + count(string[:remainder])


string = sys.stdin.readline().strip()
prefix_length = int(sys.stdin.readline().strip())

print(count_in_prefix(string, prefix_length, 'a'))
