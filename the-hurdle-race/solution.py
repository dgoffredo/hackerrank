import sys

_, limit, *hurdles = [int(word) for word in sys.stdin.read().split()]

min_doses = max(max(hurdles) - limit, 0)

print(min_doses)