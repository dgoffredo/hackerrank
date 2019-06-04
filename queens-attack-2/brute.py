import sys


def read_ints(file=sys.stdin):
    return [int(word) for word in file.read().split()]


side_length, num_obstacles, i_queen, j_queen, *coordinates = read_ints()

# input is one-based
queen = (i_queen - 1, j_queen - 1)

assert len(coordinates) / 2 == num_obstacles

obstacles = set()
for k in range(num_obstacles):
    # again, the "- 1" are to make things zero-based.
    i = coordinates[2*k] - 1
    j = coordinates[2*k + 1] - 1
    obstacles.add((i, j))


def vector_sum(left, right):
    assert len(left) == len(right)
    assert type(left) == type(right)

    return type(left)(left[i] + right[i] for i in range(len(left)))


# A "point" is (i, j)
# i says which row (zero-based)
# j says which column (zero-based)
directions = [
    (0, -1),  # left
    (0, +1),  # right
    (+1, 0),  # up
    (-1, 0),  # down
    (+1, -1), # UL
    (+1, +1), # UR
    (-1, -1), # BL
    (-1, +1)  # BR
]

moves = 0
for direction in directions:
    point = vector_sum(queen, direction)
    while True:
        i, j = point
        out_of_bounds = i < 0 or j < 0 or i >= side_length or j >= side_length
        if out_of_bounds or point in obstacles:
            break

        moves += 1
        point = vector_sum(point, direction)

print(moves)