
import sys


def main():
    _, _, seconds = (int(num) for num in sys.stdin.readline().split())
    initial = [[char == 'O' for char in row] for row in sys.stdin]
    
    result = bombs_after(initial, seconds)

    for row in result:
        for occupied in row:
            sys.stdout.write('O' if occupied else '.')
        sys.stdout.write('\n')


def bombs_after(initial, seconds):
    if len(initial) == 0 or len(initial[0]) == 0:
        return initial

    # If this is the initial second, then return `initial`.  Otherwise,
    # depending on the value of `seconds` modulo 4, we'll either return
    # `initial`, its inverse, or a full board.
    if seconds == 0:
        return initial
    
    mod = seconds % 4
    if mod == 1:
        return initial
    elif mod in (0, 2):
        rows = len(initial)
        columns = len(initial[0])
        return all_bombs(rows, columns)
    else:
        assert mod == 3
        return inverse_bombs(initial)


def inverse_bombs(initial):
    rows = len(initial)
    columns = len(initial[0])
    inverse = []
    for i in range(rows):
        row = []
        for j in range(columns):
            exploded = (initial[i][j]
                        or (i > 0 and initial[i-1][j])
                        or (j > 0 and initial[i][j-1])
                        or (i < rows - 1 and initial[i+1][j])
                        or (j < columns - 1 and initial[i][j+1]))
            row.append(not exploded)
        inverse.append(row)

    return inverse


def all_bombs(rows, columns):
    return [[True for _ in range(columns)] for _ in range(rows)]


main()

