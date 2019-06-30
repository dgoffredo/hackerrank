
import sys


def main():
    _, _, seconds = (int(num) for num in sys.stdin.readline().split())
    initial = tuple(tuple(char == 'O' for char in row.strip())
                    for row in sys.stdin)
   
    # print('initial:')
    # print_bombs(initial)
    # print()
 
    result = bombs_after(initial, seconds)

    print_bombs(result)


def bombs_after(initial, seconds):
    # silly corner cases (no rows or no columns)
    if len(initial) == 0 or len(initial[0]) == 0:
        return initial

    # If this is the initial second, then return `initial`.  If it's after one
    # second, then also return `initial`.  If it's an even number of seconds,
    # then all of the bombs are filled.  Otherwise, simulate the game until
    # either we've reached the specified number of `seconds`, or until a cycle
    # is found.  If a cycle is found, then modular arithmetic can be used to
    # determine the solution at `seconds`.
    if seconds in (0, 1):
        return initial
    elif seconds % 2 == 0:
        rows = len(initial)
        columns = len(initial[0])
        return all_bombs(rows, columns)

    odd_boards = [initial]
    board_indices = {initial: 0}   

    simulated_seconds = 3
    while True:
        new_board = inverse_bombs(odd_boards[-1])
        # print('simulated_seconds = ', simulated_seconds)
        # print_bombs(new_board)
        # print()

        if simulated_seconds == seconds:
            return new_board

        if new_board in board_indices:
            # We found a cycle in the simulation, so no more board computations
            # are needed.  Instead, we figure out where in the cycle `seconds`
            # will end up.
            cycle_end_seconds = simulated_seconds
            # print('cycle_end_seconds', cycle_end_seconds)
            cycle_begin_index = board_indices[new_board]
            # print('cycle_begin_index', cycle_begin_index)
            cycle_begin_seconds = 2 * cycle_begin_index + 1
            # print('cycle_begin_seconds', cycle_begin_seconds)
            solution_index = cycle_begin_index + (
                (seconds - cycle_begin_seconds) %
                    (cycle_end_seconds - cycle_begin_seconds)) // 2
            # print('solution_index', solution_index)
            return odd_boards[solution_index]

        # Otherwise, add it to the list and carry on.
        odd_boards.append(new_board)
        board_indices[new_board] = len(odd_boards) - 1
        simulated_seconds += 2


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
        inverse.append(tuple(row))

    return tuple(inverse)


def all_bombs(rows, columns):
    return tuple(tuple(True for _ in range(columns)) for _ in range(rows))


def print_bombs(board):
    for row in board:
        for occupied in row:
            sys.stdout.write('O' if occupied else '.')
        sys.stdout.write('\n')


main()

