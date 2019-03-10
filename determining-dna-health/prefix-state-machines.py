#!/usr/bin/env python3

"""sample input

6
a b c aa d b
1 2 3 4 5 6
3
1 5 caaab
0 4 xyz
2 4 bcdybc
"""

"""output for sample input

0 19
"""

# TODO: The idea with this one is...

import sys

from functools import reduce
from typing import Dict, List, Tuple


# debug = print
debug = lambda *_: None


def read(file):
    lines = [line.strip() for line in file]

    num_genes = int(lines[0])

    genes = lines[1].split()
    scores = [int(score) for score in lines[2].split()]
    assert len(genes) == num_genes
    assert len(scores) == num_genes

    scored_genes = list(zip(genes, scores))

    num_strands = int(lines[3])

    strands = []
    for line in lines[4:]:
        first, last, strand = line.split()
        first, last, strand = int(first), int(last), strand
        assert first >= 0
        assert last < num_genes
        strands.append((first, last, strand))

    assert len(strands) == num_strands

    return scored_genes, strands


# some type annotation names, documentation's sake
NextIndex = int
Score = float
Char = str
#           {<char>: (<next index>, <score>)}
State = Dict[Char, Tuple[NextIndex, Score]]
#               [{<char>: (<next index>, <score>)}, ...]
MachineStates = List[State]


def _combine_states(left_states: MachineStates,
                    right_states: MachineStates,
                    left_index: int,
                    right_index: int,
                    combined_index: int,
                    combined_states: MachineStates, 
                    seen_indices: Dict[Tuple[int, int], int]):
    combined_state: State = combined_states[combined_index]

    #                        [(left, right, new) ...]
    unseen_indices: List[Tuple[int, int, int]] = []

    left_state: State = left_states[left_index]
    right_state: State = right_states[right_index]

    # For each character accepted by either of the left or right states, add a
    # corresponding entry in the combined state.
    for char in left_state.keys() | right_state.keys():
        left_to, left_score = left_state.get(char, (0, 0))
        right_to, right_score = right_state.get(char, (0, 0))

        # Elide transitions that are the same as the default.
        if left_to == left_score == right_to == right_score == 0:
            continue

        next_index = seen_indices.get((left_to, right_to))
        if next_index is None:
            seen_indices[left_to, right_to] = next_index = len(combined_states)
            unseen_indices.append((left_to, right_to, next_index))
            combined_states.append({}) # filled by future recursive call

        combined_state[char] = next_index, left_score + right_score

    debug('combined_states is now', combined_states)

    # `combined_state` is now finished. Finish any new states (recur).
    for left, right, combined in unseen_indices:
        _combine_states(left_states,
                        right_states,
                        left,
                        right,
                        combined,
                        combined_states,
                        seen_indices)


def combine_states(left_states: MachineStates, right_states: MachineStates):
    assert left_states
    assert right_states

    seen_indices = {(0, 0): 0}            # to be filled in below
    combined_states: MachineStates = [{}] # to be filled in below
    
    _combine_states(left_states,
                    right_states,
                    0, # left state index
                    0, # right state index
                    0, # combined state index
                    combined_states,
                    seen_indices)

    return combined_states


class Machine:
    def __init__(self, states: MachineStates) -> None:
        assert len(states) > 0
        self.states = states
        self.current_state_index = 0

    def reset(self):
        self.current_state_index = 0

    def consume(self, char):
        i = self.current_state_index
        states = self.states

        # Move to the next state. If `char` is not found, default to beginning.
        next_state_index, score = states[i].get(char, (0, 0))
        self.current_state_index = next_state_index

        return score


def compile_states(gene, score):
    assert gene # not empty

    # special case for genes of length one (makes things simpler below):
    if len(gene) == 1:
        states = [{gene: (0, score)}]
        debug('gene', gene, 'produced the state transitions:', states)
        return states

    states = []
    for i, char in enumerate(gene):
        edges = {} # {current_char: (next_state_index, score)}

        # There are a few cases to consider (making up to two edges):
        # 1. We are about to finish matching the gene.
        # 2. We are transitioning to the next character in the gene.
        # 3. We are transitioning to some prefix of the gene.

        # 1)
        if i == len(gene) - 1:
            next_state_index = index_of_largest_suffix_prefix(gene, gene)
            edges[char] = next_state_index, score
        # 2)
        else:
            edges[char] = i + 1, 0

        # 3)
        if char != gene[0]:
            suffix = gene[:i] + gene[0]
            next_state_index = index_of_largest_suffix_prefix(gene, suffix)
            edges[gene[0]] = next_state_index, 0

        states.append(edges)

    debug('gene', gene, 'produced the state transitions:', states)
    return states


def index_of_largest_suffix_prefix(prefixed, suffixed):
    """Return the index into `prefixed` that is one past the largest proper
    prefix of `prefixed` that is a proper suffix of `suffixed`, or return zero
    if no proper prefix of `prefixed` is a proper suffix of `suffixed`.
    """
    # TODO: There's probably a better way to do this. It's probably important.
    for i in range(len(prefixed) - 1, 0, -1):
        prefix = prefixed[:i]
        if suffixed.endswith(prefix):
            return i

    return 0


def scoring(machine, strand):
    """the total score produced by running `strand` through `machine`"""
    score = sum(machine.consume(char) for char in strand)
    machine.reset()
    return score


def min_and_max_scores(prefix_machines, strands):
    min_score = None
    max_score = None

    for first, last, strand in strands:
        # first, last --> [first:last + 1] because the machine at index zero
        # matches nothing, i.e. it was either that or a special case for
        # `first == 0` (in which case we would omit the `+ 1` from `last`).
        lower_machine = prefix_machines[first]
        upper_machine = prefix_machines[last + 1]
        # score = scoring(upper_machine, strand) - scoring(lower_machine, strand)
        upper_score = scoring(upper_machine, strand)
        lower_score = scoring(lower_machine, strand)
        score = upper_score - lower_score
        debug('for', strand, score, '=', upper_score, '-', lower_score)

        if min_score is None or score < min_score:
            min_score = score
        if max_score is None or score > max_score:
            max_score = score

    return min_score, max_score


def append_gene(prefix_machines: List[Machine], scored_gene) -> List[Machine]:
    gene, score = scored_gene

    left_states: MachineStates = prefix_machines[-1].states
    right_states: MachineStates = compile_states(gene, score)

    combined_states = combine_states(left_states, right_states)
    prefix_machines.append(Machine(combined_states))
    return prefix_machines


if __name__ == '__main__':
    import time
    from sys import stderr
    try:
        start = time.time()
        scored_genes, strands = read(sys.stdin)
        finished_read = time.time()

        null_machine = Machine([{}])
        prefix_machines = reduce(append_gene, scored_genes, [null_machine])
        finished_machines = time.time()
        debug('there are', len(prefix_machines), 'prefix machines:',
              [machine.states for machine in prefix_machines])

        min_score, max_score = min_and_max_scores(prefix_machines, strands)
        finished = time.time()
        print(min_score, max_score)
        print('read input in', finished_read - start, 'seconds', file=stderr)
        print('compiled machines in', finished_machines - finished_read,
              'seconds', file=stderr)
        print('scored strands in', finished - finished_machines, 'seconds',
              file=stderr)
    except: # trying to make running out of memory --> timeout
        time.sleep(120) # seconds
