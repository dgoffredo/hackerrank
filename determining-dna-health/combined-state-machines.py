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
GeneIndices = List[int]
Char = str

#           {<char>: (<next index>, [<gene index> ...])}
State = Dict[Char, Tuple[NextIndex, GeneIndices]]

#               [{<char>: (<next index>, [<gene index> ...])}, ...]
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
        # Recall that each char maps to a pair (state_index, gene_indices...)
        # where `state_index` is the index in this machine to transition to,
        # and `gene_indicies` is a list of the indexes of genes that were
        # matched due to the char.
        left_to, left_genes = left_state.get(char, (0, []))
        right_to, right_genes = right_state.get(char, (0, []))

        # Elide transitions that are the same as the default.
        if left_to == right_to == 0 and left_genes == right_genes == []:
            continue

        next_index = seen_indices.get((left_to, right_to))
        if next_index is None:
            seen_indices[left_to, right_to] = next_index = len(combined_states)
            unseen_indices.append((left_to, right_to, next_index))
            combined_states.append({}) # filled by future recursive call

        combined_state[char] = next_index, left_genes + right_genes

    debug('combined_states is now', combined_states)

    # `combined_state` is now finished. Finish any new states (recur). To
    # avoid recusion depth limits, we return a list of argument sets to the
    # trampoline (our caller) that is simulating a call stack.
    return [(left_states,
             right_states,
             left,
             right,
             combined,
             combined_states,
             seen_indices) for left, right, combined in unseen_indices]


def combine_states(left_states: MachineStates, right_states: MachineStates):
    assert left_states
    assert right_states

    seen_indices = {(0, 0): 0}            # to be filled in below
    combined_states: MachineStates = [{}] # to be filled in below
    initial_args = (left_states,
                    right_states,
                    0, # left state index
                    0, # right state index
                    0, # combined state index
                    combined_states,
                    seen_indices)

    # Simulate recursion using a stack, where each element in the stack is a
    # tuple of arguments to be passed to `_combine_states`. `_combine_states`,
    # in turn, returns a list of such tuples to be added to the top of the
    # stack.
    call_stack = [initial_args] # top of the stack is end of the list

    while len(call_stack) > 0:
        call_stack.extend(_combine_states(*call_stack.pop()))

    return combined_states


class Machine:
    def __init__(self, states: MachineStates) -> None:
        assert len(states) > 0
        self.states = states
        self.current_state_index = 0

    def reset(self):
        self.current_state_index = 0

    def consume(self, char) -> GeneIndices:
        i = self.current_state_index
        states = self.states

        # Move to the next state. If `char` is not found, default to beginning.
        next_state_index, indices = states[i].get(char, (0, []))
        self.current_state_index = next_state_index

        return indices


def compile_states(gene, index):
    assert gene # not empty
    assert index >= 0

    # special case for genes of length one (makes things simpler below):
    if len(gene) == 1:
        states = [{gene: (0, [index])}]
        debug('gene', gene, 'produced the state transitions:', states)
        return states

    states = []
    for i, char in enumerate(gene):
        edges = {} # {current_char: (next_state_index, [gene indices...])}

        # There are a few cases to consider (making up to two edges):
        # 1. We are about to finish matching the gene.
        # 2. We are transitioning to the next character in the gene.
        # 3. We are transitioning to some prefix of the gene.

        # 1)
        if i == len(gene) - 1:
            next_state_index = index_of_largest_suffix_prefix(gene, gene)
            edges[char] = next_state_index, [index]
        # 2)
        else:
            edges[char] = i + 1, []

        # 3)
        if char != gene[0]:
            suffix = gene[:i] + gene[0]
            next_state_index = index_of_largest_suffix_prefix(gene, suffix)
            edges[gene[0]] = next_state_index, []

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


def scoring(machine, strand, gene_scores):
    score = 0
    first, last, letters = strand
    for char in letters:
        indices = machine.consume(char)
        score += sum(gene_scores[i] for i in indices if first <= i <= last)

    machine.reset()
    return score


def min_and_max_scores(prefix_machines, strands, gene_scores):
    assert len(strands) > 0

    min_score = None
    max_score = None

    for strand in strands:
        score = scoring(machine, strand, gene_scores)

        if min_score is None or score < min_score:
            min_score = score
        if max_score is None or score > max_score:
            max_score = score

    return min_score, max_score


def assimilate_gene(states: MachineStates, indexed_gene) -> MachineStates:
    i, gene = indexed_gene
    gene_states = compile_states(gene, i)

    return combine_states(states, gene_states)


if __name__ == '__main__':
    import time
    from sys import stderr

    start = time.time()
    scored_genes, strands = read(sys.stdin)
    finished_read = time.time()

    null_states = [{}]
    indexed_genes = [(i, gene) for i, (gene, score) in enumerate(scored_genes)]
    combined_states = reduce(assimilate_gene, indexed_genes, null_states)
    machine = Machine(combined_states)
    finished_machine = time.time()

    gene_scores = [score for gene, score in scored_genes]
    min_score, max_score = min_and_max_scores(machine, strands, gene_scores)
    finished = time.time()

    print(min_score, max_score)

    print('read input in', finished_read - start, 'seconds', file=stderr)
    print('compiled machine in', finished_machine - finished_read, 'seconds',
          file=stderr)
    print('scored strands in', finished - finished_machine, 'seconds',
          file=stderr)
