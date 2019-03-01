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


# This does even worse than `naive-find.py` :(


import sys


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


class Matcher:
    def __init__(self, gene, score):
        assert gene # not empty
        self.score = score
        self.final_char = gene[-1]
        self.current_state_index = 0
        self.states = compile_states(gene)

    def reset(self):
        self.current_state_index = 0

    def consume(self, char):
        i = self.current_state_index
        states = self.states

        # Move to the next state. If `char` is not found, default to beginning.
        self.current_state_index = next_state_index = states[i].get(char, 0)

        # If we just finished matching the gene, emit the score. Otherwise,
        # emit zero.
        if i == len(states) - 1 and char == self.final_char:
            return self.score
        else:
            return 0


def compile_states(gene):
    assert gene # not empty


    # special case for genes of length one:
    if len(gene) == 1:
        return [{gene: 0}]

    states = []
    for i, char in enumerate(gene):
        edges = {} # {current_char: next_state_index}

        # There are a few cases to consider (making up to two edges):
        # 1. We are about to finish matching the gene.
        # 2. We are transitioning to the next character in the gene.
        # 3. We are transitioning to some prefix of the gene.

        # 1)
        if i == len(gene) - 1:
            prefix = gene[:i+1] # includes `char`
            next_state_index = index_of_largest_suffix_prefix(prefix, gene)
            edges[char] = next_state_index
        # 2)
        else:
            edges[char] = i + 1

        # 3)
        if char != gene[0]:
            suffix = gene[:i] + gene[0]
            next_state_index = index_of_largest_suffix_prefix(gene, suffix)
            edges[gene[0]] = next_state_index

        states.append(edges)

    return states


def index_of_largest_suffix_prefix(prefixed, suffixed):
    """Return the index into `prefixed` that is one past the largest proper
    prefix of `prefixed` that is a proper suffix of `suffixed`, or return zero
    if no proper prefix of `prefixed` is a proper suffix of `suffixed`.
    """
    # TODO: There's probably a better way to do this.
    for i in range(len(prefixed) - 1, 0, -1):
        prefix = prefixed[:i]
        if suffixed.endswith(prefix):
            return i

    return 0


def score_matches(matchers, strand):
    score = sum(matcher.consume(char)
                for char in strand for matcher in matchers)

    for matcher in matchers:
        matcher.reset()

    return score


def min_and_max_scores(matchers, strands):
    min_score = None
    max_score = None

    for first, last, strand in strands:
        score = score_matches(matchers[first:last + 1], strand)
        
        if min_score is None or score < min_score:
            min_score = score
        if max_score is None or score > max_score:
            max_score = score

    return min_score, max_score


if __name__ == '__main__':
    scored_genes, strands = read(sys.stdin)
    matchers = [Matcher(gene, score) for gene, score in scored_genes]
    min_score, max_score = min_and_max_scores(matchers, strands)
    print(min_score, max_score)
