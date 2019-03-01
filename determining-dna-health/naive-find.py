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

# This solution is too slow for larger test cases. It does better than the
# other naive solutions because `str.find` uses a fancy search algorithm, but
# still this solution lacks the insight needed to solve the problem.

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


def num_occurrences(gene, strand):
    count = 0
    start = 0
    while True:
        start = strand.find(gene, start)
        if start == -1:
            return count
        else:
            count += 1
            start += 1


def min_and_max_scores(scored_genes, strands):
    min_score = None
    max_score = None

    for first, last, strand in strands:
        genes = scored_genes[first:last + 1]
        score = sum(weight * num_occurrences(gene, strand)
                    for gene, weight in genes)
        
        if min_score is None or score < min_score:
            min_score = score
        if max_score is None or score > max_score:
            max_score = score

    return min_score, max_score


if __name__ == '__main__':
    scored_genes, strands = read(sys.stdin)
    min_score, max_score = min_and_max_scores(scored_genes, strands)
    print(min_score, max_score)

