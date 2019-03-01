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

# This solution is too slow for larger test cases.

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


_num_occurrences_cache = {}


def num_occurrences(gene, strand):
    end = len(strand) - len(gene) + 1
    if end < 1:
        return 0

    key = gene, strand
    cached_result = _num_occurrences_cache.get(key)
    if cached_result is not None:
        return cached_result
    else:
        count = sum(strand[i:].startswith(gene) for i in range(0, end))
        _num_occurrences_cache[key] = count
        return count


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
