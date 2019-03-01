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


def substrings(sequence):
    for i in range(len(sequence)):
        for j in range(i + 1, len(sequence) + 1):
            yield sequence[i:j]


def prepare_lookup_tables(scored_genes, strands):
    table = {} # {(first, last): {gene: score}}

    for first, last, _ in strands:
        if (first, last) not in table:
            table[first, last] = score_by_gene(scored_genes[first:last + 1])

    return table


def score_by_gene(genes):
    result = {} # {gene: score}

    for gene, score in genes:
        previous = result.get(gene, 0)
        result[gene] = previous + score

    return result


def min_and_max_scores(scored_genes, strands):
    min_score = None
    max_score = None

    tables = prepare_lookup_tables(scored_genes, strands)

    for first, last, strand in strands:
        table = tables[first, last]
        score = sum(table.get(s, 0) for s in substrings(strand))
        
        if min_score is None or score < min_score:
            min_score = score
        if max_score is None or score > max_score:
            max_score = score

    return min_score, max_score


if __name__ == '__main__':
    scored_genes, strands = read(sys.stdin)
    min_score, max_score = min_and_max_scores(scored_genes, strands)
    print(min_score, max_score)
