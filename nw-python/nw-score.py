#!/usr/bin/env python


def InitMatrix(n, m, gap):
    """
    Initialize the score matrix for Needleman-Wunsch algorithm.
    """
    F = [[0] * (m + 1) for _ in range(n + 1)]

    for i in range(1, n + 1):
        F[i][0] = i * gap

    for j in range(1, m + 1):
        F[0][j] = j * gap

    return F


def needleman_wunsch(seq1, seq2, match=2, mismatch=-1, gap=-2):
    n = len(seq1)
    m = len(seq2)

    # Score matrix
    F = InitMatrix(n, m, gap)

    # Add a dummy character at position 0
    seq1 = " " + seq1
    seq2 = " " + seq2

    # Fill matrix
    for i in range(1, n + 1):
        for j in range(1, m + 1):
            if seq1[i] == seq2[j]:
                score = match
            else:                
                score = mismatch
            
            diag = F[i - 1][j - 1] + score
            up = F[i - 1][j] + gap
            left = F[i][j - 1] + gap

            F[i][j] = max(diag, up, left)

    return F[n][m], F


seq1="TACGAGACTCAATACA"
seq2="TACAAGAAATACA"

score, matrix = needleman_wunsch(seq1, seq2)

print(score)

for row in matrix:
    print("\t".join(map(str, row)))


