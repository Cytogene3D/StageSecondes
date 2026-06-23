
def InitMatrix(n, m, gap):
    """
    Initialize the score matrix for Needleman-Wunsch algorithm.
    """
    F = [[0] * (m + 1) for _ in range(n + 1)]

    # Pointer matrix
    # D = diagonal, U = up, L = left
    P = [[""] * (m + 1) for _ in range(n + 1)]

    for i in range(1, n + 1):
        F[i][0] = i * gap
        P[i][0] = "U"

    for j in range(1, m + 1):
        F[0][j] = j * gap
        P[0][j] = "L"

    

    return F, P


def needleman_wunsch(seq1, seq2, match=2, mismatch=-1, gap=-2):
    n = len(seq1)
    m = len(seq2)

    # Score matrix
    F, P = InitMatrix(n, m, gap)

    # Add a dummy character at position 0
    seq1 = " " + seq1
    seq2 = " " + seq2

    # Fill matrices
    for i in range(1, n + 1):
        for j in range(1, m + 1):

            if seq1[i] == seq2[j]:
                score = match
            else:
                score = mismatch

            diagonal = F[i - 1][j - 1] + score
            up = F[i - 1][j] + gap
            left = F[i][j - 1] + gap

            best = max(diagonal, up, left)
            F[i][j] = best

            if best == diagonal:
                P[i][j] = "D"
            elif best == up:
                P[i][j] = "U"
            else:
                P[i][j] = "L"
   
    align1, align2 = nw_traceback(seq1, seq2, P)

    return F[n][m], align1, align2


def nw_traceback(seq1, seq2, P):
    """
    Perform traceback to get the optimal alignment.
    """
    align1 = ""
    align2 = ""

    i, j = len(seq1)-1, len(seq2)-1

    while i > 0 or j > 0:

        if P[i][j] == "D":
            align1 = seq1[i] + align1
            align2 = seq2[j] + align2
            i -= 1
            j -= 1

        elif P[i][j] == "U":
            align1 = seq1[i] + align1
            align2 = "-" + align2
            i -= 1

        else:  # "L"
            align1 = "-" + align1
            align2 = seq2[j] + align2
            j -= 1

    return align1, align2


seq1="TACGAGACTCAATACA"
seq2="TACAAGAAATACA"

score, aln1, aln2 = needleman_wunsch(seq1, seq2)

for a1,a2 in zip(aln1, aln2):
    middle = "|" if a1==a2 else " "

print("Score:", score)
print(aln1)
print(middle)
print(aln2)

