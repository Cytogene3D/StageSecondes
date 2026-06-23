run_nw_matrix <- function(seq1, seq2,
                          match = 1,
                          mismatch = -1,
                          gap = -2) {

  s1 <- strsplit(toupper(seq1), "")[[1]]
  s2 <- strsplit(toupper(seq2), "")[[1]]

  # s1 represents the x axis sequence : length(s1)=m columns
  # s2 represents the y axis sequence : length(s2)=n lines
  m <- length(s1)
  n <- length(s2)
  
  score <- matrix(0, n + 1, m + 1)
  pointer <- matrix("", n + 1, m + 1)

  score[,1] <- 0:n * gap
  score[1,] <- 0:m * gap

  arrow <- matrix("", n + 1, m + 1)
  arrow_step <- matrix(0, n + 1, m + 1)

  pointer[,1] <- "up"
  pointer[1,] <- "left"
  pointer[1,1] <- "start"

  steps <- list()
  k <- 1

  for (i in 2:(n + 1)) {
    for (j in 2:(m + 1)) {

      diag <- score[i-1, j-1] +
        ifelse(s2[i-1] == s1[j-1], match, mismatch)
     
      up <- score[i-1, j] + gap
      left <- score[i, j-1] + gap

      best <- max(diag, up, left)

      move <- if (best == diag) "diag"
              else if (best == up) "up"
              else "left"

      score[i,j] <- best
      pointer[i,j] <- move
      arrow_step[i, j] <- k 
      arrow[i, j] <- switch(move,
                        "diag" = "↖",
                        "up"   = "↑",
                        "left" = "←")

      steps[[k]] <- list(
        k = k,
        i = i,
        j = j,
        char1 = s1[j-1],
        char2 = s2[i-1],
        diagonal = diag,
        up = up,
        left = left,
        chosen = move,
        result = best,
        pointer_snapshot = list(i = i, j = j, move = move)  
      )

      k <- k + 1
    }
  }

  list(
    score = score,
    pointer = pointer,
    arrows = get_arrows(pointer),   
    steps = steps,
    arrow = arrow,
    arrow_step = arrow_step,
    seq1 = seq1,
    seq2 = seq2
    )
}


build_matrix_step <- function(n, m, steps, k) {

  mat <- matrix(NA, nrow = n, ncol = m)

  for (s in seq_len(k)) {
    i <- steps[[s]]$i
    j <- steps[[s]]$j
    mat[i, j] <- steps[[s]]$result
  }

  mat
}

get_arrows <- function(pointer) {

  arrows <- list()

  n <- nrow(pointer)
  m <- ncol(pointer)

  for (i in 2:n) {
    for (j in 2:m) {

      dir <- pointer[i, j]

      if (dir == "diag") {
        arrows[[length(arrows)+1]] <- list(
          i1 = i-1, j1 = j-1,
          i2 = i,   j2 = j
        )
      }

      if (dir == "up") {
        arrows[[length(arrows)+1]] <- list(
          i1 = i-1, j1 = j,
          i2 = i,   j2 = j
        )
      }

      if (dir == "left") {
        arrows[[length(arrows)+1]] <- list(
          i1 = i,   j1 = j-1,
          i2 = i,   j2 = j
        )
      }
    }
  }

  arrows
}
