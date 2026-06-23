library(vctrs)

traceback_nw <- function(pointer, seq1, seq2) {
  
  cat("traceback_nw triggered\n")

  s1 <- strsplit(toupper(seq1), "")[[1]]
  s2 <- strsplit(toupper(seq2), "")[[1]]

  i <- nrow(pointer)
  j <- ncol(pointer)

  aln1 <- character()
  aln2 <- character()

  path <- data.frame(
    step = integer(),
    i = integer(),
    j = integer(),
    move = character(),
    stringsAsFactors = FALSE
  )

  steps <- list()
  step_id <- 1

  # helper to store state consistently
  store_step <- function() {
    cat("Storing step:", step_id, "\n")
    cat(aln1,"\n")
    cat(aln2,"\n")
    steps[[step_id]] <<- list(
      step = step_id,
      i = i,
      j = j,
      partial1 = paste(rev(aln1), collapse = ""),
      partial2 = paste(rev(aln2), collapse = "")
    )

    path <<- rbind(
      path,
      data.frame(
        step = step_id,
        i = i,
        j = j,
        move = move,
        stringsAsFactors = FALSE
      )
    )
  }

  while (i > 1 || j > 1) {

    move <- pointer[i, j]

    # record state BEFORE move (good for animation alignment)
    store_step()

    if (move == "diag") {

      aln1 <- c(aln1, s1[j - 1])
      aln2 <- c(aln2, s2[i - 1])

      i <- i - 1
      j <- j - 1

    } else if (move == "left") {

      aln1 <- c(aln1, s1[j - 1])
      aln2 <- c(aln2, "-")

      j <- j - 1

    } else {

      aln1 <- c(aln1, "-")
      aln2 <- c(aln2, s2[i - 1])

      i <- i - 1
    }

    step_id <- step_id + 1
  }

  # final state (IMPORTANT for slider completeness)
  move <- "end"

  steps[[step_id]] <- list(
    step = step_id,
    i = i,
    j = j,
    partial1 = paste(rev(aln1), collapse = ""),
    partial2 = paste(rev(aln2), collapse = "")
  )

  path <- rbind(
    path,
    data.frame(
      step = step_id,
      i = i,
      j = j,
      move = move,
      stringsAsFactors = FALSE
    )
  )
 
  list(
    alignment1 = paste(rev(aln1), collapse = ""),
    alignment2 = paste(rev(aln2), collapse = ""),
    path = path,
    steps = steps
  )
}

match_line <- function(a1, a2) {

  chars1 <- strsplit(a1, "")[[1]]
  chars2 <- strsplit(a2, "")[[1]]

  paste(
    ifelse(chars1 == chars2, "|", " "),
    collapse = ""
  )
}



