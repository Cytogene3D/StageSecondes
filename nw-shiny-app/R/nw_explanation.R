explain_cell <- function(step_obj) {

  paste0(
    "Cell (", step_obj$i, ",", step_obj$j, ")\n\n",
    step_obj$char2, " vs ", step_obj$char1, "\n\n",
    "Diagonal: ", step_obj$diagonal, "\n",
    "Up: ", step_obj$up, "\n",
    "Left: ", step_obj$left, "\n\n",
    "Chosen: ", step_obj$chosen, "\n",
    "Value: ", step_obj$result
  )
}

build_traceback_path <- function(tb, k) {

  tb$path[seq_len(min(k, nrow(tb$path))), ]
}


 make_arrow <- function(i1, j1, i2, j2) {
      list(
        type = "path",
        path = sprintf("M %f %f L %f %f", j1, i1, j2, i2),
        line = list(color = "blue", width = 2)
      )

    }


highlight_path <- function(mat, path) {

  z <- matrix(0, nrow = nrow(mat), ncol = ncol(mat))

  for (k in seq_len(nrow(path))) {
    i <- path$i[k]
    j <- path$j[k]
    z[i, j] <- 1
  }

  z
}

move_symbol <- function(move) {
  if (move == "diag") return("↖")
  if (move == "up")   return("↑")
  if (move == "left") return("←")
  return("")
}


