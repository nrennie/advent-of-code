
library(stringr)


# Data --------------------------------------------------------------------

input <- readLines("2025/data/input-04.txt")

input_df <- t(sapply(input, function(x) {str_split_1(x, pattern = "")}))
rownames(input_df) <- NULL
n_row <- nrow(input_df)
n_col <- ncol(input_df)


# Part 1 ------------------------------------------------------------------

check_row <- function(x) {
  if (x < 1 | x > n_row) {
    return(NA)
  } else {
    return(x)
  }
}

check_col <- function(x) {
  if (x < 1 | x > n_col) {
    return(NA)
  } else {
    return(x)
  }
}

extract_surrounding <- function(row, col) {
  if (input_df[row, col] != "@") {
    return(100)
  } else {
    top_left <- input_df[check_row(row - 1), check_col(col - 1)][1]
    top_mid <- input_df[check_row(row - 1), check_col(col)][1]
    top_right <- input_df[check_row(row - 1), check_col(col + 1)][1]
    left <- input_df[check_row(row), check_col(col - 1)][1]
    right <- input_df[check_row(row), check_col(col + 1)][1]
    bot_left <- input_df[check_row(row + 1), check_col(col - 1)][1]
    bot_mid <- input_df[check_row(row + 1), check_col(col)][1]
    bot_right <- input_df[check_row(row + 1), check_col(col + 1)][1]
    all_vals <- c(
      top_left, top_mid, top_right, left,
      right, bot_left, bot_mid, bot_right
    )
    output <- sum(all_vals == "@", na.rm = TRUE)
    return(output)
  }
}

total <- 0
for (i in 1:n_row) {
  for (j in 1:n_col) {
    num <- extract_surrounding(i, j)
    if (num < 4) {
      total <- total + 1
    }
  }
}
total


# Part 2 ------------------------------------------------------------------

total <- 1000
overall_total <- 0
while (total != 0) {
  total <- 0
  remove_i <- c()
  remove_j <- c()
  for (i in 1:n_row) {
    for (j in 1:n_col) {
      num <- extract_surrounding(i, j)
      if (num < 4) {
        total <- total + 1
        remove_i <- c(remove_i, i)
        remove_j <- c(remove_j, j)
      }
    }
  }
  if (length(remove_i) > 0) {
    for (k in 1:length(remove_i)) {
      input_df[remove_i[k], remove_j[k]] <- "x"
    }
      
  }
  overall_total <- overall_total + total
}

overall_total

