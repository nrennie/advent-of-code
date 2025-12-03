
library(stringr)


# Data --------------------------------------------------------------------

input <- readLines("2025/data/input-03.txt")


# Part 1 ------------------------------------------------------------------

process_input <- function(x) {
  new_x <- as.numeric(str_split_1(x, ""))
  n <- length(new_x)
  first_place <- which.max(new_x[1:(n-1)])
  first_digit <- new_x[first_place]
  second_digits_opts <- new_x[(first_place + 1):n]
  second_digit <- second_digits_opts[which.max(second_digits_opts)]
  output <- as.numeric(paste0(first_digit, second_digit))
  return(output)
}
sum(sapply(input, process_input))




