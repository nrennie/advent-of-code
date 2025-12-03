library(stringr)


# Data --------------------------------------------------------------------

input <- readLines("2025/data/input-03.txt")


# Part 1 ------------------------------------------------------------------

process_input <- function(x) {
  new_x <- as.numeric(str_split_1(x, ""))
  n <- length(new_x)
  first_place <- which.max(new_x[1:(n - 1)])
  first_digit <- new_x[first_place]
  second_digits_opts <- new_x[(first_place + 1):n]
  second_digit <- second_digits_opts[which.max(second_digits_opts)]
  output <- as.numeric(paste0(first_digit, second_digit))
  return(output)
}
sum(sapply(input, process_input))


# Part 2 ------------------------------------------------------------------

process_input <- function(x) {
  new_x <- as.numeric(str_split_1(x, ""))
  n <- length(new_x)

  digits_01_opts <- new_x[1:(n - 11)]
  place_01 <- which.max(digits_01_opts)
  digit_01 <- new_x[place_01]

  digits_02_opts <- new_x[(place_01 + 1):(n - 10)]
  place_02 <- place_01 + which.max(digits_02_opts)
  digit_02 <- new_x[place_02]
  
  digits_03_opts <- new_x[(place_02 + 1):(n - 9)]
  place_03 <- place_02 + which.max(digits_03_opts)
  digit_03 <- new_x[place_03]
  
  digits_04_opts <- new_x[(place_03 + 1):(n - 8)]
  place_04 <- place_03 + which.max(digits_04_opts)
  digit_04 <- new_x[place_04]
  
  digits_05_opts <- new_x[(place_04 + 1):(n - 7)]
  place_05 <- place_04 + which.max(digits_05_opts)
  digit_05 <- new_x[place_05]

  digits_06_opts <- new_x[(place_05 + 1):(n - 6)]
  place_06 <- place_05 + which.max(digits_06_opts)
  digit_06 <- new_x[place_06]
  
  digits_07_opts <- new_x[(place_06 + 1):(n - 5)]
  place_07 <- place_06 + which.max(digits_07_opts)
  digit_07 <- new_x[place_07]
  
  digits_08_opts <- new_x[(place_07 + 1):(n - 4)]
  place_08 <- place_07 + which.max(digits_08_opts)
  digit_08 <- new_x[place_08]
  
  digits_09_opts <- new_x[(place_08 + 1):(n - 3)]
  place_09 <- place_08 + which.max(digits_09_opts)
  digit_09 <- new_x[place_09]
  
  digits_10_opts <- new_x[(place_09 + 1):(n - 2)]
  place_10 <- place_09 + which.max(digits_10_opts)
  digit_10 <- new_x[place_10]
  
  digits_11_opts <- new_x[(place_10 + 1):(n - 1)]
  place_11 <- place_10 + which.max(digits_11_opts)
  digit_11 <- new_x[place_11]
  
  digits_12_opts <- new_x[(place_11 + 1):n]
  place_12 <- place_11 + which.max(digits_12_opts)
  digit_12 <- new_x[place_12]

  output <- as.numeric(paste0(
    digit_01, digit_02, digit_03, digit_04,
    digit_05, digit_06, digit_07, digit_08,
    digit_09, digit_10, digit_11, digit_12
  ))
  return(output)
}

sprintf("%s", sum(sapply(input, process_input)))
