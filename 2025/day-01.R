
library(dplyr)
library(stringr)
library(readr)


# Data --------------------------------------------------------------------

input <- readLines("2025/data/input-01.txt")


# Process -----------------------------------------------------------------

input_vec <- input |> 
  as_tibble() |> 
  mutate(number = if_else(
    str_starts(value, "L"), -1 * parse_number(value), parse_number(value)
  )) |> 
  pull(number)


# Part 1 ------------------------------------------------------------------

num_zeros <- 0
current <- 50

for (i in 1:length(input_vec)) {
  current <- (current + input_vec[i]) %% 100
  if (current == 0) {
    num_zeros <- num_zeros + 1
  }
}
num_zeros

