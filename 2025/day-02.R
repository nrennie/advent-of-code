
library(tidyverse)
library(usefunc)


# Data --------------------------------------------------------------------

input <- read.csv("2025/data/input-02.txt", header = F)
input_df <- input |> 
  as_tibble() |> 
  pivot_longer(everything()) |> 
  select(value) |> 
  separate_wider_delim(value, delim = "-", names = c("start", "end"))


# Part 1 ------------------------------------------------------------------

is_invalid <- function(x) {
  new_x <- as.character(x)
  n_x <- nchar(new_x) 
  if (is_odd(n_x)) {
    return(FALSE)
  } else {
    x_s <- str_split_1(new_x, "")
    output <- str_flatten(x_s[1:(n_x/2)]) == str_flatten(x_s[(n_x/2 + 1):n_x])
    return(output)
  }
}

invalid_ids <- list()
for (i in 1:nrow(input_df)) {
  ids_to_check <- seq(input_df$start[i], input_df$end[i])
  new_invalid_ids <- ids_to_check[sapply(ids_to_check, is_invalid)]
  if (length(new_invalid_ids) > 0) {
    invalid_ids[[i]] <- new_invalid_ids
  } else {
    invalid_ids[[i]] <- 0
  }
}

sum(unlist(invalid_ids))





