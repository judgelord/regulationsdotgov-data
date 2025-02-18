library(tidyverse)
library(magrittr)

files <- list.files(pattern = ".*comments.rda", recursive = T)

file_list=file.info(files)

file_list=file_list[order(file_list$size),]

d <- tail(file_list,1000)

d$path <- rownames(d)

df <- d |> tibble::as.tibble() |>
  select(path, size) |>
  filter(!str_detect(path, "search")) |>
  arrange(-size)

get_n <- function(c){
  load(c)

  n <- distinct(comments) |> nrow()

  return(n)

}

load(df$path[1])
distinct(comments)

get_n(df$path[1])


ns <- map_dbl(df$path, get_n)


df$n <- ns

df %<>% arrange(-n)

# bulk csv files already in folder
bulk <- c(
"FDA-2021-N-1349-0001",
"ATF-2021-0001-0001",
"WHD-2022-0003-0001",
)

head(df, 10) |> pull(path) |>
  str_remove_all(".*/|_c.*") #|> str_c(collapse = ", ")
