# load commonly used packages
source("setup.R")

metadata_root <- here::here("data", "metadata")

# the comments we already have metadata for
doc_list <- list.files(path = metadata_root,
                       pattern = ".csv", recursive = T,
                       include.dirs = T)

# a data frame of parts of the document directory path
docs <- tibble(
  csv_path = here::here(metadata_root, doc_list),
  rda_path = here::here(metadata_root, doc_list) |> str_replace(".csv$", ".rda") ,
  agency = str_remove(doc_list, "/.*"),
  docket = str_remove(doc_list, ".*?/") |> str_remove("/.*"),
  document = str_remove(doc_list, ".*/") |> str_remove("_.*")
) |>
  # drop aggregated details
  filter(docket != document)

# inspect
head(docs)

rda_path <- docs$rda_path[1]
csv_path <- docs$csv_path[1]

library(janitor)


convert <- function(csv_path, rda_path){
  d1 <- read_csv(csv_path)

  names(d1)

  d1 <- d1 |>


  if( file.exists(rda_path) ){
    load(rda_path)

    names(comment_details) %<>% janitor::make_clean_names("small_camel")
    new_names <- names(comment_details)
    new_names[which(!new_names %in% names)]
    names[which(!names %in% new_names)]


    names
  }

}



