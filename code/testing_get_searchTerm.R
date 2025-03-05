
source("setup.R")



  searchTerm =  c("national congress of american indians", "cherokee nation", "climate justice", "environmental justice")
  searchTerm = c("racial", "latino")
  searchTerm = c("racial")
  searchTerm = "climate change"

  # banned words
  library(googlesheets4)
  searchTerm = read_sheet("1LLKWDHiwnVEpvlqurI1t7NNHmdrk2liXPd-FZkNrvW0") |>
    drop_na(term) |>
    filter(!str_detect(term, "/")) |>
    pull(term) |>
    unique()



  searchTerm <- searchTerm[!searchTerm %in% c("gender identity", "accessibility", "Women", "clean energy")]

  documents = c("documents", "comments")
  # documents = "comments"


  search_to_rda <- function(searchTerm, documents, lastModifiedDate = Sys.Date() ){

    directory <- here::here("data", "search", searchTerm)
    dir.create(directory)
    file <- here::here(directory, paste0(searchTerm, "_", documents, ".rda"))

    if( !file.exists(file) ){

      # TESTING date modification
      # lastModifiedDate = "2025-02-02T04:59:59Z"
      # / testing

      d <- get_searchTerm(searchTerm,
                          lastModifiedDate = lastModifiedDate,
                          documents,
                          api_keys = keys)

      message(min(d$postedDate))

      comments <- d |> distinct()

      save(comments, file = file)
    }
  }

# map over search terms for each document type
  for(documents in documents){
    purrr::walk(searchTerm,
                documents,
                #lastModifiedDate = "2025-02-02T04:59:59Z",
                .f = search_to_rda)
  }


