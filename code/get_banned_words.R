

source("setup.R")
# keys saved up one directory from this project folder
load("../keys.rda")

library(regulationsdotgov)

# subdir = "banned"

  # banned words
  library(googlesheets4)
  searchTerm = read_sheet("1LLKWDHiwnVEpvlqurI1t7NNHmdrk2liXPd-FZkNrvW0") |>
    drop_na(term) |>
    filter(!str_detect(term, "/")) |>
    pull(term) |>
    unique()

  searchTerm <- searchTerm[!searchTerm %in% c("gender identity", "definition", "expression", "sex", "sexuality",
                                              "accessible", "entitlement", "equality",
                                              "genders", "anglo-saxon",
                                              "women",
                                              "science-based",
                                              "Americanize", "Americanization",
                                              "enhance the diversity", "female",
                                              "consultation", "orientation",
                                              "accessable", "accessibility", "Women", "clean energy",
                                              "environmental quality", "community")]



  documents = c("documents", "comments")
  # documents = "comments"


  search_to_rda <- function(searchTerm, documents, lastModifiedDate = Sys.Date() ){

    directory <- here::here("data", "search", #subdir,
                            searchTerm)
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


if(documents == "comments"){
      comments <- d |> distinct()

      save(comments, file = file)
}
      if(documents == "documents"){
        documents <- d |> distinct()

        save(documents, file = file)
      }
    }
  }

# map over search terms for each document type
  for(documents in documents){
    purrr::walk(searchTerm,
                documents,
                #subdir = subdir,
                #lastModifiedDate = "2025-02-02T04:59:59Z",
                .f = search_to_rda) #FIXME Possiblly ?
  }


