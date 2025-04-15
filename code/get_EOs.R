source("setup.R")

# keys saved up one directory from this project folder
load("../keys.rda")

library(regulationsdotgov)

EOs <- read_csv(here::here("data", "documents_of_type_presidential_document_and_of_presidential_document_type_executive_order.csv")) |>
    pull("executive_order_number")

a <- paste("E.O.", EOs)
  b <-  paste("EO", EOs)
c <- paste("Executive Order", EOs)
  searchTerm <- c(a,b,c)

  subdir = "EOs"



  documents = c("documents", "comments")
  # documents = "comments"


  search_to_rda <- function(searchTerm, documents, subdir, lastModifiedDate = Sys.Date() ){

    directory <- here::here("data", "search", subdir, searchTerm)
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
                subdir = subdir,
                #lastModifiedDate = "2025-02-02T04:59:59Z",
                .f = search_to_rda) #FIXME Possiblly ?
  }


