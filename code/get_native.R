

source("setup.R")
# keys saved up one directory from this project folder
load("../keys.rda")

library(regulationsdotgov)


  subdir = "native"

  library(googlesheets4)
  native_orgs = read_sheet("1crWSexVnc1979ob5ql3_xATpF3K9f5Z7XpZ4QpaTe3Y")

  searchTerm <- native_orgs |>
    drop_na(Strings) |>
    mutate(string = str_split(Strings, "\\|")) |>
    unnest(string) |>
    filter(!str_detect(string, "/")) |>
    mutate(string = str_squish(string)) |>
    pull(string) |>
    unique()

  head(searchTerm)



  types = c("documents", "comments")
  # type = "comments"

  directory <- here::here("data", "search", subdir)

  dir.create(directory)

  search_to_rda <- function(searchTerm, type, subdir, lastModifiedDate = Sys.Date() ){

    directory <- here::here("data", "search", subdir, searchTerm)
    dir.create(directory)
    file <- here::here(directory, paste0(searchTerm, "_", type, ".rda"))

    if( !file.exists(file) ){

      # TESTING date modification
      # lastModifiedDate = "2025-02-02T04:59:59Z"
      # / testing

      d <- get_searchTerm(searchTerm,
                          lastModifiedDate = lastModifiedDate,
                          type,
                          api_keys = keys)

      message(min(d$postedDate))


if(type == "comments"){
      comments <- d |> distinct()

      save(comments, file = file)
}
      if(type == "documents"){
        documents <- d |> distinct()

        save(documents, file = file)
      }
    }
  }

# map over search terms for each document type
  for(type in types){
    purrr::walk(searchTerm,
                type,
                subdir = subdir,
                #lastModifiedDate = "2025-02-02T04:59:59Z",
                .f = search_to_rda) #FIXME Possiblly ?
  }


