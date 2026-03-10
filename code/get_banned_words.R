

source("setup.R")
# keys saved up one directory from this project folder
load("../keys.rda")

library(regulationsdotgov)

# subdir = "banned"

  # banned words
  library(googlesheets4)
gs4_auth(email = "devin.jl@gmail.com")
  # big list
# searchTerm = read_sheet("1LLKWDHiwnVEpvlqurI1t7NNHmdrk2liXPd-FZkNrvW0") |>
# small list
  searchTerms = read_sheet("1LLKWDHiwnVEpvlqurI1t7NNHmdrk2liXPd-FZkNrvW0", sheet = "Racialized Terms")|>
    drop_na(term) |>
    filter(!str_detect(term, "/")) |>
    pull(term) |>
    unique()

  # run into problems
  searchTerms <- searchTerms[!searchTerms %in% c("American Indian","Africans", "gender identity", "definition", "expression", "sex", "sexuality",
                                              "accessible", "entitlement", "equality",
                                              "genders", "anglo-saxon",
                                              "women",
                                              "science-based",
                                              "Americanize", "Americanization",
                                              "enhance the diversity", "female",
                                              "consultation", "orientation",
                                              "accessable", "accessibility", "Women", "clean energy",
                                              "environmental quality", "community")]



  type = c("documents", "comments")
  # type = "documents"
  # type = "comments"
  # searchTerm = searchTerms[21]


  search_to_rda <- function(searchTerm, type, lastModifiedDate = Sys.Date() ){

    directory <- here::here("data", "search", #subdir,
                            searchTerm)
    dir.create(directory)
    file <- here::here(directory, paste0(searchTerm, "_", type, ".rda"))

    if( file.exists(file) ){
      message(paste(searchTerm, type, "search data exist"))
      load(file)
      if(type == "documents"){
        date <- max(documents$date)
      }
      if(type == "comments"){
        date <- max(comments$date)
      }
      if(date < as.Date("2026-01-01") ){
        d <- get_searchTerm(searchTerm,
                            lastModifiedDate = lastModifiedDate,
                            type,
                            api_keys = keys)

        if(type == "comments"){
          comments <- full_join(comments, d) |> distinct()

          save(comments, file = file)
        }
        if(type == "documents"){
          documents <- full_join(documents, d) |> distinct()

          save(documents, file = file)
        }
      }
    }


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
  for(type in type){
    purrr::walk(searchTerms,
                type,
                #subdir = subdir,
                #lastModifiedDate = "2025-02-02T04:59:59Z",
                .f = search_to_rda) #FIXME Possiblly ?
  }


