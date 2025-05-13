library(here)
library(tidyverse)
library(magrittr)

files <- here("data", "search") |>
list.files(pattern = ".rda", recursive = T, full.names = T)

# file <- files[5]

get_dockets_from_search_data <- function(file){
  message(file |> str_remove(".*/"))
  load(file)

  d <- NA

  # FIXME replace with objRDA(file) # https://rdrr.io/github/dusadrian/admisc/man/rdaFunctions.html
  if(exists("comments") & !exists("documents")){
    if("docketId" %in% names(comments)){
      d <- comments$docketId |> unique()

    } else{
      if("id" %in% names(comments)){
        d <- comments$id |>
          str_remove("-[0-9]*$")  |>
          # some ids have two parts, leaving strings loner than the docket, e.g., PHMSA-RSPA-1994-13559-0014, FAA-2004-17005-20270, FHWA-2006-25031-1539, FMCSA-2005-23151-0077
          str_replace("(-[0-9]{4,})(-[0-9]{4,})(-[0-9]{4,})$", "\\1\\2")
      # d <- comments$id |> str_extract(".*RULEMAKING|.*RULE|.*_FRDOC_[0-9]*|([A-Z]*-)+-[0-9]*(-[A-Z]*)+-[0-9]*|([A-Z]*-)+-[0-9]{4}-[0-9]*|([A-Z]*-)+-[0-9]{2}-[0-9]*") |> unique()
      # FIXME EERE-2006-BT-WAV-0174, FCIC-07-0013, FDA-1967-P-0014, FTA-1995-353, FWS-HQ-BPHR-2012-0089
      # GIPSA-2006-PSP-0028-RULE ? (NOT SURE THIS IS POSSIBLE )
      # + FEMA_FRDOC_0001
      # EPA-R02-OAR-2005-VI-0001
      # OSHA-ICR1218-0NEW1-2003-2006-0441
    } else (message(file, " missing id | n = ", length(comments)))
    }
  }

  if(!exists("comments") & exists("documents")){
    if("docketId" %in% names(documents)){
    d <- documents$docketId |> unique()
    }
  }

  return(list(d))
}

dockets <- map(.x = files,
               .f = possibly(get_dockets_from_search_data ))

dockets_from_search <- unlist(dockets) |> unique()


dockets_from_search <- tibble(
  docket = dockets_from_search ,
  agency = str_remove(dockets_from_search, "_.*|-.*") #FIXME do some agencies need dashes
) |>
  drop_na(agency)

dockets_from_search

# inspct agencies
dockets_from_search$agency |> unique()

# inspect dockets
look <- dockets_from_search |>
  ungroup() |>
  group_by(agency) |>
  mutate(n = nchar(docket)) |>
  slice_max(n = 2, order_by = n, with_ties = F) |>
  ungroup()

view(look)

# testing fix
"FMCSA-2005-23151-0077" |> str_replace("(-[0-9]{4,})(-[0-9]{4,})(-[0-9]{4,})$", "\\1\\2")
dockets_from_search$docket %<>% str_replace("(-[0-9]{4,})(-[0-9]{4,})(-[0-9]{4,})$", "\\1\\2")

save(dockets_from_search,
     file = here("data", "dockets_from_search.rda") )

# to go to get everything
dockets_df <- dockets_from_search


# make docket folders from search data (for completeness)
if(F){
  load(here::here("data", "dockets_from_search.rda") )

  dockets_df <- dockets_from_search

  purrr::walk(here::here("data", "metadata",  unique(dockets_df$agency)) , dir.create)

  purrr::walk(here::here("data", "metadata",  dockets_df$agency, dockets_df$docket), dir.create)

}
