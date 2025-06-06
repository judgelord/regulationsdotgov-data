library(tidyverse)

agencies <- list.dirs(here::here("data", "metadata"), recursive = F) |>
  str_remove(".*/") |> rev()

# faster not to count the folders
quite = T

# Aggregate docket-level documents metadata to agency level and save in agency folder
for(agency in agencies){

  # document metadata in docket folders
  files <- list.files(pattern = paste0(
    agency, ".*_documents.rda"), recursive = T
  )

  # one file per docket, should be about the same as the number of dockets
  message("|",agency, "|", length(files), " docket-level document metadata files|")


  if(quite == F){
  dir <- list.dirs(here::here("data", "metadata", agency), recursive = F)
  dirs <- list.dirs(here::here("data", "metadata", agency), recursive = T)


  message("|",agency, "|", length(dir), " docket folders|")
  message("|",agency, "|", length(dirs), " document folders|")
  }


  # # aggregated metadata for the whole agency
  # done <- list.files(pattern = paste0(
  #   agency, "_documents.rda"), recursive = T
  # )

  if(!length(files) == 0){# & length(done) == 0){
    load(files[1])
    d <- documents

    for(file in files){
      load(file)

      temp <- documents

      if(!length(temp) == 0){
      d <<- suppressMessages(full_join(d, temp))
      } else {
        message(file, " is empty")
        file.rename(file,
                    file |> str_replace("_documents.rda", "_documents_MISSING.rda"))
      }
    }

    message("|",agency, "|", nrow(d), " documents in metadata|")

    documents <- d

    save(documents, file = here::here("data", "metadata",  agency, paste0(agency, "_documents.rda")))
  } else {
    message(paste("Missing", agency))
    }
}
# END Agency-level aggregation




###################
# Aggregate all agency-level document metadata and save in data folder
files = here::here("data", "metadata", agencies, paste0(agencies, "_documents.rda"))

length(files) == length(agency)

# done <- files |> str_remove_all(".*/|-.*")

# init
load(files[1])
d <- documents

# did not work due to some file missing data
all_documents <- map_dfr(files, possibly(load, otherwise = print(file)))

# trying again, this works
for(file in files){
  if(file.exists(file)){
  load(file)

  temp <- documents

  d <<- full_join(d, temp)
  } else {
    message(file, "DOES NOT EXIST")
  }
}

load(here::here("data", "metadata",  "all_documents.rda"))

library(magrittr)
all_documents %<>% full_join(d) %>% distinct()

duplicates <- d |> count(objectId, sort = T)|> filter(n > 1)

duplicate_object_id <- d |> filter(objectId %in% duplicates$objectId) |> arrange(objectId)

duplicates <- d |> count(id, sort = T)|> filter(n > 1)

duplicates_id <- d |> filter(id %in% duplicates$id) |> arrange(id)

agency[!agency %in% d$agencyId]

save(all_documents, file = here::here("data", "metadata",  "all_documents.rda"))

documents_count <- d |>
  mutate(year = str_sub(postedDate, 1,4)) |>
  distinct(id, documentType, year, agencyId, docketId) |>
  group_by(documentType, year, agencyId, docketId) |>
  count()

documents_count |> filter(agencyId == "NSF")

save(documents_count,
     file = here::here("data", "metadata",  "documents_count.rda"))


documents_count %>%
  #full_join(documents) %>%
  group_by(documentType,docketId) %>%
  slice_max(n = 1, with_ties = F, order_by = year) %>%
  ungroup() %>%
  #mutate(year = str_sub(postedDate,1,4) %>% as.numeric()) %>%
  filter(year > 1992,
         documentType %in% c("Rule", "Proposed Rule")) %>%
  ggplot() +
  aes(x = as.factor(year),
      fill = documentType) +
  geom_bar(position = "dodge")




################################################################
# AGGREGATE COMMENTS #
################################################################

# Aggregate docket-level comments metadata to agency level and save in agency folder
for(agency in agencies){

  # comment metadata in docket folders
  files <- list.files(pattern = paste0(
    agency, ".*_comments.rda"), recursive = T
  )



  if(quite == F){
    dir <- list.dirs(here::here("data", "metadata", agency), recursive = F)
    dirs <- list.dirs(here::here("data", "metadata", agency), recursive = T)


    message("|",agency, "|", length(dir), " docket folders|")
    message("|",agency, "|", length(dirs), " document folders|")
  }

  # one file per document, should be about the same as the number of dockets
  message("|",agency, "|", length(files), " document-level comment metadata files|")

  if(length(files) > 0){

    # init
    load(files[1])
    d <- comments

    # concat files
    for(file in files){
      load(file)

      temp <- comments

      if(!length(temp) == 0){
        d <<- suppressMessages(full_join(d, temp))
      } else {
        message(file, " is empty")
        file.rename(file,
                    file |> str_replace("_comments.rda", "_comments_MISSING.rda"))
      }
    }

    # count of comments
    message("|",agency, "|", nrow(d), " comments in metadata|")

    comments <- d

    save(comments,
         file = here::here("data", "metadata",
                           agency,
                           paste0(agency, "_comments.rda") )
         )
  } else {
    message( paste("Missing", agency) )
  }
}
# END Agency-level aggregation



###################
# Aggregate all agency-level comment metadata and save in data folder
files = here::here("data", "metadata", agencies, paste0(agencies, "_comments.rda"))

length(files) == length(agency)

# init
load(files[1])
d <- comments

# trying again, this works
for(file in files){
  if(file.exists(file)){
    message( file |> str_remove(".*/") )
    load(file)

    temp <- comments

    d <<- suppressMessages(full_join(d, temp))
  } else {
    message(file, "DOES NOT EXIST")
  }
}

load(here::here("data", "metadata",  "all_comments.rda"))
