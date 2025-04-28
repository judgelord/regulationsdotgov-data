library(tidyverse)

agencies <- list.dirs(here::here("data", "metadata"), recursive = F) |>
  str_remove(".*/") |> rev()


# Aggregate docket-level documents metadata to agency level and save in agency folder
for(agency in agencies){

  # document metadata in docket folders
  files <- list.files(pattern = paste0(
    agency, ".*_documents.rda"), recursive = T
  )

  dir <- list.dirs(here::here("data", "metadata", agency), recursive = F)
  dirs <- list.dirs(here::here("data", "metadata", agency), recursive = T)


  message("|",agency, "|", length(dir), " docket folders|")
  message("|",agency, "|", length(dirs), " document folders|")
  message("|",agency, "|", length(files), " document metadata files|")

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
###################
# Aggregate all agency-level document metadata and save in data folder
# files <- list.files(pattern = "^[A-Z]*_documents.rda", recursive = T)

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



