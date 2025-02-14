
# Aggregate docket-level documents metadata to agency level and save in agency folder
for(agency in agency){

  # document metadata in docket folders
  files <- list.files(pattern = paste0(
    agency, ".*_documents.rda"), recursive = T
  )

  # aggregated metadata for the whole agency
  done <- list.files(pattern = paste0(
    agency, "_documents.rda"), recursive = T
  )

  if(!length(files) == 0 & length(done) == 0){
    load(files[1])
    d <- documents

    for(file in files){
      load(file)

      temp <- documents

      d <<- suppressMessages(full_join(d, temp))
    }

    documents <- d

    save(documents, file = here::here("data", "metadata",  agency, paste0(agency, "_documents.rda")))
  } else
    message(paste("Missing", agency))
}

###################
# Aggregate all agency-level document metadata and save in data folder
files <- list.files(pattern = "^[A-Z]*_documents.rda", recursive = T)

length(files) == length(agency)

done <- files |> str_remove_all(".*/|-.*")

load(files[1])
d <- documents

for(file in files){
  load(file)

  temp <- documents

  d <<- full_join(d, temp)
}

documents <- d

agency[!agency %in% d$agencyId]

save(documents, file = here::here("data", "metadata",  "all_documents.rda"))


d %>%
  #full_join(documents) %>%
  group_by(documentType,docketId) %>%
  slice_max(n = 1, with_ties = F, order_by = postedDate) %>%
  ungroup() %>%
  mutate(year = str_sub(postedDate,1,4) %>% as.numeric()) %>%
  filter(year > 1992,
         documentType %in% c("Rule", "Proposed Rule")) %>%
  ggplot() +
  aes(x = as.factor(year),
      fill = documentType) +
  geom_bar(position = "dodge")



