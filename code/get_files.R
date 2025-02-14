source("setup.R")

#

# the agencies we already have
doc_list <- list.dirs("data/metadata")

docs <- tibble(
  directory = doc_list,
  agency = str_remove(doc_list, ".*metadata/") |> str_remove("/.*"),
  docket = str_remove(doc_list, ".*metadata/") |> str_remove("/.+?"),
  document = str_remove(doc_list, ".*/")
)

docs <- docs |>  filter(str_detect(docket, "/")) |>
  mutate(docket = str_remove(docket, "/.*") )

example_path <- here::here(
  "data",
  "meatadata",
  #docs$document[1],
  #paste0(docs$document[1], "_comment_details.rda")
  "BLM",
  "BLM-2015-0002",
  "BLM-2015-0002-0016",
  "BLM-2015-0002-0016_comment_details.rda"
)

example_path <- here::here("data/metadata/BLM/BLM-2015-0002/BLM-2015-0002-0016/BLM-2015-0002-0016_comment_details.rda")

load(example_path)



###############################
# NOT WELL DEVELOPED #
# DOWNLOAD ATTACHMENTS

#FIXME THIS DOES NOT YET WORK
# extract attachments from details
attachments <- comment_details$attachments |>
  flatten() |>
  map(as.data.frame) |> #FIXME, can we do this in one line?
  map_dfr(~.x)

# inspect
ggplot(attachments) +
  aes(x = size, fill = format) +
  geom_bar()

ggplot(attachments |> slice_max(size, n = 100)) +
  aes(x = size, fill = format) +
  geom_bar()

#TODO? drop super large files?
# can we store a record of files lost?
# attachments %<>% filter(size < 100000000) # = < 100 MB ?

attachment_urls <- attachments |>
  pull(fileUrl) |>
  unique()


download_comments <- function(url){
  #TODO
}


download_comments(attachment_urls )





