# load commonly used packages
source("setup.R")

metadata_root <- here::here("data", "metadata")

files_root <- here::here("data", "md")

if(F){
  metadata_root %<>% str_replace("/Volumes/Devin's 5TB/",
                                 "/Users/judgelor/University of Michigan Dropbox/Devin Judge-Lord/")
}

dir.create(files_root)

# the comments we already have metadata for
doc_list <- list.files(path = metadata_root,
                       pattern = "_comment_details.rda", recursive = T,
                       include.dirs = T)

# a data frame of parts of the document directory path
docs <- tibble(
  metadata_path = paste(metadata_root, doc_list, sep = "/"),
  file_dir = here::here(files_root, doc_list) |>  str_extract(".*/") |> str_remove("/$"),
  agency = str_remove(doc_list, "/.*"),
  docket = str_remove(doc_list, ".*?/") |> str_remove("/.*"),
  document = str_remove(doc_list, ".*/") |> str_remove("_.*"),
  details = str_remove(doc_list, ".*/")
) |>
  # drop aggregated details
  filter(docket != document)

# inspect
head(docs)

##########################
# 1. CREATE DIRECTORIES  #
##########################

dir.create(here::here(files_root, docs$agency[1]))

# make sure there is a directory for each agency
agencies <- docs$agency |> unique()

for(i in agencies) {
  dir.create(here::here(files_root, i))
}

# a function to make folders for each docket
create_docket_folder <- function(agency, docket){
  dir.create(here::here(files_root, agency, docket))
}

dockets <- distinct(docs, agency, docket)

# map that function over dockets
walk2(dockets$agency, dockets$docket, create_docket_folder)

# a function to make folders for each document
create_doc_folder <- function(agency, docket, document){
  dir.create(here::here(files_root, agency, docket, document))
}

# map that function over documents
pwalk(list(docs$agency, docs$docket, docs$document), .f = create_doc_folder)
############# END CREATE DIRECTORIES ###################
