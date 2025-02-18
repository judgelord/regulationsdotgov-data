# Right now, this script gets the following metadata:
# - dockets per agency
# - documents per docket
# - comments per document
# - comment details per document (using the comments file)

# TODO
# - document details?

# OTHER
# - files will be downloaded with get_files.R

# FUTURE WORK
# - currently, it just checks if a file exists and gets the data from the API if it does not
#FIXME alternatively, specify a date of last run and merge in with existing metadata file
# - this will require improving the functions to include a "ge" (greater than or equal to) argument for foming API calls
# - currently the funtions are hard-coded to be less than or equal to ("le") the date provided


# devtools::install_github("https://github.com/judgelord/regulationsdotgov")
library(regulationsdotgov)

# other packages
library(tidyverse)
library(httr)
library(jsonlite)
library(magrittr)

# keys saved up one directory from this project folder
load("../keys.rda")


# the dockets we already have
#FIXME document folders and getting recorded as docket forlders here, causing incorrect and unnecessary calls
dockets <- list.dirs("data/metadata")

dockets <- tibble(
  agency = str_extract(dockets, ".*/") |> str_remove("/$") |> str_remove(".*/"),
  docket = str_remove(dockets, ".*/")
)

dockets <- dockets |>
  filter(str_detect(docket, "-"),
         !str_detect(agency, "-"))

# PICK AN ORDER
dockets %<>% arrange(rev(agency))
dockets %<>% arrange(agency)



agencies <- dockets$agency


## FIXME Commenting this out for now becasue these no longer seem to be problems
# problem_documents <- tribble(
#   ~docket, ~document, ~objectId,
#   "WHD-2017-0002", "WHD-2017-0002-0001",  "0900006482969fbd", # expected 140-214k
#   "WHD-2017-0003", "WHD-2017-0003-0001", "0900006482cdbe54", # expected 218-376k https://www.regulations.gov/docket/WHD-2017-0003
#   "WHD-2019-0001","WHD-2019-0001-0001", "0900006483b173dc",
#   "PHMSA-2021-0039", "PHMSA-2021-0039-2101", "0900006485c21b4f",
#   "PHMSA-2021-0058", "PHMSA-2021-0058-0002", "0900006484e3545a",
#   "NHTSA-2022-0079", "NHTSA-2022-0079-0015", "090000648637d7c0" # expecting 18,317 - https://www.regulations.gov/document/NHTSA-2022-0079-0015
#   )
#
# dockets %<>%
#   filter(!docket %in% problem_documents$docket)



# or select a new agency
agency <- "CFPB"


# create directories for each agency
walk(here::here("data", "metadata",  agency), dir.create)


# SAVE IN SEPERATE FILES IN DOCKET FOLDERS
save_dockets <- function(agency){
  message(agency)
  dockets <- map_dfr(agency, get_dockets, api_keys = keys)
  message(paste("|", agency, "| n =", nrow(dockets), "|"))
  # agency <- "NOAA"
  # dockets <- metadata

  file <- here::here("data", "metadata",
                     agency,
                     paste0(agency, "_dockets.rda"))

  save(dockets, file = file)
}

# COLLECT DOCKET METADATA WE DON'T HAVE FOR AGENCIES
for (agency in agencies){

  file <- here::here("data", "metadata",
                     agency,
                     paste0(agency, "_dockets.rda"))

  if (!file.exists(file)) {

    tryCatch({
      save_dockets(agency)
      message(agency,"_dockets.rda file created.")
    }, error = function(e) {
      message("Unable to collect", agency,"_dockets.rda.")
    })
  } else{message(agency, "_dockets.rda file already exists.")}
}

# all agency folders
# agency <- list.dirs("data", "metadata",  recursive = F) |> str_remove("data/")

# create directories for each docket
for (agency in agencies) {
  message(agency)

  tryCatch({

    suppressWarnings({

    load(here::here("data", "metadata", agency, paste0(agency, "_dockets.rda")))
    })

    if (!"id" %in% colnames(dockets)) {
      message("No 'id' column found in ", agency, "_dockets.rda.")
      next
    }

    dir_paths <- here::here("data", "metadata", agency, dockets$id)
    new_dirs <- !sapply(dir_paths, dir.exists)

    if (any(new_dirs)) {

    if ("id" %in% colnames(dockets)) {
      walk(dockets$id, ~ {
        dir_path <- here::here("data", "metadata", agency, .x)

        if (!dir.exists(dir_path)) {
          dir.create(dir_path, recursive = TRUE)
          message("Created directory: ", dir_path)
        } else {
          message("Directory already exists: ", dir_path)
        }
      })

      dir_create(dir_paths[new_dirs])

      message("Created new directories for ", agency, ": ", paste(dir_paths[new_dirs], collapse = ", "))

      }} else {
        message("All directories already exist for ", agency)
    }
  }, error = function(e) {
    message("No dockets.rda file found for ", agency)
  })
}




#### Get documents from each docket


# SAVE IN SEPERATE FILES IN DOCKET FOLDERS
save_documents <- function(docket, agency){

  file <- here::here("data", "metadata",
                     agency, #str_extract("[A-Z]"), docket),
                     # should we require an agency argument here, or is there a reliable way to split agencies and dockets, e.g., by looking for years -19[0-9][0-9]- or -20[0-9][0-9]-
                     docket,
                     paste0(docket, "_documents.rda"))


  message (paste(Sys.time(), agency, docket))

  if(!file.exists(file)){
    documents <- map_dfr(docket, get_documents, api_keys = keys)
    message(paste("|", docket, "| n =", nrow(documents), "|"))
    save(documents, file = file)
  } else {
    message(paste(file, "already exists"))
  }
}


# loop over a vector of agencies
for(agency in agencies){
  # load dockets
  load(here::here("data", "metadata",  agency, paste0(agency, "_dockets.rda")))

  # id doc metadata already downloaded

  #FIXME alternatively, specify a date of last run and merge in with exisiting metadata file
  downloaded <- list.files(path = here::here("data", "metadata",agency), pattern = "_documents.rda", recursive = TRUE) |>
    str_remove_all(".*/|_documents.rda") # I added the downloaded object back in -MK

  # if dockets metadata is not empty & docket metadata was not already downloaded, save documents for each docket
  if("id" %in% names(dockets)){

    dockets %<>% filter(!(id %in% downloaded))

    walk2(dockets$id, dockets$agencyId, possibly(save_documents,
                                                 otherwise = print("documents.rda file already exists")))
  } else{
    message(paste(agency, "has no document metadata returned by regulations.gov"))
  }
}









# Documents
save_docs <- function(docket){

  message(docket)

  # for testing
  # docket <- dockets$docket[1]
  doc_file <- here::here("data", "metadata",
                         str_extract(docket, "[A-Z]+"), # agency
                         docket,
                         paste(docket, "documents.rda", sep = "_"))

  if( file.exists(doc_file) ){
    load(doc_file) #FIXME  update this with new docs since most recent document date
  } else {
    documents <- get_documents(docket, api_keys = keys) |> distinct()

    save(documents, file = doc_file)
  }

  # subset to proposed rules and notices that took comments
  d <- documents |>
    #filter(documentType %in% c("Proposed Rule", "Notice")) |>
    rename(document_subtype = subtype,
           commentOnId = objectId,
           document_title = title) |>
    #FIXME not sure if a 0 comment document was the source of the error I got
    drop_na(commentStartDate)


  # HELPER FUNCTION
  create_doc_dir <- function(document){

    message(document)

    doc_dir <- here::here("data", "metadata",
                          str_extract(docket, "[A-Z]+"), # agency
                          docket,
                          document)

    dir.create(doc_dir)
  }

  # save comments on specific documents
  walk(d$id, create_doc_dir)


}

# test
walk(head(dockets$docket, 4), possibly(save_docs, otherwise = print("nope")))

# all of them
walk(dockets$docket, possibly(save_docs, otherwise = print("nope")))


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

### OLD CODE FIXING ERROR IN FILE PATHS
# docs <- docs |>
#   mutate(old_filename = here::here(directory, paste(document, "comments.rda", sep = "_")) |> str_remove(paste0(document, "/")),
#          new_filename = here::here(directory, paste(document, "comments.rda", sep = "_"))
#          )
#
# file.rename(docs$old_filename, docs$new_filename)


# ## OLD CODE FIXING ERROR IN FILE PATHS
# docs <- docs |>
#   mutate(old_filename = here::here(directory, paste(document, "comment_details.rda", sep = "_")) |> str_remove(paste0(document, "/")),
#          new_filename = here::here(directory, paste(document, "comment_details.rda", sep = "_"))
#          )
#
# file.rename(docs$old_filename, docs$new_filename)






################
# COMMENTS #
#######################################################
#### Get metadata for comments on a document or docket
# (Along the way, get any missing documents)

# get_comments_on_docket("[docket_id]") # retrieves all comments for a docket (e.g., including an Advanced Notice of Proposed Rulemaking and all draft proposed rules)


save_comments <- function(docket){

  message(docket)

  # for testing
  # docket <- dockets$docket[1]
  doc_file <- here::here("data", "metadata",
                         str_extract(docket, "[A-Z]+"), # agency
                         docket,
                         paste(docket, "documents.rda", sep = "_"))

  if( file.exists(doc_file) ){

    load(doc_file) #FIXME  update this with new docs since most recent document date

    documents %<>% distinct()

    # FIXME -- TEMPORARY uniquing comment files saved with rbind
    # save(documents, file = doc_file)

  } else {
    message("getting documents")
    documents <- get_documents(docket, api_keys = keys) |>
      distinct()

    save(documents, file = doc_file)
  }

  # subset to proposed rules and notices that took comments
  d <- documents |>
    filter(documentType %in% c("Proposed Rule", "Notice")) |>
    rename(document_subtype = subtype,
           commentOnId = objectId,
           document_title = title) |>
    #FIXME not sure if a 0 comment document was the source of the error I got
    drop_na(commentStartDate)


  # HELPER FUNCTION
  save_commentsOnId <- function(objectId, document){

    message(document)

    doc_dir <- here::here("data", "metadata",
                          str_extract(docket, "[A-Z]+"), # agency
                          docket,
                          document)

    dir.create(doc_dir)

    comments_on_document_file =  here::here("data", "metadata",
                                            str_extract(docket, "[A-Z]+"), # agency
                                            docket,
                                            document,
                                            paste(document, "comments.rda", sep = "_"))

    ## DETAILS
    det_file <- str_replace(comments_on_document_file, "comments.rda", "comment_details.rda")


    ## If we need comment metadata
    if( !file.exists(comments_on_document_file) ){

      message("getting comments")

      comments <- get_commentsOnId(objectId, api_keys = keys)

      save(comments,
           file = comments_on_document_file)

    } else {
      load(comments_on_document_file)

      comments %<>% distinct()

      # FIXME -- TEMPORARY uniquing comment files saved with rbind
      #save(comments, file = comments_on_document_file)
    }



    if(  nrow(comments) > 10000 ){
      save(det_file, file = det_file |> str_replace("details", "MoreThan10k"))
    }

    # GET DETAILS
    if( !file.exists(det_file) & nrow(comments) <= 10000 & "id" %in% colnames(comments)){

      message("getting comment details")

      comment_details <- get_comment_details(comments$id, api_keys = keys) |>
        distinct()

      save(comment_details, file = det_file)
    }

  }

  # save comments on specific documents
  walk2(d$commentOnId, d$id,
        possibly(save_commentsOnId, otherwise = message("FAIL"))) #FIXME WE NEED A BETTER MESSAGE

}

walk(head(dockets$docket, 4), possibly(save_comments, otherwise = print("nope")))

walk(dockets$docket, possibly(save_comments, otherwise = print("nope")))













