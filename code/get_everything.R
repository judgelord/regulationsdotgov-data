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
# - currently the functions are hard-coded to be less than or equal to ("le") the date provided

# If updating package
if(F){
  install.packages("regulationsdotgov")
  remove.packages("regulationsdotgov")
  library(regulationsdotgov) # confirm uninstall
}

if(!"regulationsdotgov" %in% installed.packages() ){
  devtools::install_github("https://github.com/judgelord/regulationsdotgov", force = T)
  install.packages("regulationsdotgov")
}

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

# drop paths to agencies or documents rather than documents
dockets <- dockets |>
  filter(str_detect(docket, "-"),
         !str_detect(agency, "-"))

# PICK AN ORDER
dockets %<>% arrange(rev(docket))
dockets %<>% arrange(docket)



agencies <- dockets$agency


problem_documents <- tribble(
  ~docket, ~document, ~objectId,
  "APHIS-2022-0003", "APHIS-2022-0003-0001", "0900006484e23ccb",
  "HHS-OS-2011-0023", "HHS-OS-2011-0023-0002", "0900006480ed4b59"
  ## FIXME Commenting out for now because these no longer seem to be problems
#   "WHD-2017-0002", "WHD-2017-0002-0001",  "0900006482969fbd", # expected 140-214k
#   "WHD-2017-0003", "WHD-2017-0003-0001", "0900006482cdbe54", # expected 218-376k https://www.regulations.gov/docket/WHD-2017-0003
#   "WHD-2019-0001","WHD-2019-0001-0001", "0900006483b173dc",
#   "PHMSA-2021-0039", "PHMSA-2021-0039-2101", "0900006485c21b4f",
#   "PHMSA-2021-0058", "PHMSA-2021-0058-0002", "0900006484e3545a",
#   "NHTSA-2022-0079", "NHTSA-2022-0079-0015", "090000648637d7c0" # expecting 18,317 - https://www.regulations.gov/document/NHTSA-2022-0079-0015
  )
#
dockets %<>%
  filter(!docket %in% problem_documents$docket)



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
  } else {
    tryCatch({
    load(file)

    fresh_dockets <- get_dockets(agency = agency, api_keys = keys)

    new_dockets <- dplyr::anti_join(fresh_dockets, dockets, by = "id")

    if (nrow(new_dockets) > 0) {
      dockets <- rbind(dockets, new_dockets)
      save(dockets, file = file)
      message("Updated ", agency, "_dockets.rda with new dockets.")
    } else {
      message("No new dockets found for ", agency, ".")
    }},error = function(e) {
      message("An error occured", e$message)
    })
  }
  }


# all agency folders
# agency <- list.dirs("data", "metadata",  recursive = F) |> str_remove("data/")

# create directories for each docket
#FIXME warnings + error message need to be worked on
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





#######################################################
         #####################################
         # DOCUMENTS AND COMMENTS ON DOCKETS #
         #####################################
#######################################################
#### Get metadata for comments on a document or docket
# (Along the way, get any missing documents)

max_number <- 1000000000

save_comments <- function(docket){

  message(docket, appendLF = F)

  # for testing
  # docket <- dockets$docket[1]

  # file path for document metadata
  doc_file <- here::here("data", "metadata",
                         str_extract(docket, "[A-Z]+"), # agency
                         docket,
                         paste(docket, "documents.rda", sep = "_"))


  if( file.exists(doc_file) ){ # if the document metadata exists, load it

    load(doc_file) #FIXME  update this with new docs since most recent document date

    documents %<>% distinct()

    # FIXME -- TEMPORARY uniquing comment files saved with rbind
    #save(documents, file = doc_file)

  } else { # if document metadata does not exist, get it
    message("getting documents")
    documents <- get_documents(docket, api_keys = keys) |>
      distinct()

    save(documents, file = doc_file)
  }

  # subset to documents that were open for comment comments
  d <- documents |>
    #filter(documentType %in% c("Proposed Rule", "Notice")) |> # FIXME, consider comments on other docs for next pass
    # rename vars to avoid conflicts with comment data and be clear about what is a document id vs a comment id
    rename(document_subtype = subtype,
           commentOnId = objectId,
           document_title = title) |>
    #FIXME not sure if a 0 comment document was the source of the error I got
    drop_na(commentStartDate)

  message(" | ", nrow(d), " document(s) open for comment | ")

  # HELPER FUNCTION TO GET AND SAVE COMMENTS AND COMMENT DETAILS
  save_commentsOnId <- function(objectId, document){

    # each document gets a folder in the docket folder
    doc_dir <- here::here("data", "metadata",
                          str_extract(docket, "[A-Z]+"), # agency
                          docket,
                          document)

    if(!dir.exists(doc_dir)){dir.create(doc_dir)}

    # in document older, the file path for comments metadata
    comments_on_document_file =  here::here("data", "metadata",
                                            str_extract(docket, "[A-Z]+"), # agency
                                            docket,
                                            document,
                                            paste(document, "comments.rda", sep = "_"))

    ## the file path for comment_details metadata
    det_file <- str_replace(comments_on_document_file, "comments.rda", "comment_details.rda")

    ## if we don't already have comment_details metadata, get it
    if(!file.exists( det_file )){

    ## If we also need comment metadata, first get it
    if( !file.exists(comments_on_document_file) ){

      message("|  Getting comments on ", document, " | ", appendLF = F)

      comments <- get_commentsOnId(objectId, api_keys = keys)

      save(comments,
           file = comments_on_document_file)

    } else { # if comment metadata file already exists, load it
      message("|  Loading comments on ", document, " | ", appendLF = F)

      load(comments_on_document_file)

      comments %<>% distinct()

      # # if the saved file contains no comments, try again, just to make sure that no comments were received
      # # FIXME This takes a long time and seems to not get anything new, so we should typically skip this chunk
      # if( !"id" %in% colnames(comments)){
      #   message(" | No comments in comments.rda | Trying again", appendLF = F)
      #
      #   comments <- get_commentsOnId(objectId, api_keys = keys)
      #
      #   save(comments,
      #        file = comments_on_document_file)
      # }

      # FIXME -- TEMPORARY uniquing comment files saved with rbind
      #save(comments, file = comments_on_document_file)
    }

      message(" | ", nrow(comments) - 1, " comments |")

      # if there are more than 100k comments, skip it (we will get these from bulk data)
    if(  nrow(comments) > max_number ){
      save(det_file, file = det_file |> str_replace("details", "MoreThan100k"))
    }

    # otherwise, GET COMMENT_DETAILS
    if( !file.exists(det_file) & nrow(comments) <= max_number & "id" %in% colnames(comments)){

      message("|   Getting comment details")

      comment_details <- get_comment_details(comments$id, api_keys = keys) |>
        distinct()

      save(comment_details, file = det_file)
    }

    } else { # if det_file exists
      message("|    Comment_details.rda exists, loading | ", appendLF = F)

      # load comments metadata
      load(comments_on_document_file)

      # load comment details
      load(det_file)

      # subset comment metadata to comments missing from comment_details
      comments_missing <- filter(comments, !id %in% comment_details$id)

      message(nrow(comments_missing), " of ", nrow(comments), " missing |")

      if(nrow(comments_missing)>0 & nrow(comments_missing)<max_number & "id" %in% colnames(comments) ){ #FIXME change to 100k when bulk data is formatted correctly such that id is id
      # get missing comment details
      comment_details2 <- get_comment_details(comments_missing$id, api_keys = keys) |>
        distinct()

      # combine old details and new details and save
      comment_details <- full_join(comment_details, comment_details2)

      save(comment_details, file = det_file)
      }

      }
  }

  # save comments on specific documents
  walk2(d$commentOnId, d$id, save_commentsOnId)

}


# test
walk(head(dockets$docket, 4), save_comments)

# apply to all
walk(dockets$docket, possibly(save_comments, otherwise = print("nope")))



##  These dockets do not return public submissions from bulk data download:

# docket <- "FDA-2015-N-0030" # DONE VIA API
# docket <- "AMS-NOP-21-0073" # DONE VIA BULK (IT DID WORK AFTER ALL)

save_comments(docket)









