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
library(here)

# keys saved up one directory from this project folder
load("../keys.rda")


# AGENCIES MISSING FROM ORIGINAL SCRAPE (BECAUSE THEY DON'T USE DOCKETS)
# THAT I KNOW HAVE DOCUMENTS ON REGULATIONS.GOV
# (see minimal counts of documents in data/missing.rda)
if(F){
agencies <- c('FCC', 'SEC', 'FRS', 'FDIC', 'CFTC', 'FHFA', 'FERC', 'FCA', 'GEO', 'FHFB', 'LSC', 'STB', 'FEC', 'MSPB', 'ARTS', 'USPS', 'CRB', 'CRC', 'OFHEO',
              'ODNI', 'RISC', 'PRC', 'PC', 'SSS', 'PHS', 'NCPC', 'ABMC', 'CIA', 'FLRA', 'PT', 'DRBC', 'RRB', 'SIGAR', 'OSTP', 'GCERC', 'HOPE', 'NWTRB', 'OPIC',
              'OSHRC', 'RATB', 'TVA', 'ADF', 'CSOSA', 'SS', 'NSA', 'EOP', 'FPPO', 'FCSIC', 'FMCS', 'NA', 'GAO', 'IIO', 'ITC', 'JBEA', 'MCC', 'MSHFRC', 'NCMNPS',
              'NMB', 'SRBC', 'CDFIF')

# create directories for each agency in docket metadata
walk(here::here("data", "metadata",  agencies), dir.create)
}

# Re-define agencies as all agency folders
agencies <- list.dirs(here("data", "metadata"),
                      recursive = F,
                      full.names = F)


######################################################
# COLLECT DOCKET METADATA WE DON'T HAVE FOR AGENCIES #
######################################################

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

# loop over all agencies
for (agency in agencies){

  file <- here::here("data", "metadata",
                     agency,
                     paste0(agency, "_dockets.rda"))

  if (!file.exists(file)) {

    tryCatch({
      save_dockets(agency)
      message(agency,"_dockets.rda file created.")
    }, error = function(e) {
      message("Unable to collect ", agency,"_dockets.rda.")
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
      message("An error occured ", e$message)
    })
  }
}



#######################################
# create directories for each docket #
######################################

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






#######################################################
         #####################################
         # DOCUMENTS AND COMMENTS ON DOCKETS #
         #####################################
#######################################################
#### Get metadata for comments on a document or docket
# (Along the way, get any missing documents)
# for testing
# docket <- dockets_df$docket[1]
# the dockets we already have
#FIXME document folders and getting recorded as docket forlders here, causing incorrect and unnecessary calls
dockets <- list.dirs("data/metadata")

dockets_df <- tibble(
  agency = str_extract(dockets, ".*/") |> str_remove("/$") |> str_remove(".*/"),
  docket = str_remove(dockets, ".*/")
)

# dockets_df$agency <- str_remove(dockets_df$docket, "-.*")

# inspect agencies with dashes in their name
dockets_df |>
  filter(str_detect(agency, "-|_")) |>
  distinct(agency)

# drop paths to agencies or documents rather than documents
dockets_df <- dockets_df |>
  filter(str_detect(docket, "-"),
         !str_detect(agency, "-|_")) #FIXME some agencies may have dashes in their names?

# look at longest paths
dockets_df |>
  #filter(agency == "VA") |>
  arrange(-nchar(docket))

# look at shortest paths
dockets_df |>
  #filter(agency == "VA") |>
  arrange(nchar(docket))

# THESE INITIALLY FALSELY RETURNED 0 DOCUMENTS, not sure why
d2 <- dockets_df |>
  filter(docket %in%
           c("EPA-HQ-OAR-2002-0037", "EPA-HQ-OAR-2018-0167", "FWS-R4-ES-2012-0103" ))


# PICK AN ORDER
dockets_df %<>% ungroup()
dockets_df %<>% arrange(rev(docket))
# dockets_df %<>% arrange(docket)



########################
# FUNCTION            #
########################
max_number <- 1000000

save_everything <- function(docket){

  message(" | ", docket, appendLF = F)
  write(docket, file = here::here("save_everything stopped at"))

  # file path for document metadata
  docket_dir <- here::here("data", "metadata",
                         str_extract(docket, "[A-Z]+"), # agency
                         docket)
  if(!dir.exists(docket_dir)){
    dir.create(docket_dir)
  }

  # file path for document metadata
  doc_file <- here::here("data", "metadata",
                         str_extract(docket, "[A-Z]+"), # agency
                         docket,
                         paste(docket, "documents.rda", sep = "_"))

  doc_det_file = str_replace(doc_file, "documents.rda", "document_details.rda")


  if( file.exists(doc_file) ){ # if the document metadata exists, load it

    load(doc_file) #FIXME  update this with new docs since most recent document date

    # if missing valid document data, try again
    if(!"id" %in% names(documents) ) {

      message("| empty documents file, trying again | ")

      documents <- get_documents(docket, api_keys = keys) |> distinct()

      save(documents, file = doc_file)
    }

    documents %<>% distinct()

    # if there documents but document details file does not exist, get detials
    if( "id" %in% names(documents) & !file.exists(doc_det_file) ){

      message(" | getting document details ")

    document_details <- map_dfr(documents$id,
                                possibly(
                                  get_document_details#,
                                  # FIXME: This message was printing every time even when there was no error
                                  #otherwise =  print(" | get_document_details Error |")# , appendLF = F)
                                  )
                                )
    save( document_details,
          file = str_replace(doc_file, "documents.rda", "document_details.rda"))
    }

  } else { # if document metadata does not exist, get it
    message(" | getting documents ")
    documents <- get_documents(docket, api_keys = keys) |>
      distinct()

    save(documents, file = doc_file)
  }

  if("objectId" %in% names(documents)){
  # subset to documents that were open for comment comments
  d <- documents |>
    #filter(documentType %in% c("Proposed Rule", "Notice")) |> # FIXME, consider comments on other docs for next pass
    # rename vars to avoid conflicts with comment data and be clear about what is a document id vs a comment id
    rename(document_subtype = subtype,
           commentOnId = objectId,
           document_title = title) |>
    #FIXME not sure if a 0 comment document was the source of the error I got
    drop_na(commentStartDate)

  message("      | ", nrow(d), " document(s) open for comment | ")

  # HELPER FUNCTION TO GET AND SAVE COMMENTS AND COMMENT DETAILS
  save_commentsOnId <- function(objectId, document, openForComment){

    if(openForComment){
    message(document, "was open for comment ", appendLF = F)
    }

    # FOR TESTING
    # objectId <- d$commentOnId[1]
    # document <- d$id[1]

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
    if(
      !file.exists( det_file ) #| openForComment
       ){

    ## If we also need comment metadata, first get it
    if(
      !file.exists(comments_on_document_file) #| openForComment
        ){

      message(" | ", document, " | ", appendLF = F)

      comments <- get_commentsOnId(objectId, api_keys = keys)

      save(comments,
           file = comments_on_document_file)

    } else { # if comment metadata file already exists, load it
      message(" | ", document, " | Loading comment metadata | ", appendLF = F)

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

      message(nrow(comments) - 1, " comments |")

      # if there are more than 100k comments, skip it (we will get these from bulk data)
    if(  nrow(comments) > max_number ){
      save(det_file, file = det_file |> str_replace("details", "MoreThan100k"))
    }

    # otherwise, GET COMMENT_DETAILS
    if( !file.exists(det_file) & nrow(comments) <= max_number & "id" %in% colnames(comments)){

      message("|   Getting comment details | ")

      comment_details <- get_comment_details(comments$id, api_keys = keys) |>
        distinct()

      save(comment_details, file = det_file)
    }

    } else { # if det_file exists
      message(" | ", document, " |  Loading comment details | ", appendLF = F)

      # load comments metadata
      load(comments_on_document_file)

      # load comment details
      load(det_file)

      # subset comment metadata to comments missing from comment_details
      comments_missing <- filter(comments, !id %in% comment_details$id)

      message(nrow(comments), " comments | ", nrow(comments_missing), " missing |")

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
  pwalk(
    list(
      d$commentOnId,
      d$id,
      d$openForComment
      ),
    .f = save_commentsOnId)

  # # if any documents were open for comment, recollect documents
  # if(sum(d$openForComment) > 0){
  #   message(" | getting documents ")
  #   documents2 <- get_documents(docket, api_keys = keys) |>
  #     distinct()
  #
  #   documents <- full_join(documents, documents2)
  #
  #   save(documents, file = doc_file)
  # }

} else( message("| No documents for ", docket, " |") )
}


# test
walk(head(dockets_df$docket, 4), save_everything)

# apply to all
walk(dockets_df |>
       #filter(agency != "FCC") |>
       pull(docket),
     possibly(save_everything, otherwise = print("nope")))



##  These dockets do not return public submissions from bulk data download:

# docket <- "FDA-2015-N-0030" # DONE VIA API
# docket <- "AMS-NOP-21-0073" # DONE VIA BULK (IT DID WORK AFTER ALL)

# save_comments(docket)

## MISSING COMMENTS TO INVESTEGATE

EPA-HQ-OAR-2002-0037
EPA-HQ-OAR-2018-0167
FWS-R4-ES-2012-0103











