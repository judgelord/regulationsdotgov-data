# Compared to get_everything.R, this script is designed to get everything from agencies that don't have dockets
# (i.e. when we get documents, docket_id is NA because they don't or use the docket structure on regulation.gov )

# Right now, this script starts with [agency]_documents.rda files and
# 1. makes a docket_id from document id
# 2. makes "docket" folders in order to create a parallel structure
# 3. puts document metadata in those folders (I believe these will always just have one document per folder, unlike agencies that do use dockets)
# it then gets gets the following metadata:
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

agencies <- list.dirs("data/metadata", recursive = F) |> str_remove(".*/")

# PICK AN ORDER
agencies %<>% rev(agencies)

# problem_documents <- tribble(
#   ~docket, ~document, ~objectId,
#   "APHIS-2022-0003", "APHIS-2022-0003-0001", "0900006484e23ccb",
#   "HHS-OS-2011-0023", "HHS-OS-2011-0023-0002", "0900006480ed4b59"
#   ## FIXME Commenting out for now because these no longer seem to be problems
# #   "WHD-2017-0002", "WHD-2017-0002-0001",  "0900006482969fbd", # expected 140-214k
# #   "WHD-2017-0003", "WHD-2017-0003-0001", "0900006482cdbe54", # expected 218-376k https://www.regulations.gov/docket/WHD-2017-0003
# #   "WHD-2019-0001","WHD-2019-0001-0001", "0900006483b173dc",
# #   "PHMSA-2021-0039", "PHMSA-2021-0039-2101", "0900006485c21b4f",
# #   "PHMSA-2021-0058", "PHMSA-2021-0058-0002", "0900006484e3545a",
# #   "NHTSA-2022-0079", "NHTSA-2022-0079-0015", "090000648637d7c0" # expecting 18,317 - https://www.regulations.gov/document/NHTSA-2022-0079-0015
#   )
# #
#
# dockets %<>%
#   filter(!docket %in% problem_documents$docket)



# or select a new agency
agency <- "FCC"

# AGENCIES MISSING DATA THAT I KNOW IS ON REGULATIONS.GOV
# (see minimal counts of documents in data/missing.rda)
# do we have them?
c('FCC', 'SEC', 'FRS', 'FDIC', 'CFTC', 'FHFA', 'FERC', 'FCA', 'GEO', 'FHFB', 'LSC', 'STB', 'FEC', 'MSPB', 'ARTS', 'USPS', 'CRB', 'CRC', 'OFHEO',
              'ODNI', 'RISC', 'PRC', 'PC', 'SSS', 'PHS', 'NCPC', 'ABMC', 'CIA', 'FLRA', 'PT', 'DRBC', 'RRB', 'SIGAR', 'OSTP', 'GCERC', 'HOPE', 'NWTRB', 'OPIC',
              'OSHRC', 'RATB', 'TVA', 'ADF', 'CSOSA', 'SS', 'NSA', 'EOP', 'FPPO', 'FCSIC', 'FMCS', 'NA', 'GAO', 'IIO', 'ITC', 'JBEA', 'MCC', 'MSHFRC', 'NCMNPS',
              'NMB', 'SRBC', 'CDFIF') %in% agencies

agencies <- c('FCC', 'SEC', 'FRS', 'FDIC', 'CFTC', 'FHFA', 'FERC', 'FCA', 'GEO', 'FHFB', 'LSC', 'STB', 'FEC', 'MSPB', 'ARTS', 'USPS', 'CRB', 'CRC', 'OFHEO',
  'ODNI', 'RISC', 'PRC', 'PC', 'SSS', 'PHS', 'NCPC', 'ABMC', 'CIA', 'FLRA', 'PT', 'DRBC', 'RRB', 'SIGAR', 'OSTP', 'GCERC', 'HOPE', 'NWTRB', 'OPIC',
  'OSHRC', 'RATB', 'TVA', 'ADF', 'CSOSA', 'SS', 'NSA', 'EOP', 'FPPO', 'FCSIC', 'FMCS', 'NA', 'GAO', 'IIO', 'ITC', 'JBEA', 'MCC', 'MSHFRC', 'NCMNPS',
  'NMB', 'SRBC', 'CDFIF')


# create directories for each docket
#FIXME warnings + error message need to be worked on
for (agency in agencies) {
  message(agency)

  tryCatch({

    suppressWarnings({

      # load aggregated data
    load(here::here("data", "metadata", agency, paste0(agency, "_documents.rda")))
    })

    if (!"id" %in% colnames(documents)) {
      message("No 'id' column found in ", agency, "_documents.rda.")
      next
    }

    if (!"docketId" %in% colnames(documents)) {
      documents %<>%
        mutate(docketId = str_remove(id, "-[0-9]*$"))
    }

    documents %<>%
      mutate(docketId = coalesce(docketId, str_remove(id, "-[0-9]*$")))

    dir_paths <- here::here("data", "metadata", agency, documents$docketId, documents$id)
    new_dirs <- !sapply(dir_paths, dir.exists)

    if (any(new_dirs)) {

    for(dir_path in dir_paths){
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
      message("Created directory: ", dir_path)
    } else {
      message("Directory already exists: ", dir_path)
    }
    }

    }

    # temp object to avoid overwriting
    d <- documents

    for(docket in d$docketId){
      # docket documents.rda path
      file = here::here("data", "metadata", agency, docket, paste0(docket, "_documents.rda") )

      if(file.exists(file)){
        message("| Loading and merging | ", docket, "_documents.rda |")
        load(file)

        suppressMessages(
        documents <- full_join(
          documents,
          d |>  filter(docketId == docket)
        ) )

        save(documents,
             file = file )
      } else{
        message("| Creating | ", docket, "_documents.rda |")
      documents <- d |>  filter(docketId == docket)
      save(documents,
           file = file )
      }
    }

  }, error = function(e) {
    message("No documents.rda file found for ", agency)
  })
}


