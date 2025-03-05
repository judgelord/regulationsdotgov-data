# load commonly used packages
source("setup.R")

# the comments we already have metadata for
doc_list <- list.files(path = here::here("data", "metadata"), pattern = "_comment_details.rda", recursive = T, include.dirs = T)

# a data frame of parts of the document path
docs <- tibble(
  metadata_path = paste0("data/metadata/", doc_list),
  file_dir = paste0("data/files/", doc_list) |>  str_extract(".*/") |> str_remove("/$"),
  agency = str_remove(doc_list, "/.*"),
  docket = str_remove(doc_list, ".*?/") |> str_remove("/.*"),
  document = str_remove(doc_list, ".*/") |> str_remove("_.*"),
  details = str_remove(doc_list, ".*/")
) |>
  # drop aggregated details
  filter(docket != document)

head(docs) #|> transmute(docket == document)

dir.create(here::here("data", "files", docs$agency[1]))

for(i in docs$agency |> unique() ) {
  dir.create(here::here("data", "files", i))
}

create_docket_folder <- function(agency, docket){
  dir.create(here::here("data", "files", agency, docket))
}

walk2(docs$agency, docs$docket, create_docket_folder)

create_doc_folder <- function(agency, docket, document){
  dir.create(here::here("data", "files", agency, docket, document))
}

pwalk(list(docs$agency, docs$docket, docs$document), .f = create_doc_folder)
############# END CREATE DIRECTORIES ###################

# for testing
agency <- docs$agency[8]
docket <- docs$docket[8]
document <- docs$document[8]
metadata_path <- docs$metadata_path[8]

# HELPERS
# file hierarchy to determine which files are downloaded
file_hierarchy <- tibble(
  format = c("rtf", "htm", "txt",
             "doc", "docx", "ppt", "pptx", "xlsx", "xls",
             "pdf",
             "jpg", "jpeg", "bmp", "tif", "png"),
  priority = c(5, 5, 5,
               4, 4, 4, 4, 4, 4,
               2,
               1, 1, 1, 1, 1)
)

### MAIN FUNCTION
# FIXME this does not yet prioritize document types or filter out large files -- it downloads all attachments
download_attachments <- function(agency, docket, document, metadata_path){
  load(metadata_path)

  attachments <- comment_details$attachments |>
    # replace NULL with NA (WHY DO WE HAVE NULLS IN THESE DATA? -- A Q FOR THE PACKAGE )
    map( ~ifelse(is.null(.x), NA, .x)) |>
    # drop NA (WHY DO WE HAVE NAs IN THESE DATA? -- A Q FOR THE PACKAGE )
    purrr::discard(is.na) |>
    bind_rows() |>
    unnest(cols = c(fileUrl, format, size))

  file_types <- unique(attachments$format)
  new_file_types <- file_types[which(!file_types %in% file_hierarchy$format)]

  if( length(new_file_types) >0 ){
    warning("File types not in file_hierarcy: ", new_file_types )
  }

  to_download <- attachments |>
    # subset to comments with attachments
    drop_na(fileUrl) |>
    # files less than 100 MB
    filter(size < 100000000) |>
    # make id and number
    mutate(number = fileUrl |> str_extract("_[0-9]*") |> str_remove("_"),
           id = fileUrl |> str_remove(".*gov/") |> str_remove("/attachment.*"),
           # make path
           path = here::here("data", "files", agency, docket, document,
                             paste0(id, "_", number, ".", format))) |>
    group_by(id, number) |>
    left_join(file_hierarchy, by = "format")  |>
    # all unlisted file types get priority 0
    mutate(priority = replace_na(priority, 0)) |>
    slice_max(order_by = priority, n = 1)


  # For each file url, download to path
  for(i in 1:length(to_download$fileUrl) ){

    if(!file.exists(to_download$path[i])){
      tryCatch({

    download.file(to_download$fileUrl[i],
                  destfile = to_download$path[i])

      }, error = function(e) {
        message(e)
      })

    } else {
      message( str_remove(to_download$fileUrl[i], ".*gov/"), " already downloaded")
      }
  }

  }

start = 0
stop = 700
# make a list to map over
metadata_list <- list(agency = docs$agency[start:stop],
                      docket = docs$docket[start:stop],
                      document = docs$document[start:stop],
                      metadata_path = docs$metadata_path[start:stop])

# WARNING THIS DOWNLOADS ALL FILES
pwalk(metadata_list, possibly(download_attachments, otherwise = "Fail"))

# show file types missing from hierarchy
warnings()

# FOR INSPECTING
load(metadata_list$metadata_path[51])

# FAILS
#33 - bulk docket needs converting
# 46 - 0 attachmetns
# 51 - 0 attachments

# END
####################################################################





