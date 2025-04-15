# load commonly used packages
 source("setup.R")

set_here(path="/Volumes/Devin's 5TB/regulationsdotgov-DATA")

metadata_root <- here::here("data", "metadata")

files_root <- here::here("data", "files")

metadata_root %<>% str_replace("/Volumes/Devin's 5TB/",  
                               "/Users/judgelor/University of Michigan Dropbox/Devin Judge-Lord/")

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

#####################
# 2. Download files #
#####################

# a document for testing
test <- 8
agency <- docs$agency[test]
docket <- docs$docket[test]
document <- docs$document[test]
metadata_path <- docs$metadata_path[test]

# HELPERS
# file hierarchy to determine which files are downloaded
file_hierarchy <- tibble(
  format = c("rtf", "htm", "txt", # plain text 
             "doc", "docx", "ppt", "pptx", # pandoc-able 
             "xlsx", "xls", "xlsm", # spreadsheets 
             "wpd", "dot",# file extension can be changed to pandoc-able (I hope) - https://mendelson.org/wpdos/wpfilesinosx.html
             "msg", "zip", #TODO unzip 
             "pdf",
             "jpg", "jpeg", "bmp", "tif", "png", "gif",
             "mp4"),
  priority = c(5, 5, 5,
               4, 4, 4, 4, 
               4, 4, 4,
               4, 4,
               3, 3,
               2,
               1, 1, 1, 1, 1,1,
               1)
)

### MAIN FUNCTION
download_attachments <- function(agency, docket, document, metadata_path){
  load(metadata_path)

  # make data frame of attachments from list
  attachments <- comment_details$attachments |>
    # replace NULL with NA (WHY DO WE HAVE NULLS IN THESE DATA? -- A Q FOR THE PACKAGE )
    map( ~ifelse(is.null(.x), NA, .x)) |>
    # drop NA (WHY DO WE HAVE NAs IN THESE DATA? -- A Q FOR THE PACKAGE )
    purrr::discard(is.na) |>
    bind_rows() |>
    unnest(cols = c(fileUrl, format, size))

  # Warn about file formats not in format hierarchy
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
           # make file_path
           file_path = here::here(files_root, agency, docket, document,
                             paste0(id, "_", number, ".", format))) |>
    group_by(id, number) |>
    left_join(file_hierarchy, by = "format")  |>
    # all unlisted file types get priority 0
    mutate(priority = replace_na(priority, 0)) |>
    slice_max(order_by = priority, n = 1)


  # For each file URL i, download to file_path i, if the file does not yet exist
  for(i in 1:length(to_download$fileUrl) ){

    if(!file.exists(to_download$file_path[i])){
      tryCatch({

    download.file(to_download$fileUrl[i],
                  destfile = to_download$file_path[i])

      }, error = function(e) {
        message(e)
      })

    } else {
      message( str_remove(to_download$fileUrl[i], ".*gov/"), " already downloaded")
      }
  }

  }
## END MAIN FUNCTION

# Index of documents from the docs data to download comments on
start = 0
stop = start + 1000 
stop = nrow(docs)

# make a list to map over
metadata_list <- list(agency = docs$agency[start:stop],
                      docket = docs$docket[start:stop],
                      document = docs$document[start:stop],
                      metadata_path = docs$metadata_path[start:stop])

## map over metadata list, downloading attachments
# WARNING THIS STARTS DOWNLOAD, WHICH IS DIFFICULT TO STOP
pwalk(metadata_list, possibly(download_attachments, otherwise = "Fail"))

# show file types missing from hierarchy
warnings()

# FOR INSPECTING
#load(metadata_list$metadata_path[1])

# FAILS
#33 - bulk docket needs converting
# 46 - 0 attachmetns
# 51 - 0 attachments

# END
####################################################################




## 
if(F){
  TO investigate 
  
  2: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
    cannot open URL 'https://downloads.regulations.gov/BIA-2019-0006-0003/attachment_1.pdf': HTTP status was '403 Forbidden'
  3: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
    URL 'http://NA/': status was 'Couldn't resolve host name'
4: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
  cannot open URL 'https://downloads.regulations.gov/BIS-2018-0002-26210/attachment_1.xlsx': HTTP status was '403 Forbidden'
5: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
  cannot open URL 'https://downloads.regulations.gov/BIS-2018-0002-26211/attachment_1.xlsx': HTTP status was '403 Forbidden'
6: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
  cannot open URL 'https://downloads.regulations.gov/BIS-2018-0002-26213/attachment_1.xlsx': HTTP status was '403 Forbidden'
7: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
  cannot open URL 'https://downloads.regulations.gov/BIS-2018-0002-26215/attachment_1.xlsx': HTTP status was '403 Forbidden'
8: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
  cannot open URL 'https://downloads.regulations.gov/BIS-2018-0002-26217/attachment_1.xlsx': HTTP status was '403 Forbidden'
9: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
  cannot open URL 'https://downloads.regulations.gov/BIS-2018-0002-26219/attachment_1.xlsx': HTTP status was '403 Forbidden'
10: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
  cannot open URL 'https://downloads.regulations.gov/BIS-2018-0006-184189/attachment_1.xlsx': HTTP status was '403 Forbidden'
11: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
  cannot open URL 'https://downloads.regulations.gov/BIS-2018-0006-184182/attachment_1.xlsx': HTTP status was '403 Forbidden'
12: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
  cannot open URL 'https://downloads.regulations.gov/BIS-2018-0006-184183/attachment_1.xlsx': HTTP status was '403 Forbidden'
13: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
  cannot open URL 'https://downloads.regulations.gov/BIS-2018-0006-184186/attachment_1.xlsx': HTTP status was '403 Forbidden'
14: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
  cannot open URL 'https://downloads.regulations.gov/BIS-2018-0006-184187/attachment_1.xlsx': HTTP status was '403 Forbidden'
15: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
  cannot open URL 'https://downloads.regulations.gov/BIS-2018-0006-184188/attachment_1.xlsx': HTTP status was '403 Forbidden'
16: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
  cannot open URL 'https://downloads.regulations.gov/BIS-2018-0006-184180/attachment_1.xlsx': HTTP status was '403 Forbidden'
17: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
  cannot open URL 'https://downloads.regulations.gov/BOEM-2020-0005-13102/attachment_1.pdf': HTTP status was '403 Forbidden'
18: Unknown or uninitialised column: `attachments`.
19: In .f(...) : File types not in file_hierarcy: mp4
20: In .f(...) : File types not in file_hierarcy: msg
21: In .f(...) : File types not in file_hierarcy: msg
22: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
  cannot open URL 'https://downloads.regulations.gov/EOIR-2020-0003-86751/attachment_1.pdf': HTTP status was '403 Forbidden'
23: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
  cannot open URL 'https://downloads.regulations.gov/EPA-HQ-OA-2013-0582-1227/attachment_1.pdf': HTTP status was '500 Internal Server Error'
24: In .f(...) : File types not in file_hierarcy:  
25: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
  URL 'https://downloads.regulations.gov/EPA-HQ-OAR-2001-0017-1233/attachment_1. ': status was 'URL using bad/illegal format or missing URL'
26: In .f(...) : File types not in file_hierarcy:  
27: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
  URL 'https://downloads.regulations.gov/EPA-HQ-OAR-2003-0062-0080/attachment_1. ': status was 'URL using bad/illegal format or missing URL'
28: In .f(...) : File types not in file_hierarcy:  
29: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
  URL 'https://downloads.regulations.gov/EPA-HQ-OAR-2003-0079-1016/attachment_1. ': status was 'URL using bad/illegal format or missing URL'
30: In .f(...) : File types not in file_hierarcy:  
31: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
  URL 'https://downloads.regulations.gov/EPA-HQ-OAR-2003-0090-0399/attachment_1. ': status was 'URL using bad/illegal format or missing URL'
32: In .f(...) : File types not in file_hierarcy:  
33: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
  URL 'https://downloads.regulations.gov/EPA-HQ-OAR-2003-0179-0258/attachment_1. ': status was 'URL using bad/illegal format or missing URL'
34: In .f(...) : File types not in file_hierarcy:  
35: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
  URL 'https://downloads.regulations.gov/EPA-HQ-OAR-2003-0197-0019/attachment_1. ': status was 'URL using bad/illegal format or missing URL'
36: In .f(...) : File types not in file_hierarcy:  
37: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
  URL 'https://downloads.regulations.gov/EPA-HQ-OAR-2004-0018-0189/attachment_1. ': status was 'URL using bad/illegal format or missing URL'
38: In .f(...) : File types not in file_hierarcy:  
39: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
  URL 'https://downloads.regulations.gov/EPA-HQ-OAR-2005-0036-0362/attachment_1. ': status was 'URL using bad/illegal format or missing URL'
40: In .f(...) : File types not in file_hierarcy:  
41: In .f(...) : File types not in file_hierarcy:  
42: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
  URL 'https://downloads.regulations.gov/EPA-HQ-OAR-2005-0175-0054/attachment_1. ': status was 'URL using bad/illegal format or missing URL'
43: In .f(...) : File types not in file_hierarcy:  
44: In .f(...) : File types not in file_hierarcy:  
45: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
  URL 'https://downloads.regulations.gov/EPA-HQ-OPP-2005-0558-0025/attachment_1. ': status was 'URL using bad/illegal format or missing URL'
46: In .f(...) : File types not in file_hierarcy: zip
47: In .f(...) : File types not in file_hierarcy:  
48: In .f(...) : File types not in file_hierarcy:  
49: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
  URL 'https://downloads.regulations.gov/EPA-HQ-RCRA-2003-0012-0123/attachment_1. ': status was 'URL using bad/illegal format or missing URL'

trying URL 'https://downloads.regulations.gov/BIA-2019-0006-0003/attachment_1.pdf'
trying URL 'NA'
trying URL 'https://downloads.regulations.gov/BIS-2018-0002-26210/attachment_1.xlsx'
trying URL 'https://downloads.regulations.gov/BIS-2018-0002-26211/attachment_1.xlsx'
trying URL 'https://downloads.regulations.gov/BIS-2018-0002-26213/attachment_1.xlsx'
trying URL 'https://downloads.regulations.gov/BIS-2018-0002-26215/attachment_1.xlsx'
trying URL 'https://downloads.regulations.gov/BIS-2018-0002-26217/attachment_1.xlsx'
trying URL 'https://downloads.regulations.gov/BIS-2018-0002-26219/attachment_1.xlsx'
trying URL 'https://downloads.regulations.gov/BIS-2018-0006-184189/attachment_1.xlsx'
trying URL 'https://downloads.regulations.gov/BIS-2018-0006-184182/attachment_1.xlsx'
trying URL 'https://downloads.regulations.gov/BIS-2018-0006-184183/attachment_1.xlsx'
trying URL 'https://downloads.regulations.gov/BIS-2018-0006-184186/attachment_1.xlsx'
trying URL 'https://downloads.regulations.gov/BIS-2018-0006-184187/attachment_1.xlsx'
trying URL 'https://downloads.regulations.gov/BIS-2018-0006-184188/attachment_1.xlsx'
trying URL 'https://downloads.regulations.gov/BIS-2018-0006-184180/attachment_1.xlsx'
trying URL 'https://downloads.regulations.gov/BOEM-2020-0005-13102/attachment_1.pdf'
trying URL 'https://downloads.regulations.gov/EOIR-2020-0003-86751/attachment_1.pdf'
trying URL 'https://downloads.regulations.gov/EPA-HQ-OAR-2001-0017-1233/attachment_1. '
trying URL 'https://downloads.regulations.gov/EPA-HQ-OAR-2003-0062-0080/attachment_1. '
trying URL 'https://downloads.regulations.gov/EPA-HQ-OAR-2003-0079-1016/attachment_1. '
trying URL 'https://downloads.regulations.gov/EPA-HQ-OAR-2003-0090-0399/attachment_1. '
trying URL 'https://downloads.regulations.gov/EPA-HQ-OAR-2003-0179-0258/attachment_1. '
trying URL 'https://downloads.regulations.gov/EPA-HQ-OAR-2003-0197-0019/attachment_1. '
trying URL 'https://downloads.regulations.gov/EPA-HQ-OAR-2004-0018-0189/attachment_1. '
trying URL 'https://downloads.regulations.gov/EPA-HQ-OAR-2005-0036-0362/attachment_1. '
trying URL 'https://downloads.regulations.gov/EPA-HQ-OAR-2005-0155-0398/attachment_2. '
trying URL 'https://downloads.regulations.gov/EPA-HQ-OAR-2005-0159-0040/attachment_1. '
trying URL 'https://downloads.regulations.gov/EPA-HQ-OAR-2005-0163-0093/attachment_1. '
trying URL 'https://downloads.regulations.gov/EPA-HQ-OAR-2005-0169-0078/attachment_1. '
trying URL 'https://downloads.regulations.gov/EPA-HQ-OAR-2005-0175-0054/attachment_1. '
trying URL 'https://downloads.regulations.gov/EPA-HQ-OPP-2005-0558-0025/attachment_1. '
trying URL 'NA'
trying URL 'https://downloads.regulations.gov/EPA-HQ-OPP-2019-0066-0486/attachment_2.jpg'
trying URL 'https://downloads.regulations.gov/EPA-HQ-OW-2011-0880-17720/attachment_1.pdf'
trying URL 'https://downloads.regulations.gov/EPA-HQ-OW-2018-0149-8092/attachment_1.jpg'
trying URL 'https://downloads.regulations.gov/EPA-HQ-RCRA-2003-0012-0123/attachment_1. '
trying URL 'https://downloads.regulations.gov/EPA-R04-OAR-2005-TN-0002-0007/attachment_1. '
trying URL 'https://downloads.regulations.gov/EPA-R05-OAR-2005-MI-0001-0003/attachment_1. '
trying URL 'https://downloads.regulations.gov/FDA-2003-N-0097-0017/attachment_1.pdf'
trying URL 'https://downloads.regulations.gov/FDA-2022-N-2390-0037/attachment_1.pdf'
trying URL 'https://downloads.regulations.gov/FWS-R8-ES-2007-0022-0041/attachment_1. '
trying URL 'https://downloads.regulations.gov/FWS-R9-ES-2012-0025-7053/attachment_1.docx'
trying URL 'https://downloads.regulations.gov/HUD-2005-0016-0016/attachment_1. '
trying URL 'https://downloads.regulations.gov/MMS-2008-OMM-0045-7527/attachment_1. '
trying URL 'https://downloads.regulations.gov/NOAA-NMFS-2009-0102-0020/attachment_1. '
trying URL 'https://downloads.regulations.gov/NOAA-NMFS-2013-0056-18402/attachment_1.pdf'
trying URL 'https://downloads.regulations.gov/NOAA-NOS-2010-0208-0077/attachment_1. '
trying URL 'https://downloads.regulations.gov/SSA-2024-0038-0025/attachment_1.pdf'
trying URL 'https://downloads.regulations.gov/USTR-2018-0025-11520/attachment_1.pdf'
trying URL 'https://downloads.regulations.gov/USTR-2018-0025-11541/attachment_1.pdf'
trying URL 'https://downloads.regulations.gov/USTR-2018-0025-11549/attachment_1.pdf'
trying URL 'https://downloads.regulations.gov/USTR-2018-0025-10997/attachment_1.pdf'
trying URL 'https://downloads.regulations.gov/USTR-2018-0025-11548/attachment_1.pdf'
There were 50 or more warnings (use warnings() to see the first 50)
> warnings()
Warning messages:
1: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
  cannot open URL 'https://downloads.regulations.gov/BIA-2019-0006-0003/attachment_1.pdf': HTTP status was '403 Forbidden'
2: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
  URL 'http://NA/': status was 'Couldn't resolve host name'
  3: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
    cannot open URL 'https://downloads.regulations.gov/BIS-2018-0002-26210/attachment_1.xlsx': HTTP status was '403 Forbidden'
  4: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
    cannot open URL 'https://downloads.regulations.gov/BIS-2018-0002-26211/attachment_1.xlsx': HTTP status was '403 Forbidden'
  5: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
    cannot open URL 'https://downloads.regulations.gov/BIS-2018-0002-26213/attachment_1.xlsx': HTTP status was '403 Forbidden'
  6: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
    cannot open URL 'https://downloads.regulations.gov/BIS-2018-0002-26215/attachment_1.xlsx': HTTP status was '403 Forbidden'
  7: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
    cannot open URL 'https://downloads.regulations.gov/BIS-2018-0002-26217/attachment_1.xlsx': HTTP status was '403 Forbidden'
  8: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
    cannot open URL 'https://downloads.regulations.gov/BIS-2018-0002-26219/attachment_1.xlsx': HTTP status was '403 Forbidden'
  9: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
    cannot open URL 'https://downloads.regulations.gov/BIS-2018-0006-184189/attachment_1.xlsx': HTTP status was '403 Forbidden'
  10: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
    cannot open URL 'https://downloads.regulations.gov/BIS-2018-0006-184182/attachment_1.xlsx': HTTP status was '403 Forbidden'
  11: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
    cannot open URL 'https://downloads.regulations.gov/BIS-2018-0006-184183/attachment_1.xlsx': HTTP status was '403 Forbidden'
  12: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
    cannot open URL 'https://downloads.regulations.gov/BIS-2018-0006-184186/attachment_1.xlsx': HTTP status was '403 Forbidden'
  13: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
    cannot open URL 'https://downloads.regulations.gov/BIS-2018-0006-184187/attachment_1.xlsx': HTTP status was '403 Forbidden'
  14: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
    cannot open URL 'https://downloads.regulations.gov/BIS-2018-0006-184188/attachment_1.xlsx': HTTP status was '403 Forbidden'
  15: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
    cannot open URL 'https://downloads.regulations.gov/BIS-2018-0006-184180/attachment_1.xlsx': HTTP status was '403 Forbidden'
  16: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
    cannot open URL 'https://downloads.regulations.gov/BOEM-2020-0005-13102/attachment_1.pdf': HTTP status was '403 Forbidden'
  17: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
    cannot open URL 'https://downloads.regulations.gov/EOIR-2020-0003-86751/attachment_1.pdf': HTTP status was '403 Forbidden'
  18: In .f(...) : File types not in file_hierarcy:  
    19: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
    URL 'https://downloads.regulations.gov/EPA-HQ-OAR-2001-0017-1233/attachment_1. ': status was 'URL using bad/illegal format or missing URL'
  20: In .f(...) : File types not in file_hierarcy:  
    21: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
    URL 'https://downloads.regulations.gov/EPA-HQ-OAR-2003-0062-0080/attachment_1. ': status was 'URL using bad/illegal format or missing URL'
  22: In .f(...) : File types not in file_hierarcy:  
    23: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
    URL 'https://downloads.regulations.gov/EPA-HQ-OAR-2003-0079-1016/attachment_1. ': status was 'URL using bad/illegal format or missing URL'
  24: In .f(...) : File types not in file_hierarcy:  
    25: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
    URL 'https://downloads.regulations.gov/EPA-HQ-OAR-2003-0090-0399/attachment_1. ': status was 'URL using bad/illegal format or missing URL'
  26: In .f(...) : File types not in file_hierarcy:  
    27: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
    URL 'https://downloads.regulations.gov/EPA-HQ-OAR-2003-0179-0258/attachment_1. ': status was 'URL using bad/illegal format or missing URL'
  28: In .f(...) : File types not in file_hierarcy:  
    29: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
    URL 'https://downloads.regulations.gov/EPA-HQ-OAR-2003-0197-0019/attachment_1. ': status was 'URL using bad/illegal format or missing URL'
  30: In .f(...) : File types not in file_hierarcy:  
    31: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
    URL 'https://downloads.regulations.gov/EPA-HQ-OAR-2004-0018-0189/attachment_1. ': status was 'URL using bad/illegal format or missing URL'
  32: In .f(...) : File types not in file_hierarcy:  
    33: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
    URL 'https://downloads.regulations.gov/EPA-HQ-OAR-2005-0036-0362/attachment_1. ': status was 'URL using bad/illegal format or missing URL'
  34: In .f(...) : File types not in file_hierarcy:  
    35: In .f(...) : File types not in file_hierarcy:  
    36: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
    URL 'https://downloads.regulations.gov/EPA-HQ-OAR-2005-0155-0398/attachment_2. ': status was 'URL using bad/illegal format or missing URL'
  37: In .f(...) : File types not in file_hierarcy:  
    38: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
    URL 'https://downloads.regulations.gov/EPA-HQ-OAR-2005-0159-0040/attachment_1. ': status was 'URL using bad/illegal format or missing URL'
  39: In .f(...) : File types not in file_hierarcy:  
    40: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
    URL 'https://downloads.regulations.gov/EPA-HQ-OAR-2005-0163-0093/attachment_1. ': status was 'URL using bad/illegal format or missing URL'
  41: In .f(...) : File types not in file_hierarcy:  
    42: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
    URL 'https://downloads.regulations.gov/EPA-HQ-OAR-2005-0169-0078/attachment_1. ': status was 'URL using bad/illegal format or missing URL'
  43: In .f(...) : File types not in file_hierarcy:  
    44: In .f(...) : File types not in file_hierarcy:  
    45: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
    URL 'https://downloads.regulations.gov/EPA-HQ-OAR-2005-0175-0054/attachment_1. ': status was 'URL using bad/illegal format or missing URL'
  46: In .f(...) : File types not in file_hierarcy:  
    47: In .f(...) : File types not in file_hierarcy:  
    48: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
    URL 'https://downloads.regulations.gov/EPA-HQ-OPP-2005-0558-0025/attachment_1. ': status was 'URL using bad/illegal format or missing URL'
  49: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
    URL 'http://NA/': status was 'Couldn't resolve host name'
50: In download.file(to_download$fileUrl[i], destfile = to_download$file_path[i]) :
  cannot open URL 'https://downloads.regulations.gov/EPA-HQ-OPP-2019-0066-0486/attachment_2.jpg': HTTP status was '403 Forbidden'
  
  }

