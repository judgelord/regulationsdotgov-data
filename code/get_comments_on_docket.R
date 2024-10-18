# install regulationsdotgov
# devtools::install_github("https://github.com/judgelord/regulationsdotgov")

library(regulationsdotgov)

# other packages
library(tidyverse)
library(httr)
library(jsonlite)
library(magrittr)
library(devtools)

# keys saved up one directory
load("../keys.rda")

agency <- "IRS"
docket <- dockets <- "IRS-2022-0020"








#######################################################
#### Get metadata for comments on a document or docket

# get_comments_on_docket("[docket_id]") # retrieves all comments for a docket (e.g., including an Advanced Notice of Proposed Rulemaking and all draft proposed rules)

save_comments <- function(docket){
  #comments <- map_dfr(docket, get_comments_on_docket)

  # for testing
  # docket <- "EPA-HQ-OAR-2021-0317"
  documents <- get_documents(docket)

  # documents |> count(documentType)

  documents |> filter(documentType == "Proposed Rule") |> select(commentStartDate, commentEndDate, withdrawn, subtype, objectId)

  # subset to proposed rules
  d <- documents |>
    filter(documentType == "Proposed Rule") |>
    select(document_subtype = subtype,
           commentStartDate, commentEndDate, frDocNum,
           commentOnId = objectId,
           document_title = title) |>
    #FIXME not sure if a 0 comment document was the source of the error I got
    drop_na(commentStartDate)


  # get comments
  c <- map_dfr( d$commentOnId, get_commentsOnId, api_keys = keys)

  # join back in document metadata
  c %<>% left_join(d)

  comments <- c  #|> filter(is.na(subtype )) # NA subtype = normal PR?

  save(comments, file = here::here("data",
                             str_extract(docket, "[A-Z]+"), # agency
                             docket,
                            paste(docket, "comments.rda", sep = "_")
  )
  )
}

walk(dockets, save_comments)


#### Get detailed metadata about a comment
# get_comment_details("[comment_id]") # retrieves comments on a specific document (e.g., a specific proposed rule)


save_comment_details <- function(comments){
  #comments <- map_dfr(docket, get_comments_on_docket)
  comment_details <- get_comment_details(comments$id, api_keys = keys)

  save(comment_details, file = here::here("data",
                            str_extract(docket, "[A-Z]+"), # agency
                            docket,
                            paste(docket, "comment_details.rda", sep = "_")
  )
  )
}

walk(dockets, save_comment_details)


# THIS IS GOOD ENOUGH FOR NOW
# In regulations.gov, the make_comments_codeing_sheet.R file pulls in a comments.rda and a comment_details.rda file for each docket














# DOWNLOAD ATTACHMENTS

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

#TODO? drop large pdf files?
# attachments %<>% filter(size < 100000000) # = < 100 MB ?

attachment_urls <- attachments |>
  pull(fileUrl) |>
  unique()


download_comments(attachment_urls )




# CONVERT TO TXT
files <- list.files("comments", recursive = T)
length(files)
head(files)
converted <- list.files("comment_text", recursive = T) |> str_replace("txt", "pdf")
head(converted)
# files not converted
not_converted <- files[!files %in% converted]
head(not_converted)

# pdfs not converted
pdfs <- not_converted[str_detect(not_converted, "pdf")]

walk(pdfs, possibly(pdf_to_txt, otherwise = print("nope")))
