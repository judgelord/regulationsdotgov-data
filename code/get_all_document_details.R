save_documents <- function(docket){

  message(docket, appendLF = F)

  # for testing
  # docket <- dockets$docket[1]

  # file path for document metadata
  doc_file <- here::here("data", "metadata",
                         str_extract(docket, "[A-Z]+"), # agency
                         docket,
                         paste(docket, "documents.rda", sep = "_"))

  det_file <- str_replace(doc_file, "documents.rda", "document_details.rda")


  if( file.exists(doc_file) ){ # if the document metadata exists, load it

    if(  !file.exists(det_file)){
      message("| getting details |")
      load(doc_file) #FIXME  update this with new docs since most recent document date

      documents %<>% distinct()

      # FIXME -- TEMPORARY uniquing comment files saved with rbind
      #save(documents, file = doc_file)

      # FIXME when get_document_details is working
      document_details <- map_dfr(documents$id,
                                  possibly(
                                    get_document_details,
                                    message(" | Error |")# , appendLF = F)
                                  )
      )
      save( document_details,
            file = str_replace(doc_file, "documents.rda", "document_details.rda"))
    }

  } else { # if document metadata does not exist, get it
    message(" | getting documents")
    documents <- get_documents(docket, api_keys = keys) |>
      distinct()

    save(documents, file = doc_file)
  }

}

# test
walk(head(dockets$docket, 4), save_documents)

# apply to all
walk(dockets |>
       filter(agency != "FCC") |>
       pull(docket),
     possibly(save_documents, otherwise = print("nope")))
