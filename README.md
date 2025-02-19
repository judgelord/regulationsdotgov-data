# regulationsdotgov-data

This repository contains documentation and code for constructing a directory of metadata and files from regulations.gov using the [`regulationsdotgov`](https://github.com/judgelord/regulationsdotgov) package. 

- metadata
   - [agency]/
      - [agency]_dockets.rda (metadata returned by `get_dockets`)
      - [docket]/
         - [docket]_documents.rda (metadata returned by `get_documents`)
         - [document]/
            - [document]_details.rda (metadata returned by `get_document_details`)
            - [document]_comments.rda (metadata returned by `get_commentsOnId`)
            - [document]_comment_details.rda (metadata returned by `get_comment_details`)
- search 
   - [searchTerm]/
      - [searchTerm]_documents.rda (metadata returned by `get_searchTerm` )
      - [searchTerm]_document_details.rda (corresponding metadata returned by `get_document_details`)
      - [searchTerm]_comments.rda (metadata returned by `get_searchTerm` )
      - [searchTerm]_comment_details.rda (corresponding metadata returned by `get_comment_details`)
- files 
   - [agency]/
      - [docket] /
         - [document].pdf
         - [document].htm (see #7)
         - [document]/
            - [comment_id1_1].pdf  (or whatever formats exist for attachment 1 --- most comments have zero attachments, few have more than one)
            - [comment_id1_2].doc 
            - [comment_id2_1].jpg  
            - [comment_id2_2].jpg
- htm  (a mirror of files if we can download files in htm --- I'm not sure this is possible, but we know they have OCRd everything) 
- txt (a mirror of 'files' with readable files converted to txt)
