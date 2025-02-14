# regulationsdotgov-data

This repository contains documentation and code for constructing a directory of metadata and files from regulations.gov using the [`regulationsdotgov`](https://github.com/judgelord/regulationsdotgov) package. 

- metadata
   - [agency]/
      - [agency]_dockets.rda (metadata returned by `get_dockets`)
      - [docket]/
         - [docket]_documents.rda (metadata returned by `get_documents`)
         - [document]/
            - [document]_comments.rda (metadata returned by `get_commentsOnId`)
            - [document]_comment_details.rda (metadata returned by `get_comment_details`)
- search (metadata returned by `get_searchTerm` )
   - [searchTerm]/
      - [searchTerm]documents.rda
      - [searchTerm]comments.rda
      - [searchTerm]comment_details.rda
- files 
   - [agency]/
      - [docket] /
         - [document].pdf
         - [document]/
            - [comment_id1].pdf  
            - [comment_id1].doc (or whatever formats exist) 
- htm  (a mirror of files if we can download files in htm --- I'm not sure this is possible, but we know they have OCRd everything) 
- txt (a mirror of 'files')
