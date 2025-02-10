
source("setup.R")



  searchTerm =  c("national congress of american indians", "cherokee nation", "climate justice", "environmental justice")
  searchTerm = c("racial", "latino")
  searchTerm = c("racial")

  searchTerm = c(
    "Critical Race Theory",
  #"DACA",
  "Diversity, equity",
  "Illegal Alien",
  #"MENA"
  "Mexican Cartel" ,
  "Muslim",
  #"Racism",
  #"Racist",
  "Slavery",
  "Unaccompanied Alien Childen",
  "White Privilege",
  "Affirmative Action",
  #"African American",
  "Black american",
  "Black woman",
  "Black men",
  "Arab American",
  "Border Crisis",
  #"Drug Cartel" ,
  "Ethnicity",
  "Hispanic",
  "Meritocracy",
  "Native American",
  "Secure the border",
  "Secure Border",
  "Terrorist",
  "Undocumented",
  "Asian American",
  "Citizenship",
  "Civil Rights",
  "Colorblindness",
  "Gang",
  "Immigrant",
  "Immigration")

  documents = c("documents", "comments")


  search_to_rda <- function(searchTerm, documents){

    directory <- here::here("data", "search", searchTerm)
    dir.create(directory)
    file <- here::here(directory, paste0(searchTerm, "_", documents, ".rda"))

    if( !file.exists(file) ){

      d <- get_searchTerm(searchTerm, documents, api_keys = keys)

      save(d, file = file)
    }
  }

# map over search terms for each document type
  for(documents in documents){
    purrr::walk(searchTerm, documents, .f = search_to_rda)
  }


