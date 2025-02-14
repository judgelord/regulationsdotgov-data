
source("setup.R")



  searchTerm =  c("national congress of american indians", "cherokee nation", "climate justice", "environmental justice")
  searchTerm = c("racial", "latino")
  searchTerm = c("racial")
  searchTerm = "climate change"

  # banned words
  searchTerm = c(
    # https://www.nytimes.com/2025/02/11/us/politics/trump-wordplay.html?smid=nytcore-ios-share&referringSource=articleShare
    "gender ideology",
    "undocumented",
    "alien",
    "Equity",
    "Gender",
    "Transgender",
    "Nonbinary",
    "Pregnant people",
    "Assigned male at birth",
    "Antiracist",
    "Trauma",
    "Hate speech",
    "Intersectional",
    "Multicultural",
    "Oppression",
    "D.E.I.",
    #https://www.washingtonpost.com/science/2025/02/04/national-science-foundation-trump-executive-orders-words/
    "gender",
                 "gender identity",
                 "transgender",
                 "pregnant person",
                 "pregnant people",
                 "LGBT",
                 "transsexual",
                 "nonbinary",
                 "assigned male at birth",
                 "biologically male",
                 "biologically female",
                 "he/she/they/them",
                 "diversity",
                 "equity",
                 "inclusion",
                 "accessibility",
                 "Women",
                 "Diverse",
                 "Institutional",
                 "Historically",
                 # NOAA - https://www.nytimes.com/2025/02/10/climate/noaa-trump-executive-orders.html
                 "climate science",
                 "climate crisis",
                 "clean energy",
                 "pollution",
                 "environmental quality"
                 )

  searchTerm = c(
    "Critical Race Theory",
  "DACA",
  "Diversity, equity",
  "Illegal Alien",
  "MENA",
  "Mexican Cartel" ,
  "Muslim",
  "Racism",
  "Racist",
  "Slavery",
  "Unaccompanied Alien Childen",
  "White Privilege",
  "Affirmative Action",
  "African American",
  "Black american",
  "Black woman",
  "Black men",
  "Arab American",
  "Border Crisis",
  "Drug Cartel" ,
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
  # documents = "comments"


  search_to_rda <- function(searchTerm, documents, lastModifiedDate = Sys.Date() ){

    directory <- here::here("data", "search", searchTerm)
    dir.create(directory)
    file <- here::here(directory, paste0(searchTerm, "_", documents, ".rda"))

    if( !file.exists(file) ){

      # TESTING date modification
      # lastModifiedDate = "2025-02-02T04:59:59Z"
      # / testing

      d <- get_searchTerm(searchTerm,
                          lastModifiedDate = lastModifiedDate,
                          documents,
                          api_keys = keys)

      message(min(d$postedDate))

      comments <- d

      save(comments, file = file)
    }
  }

# map over search terms for each document type
  for(documents in documents){
    purrr::walk(searchTerm,
                documents,
                #lastModifiedDate = "2025-02-02T04:59:59Z",
                .f = search_to_rda)
  }


