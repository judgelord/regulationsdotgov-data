## Script for checking data completeness


# Pull metadata and update our record of docket counts

pull_metadata <- function(api_keys){

  api_key <- sample(api_keys,1)

  n <- httr::GET(paste0("https://api.regulations.gov",
                        "/v4/",
                        "dockets",
                        "?",
                        "filter[agencyId]=OMB", "&",
                        "api_key=", api_key))

  content <- jsonlite::fromJSON(rawToChar(n$content))

  metadata <- content$meta$aggregations$agencyId

  metadata$date <- Sys.time()

  metadata <- metadata %>%
    rename(!!paste0("docCount_", str_extract(metadata$date[1], pattern = "\\d{4}-\\d{2}-\\d{2}")) := docCount,
           agencies = value) %>%
    select(-date)

  load(here::here("data", "metadata", "metadata_record.rda"))

  metadata_record <- full_join(metadata_record, metadata, by = "agencies")

  save(metadata_record, file = "~/University of Michigan Dropbox/Maya Khuzam/regulationsdotgov-data/data/metadata/metadata_record.rda")
}

# Running the function will create a new column for today's date with a docCount
#pull_metadata(api_keys = keys)


load(here::here("data", "metadata","metadata_record.rda"))



data.complete <- data.frame(agency = character(), docCount = numeric(), ndockets = numeric(), complete = logical(), stringsAsFactors = FALSE)

# Loop through each agency
for (i in seq_along(metadata_record$agencies)) {

  agency <- metadata_record$agencies[i]
  docCount <- metadata_record[i, ncol(metadata_record)]

  tryCatch({
    dockets <- list.dirs(paste0("data/metadata/", agency), recursive = FALSE)
    ndockets <- length(dockets) - 1
    complete <- abs(ndockets - docCount) <= 100
  }, error = function(e) {
    message("No directory found for:", agency)
  })

  data.complete <- rbind(data.complete, data.frame(agency = agency, docCount = docCount, ndockets = ndockets, complete = complete, stringsAsFactors = FALSE))
}
# works but column names are messed up

write.csv(data.complete, file = "~/Desktop/datacompleteness.csv", row.names = FALSE)







