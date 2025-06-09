

library(regulationsdotgov)

# other packages
library(tidyverse)
library(httr)
library(jsonlite)
library(magrittr)
library(here)


d <- map_dfr(dockets, get_documents, api_keys =  keys)

d %>% drop_na(commentStartDate) %>%
  filter(documentType == "Proposed Rule") %>%
  distinct(title, docketId) %>%
  write_csv(file = here::here("data", "docket_titles_to_code.csv"))

