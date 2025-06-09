 source("setup.R")
library(regulationsdotgov)
 #TODO write this script so that it just takes a vector of docket ids by replacing docket with dockets in the walk(dockets)
dockets <- c(
"TREAS-DO-2013-0005"
  # "USDA-2020-0009",
  # "NPS-2022-0004",
  # "IRS-2024-0026",
  # "BSEE-2015-0002",
  # "EPA-HQ-OW-2022-0901"
)

dockets <- ej_framed_dockets

# INVESTIGATE
dockets <- dockets[which(!dockets == "EPA-HQ-OAR-2004-0394")]

agencies <- str_extract(dockets, "[A-Z]+")

# create directories for each agency
walk(here::here("data", "datasheets", agencies), dir.create)

for(i in dockets){

#  docket <- dockets[1]
docket <- i
message(i)
agency <- str_extract(docket, "[A-Z]+") # str_remove(dockets, "-.*") #FIXME WHICH ONE DO WE WANT

####################
# Comment metadata #
####################
comments_files <- list.files(
  here::here("data", "metadata",agency,docket),
  pattern = "comments.rda",
  recursive = T,
  full.names = T)

load(comments_files[1])

# init
comments_combined <- comments  |> select(-objectId)

# loop to combine
for(j in comments_files){
  load(j)
  comments_combined <<- full_join(comments_combined, comments)
}

comments_combined %<>%
  # drop columns that cause problems for merge because they may not be the same in document_details
  select(-any_of(c("objectId", "lastpage")))

  comments_combined %<>%
    select(-any_of(c("objectId",
                   "attachments",
                   "documentType",
                   #postedDate,
                   #title,
                   "docketId", # WHY IS THIS NA
                   "withdrawn",
                   "openForComment")))




###########
# details #
###########
comment_details_files <- list.files(
  here::here("data", "metadata",agency,docket),
  pattern = "comment_details.rda",
  recursive = T,
  full.names = T)

load(comment_details_files[1])

# init
comment_details_combined <- comment_details


# loop to combine
for(j in comment_details_files){
  load(j)
  comment_details_combined <<- full_join(
    comment_details_combined,
    comment_details |> select(-objectId))
}

d <- comment_details_combined |>
  full_join(comments_combined ) |>
  #full_join(comments_combined, by = "id") |>
  group_by(id) |>
  mutate(attachment_count = unlist(attachments) |> length()/3,
         attachment_urls = unlist(attachments) |> paste(collapse  = ";", sep = ";") |> str_remove(";.*") ) |>
  ungroup()

d |> select(contains("."))


head(d$attachment_urls)

d$attachment_count %>% head()


#FIXME with updated org_names from hand-coding

names(d)

# rename regulations.gov
namingthings <- function(x){
  names(x)  <- names(x) %>%
    str_replace_all("([A-Z])", "_\\1") %>%
    str_to_lower()

  return(x)
}
  # standardize
d %<>% namingthings()



# if organization is missing use title
if(!"organization" %in% names(d)){
  d %<>% mutate(organization = title)
}
names(d)

d %<>% mutate(organization = coalesce(organization, title),
              comment_text = comment,
              agency_acronym = agency_id,
              document_id = id)

nonorgs <- "^personal$|illegible|default organization name|^anonymous|individual$|myself|^mr$|^ms$|^mrs$|^mr.$|^ms.$|^mrs.$|me, myself, and i|not applicable|a youtuber|retired federal employee|the human race|the american people$|^the people$|truck driver$|^human race|^attorney at law|^na$|n/a|no name|^unknown|^none$|^none |noneindividual|^my |^myself|^self$|seslf|private citizen|^citizen.$|^select$|^attorney$| owner$|christian$|catholic$|please select|the dodo|^other$|individual$|just me|anonymous|retired$|citizen$|citzen|^citizens$|citizen/consumer|^citizen, |^citizen of|^citizen at|^citizen -|^citizen and|me, myself and i|me myself and i|^no organization|concerned american|self.employed|self and|regulations.gov|public comment|private party|private owner|private citizan|^private$|personal (comment|opinion)| taxpayer$| voter$| human$|^concerned citizens$|^consumer$| consumer$| mom"


d %<>% filter(attachment_count > 0,
              !str_detect(organization, nonorgs),
              !str_detect(organization, "^.\\. |illegible|surname|last name|forename|no name|^unknown$"),
              !str_detect(title, "illegible|surname|last name|forename|no name") )
dim(d)



# apply auto-coding
#FIXME with updated org_names from hand-coding


source(here::here("code", "org_name.R"))

#FIXME source(here::here("code", "comment_position.R"))

save(d, file = here::here("data", "temp_comments4datasheet.Rdata"))
# load(here::here("data", "comments4datasheets.Rdata"))
dim(d)
temp <- d
d <- temp

d %>% count(org_name, sort = T)

d %<>% mutate(org_name = ifelse(str_dct(title, "Chief,"),
                                str_remove(title, ".*Chief,"),
                                org_name),
              org_name = ifelse(str_dct(title, "Member of Congress|Senat|Rep\\.|Sen\\.|House of Representatives"),
                                title,
                                org_name),
              org_name = ifelse(str_dct(title, ", Counsel,"),
                                str_remove(title, ".*, Counsel,"),
                                org_name),
              org_name = ifelse(str_dct(title, "President,"),
                                str_remove(title, ".*President,"),
                                org_name),
              org_name = ifelse(str_dct(title, ", Chairman,"),
                                str_remove(title, ".*, Chairman,"),
                                org_name),
              org_name = ifelse(str_dct(title, "Chair,"),
                                str_remove(title, ".*Chair, "),
                                org_name),
              org_name = ifelse(str_dct(title, ", MPA,"),
                                str_remove(title, ".*, MPA,"),
                                org_name),
              org_name = ifelse(str_dct(title, ", Council Chair,"),
                                str_remove(title, ".*, Council Chair,"),
                                org_name),
              org_name = ifelse(str_dct(title, "Director,"),
                                str_remove(title, ".*Director,"),
                                org_name),
              org_name = ifelse(str_dct(title, "Commissioner,"),
                                str_remove(title, ".*Commissioner,"),
                                org_name),
              org_name = ifelse(str_dct(title, "Manager,"),
                                str_remove(title, ".*Manager,"),
                                org_name),
              org_name = ifelse(str_dct(title, "City of"),
                                str_remove(title, ".*City of"),
                                org_name),
              org_name = ifelse(str_dct(title, "Treasurer,"),
                                str_remove(title, ".*Treasurer,"),
                                org_name),
              org_name = ifelse(str_dct(title, "Consultant,"),
                                str_remove(title, ".*Consultant,"),
                                org_name),
              org_name = ifelse(str_dct(title, "Attorney General,"),
                                str_remove(title, ".*Attorney General,"),
                                org_name),
              org_name = ifelse(str_dct(title, "Engineer,"),
                                str_remove(title, ".*Engineer,"),
                                org_name),
              org_name = ifelse(str_dct(title, "Judge,"),
                                str_remove(title, ".*Judge,"),
                                org_name),
              org_name = ifelse(str_dct(title, " CEO,"),
                                str_remove(title, ".*CEO,"),
                                org_name),
              org_name = ifelse(str_dct(title, ", Secretary,"),
                                str_remove(title, ".*, Secretary,"),
                                org_name),
              org_name = ifelse(str_dct(title, "Attorney,"),
                                str_remove(title, ".*Attorney,"),
                                org_name),
              org_name = ifelse(str_dct(title, " \\(C.O\\),"),
                                str_remove(title, ".* \\(C.O\\),"),
                                org_name),
              org_name = ifelse(str_dct(title, " Chief Technology Officer,"),
                                str_remove(title, ".*Chief Technology Officer,"),
                                org_name),
              org_name = ifelse(str_dct(title, "Governmental Affairs,"),
                                str_remove(title, ".*Governmental Affairs,"),
                                org_name),
              org_name = ifelse(str_dct(title, "Mayor, "),
                                str_remove(title, ".*Mayor, "),
                                org_name),
              org_name = ifelse(str_dct(title, "Affairs,"),
                                str_remove(title, ".*Affairs,"),
                                org_name),
              org_name = ifelse(str_dct(title, "Governor,"),
                                str_remove(title, ".*Governor,"),
                                org_name),
              org_name = ifelse(str_dct(title, " Operations,"),
                                str_remove(title, ".* Operations,"),
                                org_name),
              org_name = ifelse(str_dct(title, " Fellow,"),
                                str_remove(title, ".* Fellow,"),
                                org_name),
              org_name = ifelse(str_dct(title, "ember,"),
                                str_remove(title, ".*ember,"),
                                org_name),
              org_name = ifelse(str_dct(title, "Board Member,"),
                                str_remove(title, ".*Board Member,"),
                                org_name),
              org_name = ifelse(str_dct(title, "Chairman et al.,"),
                                str_remove(title, ".*Chairman et al.,"),
                                org_name),
              org_name = ifelse(str_dct(title, "Officer,"),
                                str_remove(title, ".*Officer,"),
                                org_name),
              org_name = ifelse(str_dct(title, "Secretary,"),
                                str_remove(title, ".*Secretary,"),
                                org_name),
              org_name = ifelse(str_dct(title, "Chair of the Board,"),
                                str_remove(title, ".*Chair of the Board,"),
                                org_name)
)

d$title %<>% str_remove("Comment submitted by |Comment from |Comments from |Comment on |Comments|Submitted Electronically via eRulemaking Portal")

d$org_name%<>% str_squish()

save(d, file = here::here("data", "temp_comments4datasheet.Rdata"))
# load(here::here("data", "comments4datasheets.Rdata"))

d %<>% mutate(number_of_comments_received = duplicate_comments + 1)

# filter down to org comments
d %<>%
  group_by(docket_id, org_name) %>%
  add_count(name = "org_unique_comments") %>%
  ungroup() %>%
  arrange(-number_of_comments_received) %>%
  filter(attachment_count > 0,
         str_detect(str_c(title, org_name), "Congress|Senat|Rep\\.|Sen\\.|Representative") |
           # individual names
           !str_detect(org_name, "^.\\.$|^.\\. |^.\\.[A-Z][a-z]|^.\\. [A-Z][a-z]|^\\w+ .\\.|illegible|no surname"),
         !str_detect(title, "illegible|surname| suranme |last name|forename|no name"),
         nchar(org_name) > 1) %>%
  mutate(org_name = org_name %>% replace_na("NA") ) %>%
  filter(number_of_comments_received > 9 | !org_name %in% c("NA", "na", "","unknown")) %>%
  add_count(docket_id, name = "org_total_comments")


d %>% count(org_name, sort = T)

# random sample
d %>% distinct(org_name,title) %>% slice_sample(n = 100) %>% knitr::kable()

d %>%
  #filter(n > 10, n < 20) %>%
  count(docket_id, sort = T) %>% knitr::kable()



## AUGMENT FUNCTION
# ad document name and link
d %<>%
  mutate(comment_url = str_c("https://www.regulations.gov/comment/",
                             document_id),
         comment_on_document_url = str_c("https://www.regulations.gov/document/",
                             comment_on_document_id),
         docket_url = str_c("https://www.regulations.gov/docket/",
                            document_id %>% str_remove("-[0-9]*$")))

# d$attachment_txt[1] #TODO
d$comment_url[1]
d$comment_on_document_url[1]
d$docket_url[1]

d %<>% rename(comment_title = title)
names(d)
## PREP SHEETS
d %<>% select(
  #agency_acronym,
  docket_id,
  docket_url,
  #docket_title,
  comment_on_document_url,
  document_id,
  posted_date,
  comment_url,
  comment_text,
  #attachment_txt,
  organization,
  comment_title,
  attachment_count,
  attachment_urls,
  number_of_comments_received,
  org_name)

# add blanks
d %<>% mutate(comment_type = "",
              comment_signers = "",
              org_name_short = "",
              org_type = "",
              org_geography	= "",
              position = "",
              position_certainty = "",
              coalition_comment = "",
              coalition_type = "",
              # org_name = organization, # run scratchpad/orgnames.R until this is a function
              ask = "",
              ask1 = "",
              ask2 = "",
              ask3 = "",
              asks_procedural = "",
              ask_geography = "",
              success = "",
              success_certainty = "",
              success1 = "",
              success2 = "",
              success3 = "",
              success_procedural = "",
              response = "",
              pressure_phrases = "",
              accept_phrases = "",
              compromise_phrases = "",
              reject_phrases = "",
              mass_platform = "",
              mass_transparent = "",
              notes = "")

names(d)

# unique(d$organization)

count(d, organization, sort = T) %>% head()
count(d, org_name, sort = T) %>% head()

d %>% filter( organization  == "N/A") %>% distinct(org_name, number_of_comments_received)
d %>% filter(org_name == "unknown") %>% distinct(org_name, number_of_comments_received)

# create new directory if needed
if (!dir.exists(here::here("data", "datasheets") ) ){
  dir.create( here::here("data", "datasheets") )
}


write_comment_sheets <- function(docket){
  d %>%
    filter(docket_id == docket) %>%
    write_csv(file = here::here("data", "datasheets",
                                agency,
                                #str_extract("^[A-Z]"), # agency
                                str_c(docket, "_org_comments BLANK.csv")))
}


names(d)
d %<>% mutate(comment_type = ifelse(number_of_comments_received > 99, "mass", comment_type))

unique(d$docket_id)

d %<>% drop_na(docket_id)

d %<>% filter(docket_id == i)

walk(unique(d$docket_id), write_comment_sheets)

# STRAIGHT TO GOOGLE DRIVE

# create a blank sheet with named as the docket_id
library(googlesheets4)
ss <- gs4_create(name = paste0(unique(d$docket_id), "_org_comments BLANK"),

           sheets = unique(d$docket_id) )

d |>
  sheet_write(ss,
              sheet = unique(d$docket_id))


# Find the proper folder based on the agency name
library(googledrive)

folder_id <- drive_find(n_max = 10,
                        type = "folder",
                        pattern = agency)$id
folder_id
# folder_id <- "1q4FhGfqziamvDoogM-d__Xfv2za0KU2M"

# put in the propoer folder
drive_mv(file = ss,
         path = as_id(folder_id),
          overwrite = T)

# remove from list when done
dockets <- dockets[which(!dockets == i)]


}

# make a table of the docket ids and titles
load("../keys.rda")
d <- map_dfr(dockets, get_documents, api_keys = keys)


 d |>
   drop_na(commentStartDate) |>
   filter(documentType == "Proposed Rule") |>
   distinct(docketId, title) |>
   group_by(docketId) |>
   mutate(title = paste(title, sep = ";", collapse = ";")) |>
   distinct(docketId, title) |>
   write_csv(here::here("data", "docket_titles_to_code-EJframed.csv"))



 # TEMP FIX
 if(F){
 ss <- drive_find(n_max = 500,
                  type = "spreadsheet",
                  " _org_comments")

fix <- function(id, name){
   drive_rename(id, name = str_replace(name, " _org_comments", "_org_comments"))
}

walk2(ss$id, ss$name, .f = fix)
 }
