
# problem dockets / documents to investegate


problem_documents <- tribble(
  ~docket, ~document, ~objectId,
  "APHIS-2022-0003", "APHIS-2022-0003-0001", "0900006484e23ccb",
  "HHS-OS-2011-0023", "HHS-OS-2011-0023-0002", "0900006480ed4b59"
  # "FNS-2007-0038-0001", "FNS-2007-0038-0001", ?????,
  ## FIXME Commenting out for now because these no longer seem to be problems
  #   "WHD-2017-0002", "WHD-2017-0002-0001",  "0900006482969fbd", # expected 140-214k
  #   "WHD-2017-0003", "WHD-2017-0003-0001", "0900006482cdbe54", # expected 218-376k https://www.regulations.gov/docket/WHD-2017-0003
  #   "WHD-2019-0001","WHD-2019-0001-0001", "0900006483b173dc",
  #   "PHMSA-2021-0039", "PHMSA-2021-0039-2101", "0900006485c21b4f",
  #   "PHMSA-2021-0058", "PHMSA-2021-0058-0002", "0900006484e3545a",
  #   "NHTSA-2022-0079", "NHTSA-2022-0079-0015", "090000648637d7c0" # expecting 18,317 - https://www.regulations.gov/document/NHTSA-2022-0079-0015
)
#
dockets %<>%
  filter(!docket %in% problem_documents$docket)
