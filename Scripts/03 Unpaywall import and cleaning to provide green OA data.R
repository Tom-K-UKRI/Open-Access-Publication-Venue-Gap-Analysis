#03 Unpaywall import and cleaning to provide green OA data

# library(googlesheets4) - this is no longer needed unless using Dimensions google sheets api again
library(dplyr)
library(stringr)
library(tidyr)
library(readr)
library(openxlsx)

#XXXXXXXXXXXXXXX
# Clear work space
rm(list=ls())

# Import Dimensions to create list of DOIs
load("Data/dimensions_filtered.Rda")

dimensions_dois <- dimensions %>% distinct(DOI)

dimensions_dois1 <- dimensions_dois[1:10000,]
dimensions_dois2 <- dimensions_dois[10001:95000,]
dimensions_dois3 <- dimensions_dois[95001:163126,]

# Run Unpaywall API

dim_upw_dois_1 <- roadoi::oadoi_fetch(dois = c(paste(dimensions_dois1$DOI, sep = ", ")), 
                                         email = "tom.kenny@ukri.org", .flatten = TRUE, .progress = "text")

dim_upw_dois_2 <- roadoi::oadoi_fetch(dois = c(paste(dimensions_dois2$DOI, sep = ", ")), 
                                      email = "tom.kenny@ukri.org", .flatten = TRUE, .progress = "text")

dim_upw_dois_3 <- roadoi::oadoi_fetch(dois = c(paste(dimensions_dois3$DOI, sep = ", ")), 
                                      email = "tom.kenny@ukri.org", .flatten = TRUE, .progress = "text")


unpaywall <- bind_rows(dim_upw_dois_1, dim_upw_dois_2, dim_upw_dois_3)

# save output
save(unpaywall, file = "Data/Raw data/unpaywall.RData")

# See guidance on how unpaywall defines categories here: https://support.unpaywall.org/support/solutions/articles/44001777288-what-do-the-types-of-oa-status-green-gold-hybrid-and-bronze-mean-

# Derive green OA variable
  # Essentially this code creates a new row for every oa location recorded in unpaywall, then filters to only include ones which have a green VoR or AAM, then ranks them so that published versions come first, then keeps only the first version for each DOI/ article.
  # This means we end up with 1 row for each article with a green AAM/VoR, with VoR selected over AAM. The upw_green variable therefore means that at least the AAM is available in a repository.
  # NB. Our definition of the best green location differs from Unpaywall's as we prioritise a compliant license over the version (compliant licence AAM better than non-compliant licence VoR)


unpaywall_green <- unpaywall %>%
  filter(host_type == "repository" & grepl("publishedVersion|acceptedVersion", version)) %>%
  mutate(licence_order = ifelse(license %in% c("cc-by", "cc0"), 1,
                                ifelse(license == "cc-by-sa", 2,
                                       ifelse(license == "cc-by-nd", 3,
                                              ifelse(license == "cc-by-nc", 4,
                                                     ifelse(license == "cc-by-nc-sa", 5,
                                                            ifelse(license == "cc-by-nc-nd", 6,
                                                                   ifelse(!is.na(license), 7, 8)))))))) %>%
  mutate(compliant_licence = ifelse(licence_order %in% c(1:4), "Compliant", "Not compliant")) %>%
  arrange(compliant_licence, desc(version), desc(is_best), licence_order) %>%
  distinct(doi, .keep_all = TRUE) %>%
  rename(upw_green_licence = license, upw_green_version = version) %>%
  select(doi, title, url, evidence, upw_green_licence, upw_green_version, host_type, is_best, oa_status)

# Derive licence for gold OA versions (not including bronze)
unpaywall_gold <- unpaywall %>%
  filter(host_type == "publisher", !is.na(license), version == "publishedVersion") %>%
  arrange(desc(is_best)) %>%
  distinct(doi, .keep_all = TRUE) %>%
  rename(upw_gold_licence = license)

# Merge unpaywall green and unpaywall gold licences
unpaywall_green_plus_gold_licences <- unpaywall %>%
  arrange(desc(is_best)) %>%
  distinct(doi, .keep_all = TRUE) %>%
  select(doi, oa_status, journal_is_oa, journal_is_in_doaj) %>%
  rename(best_oa_status_upw = oa_status) %>%
  left_join(unpaywall_green %>% select(doi, upw_green_licence, upw_green_version), by = "doi") %>%
  left_join(unpaywall_gold %>% select(doi, upw_gold_licence), by = "doi")

unpaywall_green_plus_gold_licences$upw_green_licence[unpaywall_green_plus_gold_licences$upw_green_licence == ""] <- "No green licence found"


# 117,583/163,009 (72%) have a green AAM or VoR

save(unpaywall_green_plus_gold_licences, file = "Data/unpaywall_green_plus_gold_licences.RData")





# THIS IS OLD VERSION OF CODE - DATAHUB WAS MISSING DATA SO HAD TO GO DIRECT TO UNPAYWALL API
# 
# # Open an ODBC connection to the Data Hub ----
# con <- DBI::dbConnect(odbc::odbc(),
#                       Driver = "ODBC Driver 17 for SQL Server",
#                       Server = "ausorizmidb01.fbf0f5dae340.database.windows.net",
#                       Authentication = "ActiveDirectoryIntegrated")
# 
# # Upload Dimensions DOIs to datahub sandbox as a table
# table_id <- DBI::Id(catalog = "SandBoxEndUsers", schema = "dbo", table = "pvga_dois")
# 
# # write the table to the Data Hub Sandbox
# odbc::dbWriteTable(conn = con,
#                    value = dimensions_dois,
#                    table_id,
#                    overwrite = TRUE)
# 
# # Use the list of DOIs to select required data from Unpaywall
# unpaywall <- odbc::dbGetQuery(conn = con,
#                                 "SELECT refs.DOI, other.doi, other.year, other.title, other.oa_locations, other.best_oa_location
#                          FROM [SandboxEndUsers].dbo.[pvga_dois] AS refs
#                           LEFT JOIN [UnPaywall_DOI].[dbo].[doi] AS other
#                             ON refs.DOI = other.doi")
# 
# # save output
# save(unpaywall, file = "Data/Raw data/unpaywall.RData")

# Derive green OA variable
# Essentially this code creates a new row for every oa location recorded in unpaywall, then filters to only include ones which have a green VoR or AAM, then ranks them so that published versions come first, then keeps only the first version for each DOI/ article.
# This means we end up with 1 row for each article with a green AAM/VoR, with VoR selected over AAM. The upw_green variable therefore means that at least the AAM is available in a repository.


# unpaywall_green <- unpaywall %>%
#   separate(oa_locations, into = c("loc1", "loc2", "loc3", "loc4", "loc5", "loc6", "loc7", "loc8", "loc9", "loc10", "loc11", "loc12", "loc13", "loc14", "loc15",
#                                   "loc16", "loc17", "loc18", "loc19", "loc20", "loc21", "loc22"), sep = "}, {", remove = FALSE) %>%
#   pivot_longer(c(loc1, loc2, loc3, loc4, loc5, loc6, loc7, loc8, loc9, loc10, loc11, loc12, loc13, loc14, loc15, loc16, loc17,
#                  loc18, loc19, loc20, loc21, loc22), names_to = "loc_all", values_to ="locations") %>%
#   filter(grepl("\"host_type\": \"repository\"", locations) & grepl("publishedVersion|acceptedVersion", locations)) %>%
#   mutate(VoR_green = grepl("publishedVersion", locations), AAM_green = grepl("acceptedVersion", locations)) %>%
#   arrange(desc(VoR_green)) %>%
#   distinct(DOI, .keep_all = TRUE) %>%
#   rename(upw_green_location = locations, upw_oa_locations = oa_locations) %>%
#   mutate(upw_green_licence = str_extract_all(upw_green_location, "(?<=license\": \")(.*?)(?=\",)", simplify = TRUE)) %>%
#   select(-DOI)
# 
# # Derive license for gold OA versions (not including bronze)
# unpaywall_gold <- unpaywall %>%
#   mutate(upw_gold_licence = str_extract_all(best_oa_location, "(?<=license\": \")(.*?)(?=\",)", simplify = TRUE)) %>%
#   filter(grepl("host_type\": \"publisher", best_oa_location), !grepl("license\": null", best_oa_location)) # this chooses gold OA but not bronze OA
# 
# # Merge unpaywall green and unpaywall gold licenses
# unpaywall_green_plus_gold_licences <- unpaywall %>% select(doi, oa_locations, best_oa_location) %>%
#   left_join(unpaywall_green %>% select(doi, upw_green_location, upw_green_licence), by = "doi") %>%
#   left_join(unpaywall_gold %>% select(doi, upw_gold_licence), by = "doi")
# 
# unpaywall_green_plus_gold_licences$upw_green_licence[unpaywall_green_plus_gold_licences$upw_green_licence == ""] <- "No green license found"
# 
# 
# # 167455/312294 have a green AAM or VoR
# 
# save(unpaywall_green_plus_gold_licences, file = "Data/unpaywall_green_plus_gold_licences.RData")
