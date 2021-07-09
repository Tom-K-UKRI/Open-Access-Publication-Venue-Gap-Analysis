#03 Unpaywall API and cleaning

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

# 117,583/163,009 (72%) have a green AAM or VoR

save(unpaywall_green_plus_gold_licences, file = "Data/unpaywall_green_plus_gold_licences.RData")