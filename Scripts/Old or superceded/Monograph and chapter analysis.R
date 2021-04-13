# Monograph and chapter analysis

# Importing and merging Dimensions google sheets data
library(googlesheets4)
library(dplyr)

#XXXXXXXXXXXXXXX
# Clear work space
rm(list=ls())

# Import and collate dimensions data----
# from google sheets prepared for each year using Dimensions Google Sheets API with query in format = "search publications where year in [2017:2020] and funders in ["grid.8682.4", "grid.14105.31", "grid.14467.30", "grid.418100.c", "grid.421091.f", "grid.423443.6", "grid.426413.6", "grid.434257.3", "grid.453088.2", "grid.496779.2"] and type in ["monograph"] return publications[type + category_for + date + dimensions_url + doi + funder_countries + funders + issn + journal + linkout + open_access_categories + publisher + research_org_country_names + supporting_grant_ids + title  + year]" Plus another search with type in "chapter"
# This data was most recently updated on 22.12.20

# dim_mono_2017_2020 <- read_sheet("https://docs.google.com/spreadsheets/d/115eypCkI8MNQ9mVcZ-mPAjwLAMjHf0MV25UNCdjL5N0/edit#gid=0")
# 
# # Ouput to .rdata
# save(dim_mono_2017_2020, file = "Data/dim_mono_2017_2020.Rda")

library(dplyr)

# Import data (if up to date)
load("Data/dim_mono_2017_2020.Rda")

# Open access categories----

  # renaming (simplifying) values in open_access_categories.
dim_mono_2017_2020$open_access_categories[grepl("Closed", dim_mono_2017_2020$open_access_categories)] <- "Closed"
dim_mono_2017_2020$open_access_categories[grepl("Bronze", dim_mono_2017_2020$open_access_categories)] <- "Bronze"
dim_mono_2017_2020$open_access_categories[grepl("Hybrid", dim_mono_2017_2020$open_access_categories)] <- "Hybrid"
dim_mono_2017_2020$open_access_categories[grepl("Pure Gold", dim_mono_2017_2020$open_access_categories)] <- "Pure gold"
dim_mono_2017_2020$open_access_categories[grepl("Green, Accepted", dim_mono_2017_2020$open_access_categories)] <- "Green, accepted"
dim_mono_2017_2020$open_access_categories[grepl("Green, Submitted", dim_mono_2017_2020$open_access_categories)] <- "Green, submitted"
dim_mono_2017_2020$open_access_categories[grepl("Green, Published", dim_mono_2017_2020$open_access_categories)] <- "Green, published"

  # Recoding open_access_categories into simpler form
dim_mono_2017_2020$open_access_categories2 <- dim_mono_2017_2020$open_access_categories
dim_mono_2017_2020$open_access_categories2[dim_mono_2017_2020$open_access_categories2 == "Hybrid"] <- "Hybrid gold" # to avoid confusion with journal type
dim_mono_2017_2020$open_access_categories2[dim_mono_2017_2020$open_access_categories2 == "Green, published" | dim_mono_2017_2020$open_access_categories2 == "Green, accepted"] <- "Green"
dim_mono_2017_2020$open_access_categories2[dim_mono_2017_2020$open_access_categories2 == "Bronze" | dim_mono_2017_2020$open_access_categories2 == "Green, submitted" | dim_mono_2017_2020$open_access_categories2 == "Closed"] <- "Closed" #since these are all closed from the perspective of UKRI policyopen_access_categories[grepl("Green, Published", dim_mono_2017_2020$open_access_categories)] <- "Green, published"

# ANALYSIS----

# 1. Breakdown of book chapters and monographs
dim_mono_chapters <- dim_mono_2017_2020 %>%
  count(type) %>%
  mutate(percent = round(n/sum(n)*100,1))

# 2. Open access categories for all monographs and book chapters----
dim_oac_mono <- dim_mono_2017_2020 %>%
  count(open_access_categories) %>%
  mutate(percent = round(n/sum(n)*100,1))

openxlsx::write.xlsx(as.data.frame(dim_oac_mono), 'Output/Tables/dim_oac_mono.xlsx')

# 3. Open access categories for monographs only----

dim_oac_mono_only <- dim_mono_2017_2020 %>%
  filter(type == "monograph") %>%
  count(open_access_categories2) %>%
  mutate(percent = round(n/sum(n)*100,1))

openxlsx::write.xlsx(as.data.frame(dim_oac_mono_only), 'Output/Tables/dim_oac_mono_only.xlsx')

# Funders ----
# Creating new variable ukri_funders pulling out the UKRI funders for each article and cleaning it up (these will later be merged into ukri_funders and then removed)
dim_mono_2017_2020$AHRC[grepl("AHRC", dim_mono_2017_2020$funders)] <- "AHRC"
dim_mono_2017_2020$BBSRC[grepl("BBSRC", dim_mono_2017_2020$funders)] <- "BBSRC"
dim_mono_2017_2020$ESRC[grepl("ESRC", dim_mono_2017_2020$funders)] <- "ESRC"
dim_mono_2017_2020$EPSRC[grepl("EPSRC", dim_mono_2017_2020$funders)] <- "EPSRC"
dim_mono_2017_2020$Innovate_UK[grepl("Innovate UK", dim_mono_2017_2020$funders)] <- "Innovate UK"
dim_mono_2017_2020$MRC[grepl("MRC", dim_mono_2017_2020$funders)] <- "MRC"
dim_mono_2017_2020$NC3Rs[grepl("NC3Rs", dim_mono_2017_2020$funders)] <- "NC3Rs"
dim_mono_2017_2020$NERC[grepl("NERC", dim_mono_2017_2020$funders)] <- "NERC"
dim_mono_2017_2020$Research_England[grepl("Research England", dim_mono_2017_2020$funders)] <- "Research England"
dim_mono_2017_2020$STFC[grepl("STFC", dim_mono_2017_2020$funders)] <- "STFC"
dim_mono_2017_2020$UKRI[grepl("UKRI", dim_mono_2017_2020$funders)] <- "UKRI"

# pull it all together into one column ukri_funders then remove NAs
dim_mono_2017_2020$ukri_funders <- paste(dim_mono_2017_2020$AHRC, dim_mono_2017_2020$BBSRC, dim_mono_2017_2020$ESRC, dim_mono_2017_2020$EPSRC, dim_mono_2017_2020$Innovate_UK, dim_mono_2017_2020$MRC, dim_mono_2017_2020$NC3Rs, dim_mono_2017_2020$NERC, dim_mono_2017_2020$Research_England, dim_mono_2017_2020$STFC, dim_mono_2017_2020$UKRI, sep = ", ") 
dim_mono_2017_2020$ukri_funders <- gsub("NA, ", "", dim_mono_2017_2020$ukri_funders)
dim_mono_2017_2020$ukri_funders <- gsub(", NA", "", dim_mono_2017_2020$ukri_funders)

dim_ukri_funders_mono <- dim_mono_2017_2020 %>%
  count(ukri_funders) %>%
  mutate(percent = round(n/sum(n)*100,1))

openxlsx::write.xlsx(as.data.frame(dim_ukri_funders_mono), 'Output/Tables/dim_ukri_funders_mono.xlsx')


# Publishers----
dim_mono_publishers <- dim_mono_2017_2020 %>%
  count(publisher) %>%
  arrange(desc(n)) %>%
  mutate(percent = round(n/sum(n)*100,1), cml = cumsum(percent))
