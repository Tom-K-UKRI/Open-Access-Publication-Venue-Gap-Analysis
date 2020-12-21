# Importing and merging Dimensions google sheets data
library(googlesheets4)
library(dplyr)
library(stringr)
library(tidyr)

# Import and collate dimensions data----
  # from google sheets prepared for each year using Dimensions Google Sheets API with query in format = "search publications where year in [2017:2017] and funders in ["grid.8682.4", "grid.14105.31", "grid.14467.30", "grid.418100.c", "grid.421091.f", "grid.423443.6", "grid.426413.6", "grid.434257.3", "grid.453088.2", "grid.496779.2"] and type in ["article"] return publications[type + category_for + date + dimensions_url + doi + funder_countries + funders + issn + journal + linkout + open_access_categories + publisher + research_org_country_names + supporting_grant_ids + title  + year]"
  # Needs to be done by year and then merged because each query has limit of 50,000 returns
  # This data was most recently updated on 16.12.20

# dim_2017 <- read_sheet("https://docs.google.com/spreadsheets/d/1V1B8sb7xhgbsLNCtqBVlPcggo3WOCQWi64zC-TUgoGk/edit#gid=0")
# dim_2018 <- read_sheet("https://docs.google.com/spreadsheets/d/1xqNpny6yZv_JISD8QfBBn2Sb69LKzq9e_71fXKHvbs8/edit#gid=0")
# dim_2019 <- read_sheet("https://docs.google.com/spreadsheets/d/1QufAZ7499LI57kJrQkL1EIhj5bHKPHL0pl68IGhLbIk/edit#gid=0")
# dim_2020 <- read_sheet("https://docs.google.com/spreadsheets/d/1opigG05jN745YkTXlVObh0jQ5q52MIGbT-rlhyks1AA/edit#gid=0")
# 
# Dim_UKRI_2017_2020 <- bind_rows(dim_2017, dim_2018, dim_2019, dim_2020)
# 
#   # save collated data to project folder
# openxlsx::write.xlsx(as.data.frame(Dim_UKRI_2017_2020), 'Data/Raw data/Dim_UKRI_2017_2020.xlsx')
# save(Dim_UKRI_2017_2020, file = "Data/Raw Data/Dim_UKRI_2017_2020.Rda")

  # create working dataframe 'dimensions'
load("Data/Raw Data/Dim_UKRI_2017_2020.Rda")
dimensions <- Dim_UKRI_2017_2020

# Cleaning dimensions data and creating derived variables ----

# Remove duplicate rows (using doi and title - the latter may remove a few different articles which have the same name but manual checks suggest the vast majority of duplicate titles are the same article or things like corrections and they skew very heavily towards open access). Remove articles without an ISSN.
dimensions <- dimensions %>%
  filter(!duplicated(doi), !duplicated(title), !is.na(issn))

# Lowercase journal_title to aid matching with Sherpa
dimensions$journal_title <- tolower(dimensions$journal_title)

# renaming values in research_org_country_names to leave only the names themselves
dimensions <- dimensions %>%
  mutate(research_org_country_names = gsub("\\[\"", "", research_org_country_names)) %>%
  mutate(research_org_country_names = gsub("\",\"", ", ", research_org_country_names)) %>%
  mutate(research_org_country_names = gsub("\"\\]", "", research_org_country_names))

# renaming (simplifying) values in open_access_categories.
dimensions$open_access_categories[grepl("Closed", dimensions$open_access_categories)] <- "Closed"
dimensions$open_access_categories[grepl("Bronze", dimensions$open_access_categories)] <- "Bronze"
dimensions$open_access_categories[grepl("Hybrid", dimensions$open_access_categories)] <- "Hybrid"
dimensions$open_access_categories[grepl("Pure Gold", dimensions$open_access_categories)] <- "Pure gold"
dimensions$open_access_categories[grepl("Green, Accepted", dimensions$open_access_categories)] <- "Green, accepted"
dimensions$open_access_categories[grepl("Green, Submitted", dimensions$open_access_categories)] <- "Green, submitted"
dimensions$open_access_categories[grepl("Green, Published", dimensions$open_access_categories)] <- "Green, published"

# Creating new variable ukri_funders pulling out the UKRI funders for each article and cleaning it up (these will later be merged into ukri_funders and then removed)
dimensions$AHRC[grepl("AHRC", dimensions$funders)] <- "AHRC"
dimensions$BBSRC[grepl("BBSRC", dimensions$funders)] <- "BBSRC"
dimensions$ESRC[grepl("ESRC", dimensions$funders)] <- "ESRC"
dimensions$EPSRC[grepl("EPSRC", dimensions$funders)] <- "EPSRC"
dimensions$Innovate_UK[grepl("Innovate UK", dimensions$funders)] <- "Innovate UK"
dimensions$MRC[grepl("MRC", dimensions$funders)] <- "MRC"
dimensions$NC3Rs[grepl("NC3Rs", dimensions$funders)] <- "NC3Rs"
dimensions$NERC[grepl("NERC", dimensions$funders)] <- "NERC"
dimensions$Research_England[grepl("Research England", dimensions$funders)] <- "Research England"
dimensions$STFC[grepl("STFC", dimensions$funders)] <- "STFC"
dimensions$UKRI[grepl("UKRI", dimensions$funders)] <- "UKRI"

  # pull it all together into one column ukri_funders then remove NAs
dimensions$ukri_funders <- paste(dimensions$AHRC, dimensions$BBSRC, dimensions$ESRC, dimensions$EPSRC, dimensions$Innovate_UK, dimensions$MRC, dimensions$NC3Rs, dimensions$NERC, dimensions$Research_England, dimensions$STFC, dimensions$UKRI, sep = ", ") 
dimensions$ukri_funders <- gsub("NA, ", "", dimensions$ukri_funders)
dimensions$ukri_funders <- gsub(", NA", "", dimensions$ukri_funders)

# editing category_for (fields of research)

  # Pull out divisions (dimensions$for_division)
dimensions <- dimensions %>% mutate(for_division = str_extract_all(category_for, "\"[0-9]{2}[:space:]{1}.+?(?=\")")) %>%
  separate(for_division, into = c("for_division1", "for_division2", "for_division3", "for_division4"), sep = "\", \"") %>%
  mutate(for_division1 = str_extract_all(for_division1, "[0-9]{2}[:space:]{1}.*"), 
         for_division2 = str_extract_all(for_division2, "[0-9]{2}[:space:]{1}.*"), 
         for_division3 = str_extract_all(for_division3, "[0-9]{2}[:space:]{1}.*"), 
         for_division4 = str_extract_all(for_division4, "[0-9]{2}[:space:]{1}.*"),
         for_division2 = gsub('")', "", for_division2, fixed = TRUE),
         for_division3 = gsub('")', "", for_division3, fixed = TRUE),
         for_division4 = gsub('")', "", for_division4, fixed = TRUE))

dimensions <- dimensions %>%
  mutate(for_division = paste(for_division1, for_division2, for_division3, for_division4, sep = ", ")) %>%
  mutate(for_division = gsub("NA, ", "", for_division),
         for_division = gsub(", NA", "", for_division))

dimensions$for_division[dimensions$for_division == "NA"] <- "Missing"

  # Pull out groups (dimensions$for_group)
dimensions <- dimensions %>% mutate(for_group = str_extract_all(category_for, "\"[0-9]{4}[:space:]{1}.+?(?=\")")) %>%
  separate(for_group, into = c("for_group1", "for_group2", "for_group3", "for_group4"), sep = "\", \"") %>%
  mutate(for_group1 = str_extract_all(for_group1, "[0-9]{4}[:space:]{1}.*"), 
         for_group2 = str_extract_all(for_group2, "[0-9]{4}[:space:]{1}.*"), 
         for_group3 = str_extract_all(for_group3, "[0-9]{4}[:space:]{1}.*"), 
         for_group4 = str_extract_all(for_group4, "[0-9]{4}[:space:]{1}.*"),
         for_group2 = gsub('")', "", for_group2, fixed = TRUE),
         for_group3 = gsub('")', "", for_group3, fixed = TRUE),
         for_group4 = gsub('")', "", for_group4, fixed = TRUE))

dimensions <- dimensions %>%
  mutate(for_group = paste(for_group1, for_group2, for_group3, for_group4, sep = ", ")) %>%
  mutate(for_group = gsub("NA, ", "", for_group),
         for_group = gsub(", NA", "", for_group))

dimensions$for_group[dimensions$for_group == "NA"] <- "Missing"

  # Creating dimensions$discipline, a proxy for discipline (NB. This is imperfect, in particular because 10 Technology is also part Life Sciences, 12 Built Environment is also part social sciences, and 17 Psychology is also part Life Sciences)

physical_sciences <- c("01 Mathematical Sciences", "02 Physical Sciences", "03 Chemical Sciences", "04 Earth Sciences", "05 Environmental Sciences", "08 Information and Computing Sciences", "09 Engineering", "10 Technology", "12 Built Environment and Design")
health_sciences <- c("11 Medical and Health Sciences")
social_sciences <- c("13 Education", "14 Economics", "15 Commerce, Management, Tourism, and Services", "16 Studies in Human Society", "17 Psychology and Cognitive Sciences", "18 Law and Legal Sciences")
life_sciences <- c("06 Biological Sciences", "07 Agricultural and Veterinary Sciences")
arts_humanities <- c("19 Studies in Creative Arts and Writing", "20 Language, Communication and Culture", "21 History and Archaeology", "22 Philosophy and Religious Studies")

dimensions$physical <- NA  
dimensions$physical[grep(paste(physical_sciences, collapse = "|"), dimensions$for_division)] <- "Physical Sciences"
dimensions$health <- NA  
dimensions$health[grep(paste(health_sciences, collapse = "|"), dimensions$for_division)] <- "Health Sciences"
dimensions$social <- NA  
dimensions$social[grep(paste(social_sciences, collapse = "|"), dimensions$for_division)] <- "Social Sciences"
dimensions$life <- NA  
dimensions$life[grep(paste(life_sciences, collapse = "|"), dimensions$for_division)] <- "Life Sciences"
dimensions$arts_humanities <- NA  
dimensions$arts_humanities[grep(paste(arts_humanities, collapse = "|"), dimensions$for_division)] <- "Arts & Humanities"

dimensions <- dimensions %>%
  mutate(discipline = paste(physical, health, social, life, arts_humanities, sep = ", ")) %>%
  mutate(discipline = gsub("NA, ", "", discipline),
         discipline = gsub(", NA", "", discipline))

dimensions$discipline[dimensions$discipline == "NA"] <- "Missing"

    # Frequency of each discipline
Discipline <- c("Arts & Humanities", "Physical Sciences", "Health Sciences", "Social Sciences", "Life Sciences")
n <- c(NA,NA,NA,NA,NA)
dimensions_disciplines <- data.frame(Discipline, n)
dimensions_disciplines$n[Discipline == "Arts & Humanities"] <- sum(str_count(dimensions$discipline, "Arts & Humanities"))
dimensions_disciplines$n[Discipline == "Physical Sciences"] <- sum(str_count(dimensions$discipline, "Physical Sciences"))
dimensions_disciplines$n[Discipline == "Health Sciences"] <- sum(str_count(dimensions$discipline, "Health Sciences"))
dimensions_disciplines$n[Discipline == "Social Sciences"] <- sum(str_count(dimensions$discipline, "Social Sciences"))
dimensions_disciplines$n[Discipline == "Life Sciences"] <- sum(str_count(dimensions$discipline, "Life Sciences"))
dimensions_disciplines <- dimensions_disciplines %>%
  mutate(percent=round(n/sum(n)*100,1))
         

# creating new variables to return individual ISSNs in consistent format (8 numbers each - up to four per journal)
dimensions <- dimensions %>% mutate(issn=str_extract_all(issn, "[0-9A-Za-z]{4}-[0-9A-Za-z]{4}")) %>%
  separate(issn, into = c("issn1", "issn2", "issn3", "issn4"), sep = ",") %>%
  mutate(issn1=str_extract_all(issn1, "[0-9A-Za-z]{4}-[0-9A-Za-z]{4}"), issn2=str_extract_all(issn2, "[0-9A-Za-z]{4}-[0-9A-Za-z]{4}"), issn3=str_extract_all(issn3, "[0-9A-Za-z]{4}-[0-9A-Za-z]{4}"), issn4=str_extract_all(issn4, "[0-9A-Za-z]{4}-[0-9A-Za-z]{4}")) %>%
  mutate(issn1 = gsub("-","",issn1), issn2 = gsub("-","",issn2), issn3 = gsub("-","",issn3), issn4 = gsub("-","",issn4))

# Recoding open_access_categories into simpler form
dimensions$open_access_categories2 <- dimensions$open_access_categories
dimensions$open_access_categories2[dimensions$open_access_categories2 == "Hybrid"] <- "Hybrid gold" # to avoid confusion with journal type
dimensions$open_access_categories2[dimensions$open_access_categories2 == "Green, published" | dimensions$open_access_categories2 == "Green, accepted"] <- "Green"
dimensions$open_access_categories2[dimensions$open_access_categories2 == "Bronze" | dimensions$open_access_categories2 == "Green, submitted" | dimensions$open_access_categories2 == "Closed"] <- "Closed" #since these are all closed from the perspective of UKRI policy

# changing column order (this also serves to remove unnecessary columns. If any new columns added they will need to be added here)
col_order <- (c("title", "date", "journal_title", "publisher", "issn1", "issn2", "issn3", "issn4", "doi", "category_for", "for_division", "for_group", "discipline", "open_access_categories", "open_access_categories2", "ukri_funders"))
dimensions <- dimensions[, col_order]

openxlsx::write.xlsx(as.data.frame(dimensions), 'Data/Dimensions.xlsx')
save(dimensions, file = "Data/Dimensions.Rda")