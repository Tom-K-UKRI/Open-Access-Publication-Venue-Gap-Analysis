# Importing and cleaning Dimensions data

# library(googlesheets4) - this is no longer needed unless using Dimensions google sheets api again
library(dplyr)
library(stringr)
library(tidyr)
library(readr)
library(openxlsx)

#XXXXXXXXXXXXXXX
# Clear work space
rm(list=ls())

# IMPORT DIMENSIONS DATA----

# dimensions_uk <- read_tsv(file = "Data/Raw data/20210401 Dimensions export pubs UK ROs 2014-2020.tsv") # this is all articles associated with UK ROs 2014-2020 but not using this in current version of analysis

dimensions_ukri <- read_tsv(file = "Data/Raw data/20210401 Dimensions export pubs UKRI funded 2014-2020.tsv") # this is all articles associated with UKRI funding 2014-2020

# Make column names tidy - this is needed because the version sent by Dimensions had spaces in column titles
names(dimensions_ukri) <- make.names(names(dimensions_ukri), unique = TRUE)

# Rename to Dimensions
dimensions <- dimensions_ukri

# CLEANING DIMENSIONS DATA AND DERIVING NEW VARIABLES ----

# Remove duplicate rows---- 
  #(using doi and title - the latter may remove a few different articles which have the same name but manual checks suggest the vast majority of duplicate titles are the same article or things like corrections and they skew very heavily towards open access).
dimensions <- dimensions %>%
  arrange(rowSums(sapply((dimensions %>% select(-c(PMID, Country.of.Research.organization, Units.of.Assessment))), is.na))) %>% # this ensures that the final data has the minimum possible number of missing values (by ranking according to number of missing values (excluding variables we are less interested in))
  filter(!duplicated(DOI) & !(duplicated(Title)))

# Lowercase Source.title to aid matching with Sherpa----
dimensions$Source.title <- tolower(dimensions$Source.title)

# Removing 'all OA' from Dimensions OACs
dimensions <- dimensions %>%
  mutate(Open.Access = gsub("All OA; ", "", Open.Access))

# Recoding Open.Access into simpler form
dimensions$Open.Access2 <- dimensions$Open.Access
dimensions$Open.Access2[dimensions$Open.Access2 == "Hybrid"] <- "Hybrid gold" # to avoid confusion with journal type
dimensions$Open.Access2[dimensions$Open.Access2 == "Green, Published" | dimensions$Open.Access2 == "Green, Accepted"] <- "Green"
dimensions$Open.Access2[dimensions$Open.Access2 == "Bronze" | dimensions$Open.Access2 == "Green, Submitted" | dimensions$Open.Access2 == "Closed"] <- "Closed" #since these are all closed from the perspective of UKRI policy

# Creating new variable ukri_Funder---- 
  #pulling out the UKRI Funder for each article and cleaning it up (these will later be merged into ukri_Funder and then removed)
dimensions$AHRC[grepl("Arts and Humanities Research Council|AHRC", dimensions$Funder)] <- "AHRC"
dimensions$BBSRC[grepl("Biotechnology and Biological Sciences Research Council|BBSRC", dimensions$Funder)] <- "BBSRC"
dimensions$ESRC[grepl("Economic and Social Research Council|ESRC", dimensions$Funder)] <- "ESRC"
dimensions$EPSRC[grepl("Engineering and Physical Sciences Research Council|EPSRC", dimensions$Funder)] <- "EPSRC"
dimensions$Innovate_UK[grepl("Innovate UK", dimensions$Funder)] <- "Innovate UK"
dimensions$MRC[grepl("Medical Research Council|MRC", dimensions$Funder)] <- "MRC"
dimensions$NC3Rs[grepl("National Centre for the Replacement Refinement and Reduction of Animals in Research|NC3Rs", dimensions$Funder)] <- "NC3Rs"
dimensions$NERC[grepl("Natural Environment Research Council|NERC", dimensions$Funder)] <- "NERC"
dimensions$Research_England[grepl("Research England", dimensions$Funder)] <- "Research England"
dimensions$STFC[grepl("Science and Technology Facilities Council|STFC", dimensions$Funder)] <- "STFC"
dimensions$UKRI[grepl("UK Research and Innovation|UKRI", dimensions$Funder)] <- "UKRI"

  # pull it all together into one column ukri_funders then remove NAs
dimensions$ukri_funders <- paste(dimensions$AHRC, dimensions$BBSRC, dimensions$ESRC, dimensions$EPSRC, dimensions$Innovate_UK, dimensions$MRC, dimensions$NC3Rs, dimensions$NERC, dimensions$Research_England, dimensions$STFC, dimensions$UKRI, sep = ", ") 
dimensions$ukri_funders <- gsub("NA, ", "", dimensions$ukri_funders)
dimensions$ukri_funders <- gsub(", NA", "", dimensions$ukri_funders)

# editing FOR..ANZSRC..Categories (fields of research)----

  # Pull out divisions (dimensions$for_division)
dimensions <- dimensions %>% mutate(for_division = FOR..ANZSRC..Categories) %>%
  mutate(for_division = gsub("; [0-9]{4}.*", "", for_division)) %>% # this works because the FOR divisions (which are prefaced with 2 numbers) always come before the FOR groups (which are prefaced with 4)
  mutate(for_division = gsub(";$", "", for_division))

dimensions$for_division[is.na(dimensions$for_division)] <- "Missing"

  # Pull out groups (dimensions$for_group)
dimensions <- dimensions %>% mutate(for_group = str_extract_all(FOR..ANZSRC..Categories, "[0-9]{4}[:space:]{1}.*")) %>% # this works because the FOR divisions (which are prefaced with 2 numbers) always come before the FOR groups (which are prefaced with 4)
  mutate(for_group = gsub("character(0)", "No FOR group recorded", for_group, fixed = TRUE))

dimensions$for_group[is.na(dimensions$for_group)] <- "No FOR group recorded"

# Creating dimensions$discipline----
  #a proxy for discipline (NB. This is imperfect, in particular because 10 Technology is also part Life Sciences, 12 Built Environment is also part social sciences, and 17 Psychology is also part Life Sciences)

physical_sciences <- c("01 Mathematical Sciences", "02 Physical Sciences", "03 Chemical Sciences", "04 Earth Sciences", "05 Environmental Sciences", "08 Information and Computing Sciences", "09 Engineering", "10 Technology", "12 Built Environment and Design")
health_sciences <- c("11 Medical and Health Sciences")
social_sciences <- c("13 Education", "14 Economics", "15 Commerce, Management, Tourism and Services", "16 Studies in Human Society", "17 Psychology and Cognitive Sciences", "18 Law and Legal Studies")
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
  mutate(discipline = paste(physical, health, social, life, arts_humanities, sep = "; ")) %>%
  mutate(discipline = gsub("NA; ", "", discipline),
         discipline = gsub("; NA", "", discipline))

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

write.xlsx(dimensions_disciplines, file = "Output/Tables/Discipline frequency.xlsx")

# Breaking up Springer Nature----
  # Editing Publisher to split up Springer and Nature (this is important because as of Mar 2021 former has TA and latter doesn't)     
dimensions$Publisher[dimensions$Publisher == "Springer Nature" & grepl("nature", dimensions$Source.title)] <- "Nature"
dimensions$Publisher[dimensions$Publisher == "Springer Nature" & !grepl("nature", dimensions$Source.title)] <- "Springer"


# changing column order (this also serves to remove unnecessary columns. If any new columns added they will need to be added here)
col_order <- (c("Title", "DOI", "Publication.Date", "PubYear", "Source.title", "Publisher", "ISSN", "e.ISSN", "Publication.Type", 'Country.of.Research.organization', "FOR..ANZSRC..Categories", "for_division", "for_group", "discipline", "Open.Access", "Open.Access2", "Funder", "ukri_funders", "Units.of.Assessment"))
dimensions <- dimensions[, col_order]

openxlsx::write.xlsx(as.data.frame(dimensions), 'Data/Dimensions.xlsx')
save(dimensions, file = "Data/Dimensions.Rda")