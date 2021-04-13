# Testing Dimensions (2nd attempt for funding work)

# Clear work space
rm(list=ls())

# There is a big in Microsoft ODBC which seems to be fixed by reinstalling odbc each session https://github.com/r-dbi/odbc/issues/343
install.packages("odbc")

# Load packages
library(readr)
library(tidyverse)
library(odbc)
library(openxlsx)

# Import GtR from SQL published 2017-19----
  # Prerequisite - you must be set up on Azure and have ODBC Data Source Administrator set up for it

  # Connect to Azure Data Hub

    # Open an ODBC connection to the Data Hub
con <- DBI::dbConnect(odbc::odbc(), "Azure_UKRI_DataHub") # NB Name of ODBC will vary depending on how you set it up

  # Fetch data from SQL (this needs to be done soon after connecting)
gtr_2017_2019 <- odbc::dbGetQuery(conn = con,
                                           "SELECT PublicationTitle, PublicationType, DOI, PMID, ISBN, ISSN, Publisher, JournalTitle, DatePublished, GrantRefNumber
                                           FROM [GtR_WebSource].dbo.[Publication]
                                           WHERE DatePublished >= '20170101'
                                           AND DatePublished < '20200101' ")

  # Add column for publication year
gtr_2017_2019 <- gtr_2017_2019 %>% 
  mutate(PubYear = substr(DatePublished, 1, 4))

  # this code is needed to make to improve the merge later on (removes special characters and goes to lowercase) but it is better to do it now as it results in some duplicates being identified from GtR
gtr_2017_2019$DOI <- str_replace_all(gtr_2017_2019$DOI,"[^[:graph:]]", " ") 
gtr_2017_2019$PublicationTitle <- str_replace_all(gtr_2017_2019$PublicationTitle,"[^[:graph:]]", " ") 
gtr_2017_2019$DOI <- tolower(gtr_2017_2019$DOI)
gtr_2017_2019$PublicationTitle <- tolower(gtr_2017_2019$PublicationTitle)

  # Remove duplicates
gtr_2017_2019 <- gtr_2017_2019 %>%
  filter(!duplicated(DOI), !duplicated(PublicationTitle))

  # New variable for number of articles per journal
articles_per_journal = gtr_2017_2019 %>% group_by(JournalTitle) %>% count()
gtr_2017_2019 <- left_join(gtr_2017_2019, articles_per_journal, by = "JournalTitle") %>%
  rename(articles_in_journal = n)

  # Export data to xlsx
write.xlsx(gtr_2017_2019, "Data/Raw data/gtr_2017_2020.xlsx")

# Import Dimensions data 2017-19----
dim_ukri_2017_2019 <- read.xlsx("Data/Raw data/Dim_UKRI_2017_2020.xlsx") # when updating be careful to include all UKRI Dimensions (NOT UK filtered)

  # filter for 2017-2019
dim_ukri_2017_2019 <- dim_ukri_2017_2019 %>%
  filter(year %in% c(2017,2018,2019))

# Make DOIs and Titles lower case to improve merging
dim_ukri_2017_2019$doi <- tolower(dim_ukri_2017_2019$doi)
dim_ukri_2017_2019$title <- tolower(dim_ukri_2017_2019$title)

  # Remove duplicates
dim_ukri_2017_2019 <- dim_ukri_2017_2019 %>%
  filter(!duplicated(doi), !duplicated(title), !is.na(issn))

  # Remove unnecessary columns
dim_ukri_2017_2019 <- subset(dim_ukri_2017_2019, select = -c(dimensions_url, funder_countries, journal_id, linkout))

  # Add column for has UK author
dim_ukri_2017_2019$uk_author <- ifelse(grepl("United Kingdom", dim_ukri_2017_2019$research_org_country_names), "Yes", "No")


# Merge GtR and Dimensions data----

# Add unique ID columns to enable removal of duplicates
dim_ukri_2017_2019$dimID <- 1:nrow(dim_ukri_2017_2019)
gtr_2017_2019$gtrID <- 1:nrow(gtr_2017_2019)

# Series of inner joins to find all possible matches
gtr_dim1 <- inner_join(gtr_2017_2019, dim_ukri_2017_2019, by = c("DOI" = "doi"))
gtr_dim2 <- inner_join(gtr_2017_2019, dim_ukri_2017_2019, by = c("PublicationTitle" = "title"))

# Bind rows to bring together all matching rows, then keep only first match for each article

gtr_dim3 <- bind_rows(gtr_dim1, gtr_dim2) %>%
  filter(!duplicated(dimID), !duplicated(gtrID))

# Identify rows with no match
gtr_dim_missing_gtr <- anti_join(gtr_2017_2019, gtr_dim3, by = "gtrID")
gtr_dim_missing_dim <- anti_join(dim_ukri_2017_2019, gtr_dim3, by = "dimID")

# Merge these non-matched rows with the matched rows
gtr_dim <- bind_rows(gtr_dim_missing_gtr, gtr_dim3)
gtr_dim <- bind_rows(gtr_dim_missing_dim, gtr_dim)

# Create variables for whether each row has Dimensions and or GtR
gtr_dim$HasDim <- ifelse(gtr_dim$dimID > 0, "Yes", "No")
gtr_dim$HasGtr <- ifelse(gtr_dim$gtrID > 0, "Yes", "No")

rm(articles_per_journal, con, gtr_dim_missing_dim, gtr_dim_missing_gtr, gtr_dim1, gtr_dim2, gtr_dim3)

# Output merged gtr_dimensions data----
write.xlsx(gtr_dim, "Data/gtr_dim_merged.xlsx")
save(gtr_dim, file = "Data/gtr_dim.Rda")

# Analysis----

# import data
load("Data/gtr_dim.Rda")


# What proportion of publications are missing from dimensions and gtr

  # All publications
gtr_dim %>% group_by(HasDim, HasGtr) %>% count() %>% ungroup() %>% mutate(percent = round(n/sum(n)*100,1))

  # Articles etc
gtr_dim %>% 
  filter((grepl("Conference|Journal", PublicationType)) | (grepl("article", type))) %>%
    group_by(HasDim, HasGtr) %>% count() %>% ungroup() %>% mutate(percent = round(n/sum(n)*100,1))

# Long form (monographs, book chapters)
gtr_dim %>% 
  filter(grepl("Book", PublicationType)) %>%
  group_by(HasDim, HasGtr) %>% count() %>% ungroup() %>% mutate(percent = round(n/sum(n)*100,1))


  # Long form (monographs/ book chapters)