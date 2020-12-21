# Testing Dimensions' coverage compared to GtR
# Author: Tom Kenny
# Created: December 2020
# Last Updated: December 2020

# Purpose of code: This code merges data on UKRI funded articles from Gateway to Research and Dimensions for 2018 to explore differences in coverage. We were able to find a match in Dimensions for 89% of the articles in GtR from 2018

# Prerequisite: 



#XXXXXXXXXXXXXXX
# Clear work space
rm(list=ls())

# Set working directory
mainDir <- "C:\\Users\\TKen02\\UKRI\\Policy Analysis - Documents\\Open Access\\Projects\\Publication Venue Gap Analysis\\Data\\Original data\\GtR"
setwd(mainDir)

library(tidyverse)
library(openxlsx)


#XXXXXXXXX
# Import data----
gtr18 <- read.xlsx("gtr_ukri_2018.xlsx")
dim18 <- read.xlsx("dim_ukri_2018.xlsx")

# Format data----

  # Remove unncecessary columns
gtr18 <- subset(gtr18, select = -c(PublicationType, Volume, Issue, Pages, PMID, GtRPublicationURL, GtR.OutcomeId, ProjectId, FundingOrgID, LeadROID))

dim18 <- subset(dim18, select = -c(date, dimensions_url, funder_countries, journal_id, journal_lists, linkout, research_org_countries, research_org_country_names, supporting_grant_ids, type, year))

  # Remove duplicates
# gtr182 <- gtr18[!duplicated(gtr18$Title), ] # this removes almost half the records (from 79306 to 40566)
gtr18 <- gtr18[!duplicated(gtr18$DOI), ] # this removes over half the records (from 79306 to 36169 - this captures all of the ones removed due to title match also so no need to do that)
dim18 <- dim18[!duplicated(dim18$doi), ]

  # Reformat merging variables
gtr18$DOI <- tolower(gtr18$DOI)
dim18$doi <- tolower(dim18$doi)

gtr18$Title <- tolower(gtr18$Title)
dim18$title <- tolower(dim18$title)

  # New variable for number of articles per journal
articles_per_journal = gtr18 %>% group_by(JournalName) %>% count()
gtr18 <- left_join(gtr18, articles_per_journal, by = "JournalName") %>%
  rename(articles_in_journal = n)

# Merge data----

    # Series of inner joins to find all possible matches
gtr_test1 <- inner_join(gtr18, dim18, by = c("DOI" = "doi"))
gtr_test2 <- inner_join(gtr18, dim18, by = c("Title" = "title"))


# Bind rows to bring together all matching rows, then keep only first match for each article
gtr_test3 <- bind_rows(gtr_test1, gtr_test2) %>%
  filter(!duplicated(DOI)) # This has 32431 rows, meaning we were able to find a match in Dimensions for 89% of the articles in GtR

# Identify rows with no match
gtr_test0 <- anti_join(gtr18, gtr_test3, by = "DOI")

# Merge these non-matched rows with the matched rows
gtr_test <- bind_rows(gtr_test0, gtr_test3) # this has 35945 


# Characteristics of GtR articles with no match in Dimensions----

  # Funder
no_dim_match_funder <- gtr_test0 %>%
  count(FundingOrg) %>%
  mutate(percent = round(n/sum(n)*100,1))

gtr_funder <- gtr18 %>%
  count(FundingOrg) %>%
  mutate(percent = round(n/sum(n)*100,1))

no_dim_match_funder <- bind_cols(no_dim_match_funder, gtr_funder) %>%
  rename(n_no_match = n...2, percent_no_match = percent...3, percent_all_gtr = percent...6) %>%
  select(-4,-5)

      # Articles with no match in dimensions are 3 times more likely to be funded by AHRC and almost 3 times more likely to be funded by ESRC. Conversely the proportion of articles funded by MRC without a match in Dimensions is half the proportion in the full data. I.e. an article funded by MRC is 6 times less likely not to have a match than one funded by AHRC. 

  # Lead RO institution

no_dim_match_leadRO <- gtr_test0 %>%
  count(LeadRO) %>%
  mutate(percent = round(n/sum(n)*100,1))

gtr_leadRO <- gtr18 %>%
  count(LeadRO) %>%
  mutate(percent = round(n/sum(n)*100,1))

no_dim_match_leadRO <- inner_join(gtr_leadRO, no_dim_match_leadRO, by = "LeadRO")
        
        # No major difference here - top institutions roughly the same in missing data as full data - some slight differences which make sense based on the institutions (e.g. imperial have less missing)


  # Journals
no_dim_match_JournalName <- gtr_test0 %>%
  count(JournalName) %>%
  mutate(percent = round(n/sum(n)*100,1))

gtr_JournalName <- gtr18 %>%
  count(JournalName) %>%
  mutate(percent = round(n/sum(n)*100,1))

no_dim_match_JournalName <- inner_join(gtr_JournalName, no_dim_match_JournalName, by = "JournalName")

no_dim_match_JournalName <- left_join(no_dim_match_JournalName, gtr18[!duplicated(gtr18$JournalName), c('JournalName', 'articles_in_journal')], by = "JournalName")

no_dim_match_journal <- no_dim_match_JournalName %>%
  filter(articles_in_journal == 1) %>%
  count()

no_dim_match_articles_per_journal <- no_dim_match_JournalName %>%
  count(articles_in_journal) %>%
  mutate(percent = round(n/sum(n)*100,1))

gtr18_articles_per_journal <- gtr18 %>%
  count(articles_in_journal) %>%
  mutate(percent = round(n/sum(n)*100,1))

articles_per_journal_analysis <- bind_cols(gtr18_articles_per_journal[1:40, ], no_dim_match_articles_per_journal[1:40, ])


      # By funder
num_articles_funder <- gtr18 %>%
  filter(articles_in_journal %in% c(1:2)) %>%
  count(FundingOrg) %>%
  mutate(percent = round(n/sum(n)*100,1))

gtr_funder <- gtr18 %>%
  count(FundingOrg) %>%
  mutate(percent = round(n/sum(n)*100,1))

no_dim_match_articles_funder <- bind_cols(num_articles_funder, gtr_funder) %>%
  rename(n_no_match = n...2, percent_no_match = percent...3, percent_all_gtr = percent...6) %>%
  select(-4,-5)


      # There are some journals which are all or almost all missing from Dimensions. Crucially this includes a lot of journals with a small number of UKRI articles

# Characteristics of Dimensions articles with no match in GtR----
