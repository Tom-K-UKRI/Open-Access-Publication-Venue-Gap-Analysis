# Testing Dimensions' coverage compared to GtR
# Author: Tom Kenny
# Created: December 2020
# Last Updated: December 2020

# Purpose of code: This code merges data on UKRI funded articles from Gateway to Research and Dimensions for 2018 to explore differences in coverage. We were able to find a match in Dimensions for 89% of the articles in GtR from 2018

# Prerequisite: 



#XXXXXXXXXXXXXXX
# Clear work space
rm(list=ls())


library(tidyverse)
library(openxlsx)
library(readr)
library(janitor)


#XXXXXXXXX
# Import data----
gtr <- read_csv("Data/Raw data/gtr_2016_2020.csv")
dim18 <- read_tsv("Data/Raw data/20210401 Dimensions export pubs UKRI funded 2014-2020.tsv") %>%
  filter(PubYear == 2018) %>%
  clean_names()

load("Data/Raw data/TEMP_oa_block_grant_articles.Rda")

# Format data----

  # Remove duplicates
gtr <- gtr %>% distinct(DOI, .keep_all = TRUE) %>% select(DOI, Title, Year, JournalName)
dim18 <- dim18 %>% distinct(DOI, .keep_all = TRUE) %>% select(doi, title, source_title, pub_year, open_access)
oabg <- oa_block_grant_articles %>% distinct(doi, .keep_all = TRUE) %>% select(doi, title, year)

  # Reformat merging variables
gtr$Title <- tolower(gtr$Title)
dim18$title <- tolower(dim18$title)
oabg$title <- tolower(oabg$title)

oabg_18 <- oabg %>% filter(year == 2018)

  # New variable for number of articles per journal
articles_per_journal = gtr %>% group_by(JournalName) %>% count()
gtr <- left_join(gtr, articles_per_journal, by = "JournalName") %>%
  rename(articles_in_journal = n)

# Merge data----

    # Series of inner joins to find all possible matches
gtr_test1 <- inner_join(gtr, dim18, by = c("DOI" = "doi"))
gtr_test2 <- inner_join(gtr, dim18, by = c("Title" = "title"))

gtr_test_bg_1 <- inner_join(gtr, oabg_18, by = c("DOI" = "doi"))
gtr_test_bg_2 <- inner_join(gtr, oabg_18, by = c("Title" = "title"))


# Bind rows to bring together all matching rows, then keep only first match for each article
gtr_test3 <- bind_rows(gtr_test1, gtr_test2) %>%
  filter(!duplicated(DOI)) # This has 33529 rows, meaning we were able to find a match in GtR for 70% of articles in Dimensions

gtr_test_bg_3 <- bind_rows(gtr_test_bg_1, gtr_test_bg_2) %>%
  filter(!duplicated(DOI)) # This has 9826 rows meaning we were able to find a match for 68% of block grant returns in Dimensions

# Identify rows with no match
gtr_test0 <- anti_join(gtr, gtr_test3, by = "DOI")

# Merge these non-matched rows with the matched rows
gtr_test <- bind_rows(gtr_test0, gtr_test3) # this has 35945 


# merge block grant and gateway to research and compare this to Dimensions
oabg_gtr <- oa_block_grant_articles %>%
  distinct(doi, .keep_all = TRUE) %>% select(doi, title) %>% rename(DOI = doi, Title = title) %>%
  bind_rows(gtr %>% distinct(DOI, .keep_all = TRUE) %>% select(DOI, Title)) %>%
  distinct(DOI, .keep_all = TRUE) %>%
  distinct(Title, .keep_all = TRUE)
  
# Series of inner joins to find all possible matches
gtr_bg_dim_test1 <- inner_join(oabg_gtr, dim18, by = c("DOI" = "doi"))
gtr_bg_dim_test2 <- inner_join(oabg_gtr, dim18, by = c("Title" = "title"))

# Bind rows to bring together all matching rows, then keep only first match for each article
gtr_bg_dim_test3 <- bind_rows(gtr_bg_dim_test1, gtr_bg_dim_test2) %>%
  filter(!duplicated(DOI)) # This has 33529 rows meaning a match with 77% of Dimensions

gtr_bg_dim_test0 <- anti_join(oabg_gtr, gtr_bg_dim_test3, by = "DOI")



# Testing dimensions
# Import data----
gtr <- read_csv("Data/Raw data/gtr_2016_2020.csv")
dim <- read_tsv("Data/Raw data/20210401 Dimensions export pubs UKRI funded 2014-2020.tsv") %>%
  clean_names()
load("Data/Raw data/TEMP_oa_block_grant_articles.Rda")

dim_clean <- dim %>% distinct(doi, .keep_all = TRUE) %>% distinct(title, .keep_all = TRUE)
gtr_18 <- gtr %>% filter(Year == 2018) %>% distinct(DOI, .keep_all = TRUE) %>% distinct(Title, .keep_all = TRUE)
oabg_18 <- oa_block_grant_articles %>% filter(year == 2018) %>% distinct(doi, .keep_all = TRUE) %>% distinct(title, .keep_all = TRUE)

dim_gtr_test1 <- inner_join(dim, gtr_18, by = c("doi" = "DOI"))
dim_gtr_test2 <- inner_join(dim, gtr_18, by = c("title" = "Title"))

# Bind rows to bring together all matching rows, then keep only first match for each article
dim_gtr_test3 <- bind_rows(dim_gtr_test1, dim_gtr_test2) %>%
  filter(!duplicated(doi)) # This has 33565 rows, meaning we were able to find 88% of 2018 GtR articles in Dimensions 2018

dim_bg_test1 <- inner_join(dim, oabg_18, by = c("doi" = "doi"))
dim_bg_test2 <- inner_join(dim, oabg_18, by = c("title" = "title"))

# Bind rows to bring together all matching rows, then keep only first match for each article
dim_bg_test3 <- bind_rows(dim_bg_test1, dim_bg_test2) %>%
  filter(!duplicated(doi)) # This has 13085 rows meaning we can find 91% of 2018 BG articles in Dimension



# EXploring articles with no match in Dimensions----

dim_gtr_test1 <- inner_join(dim, gtr_18, by = c("doi" = "DOI"))
dim_gtr_test2 <- inner_join(dim, gtr_18, by = c("title" = "Title"))

# Bind rows to bring together all matching rows, then keep only first match for each article
dim_gtr_test3 <- bind_rows(dim_gtr_test1, dim_gtr_test2) %>%
  filter(!duplicated(doi)) # This has 33565 rows, meaning we were able to find 88% of 2018 GtR articles in Dimensions 2018

in_gtr_missing_from_dimensions <- anti_join(gtr_18, dim_gtr_test3, by = c("DOI" = "doi"))







# Characteristics of GtR articles with no match in Dimensions----

no_dim_match <- gtr_test0 %>%
  count() %>%
  mutate(percent = round(n/nrow(gtr18)*100,1))

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

no_dim_match_funder <- no_dim_match_funder %>%
  mutate(prop_missing = round(n_no_match / gtr_funder$n*100,1))



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
