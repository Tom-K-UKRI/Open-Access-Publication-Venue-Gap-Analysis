# SHERPA ALL POLICIES  TESTING

# Author: Tom Kenny
# Created: November 2020
# Last Updated: November 2020

# Purpose of code: This code is for testing wider green policies other than just the most permissive, in particular to explore the assumption that no requirement is the most  permissive option.

# Prerequisite: this relies on the dataset sherpa_all_policies (downloaded using code SHERPA RoMEO API and cleaning)

#XXXXXXXXXXXXXXX
# Clear work space
rm(list=ls())

library(tidyverse)
library(psych)
library(openxlsx)
library(janitor)

# IMPORTING SHERPA ROMEO DATA AND EDITING VARIABLES----
# NB. See code 'Sherpa Romeo API (clean)' for how to download this data'
# NB2. We were advised by JISC that missing data should be interpreted as 'no requirement' which means missing is actually equivalent to the most permissive policy option.

sherpa <- read.xlsx("Data/Raw data/sherpa_all_policies.xlsx") # this is data created by the R code file named SHERPA RoMEO API (clean)

# would try to remove duplicates (where multiple sherpa ids exist for the same journal) but don't know how to do this with multiple policies per journal

# unique identifier for each policy
sherpa$unique_id <- 1:nrow(sherpa)

#a. general cleaning of data----
# rename title and id to avoid confusion with title in Dimensions
sherpa <- rename(sherpa, j_title = title, sherpa_id = id, sherpa_publisher = name)

# Lowercase j_title to aid matching with Dimensions
sherpa$j_title <- tolower(sherpa$j_title)

# Remove unnecessary columns
sherpa <- sherpa[,!(names(sherpa) %in% c("issn_NA","issn_legacy", "pubpol_id"))]

# Clean up issn codes - this is needed to enable merged with Dimensions on issn
sherpa$issn_print <- gsub("-", "", sherpa$issn_print)
sherpa$issn_electronic <- gsub("-", "", sherpa$issn_electronic)

# Recode embargo to simpler form (first 0, 6, 12, >12, then just zero or not)
sherpa$embargo2 <- NA
sherpa$embargo2[sherpa$embargo == 0] <- 0
sherpa$embargo2[sherpa$embargo > 0 & sherpa$embargo <= 6] <- 6
sherpa$embargo2[sherpa$embargo > 6 & sherpa$embargo <= 12] <- 12
sherpa$embargo2[sherpa$embargo > 12] <- 13

# Binary embargo variable
sherpa$embargo3[sherpa$embargo == 0] <- "zero embargo"
sherpa$embargo3[sherpa$embargo != 0] <- "has embargo"

#b. Work with license variable----
# Create new columns recoding license into CC-BY, CC-BY-ND, CC-BY-SA and license_not_compliant (return missing data as CC-BY(m))
# NB Jisc advised us that missing means no requirement making it the most permissive category
# Alternative: could do a case_when to clean this up and avoid for loop

# clean up sherpa$license - this all just standardises the way licenses are described - making it all lower case, getting rid of hyphens and underscore. We also change blank values into no license requirement since JISC advised this is correct.
sherpa$license <- tolower(sherpa$license) 
sherpa$license <- gsub("-", "", sherpa$license)
sherpa$license <- gsub(" ", "", sherpa$license)
sherpa$license <- gsub("_", "", sherpa$license)
sherpa$license <- gsub(",", ", ", sherpa$license)
sherpa$license[sherpa$license == ""] <- "no license requirement"
sherpa$license[is.na(sherpa$license)] <- "no license requirement"

# Derive license1 to simplify license and only return one license for each policy (the most permissive)
sherpa$license1 <- sherpa$license
sherpa$license1[grepl("ccby,", sherpa$license1)] <- "cc_by"
sherpa$license1[grepl("publicdomain,", sherpa$license1)] <- "cc_by"
sherpa$license1[grepl("gnugpl,", sherpa$license1)] <- "cc_by"
sherpa$license1[grepl("ccbynd,", sherpa$license1)] <- "cc_by_nd"
sherpa$license1[grepl("ccbysa,", sherpa$license1)] <- "cc_by_sa"
sherpa$license1[grepl("ccbync,", sherpa$license1)] <- "cc_by_nc"
sherpa$license1[grepl("ccbyncnd,", sherpa$license1)] <- "cc_by_nc_nd"
sherpa$license1[grepl("ccbyncsa,", sherpa$license1)] <- "cc_by_nc_sa"
sherpa$license1[grepl("bespokelicense,", sherpa$license1)] <- "bespoke_license"
sherpa$license1[grepl("allrightsreserved,", sherpa$license1)] <- "all_rights_reserved"

# Simplify license further into new var license2 merging all licenses which are never compliant
sherpa$license2 <- sherpa$license1
sherpa$license2[sherpa$license2 == "no license requirement"] <- "cc_by"
sherpa$license2[sherpa$license2 != "cc_by" & sherpa$license2 != "cc_by_nd" & sherpa$license2 != "cc_by_nc"] <- "no compliant license"

# Binary license variable - is cc_by or not
sherpa$license3[sherpa$license2 != "cc_by"] <- "not cc_by"
sherpa$license3[sherpa$license2 == "cc_by"] <- "cc_by"


#c. Preparing other variables to rank Sherpa policies ----

# Create new column recoding repository into compliant and not compliant. The list of compliant repositories was agreed with Rachel Bruce (essentially it's proper repositories rather than author or institutions homepages) 
sherpa$location.location[sherpa$location.location == ""] <- "no repository requirement"
sherpa$location.location[is.na(sherpa$location.location)] <- "no repository requirement"

sherpa_repositories <- sherpa %>%
  select(c('sherpa_id', 'location.location', 'unique_id' ,'additional_oa_fee'))
sherpa_repositories$no_repository_requirement[sherpa$location.location == "no repository requirement"] <- 1
sherpa_repositories$academic_social_network[grepl("academic_social_network", sherpa_repositories$location.location)] <- 1
sherpa_repositories$any_repository[grepl("any_repository", sherpa_repositories$location.location)] <- 1
sherpa_repositories$any_website[grepl("any_website", sherpa_repositories$location.location)] <- 1
sherpa_repositories$authors_homepage[grepl("authors_homepage", sherpa_repositories$location.location)] <- 1
sherpa_repositories$funder_designated_location[grepl("funder_designated_location", sherpa_repositories$location.location)] <- 1
sherpa_repositories$institutional_repository[grepl("institutional_repository", sherpa_repositories$location.location)] <- 1
sherpa_repositories$institutional_website[grepl("institutional_website", sherpa_repositories$location.location)] <- 1
sherpa_repositories$named_academic_social_network[grepl("named_academic_social_network", sherpa_repositories$location.location)] <- 1
sherpa_repositories$named_repository[grepl("named_repository", sherpa_repositories$location.location)] <- 1
sherpa_repositories$non_commercial_institutional_repository[grepl("non_commercial_institutional_repository", sherpa_repositories$location.location)] <- 1
sherpa_repositories$non_commercial_repository[grepl("non_commercial_repository", sherpa_repositories$location.location)] <- 1
sherpa_repositories$non_commercial_social_network[grepl("non_commercial_social_network", sherpa_repositories$location.location)] <- 1
sherpa_repositories$non_commercial_subject_respository[grepl("non_commercial_subject_repository", sherpa_repositories$location.location)] <- 1
sherpa_repositories$non_commercial_website[grepl("non_commercial_website", sherpa_repositories$location.location)] <- 1
sherpa_repositories$preprint_repository[grepl("preprint_respository", sherpa_repositories$location.location)] <- 1
sherpa_repositories$subject_repository[grepl("subject_repository", sherpa_repositories$location.location)] <- 1
sherpa_repositories$this_journal[grepl("this_journal", sherpa_repositories$location.location)] <- 1
sherpa_repositories$oa_journal[grepl("oa_journal", sherpa_repositories$location.location)] <- 1

compliant_repository <- c("any_website", "any_repository", "institutional_repository", "non_commericial_institutional_repository", "non_commercial_repository", "non_commercial_subject_repository", "subject_repository", "non_commerical_website", "funder_designated_location", "preprint_repository", "no repository requirement")
sherpa$compliant_repository <- grepl(paste(compliant_repository, collapse = "|"), sherpa$location.location)

# Create new columns recoding article_version into version (only includes published vs does not include published)
sherpa$article_version[sherpa$article_version == ""] <- "no version specified"
sherpa$article_version[is.na(sherpa$article_version)] <- "no version specified"
sherpa$version_published <- grepl(paste(c("published", "no version specified"), collapse = "|"), sherpa$article_version)

# Replace empty results for copyright and create new simplified copyright column (either authors or publishers)
sherpa$copyright_owner[sherpa$copyright_owner == "" | is.na(sherpa$copyright_owner)] <- "no copyright requirement"

sherpa$copyright <- sherpa$copyright_owner
sherpa$copyright[sherpa$copyright == "authors" | sherpa$copyright == "no copyright requirement"] <- "authors"
sherpa$copyright[sherpa$copyright != "authors" & sherpa$copyright != "no copyright requirement"] <- "publishers"

# DESCRIBING ALL POLICIES----

# Create variable for number of policies per journal
num_policies <- sherpa %>%
  group_by(j_title) %>%
  count() %>%
  rename(num_policies = n)

sherpa <- left_join(sherpa, num_policies)

# Basic descriptive statistics for policies grouped by journal
journal_stats <- psych::describe(sherpa$num_policies)
  # Min = 1, Max = 36, Median = 6, Mean = 6, SD = 3.8

# All journals open access prohibited?
  # Only 421 journals have oa prohibited out of 31,037
open_access_prohibited <- sherpa %>%
  filter(!duplicated(sherpa_id)) %>%
  count(open_access_prohibited)

# All policies license
all_policies_license <- sherpa %>%
  filter(!is.na(additional_oa_fee) & !(listed_in_doaj == "yes" & additional_oa_fee == "no")) %>%
  group_by(additional_oa_fee) %>%
  count(license1) %>%
  mutate(percent = round(n/sum(n)*100,1)) %>%
  filter(grepl("cc_by", license1) | license1 == "no license requirement")

# All policies article version
all_policies_version <- sherpa %>%
  filter(!is.na(additional_oa_fee) & !(listed_in_doaj == "yes" & additional_oa_fee == "no")) %>%
  group_by(additional_oa_fee) %>%
  count(article_version) %>%
  mutate(percent = round(n/sum(n)*100,1))

# All policies copyright owner
all_policies_copyright <- sherpa %>%
  filter(!is.na(additional_oa_fee) & !(listed_in_doaj == "yes" & additional_oa_fee == "no")) %>%
  group_by(additional_oa_fee) %>%
  count(copyright_owner) %>%
  mutate(percent = round(n/sum(n)*100,1))



# GREEN ANALYSIS----

# All policies repository locations
fee_policies_repositories1 <- sherpa_repositories %>% filter(additional_oa_fee == "yes")
fee_policies_repositories2 <- colSums(fee_policies_repositories1[, -c(1,2,3,4)], na.rm = TRUE)
fee_policies_repositories <- bind_rows(fee_policies_repositories1, fee_policies_repositories2)
fee_policies_repositories[33874,c(1,2,3,4)] <- "TOTALS"

green_policies_repositories1 <- sherpa_repositories %>% filter(additional_oa_fee == "no")
green_policies_repositories2 <- colSums(green_policies_repositories1[, -c(1,2,3,4)], na.rm = TRUE)
green_policies_repositories <- bind_rows(green_policies_repositories1, green_policies_repositories2)
green_policies_repositories[80530,c(1,2,3,4)] <- "TOTALS"

all_policies_repositories <- bind_rows(fee_policies_repositories[33874,-c(1,2,3,4)], green_policies_repositories[80530,-c(1,2,3,4)])

all_policies_repositories <- all_policies_repositories %>%
  mutate(
    percent_no_repository_requirement = round(no_repository_requirement/114402*100,0), 
    percent_academic_social_network = round(academic_social_network/114402*100,0), 
    percent_any_repository = round(any_repository/114402*100,0), 
    percent_any_website = round(any_website/114402*100,0), 
    percent_authors_homepage = round(authors_homepage/114402*100,0), 
    percent_funder_designated_location = round(funder_designated_location/114402*100,0), 
    percent_institutional_repository = round(institutional_repository/114402*100,0), 
    percent_institutional_website = round(institutional_website/114402*100,0), 
    percent_named_academic_social_network = round(named_academic_social_network/114402*100,0), 
    percent_named_repository = round(named_repository/114402*100,0), 
    percent_non_commercial_institutional_repository = round(non_commercial_institutional_repository/114402*100,0), 
    percent_non_commercial_repository = round(non_commercial_repository/114402*100,0),
    percent_non_commercial_social_network = round(non_commercial_social_network/114402*100,0),
    percent_non_commercial_subject_respository = round(non_commercial_subject_respository/114402*100,0),
    percent_non_commercial_website = round(non_commercial_website/114402*100,0),
    percent_preprint_repository = round(preprint_repository/114402*100,0),
    percent_subject_repository = round(subject_repository/114402*100,0),
    percent_this_journal = round(this_journal/114402*100,0)) %>%
  select(-20) %>%
  relocate(1,2,20,3,21,4,22,5,23,6,24,7,25,8,26,9,27,10,28,11,29,12,30,13,31,14,32,15,33,16,34,17,35,18,36,19)

all_policies_repositories2 <- all_policies_repositories %>% 
  rownames_to_column() %>% 
  gather(variable, value, -rowname) %>% 
  spread(rowname, value) %>%
  rename(paid_oa = '1', green_oa = '2') %>%
  mutate(percent_paid = round(paid_oa/33874*100,0), percent_green = round(green_oa/80530*100,0)) %>%
  relocate(1,3,2,4)         
           
           

rm(green_policies_repositories1, green_policies_repositories2, fee_policies_repositories1, fee_policies_repositories2)

# Checking additional oa fee policies to see if any of them involve embargos
# 65/ 33873 policies have embargos
fee_policies_test <- sherpa %>%
  filter(additional_oa_fee == "yes") %>%
  count(embargo)

# Fee policies license
# 70% cc_by, 22% cc_by_nc_nd, 6% cc_by_nc, 2% no license requirement


# Fee policies article version = 99% published
fee_policies_version <- sherpa %>%
  filter(additional_oa_fee == "no") %>%
  count(article_version) %>%
  mutate(percent = round(n/sum(n)*100,1))

# Fee policies copyright owner - 40% authors + 49% no requirement
fee_policies_copyright <- sherpa %>%
  filter(additional_oa_fee == "no") %>%
  count(copyright_owner) %>%
  mutate(percent = round(n/sum(n)*100,1))

# Are policies associated with a fee similar for each publisher? A: No it looks like virtually all publishers offer different gold policies with different gold licenses
fee_policies_publisher <- sherpa  %>%
  group_by(sherpa_publisher) %>%
  count(license1)




