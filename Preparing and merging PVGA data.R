# PREPARING AND MERGING DATA FOR PUBLICATION VENUE GAP ANALYSIS

# Author: Tom Kenny
# Created: August 2020
# Last Updated: November 2020

# Project: this is part of the R Project OA publication venue gapa analysis

# Purpose of code: This code prepares data from Dimensions, SHERPA RoMEO and ESAC to be used for analysing open access policies. It then merges the three data together into a merged_pvga dataset which can be usef for publication venue gap analysis

# Prerequisite: This code requires data from Dimensions and SHERPA RoMEO - the former was downloaded using the Google Sheets API for Dimensions to download all articles associated with UKRI funder group from 2017-2020. The latter is downloaded via API using Tom Kenny's code (SHERPA RoMEO API (clean)). Both data are saved in the Publication Venue Gap Analysis folder in the Policy Analysis drive.

# Clear work space
rm(list=ls())

#library(readr)
library(tidyverse)
library(openxlsx)
library(xml2)
library(rvest)

######### 1. IMPORTING DIMENSIONS DATA----
# NB. This data was downloaded and cleaned in script "Downloading and cleaning Dimensions data"

load("Data/Dimensions.Rda")

#XXXXXXXXXXX
# 2. DOWNLOADING AND PREPARING ESAC DATA----

library(rvest)

  # Download ESAC data - this downloads the latest version - last time this code was updated there were 19 publishers with TAs negotiated by JISC. If this number goes up in the future we will need to check if the publisher names match the names in dimensions or else they won't merge properly and there won't be an error to highlight this (see code below). There are also some additional agreements not on ESAC, most notably Royal Society of Chemistry which are not included in this.
esac_url <- "https://esac-initiative.org/about/transformative-agreements/agreement-registry/"
esac <- xml2::read_html(esac_url)
esac <- rvest::html_table(esac)[[1]] %>%
  tibble::as_tibble(.name_repair = "unique")

  # Select only JISC-negotiated deals
esac <- dplyr::filter(esac, Organization == "Jisc")

  # Rename 'publisher' names to match Dimensions data
esac$Publisher <- gsub("Association for Computing Machinery", "Association for Computing Machinery (ACM)", esac$Publisher)
esac$Publisher <- gsub("Company of Biologists", "The Company of Biologists", esac$Publisher)
esac$Publisher <- gsub("European Respiratory Society", "European Respiratory Society (ERS)", esac$Publisher)
esac$Publisher <- gsub("Karger", "Karger Publishers", esac$Publisher)
esac$Publisher <- gsub("Sage", "SAGE Publications", esac$Publisher)
esac$Publisher <- gsub("The Geological Society of London", "Geological Society of London", esac$Publisher)
esac$Publisher <- gsub("Cold Spring Harbor Laboratory Press", "Cold Spring Harbor Laboratory", esac$Publisher)
esac$Publisher <- gsub("Walter de Gruyter", "De Gruyter", esac$Publisher)


  # Create new column has_ta = yes. This means that when it is merged into the dimensions data it will highlight all the articles with a publisher with ta.
esac$has_ta <- "yes"

  # Remove duplicates (Karger has two agreements which might create duplicates in the merge)
esac <- esac[!duplicated(esac$Publisher, incomparables = NA), ]

#XXXXXXXXXXXX
# 3. PREPARING SHERPA ROMEO DATA----
  # NB. See code 'Sherpa Romeo API (clean)' for how to download this data'
  # NB2. We were advised by JISC that missing data should be interpreted as 'no requirement' which means missing is actually equivalent to the most permissive policy option.

sherpa <- read.xlsx("Data/Raw data/sherpa_all_policies.xlsx") # this is data created by the R code file named SHERPA RoMEO API (clean)

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
  
    # Simplify license further into new var license2 merging all licenses which are never compliant in the new policy scenarios
sherpa$license2 <- sherpa$license1
sherpa$license2[sherpa$license2 != "cc_by" & sherpa$license2 != "cc_by_nd" & sherpa$license2 != "cc_by_nc"] <- "no compliant license"

    # Binary license variable - is cc_by or isn't
sherpa$license3[sherpa$license2 != "cc_by"] <- "not cc_by"
sherpa$license3[sherpa$license2 == "cc_by"] <- "cc_by"

#c. Preparing other variables to rank Sherpa policies ----

    # Create new column recoding repository into compliant and not compliant. The list of compliant repositories was agreed with Rachel Bruce (essentially it's proper repositories rather than author or institutions homepages) 
sherpa$location.location[sherpa$location.location == ""] <- "no repository requirement"
sherpa$location.location[is.na(sherpa$location.location)] <- "no repository requirement"

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


#d. Create new column to rank Green OA policies----

  # Select only policies where green OA is relevant (additional_oa_fee = no). I originally also excluded listed_in_doaj here but then realised that they many pure gold journals still do have green OA policies (this is presumably why they are in sherpa - in fact almost none of the listed_in_doaj policies are associated with a fee). Also remove policies relating to submitted articles (these aren't peer reviewed)
sherpa_green <- sherpa %>%
  filter(additional_oa_fee == "no", article_version != "submitted")

  #Create green_ranking which will be used to select the most permissive green policy for each journal

      # Pull out options for each key variable we will ultimately match on (this is only to help fill the strings below)
# print(sherpa_green$license2[!duplicated(sherpa_green$license2)])
# print(sherpa_green$embargo2[!duplicated(sherpa_green$embargo2)])
# print(sherpa_green$compliant_repository[!duplicated(sherpa_green$compliant_repository)])
# print(sherpa_green$copyright[!duplicated(sherpa_green$copyright)])
# print(sherpa_green$version_published[!duplicated(sherpa_green$version_published)])
# print(sherpa_green$embargo3[!duplicated(sherpa_green$embargo3)])
# print(sherpa_green$license3[!duplicated(sherpa_green$license3)])

require(utils)

      # Use expand.grid to create table with every option. NB. The order of both the variables and strings is important here - the most important variables need to come last for the ordering to work and the most permissive options need to come first within each string.
green_ranking <- expand.grid(version_published = c(TRUE, FALSE), copyright = c("authors", "publishers"), compliant_repository = c(TRUE, FALSE), embargo2 = c(0, 6, 12, 13), license1 = c("cc_by", "cc_by_nd", "cc_by_nc", "cc_by_sa", "cc_by_nc_nd", "cc_by_nc_sa", "bespoke license", "all rights reserved", "no license requirement"), embargo3 = c("zero embargo", "has embargo"), license3 = c("cc_by", "not cc_by"))

      # Since the observations are ordered by permissiveness, this code simply adds a number for each row which will be the rank
green_ranking <- green_ranking %>%
  mutate(rank_green = 1:nrow(green_ranking))

      # Merge green_ranking into sherpa_green to rank all policies
sherpa_green_ranked <- left_join(sherpa_green, green_ranking)

      # Sort by rank_green and select only the first policy for each journal so we have the most permissive for each
sherpa_green_ranked <- sherpa_green_ranked[order(sherpa_green_ranked$rank_green),]

    # Select only first policy from sherpa_green_ranked (if two policies are in the same rank then this will just pick the first since they are essentially the same for our purposes)
sherpa_green_1row <- sherpa_green_ranked[!duplicated(sherpa_green_ranked$sherpa_id, incomparables = NA), ]


    # rename columns in sherpa_green_ranked to avoid confusion to make it clear which policy is which
sherpa_green_1row <- sherpa_green_1row %>% 
  rename(g_article_version = article_version, g_license = license, g_license1 = license1, g_license2 = license2, g_license3 = license3, g_copyright_owner = copyright_owner, g_conditions = conditions, g_location.location = location.location,  g_embargo.amount = embargo.amount, g_embargo.units = embargo.units, g_embargo = embargo, g_embargo2 = embargo2, g_embargo3 = embargo3, g_compliant_repository = compliant_repository, g_copyright = copyright, g_version_published = version_published)

#e. Create new column to rank paid OA policies----
  #(based on correspondence with JISC we think this is mostly hybrid journals with APCs - fees associated with pure gold OA are not generally recorded as additional_oa_fee)
  # Cutting out additional_oa_fee = "no" will cut out lots of policies from pure gold journals which don't have additional fees because all their articles are associated with fees. So we need to get these back in (see filter below).

  # Select only policies where paid OA is relevant (additional_oa_fee = yes or it's published only version of a pure gold journal, and remove submitted). 
sherpa_fee <- sherpa %>%
  filter((additional_oa_fee == "yes" | (listed_in_doaj == "yes" & article_version == "published")) & article_version != "submitted" & article_version != "accepted")

    # Create column 'rank_fee' to rank policies from most to least permissive

      # Use expand.grid to create table with every option. NB. The order is important here - the most important variables need to come last for the ordering to work. The only variables we are interested in here are license (which must be cc_by) and copyright retention (authors preferable)
fee_ranking <- expand.grid(copyright = c("authors", "publishers"), license3 = c("cc_by", "not cc_by"))

fee_ranking <- fee_ranking %>%
  mutate(rank_fee = 1:nrow(fee_ranking))

    # Merge fee_ranking into sherpa_fee to rank all policies
sherpa_fee_ranked <- left_join(sherpa_fee, fee_ranking)

    # Sort sherpa_fee  by rank (since we will select only the first/ most permissive policy for each)
sherpa_fee_ranked <- sherpa_fee_ranked[order(sherpa_fee_ranked$rank_fee),]

    # Select only first policy from each (if two policies are in the same rank then this will just pick the first since they are essentially the same for our purposes)
sherpa_fee_1row <- sherpa_fee_ranked[!duplicated(sherpa_fee_ranked$sherpa_id, incomparables = NA), ]

    # rename columns in sherpa_fee to avoid confusion to make it clear which policy is which
sherpa_fee_1row <- sherpa_fee_1row %>%
  rename(fee_article_version = article_version, fee_license = license, fee_license1 = license1, fee_license2 = license2, fee_license3 = license3, fee_copyright_owner = copyright_owner, fee_conditions = conditions, fee_location.location = location.location,  fee_embargo.amount = embargo.amount, fee_embargo.units = embargo.units, fee_embargo = embargo, fee_embargo2 = embargo2, fee_embargo3 = embargo3, fee_compliant_repository = compliant_repository, fee_copyright = copyright, g_version_published = version_published)


#f. Final process of reducing sherpa to one row per journal----

  # Return only one row for each journal for sherpa and remove columns that vary by policy
sherpa_1row <- sherpa[!duplicated(sherpa$sherpa_id, incomparables = NA), ]
sherpa_1row <- sherpa_1row[,!(names(sherpa) %in% c("additional_oa_fee", "article_version", "license", "copyright_owner", "conditions", "location.location", "embargo.amount", "embargo.units", "embargo", "embargo1", "embargo2", "embargo3",  "compliant_license", "license1", "license2", "license3", "compliant_repository", "copyright", "version_published"))]
sherpa_all_policies <- sherpa # this is just renaming it to make it clearer which is which
rm(sherpa)

  # Merge sherpa_green and sherpa_fee into sherpa (essentially this first chooses the first for each of green and fee then merge fee into green and then that into sherpa)
sherpa_1row <- left_join(sherpa_1row, sherpa_green_1row[ , c("sherpa_id", "g_article_version", "g_version_published", "g_license", "g_license1", "g_license2", "g_license3", "g_copyright_owner", "g_conditions", "g_location.location", "g_embargo.amount", "g_embargo.units", "g_embargo", "g_embargo2","g_embargo3", "g_compliant_repository", "g_copyright", "rank_green")], by = "sherpa_id")

sherpa_1row <- left_join(sherpa_1row, sherpa_fee_1row[ , c("sherpa_id", "fee_article_version", "fee_license", "fee_license1", "fee_license2", "fee_license3", "fee_copyright_owner", "fee_conditions",  "fee_copyright", "rank_fee")], by = "sherpa_id")

# Remove duplicated rows -  I was told by JISC that this had been fixed (i.e. duplicate rows had been removed) but judging from what happened when I reran the code this is not correct. Since this hasn't been fixed I've added another command which sorts by permissibility (i.e. meaning that most permissive of duplicates will be retained)
sherpa_1row <- sherpa_1row[order(sherpa_1row$rank_green, sherpa_1row$rank_fee),]
sherpa_1row <- sherpa_1row[!duplicated(sherpa_1row$issn_print, incomparables = NA), ]
sherpa_1row <- sherpa_1row[!duplicated(sherpa_1row$issn_electronic, incomparables = NA), ]
    
    # renaming duplicate journal titles (as some seperate journals have the same name so you can't just delete duplicates)
sherpa_1row$j_title <- with(sherpa_1row, make.unique(as.character(j_title)))

#openxlsx::write.xlsx(as.data.frame(sherpa_1row), 'sherpa_1row.xlsx') # This is the complete SHERPA database (most permissive policies for each journal)

#XXXXXXXXXX
# 4. LEFT-JOINING ESAC INTO DIMENSIONS MATCHING ON 'PUBLISHER'----

merged_pvga <- left_join(dimensions, esac[ , c("Publisher", "has_ta")], by = c("publisher" = "Publisher"))

merged_pvga$has_ta[is.na(merged_pvga$has_ta)] <- "no" # this couldn't be created before the merge (i.e. it's highlighting which rows were not merged with esac entries with TAs)

#XXXXXXXXXXXX
# 5. LEFT-JOINING SHERPA INTO MERGED DIMENSIONS-ESAC----

# Convert ISSNs to remove NAs (just because this was causing issues with the below code- it is changed back below)
merged_pvga$issn2[is.na(merged_pvga$issn2)] <- "0999"
merged_pvga$issn3[is.na(merged_pvga$issn3)] <- "0999"
merged_pvga$issn4[is.na(merged_pvga$issn4)] <- "0999"

# Select rows to keep from sherpa
sherpa_keep_rows <- c("issn_print", "issn_electronic", "j_title", "sherpa_id", "listed_in_doaj", "sherpa_publisher", "system_metadata.uri", "open_access_prohibited", "g_article_version", "g_license", "g_license1", "g_license2", "g_license3", "g_copyright_owner", "g_conditions", "g_location.location", "g_embargo.amount", "g_embargo.units", "g_embargo", "g_embargo2", "g_embargo3", "g_compliant_repository", "g_copyright", "rank_green", "fee_article_version", "fee_license", "fee_license1", "fee_license2", "fee_license3", "fee_copyright_owner", "fee_conditions", "fee_copyright", "rank_fee")

# Series of inner joins to find all possible matches - the reason this is necessary is because no one variable can identify all the matches (e.g. because the wrong ISSN is listed, or because the title is formatted differently in sherpa and dimensions)
merged_pvga1 <- inner_join(merged_pvga, sherpa_1row[ , sherpa_keep_rows], by = c("issn1" = "issn_print"))
merged_pvga2 <- inner_join(merged_pvga, sherpa_1row[ , sherpa_keep_rows], by = c("issn1" = "issn_electronic"))
merged_pvga3 <- inner_join(merged_pvga, sherpa_1row[ , sherpa_keep_rows], by = c("journal_title" = "j_title"))
merged_pvga4 <- inner_join(merged_pvga, sherpa_1row[ , sherpa_keep_rows], by = c("issn2" = "issn_print"))
merged_pvga5 <- inner_join(merged_pvga, sherpa_1row[ , sherpa_keep_rows], by = c("issn2" = "issn_electronic"))
merged_pvga6 <- inner_join(merged_pvga, sherpa_1row[ , sherpa_keep_rows], by = c("issn3" = "issn_print"))
merged_pvga7 <- inner_join(merged_pvga, sherpa_1row[ , sherpa_keep_rows], by = c("issn3" = "issn_electronic"))

# Bind rows to bring together all matching rows, then keep only first match for each article
merged_pvga8 <- bind_rows(merged_pvga1, merged_pvga2, merged_pvga3, merged_pvga4, merged_pvga5, merged_pvga6, merged_pvga7) %>%
  filter(!duplicated(doi))
rm(merged_pvga1, merged_pvga2, merged_pvga3, merged_pvga4, merged_pvga5, merged_pvga6, merged_pvga7)

# Identify rows with no match
merged_pvga0 <- left_join(merged_pvga, merged_pvga8[ , c("doi", "sherpa_id")], by = "doi") %>%
  filter(is.na(sherpa_id))

# Merge these non-matched rows with the matched rows
merged_pvga <- bind_rows(merged_pvga0, merged_pvga8)


#XXXXXXXXXXXXX
# 6. EDITS TO MERGED_PVGA----

# Return ISSNs to missing
merged_pvga$issn2 <- gsub("0999", "", merged_pvga$issn2)
merged_pvga$issn3 <- gsub("0999", "", merged_pvga$issn3)
merged_pvga$issn4 <- gsub("0999", "", merged_pvga$issn4)

# Generate new variables for journal type
merged_pvga$journal_type <- NA
merged_pvga$journal_type[merged_pvga$listed_in_doaj == "yes" | merged_pvga$open_access_categories == "Pure gold"] <- "Pure gold"
merged_pvga$journal_type[is.na(merged_pvga$journal_type) & (!is.na(merged_pvga$fee_article_version) | merged_pvga$open_access_categories == "Hybrid")] <- "Hybrid"
merged_pvga$journal_type[is.na(merged_pvga$journal_type)] <- "Closed or insufficient information"

# Create variable for number of articles per journal
articles_per_journal = merged_pvga %>% group_by(journal_title) %>% count()
merged_pvga <- left_join(merged_pvga, articles_per_journal, by = "journal_title") %>%
  rename(articles_in_journal = n)

# Remove observations with no match for Sherpa = 1.6% of total observations, most of which do not have a value for compliance, but are broadly typical for open_access_categories other than having slightly more closed and slightly more pure gold. The only real issue is they are mostly smaller journals (average 3 articles each, compared to wider sample which has more like 15 articles each). A lot  of these journals (looks like clear majority of smaller ones in particular) are also international journals.
merged_pvga <- merged_pvga %>% filter(!is.na(sherpa_id))

# Remove unnecessary variables (these were mostly metadata not used for analysis or only needed for testing)
merged_pvga <- subset(merged_pvga, select = -c(issn1, issn2, issn3, issn4, issn_electronic, issn_print, j_title, sherpa_publisher, system_metadata.uri, g_copyright_owner, g_conditions, g_location.location, g_embargo.amount, fee_license, fee_copyright_owner, fee_conditions))

#XXXXXXXXXXXXX
#7. CREATE COMPLIANCE VARIABLES TO INDICATE COMPLIANCE WITH DIFFERENT POLICY SCENARIOS

#a. Create Cartesian tables for compliance with current policy.----
  # These basically list out every possible combination of the key variables we are interested in for each element of the policy. We need to do several separate tables to make it simpler.
    
    #Table 1: Compliance with gold OA
compliance_fee <- expand.grid(
  journal_type = c("Pure gold", "Hybrid", "Closed or insufficient information"),
  fee_license3 = c("cc_by", NA, "not cc_by"))

compliance_fee <- compliance_fee %>%
  mutate(num_fee = 1:nrow(compliance_fee))

  # Table 2: compliance with green OA in current policy

    # There is an exception in the current policy for researchers funded by AHRC and ESRC, so need to derive a temporary variable for this
merged_pvga$embargo_exception <- NA
merged_pvga$embargo_exception[merged_pvga$ukri_funders %in% "AHRC" | merged_pvga$ukri_funders %in% "ESRC"] <- TRUE

compliance_current_green <- expand.grid(
  g_embargo2 = c(0, 6, 12, 13),
  embargo_exception = c(TRUE, NA),
  g_license2 = c("cc_by", "cc_by_nd", "cc_by_nc", "no compliant license"))

compliance_current_green <- compliance_current_green %>%
  mutate(num_current_green = 1:nrow(compliance_current_green))

  # Table 3: compliance with green OA in new policy
compliance_new_green <- expand.grid(
  g_embargo3 = c("zero embargo", "has embargo"),
  g_license2 = c("cc_by", "cc_by_nd", "cc_by_nc", "no compliant license")) # this will lead to one article with NA as there is one cc_by_nd, but including it would make the table much more complex)

compliance_new_green <- compliance_new_green %>%
  mutate(num_new_green = 1:nrow(compliance_new_green))

  # Then merge all these in to create new columns showing the ranks in merged_pvga
merged_pvga <- left_join(merged_pvga, compliance_fee)
merged_pvga <- left_join(merged_pvga, compliance_current_green)
merged_pvga <- left_join(merged_pvga, compliance_new_green)

merged_pvga$num_current_green[is.na(merged_pvga$num_current_green)] <- 99
merged_pvga$num_new_green[is.na(merged_pvga$num_new_green)] <- 99

merged_pvga <- merged_pvga %>%
  select(-embargo_exception) # this var was only created to aid with the ranking/ join

#b. Potential compliance with current policy----
# This looks at whether the most permissive policies for each journal/ article should provide a route to compliance with the current UKRI policy. Routes to compliance with this policy are:
  # a. published gold OA (pure or hybrid) with a CC_BY license
  # b. published green OA with a cc_by or cc_by_nc license and 6 month embargo (extended to 12 month for A&Hs)
merged_pvga$compliance_current <- NA
merged_pvga$compliance_current[merged_pvga$num_fee %in% c(1,4)] <- "c: pure gold"
merged_pvga$compliance_current[merged_pvga$num_fee %in% c(2,5)] <- "c: hybrid gold"
merged_pvga$compliance_current[!merged_pvga$num_fee %in% c(1,2,4,5) & merged_pvga$num_current_green %in% c(1,2,3,5,6,9,10,11,13,14,17,18,19,21,22)] <- "c: confirmed green oa"
merged_pvga$compliance_current[!merged_pvga$num_fee %in% c(1,2,4,5) & !merged_pvga$num_current_green %in% c(1,2,3,5,6,9,10,11,13,14,17,18,19,21,22) & (merged_pvga$g_embargo == 0 & merged_pvga$g_license1 == "no license requirement")] <- "nc: unconfirmed green oa"
merged_pvga$compliance_current[is.na(merged_pvga$compliance_current)] <- "not compliant"

    # simplified binary version of compliance_current (to make analysis tables/ charts easier)
merged_pvga$compliance_current2 <- merged_pvga$compliance_current
merged_pvga$compliance_current2[merged_pvga$compliance_current2 != "not compliant" & merged_pvga$compliance_current2 != "nc: unconfirmed green oa"] <- "compliant"
merged_pvga$compliance_current2[merged_pvga$compliance_current2 == "not compliant" | merged_pvga$compliance_current2 == "nc: unconfirmed green oa"] <- "not compliant"


#c. Potential compliance with new policy scenario 1 (hybrid gold allowed)----
  # This looks at whether the most permissive policies for each journal/ article should provide a route to compliance with the current UKRI policy.   Routes to compliance with this policy are:
    # a. published gold OA (pure or hybrid) with a CC_BY license
    # b. published green OA with a cc_by license and zero embargo
    # c. covered by a transformative agreement

merged_pvga$compliance_new_hybrid <- NA
merged_pvga$compliance_new_hybrid[merged_pvga$num_fee %in% c(1,4)] <- "c: pure gold"
merged_pvga$compliance_new_hybrid[merged_pvga$num_fee %in% c(2,5)] <- "c: hybrid gold"
merged_pvga$compliance_new_hybrid[!merged_pvga$num_fee %in% c(1,2,4,5) & merged_pvga$num_new_green %in% c(1,3)] <- "c: confirmed green oa"
merged_pvga$compliance_new_hybrid[!merged_pvga$num_fee %in% c(1,2,4,5) & !merged_pvga$num_new_green %in% c(1,3) & (merged_pvga$g_embargo == 0 & merged_pvga$g_license1 == "no license requirement")] <- "nc: unconfirmed green oa"
merged_pvga$compliance_new_hybrid[is.na(merged_pvga$compliance_new_hybrid)] <- "not compliant"

    # simplified binary version of compliance_new_hybrid

merged_pvga$compliance_new_hybrid2 <- merged_pvga$compliance_new_hybrid
merged_pvga$compliance_new_hybrid2[merged_pvga$compliance_new_hybrid2 != "not compliant" & merged_pvga$compliance_new_hybrid2 != "nc: unconfirmed green oa"] <- "compliant"
merged_pvga$compliance_new_hybrid2[merged_pvga$compliance_new_hybrid2 == "not compliant" | merged_pvga$compliance_new_hybrid2 == "nc: unconfirmed green oa"] <- "not compliant"

#d. Potential compliance with new policy scenario 2 (no hybrid gold)----
# This looks at whether the most permissive policies for each journal/ article should provide a route to compliance with the current UKRI policy. Routes to compliance with this policy are:
# a. published gold OA (pure or hybrid) with a CC_BY license
# b. published green OA with a cc_by license and zero embargo
# c. covered by a transformative agreement

merged_pvga$compliance_new <- NA
merged_pvga$compliance_new[merged_pvga$num_fee %in% c(1,4)] <- "c: pure gold"
merged_pvga$compliance_new[merged_pvga$has_ta == "yes" & merged_pvga$num_fee %in% c(2,5)] <- "c: hybrid gold with a TA"
merged_pvga$compliance_new[!merged_pvga$num_fee %in% c(1,4) & !(merged_pvga$has_ta == "yes" & merged_pvga$num_fee %in% c(2,5)) & merged_pvga$num_new_green %in% c(1,3)] <- "c: confirmed green oa"
merged_pvga$compliance_new[!merged_pvga$num_fee %in% c(1,4) & !(merged_pvga$has_ta == "yes" & merged_pvga$num_fee %in% c(2,5)) & !merged_pvga$num_new_green %in% c(1,3) & (merged_pvga$g_embargo == 0 & merged_pvga$g_license1 == "no license requirement")] <- "nc: unconfirmed green oa"
merged_pvga$compliance_new[is.na(merged_pvga$compliance_new)] <- "not compliant"

    # simplified binary version of compliance_new

merged_pvga$compliance_new2 <- merged_pvga$compliance_new
merged_pvga$compliance_new2[merged_pvga$compliance_new2 != "not compliant" & merged_pvga$compliance_new2 != "nc: unconfirmed green oa"] <- "compliant"
merged_pvga$compliance_new2[merged_pvga$compliance_new2 == "not compliant" | merged_pvga$compliance_new2 == "nc: unconfirmed green oa"] <- "not compliant"


#####e. Final edits to merged_pvga (including creating factors)----

# Recode open_access_categories as factor
merged_pvga$open_access_categories <- factor(merged_pvga$open_access_categories, ordered = TRUE, levels = c("Pure gold", "Hybrid", "Green, published", "Green, accepted", "Bronze", "Green, submitted", "Closed"))
merged_pvga$open_access_categories2 <- factor(merged_pvga$open_access_categories2, ordered = TRUE, levels = c("Pure gold", "Hybrid gold", "Green", "Closed"))

# Recode g_embargo3 as a factor
merged_pvga$g_embargo <- factor(merged_pvga$g_embargo, ordered = TRUE, levels = c(0,3,6,12,18,24,36,48,NA))
merged_pvga$g_embargo2 <- factor(merged_pvga$g_embargo2, ordered = TRUE, levels = c(0,6,12,13))
merged_pvga$g_embargo3 <- factor(merged_pvga$g_embargo3, ordered = TRUE, levels = c("zero embargo", "has embargo"))

# Recode g_compliant_repository as a factor
merged_pvga$g_compliant_repository <- factor(merged_pvga$g_compliant_repository, ordered = TRUE, levels = c(TRUE, FALSE))

# Turn license vars into factor to order by permissiveness (just to make outputs clearer)
merged_pvga$g_license1 <- factor(merged_pvga$g_license1, ordered = TRUE, c("cc_by", "cc_by_nd", "cc_by_nc", "cc_by_sa", "cc_by_nc_nd", "cc_by_nc_sa", "bespoke_license", "all_rights_reserved", "no license requirement", NA))
merged_pvga$g_license2 <- factor(merged_pvga$g_license2, ordered = TRUE, c("cc_by", "cc_by_nd", "cc_by_nc", "no compliant license", NA))

# Recode compliance vars as factors
merged_pvga$compliance_current <- factor(merged_pvga$compliance_current, ordered = TRUE, levels = c("c: pure gold", "c: hybrid gold", "c: confirmed green oa", "nc: unconfirmed green oa", "not compliant"))

merged_pvga$compliance_new_hybrid <- factor(merged_pvga$compliance_new_hybrid, ordered = TRUE, levels = c("c: pure gold", "c: hybrid gold", "c: confirmed green oa", "nc: unconfirmed green oa",  "not compliant"))

merged_pvga$compliance_new <- factor(merged_pvga$compliance_new, ordered = TRUE, levels = c("c: pure gold", "c: hybrid gold with a TA", "c: confirmed green oa", "nc: unconfirmed green oa", "not compliant"))

# Recode journal_type as factor
merged_pvga$journal_type <- factor(merged_pvga$journal_type, ordered = TRUE, levels = c("Pure gold", "Hybrid", "Closed or insufficient information"))

# Code which will allow for reproducible random reordering of rows (e.g. when getting rid of duplicates)
set.seed(42)
rows <- sample(nrow(merged_pvga))
merged_pvga <- merged_pvga[rows, ]


#XXXXXXXXXX
# 7. WRITE MERGED_PVGA TO EXCEL AND RDA ----

openxlsx::write.xlsx(as.data.frame(merged_pvga), 'Data/merged_pvga.xlsx')
save(merged_pvga, file = "Data/merged_pvga.Rda")

# Merged PVGA: We have a Sherpa link for 98.4% of the publications in the Dimensions sample. We have green (no fee) routes for almost all of these and paid (fee) routes for about three-fifths. This leaves us with 2368 articles in Dimensions not covered by Sherpa data (made up of 844 journals). merged_pvga excludes all articles without a sherpa link, leaving 145,925 articles in 8896 journals
