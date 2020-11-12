# PREPARING AND MERGING DATA FOR PUBLICATION VENUE GAP ANALYSIS

# Author: Tom Kenny
# Created: August 2020
# Last Updated: November 2020

# Purpose of code: This code prepares data from Dimensions, SHERPA RoMEO and ESAC to be used for analysing open access policies. It then merges the three data together into a merged_pvga dataset which can be usef for publication venue gap analysis

# Prerequisite

# This code requires data from Dimensions and SHERPA RoMEO - the former was downloaded using the Google Sheets API for Dimensions to download all articles associated with UKRI funder group from 2017-2020. The latter is downloaded via API using Tom Kenny's code (SHERPA RoMEO API (clean)). Both data are saved in the Publication Venue Gap Analysis folder in the Policy Analysis drive.

#################
# Clear work space
rm(list=ls())

# Set working directory
mainDir <- "C:\\Users\\TKen02\\UKRI\\Policy Analysis - Documents\\Open Access\\Projects\\Publication Venue Gap Analysis\\Data"
setwd(mainDir)

#library(readr)
library(tidyverse)
library(openxlsx)
library(xml2)
library(rvest)

setwd(mainDir)

#############
# 1. PREPARING DIMENSIONS DATA
# NB. This data was downloaded using the Dimensions API plugin for google sheets, with queries for each year in the format: "search publications where year in [2018:2018] and funders in ["grid.8682.4", "grid.14105.31", "grid.14467.30", "grid.418100.c", "grid.421091.f", "grid.423443.6", "grid.426413.6", "grid.434257.3", "grid.453088.2", "grid.496779.2"] and type in ["article"] return publications[type + category_uoa + date + dimensions_url + doi + funder_countries + funders + issn + journal + journal_lists + linkout + open_access_categories + publisher + research_org_countries + research_org_country_names + supporting_grant_ids + title  + year]"

# dimensions <- read.xlsx("Dimensions_orig.xlsx")
dimensions <- read.xlsx("Dimensions_edited.xlsx") # this version has manually recoded values for open_access_categories as I couldn't rename the values because they had so many characters needing escaping (see also below)

# Remove duplicate rows (using doi)
dimensions <- dimensions %>%
  filter(!duplicated(doi))

# renaming values in research_org_country_names to leave only the names themselves
dimensions$research_org_country_names <- gsub("\\[\"", "", dimensions$research_org_country_names)
dimensions$research_org_country_names <- gsub("\",\"", ", ", dimensions$research_org_country_names)
dimensions$research_org_country_names <- gsub("\"\\]", "", dimensions$research_org_country_names)

# renaming values in funders - can't get this to work but not essential

# renaming values in open_access_categories. NB I temporarily did this in Excel as I couldn't get any other than the first to work
# dimensions$open_access_categories <- gsub("{\"id\":\"oa_all\",\"description\":\"Article is freely available\",\"name\":\"All OA\"},{\"id\":\"green_acc\",\"description\":\"Free copy of accepted version in an OA repository\",\"name\":\"Green, Accepted\"}", "Green, Accepted", dimensions$open_access_categories, perl=TRUE)
# dimensions$open_access_categories <- gsub("{\"id\":\"oa_all\",\"description\":\"Article is freely available\",\"name\":\"All OA\"},{\"id\":\"gold_pure\",\"description\":\"Version Of Record (VOR) is free under an open licence from a full OA journal\",\"name\":\"Pure Gold\"}", "Pure gold", dimensions$open_access_categories, perl=TRUE)

# creating category_uoa1 which will show only the first subject from category_uoa, then category uoa2 which will show only the second (where one exists). An example of a category_uoa with two UoAs is [{"id":"30011","name":"B11 Computer Science and Informatics"},{"id":"30012","name":"B12 Engineering"}]
dimensions$category_uoa1 <- dimensions$category_uoa
dimensions$category_uoa1 <- substr(dimensions$category_uoa1, 24, nchar(dimensions$category_uoa1)-3)
dimensions$category_uoa1[is.na(dimensions$category_uoa1)] <- "Missing"

dimensions$category_uoa2 <- dimensions$category_uoa1
dimensions$category_uoa2 <- gsub(".*{", "", dimensions$category_uoa2, perl=TRUE) # deleting the first subject (i.e. everything before {)
dimensions$category_uoa2 <- gsub("^A.*", "", dimensions$category_uoa2) # deleting everything before the first letter of second subject to clean it up
dimensions$category_uoa2 <- gsub("^B.*", "", dimensions$category_uoa2)
dimensions$category_uoa2 <- gsub("^C.*", "", dimensions$category_uoa2)
dimensions$category_uoa2 <- gsub("^D.*", "", dimensions$category_uoa2)
dimensions$category_uoa2 <- gsub(".*name\":\"", "", dimensions$category_uoa2) # deleting the end of the string to clean it up
dimensions$category_uoa2 <- gsub("Missing", "", dimensions$category_uoa2) # deleting missing from uoa2 since it's just that it doesn't have second subject rather than being missing
dimensions$category_uoa1 <- gsub("\".*", "", dimensions$category_uoa1) # removing everything after first subject from uoa1

# Creating derived variables ref_panel and ref panel2 corresponding to uoa_1 and uoa_2 (so we can do analysis at the level of REF panel rather than individual REF units of assessment/ disciplines).
dimensions$ref_panel <- NA
dimensions$ref_panel <- substr(dimensions$category_uoa1,1,1) # Selecting the first letter of the UoA (which is the REF panel letter)
dimensions$ref_panel <- gsub("M", "Missing", dimensions$ref_panel) # Changing M back to missing
dimensions$ref_panel2 <- NA
dimensions$ref_panel2 <- substr(dimensions$category_uoa2,1,1) 

# Creating derived variable for each panel to show if it exists for each article, i.e. if the panel exists in ref_panel or ref_panel2 then return true
dimensions$ref_panel_a[dimensions$ref_panel == "A" | dimensions$ref_panel2 == "A"] <- TRUE
dimensions$ref_panel_b[dimensions$ref_panel == "B" | dimensions$ref_panel2 == "B"] <- TRUE
dimensions$ref_panel_c[dimensions$ref_panel == "C" | dimensions$ref_panel2 == "C"] <- TRUE
dimensions$ref_panel_d[dimensions$ref_panel == "D" | dimensions$ref_panel2 == "D"] <- TRUE

# remove articles without a DOI or ISSN
dimensions <- dimensions[!is.na(dimensions$issn), ]

# creating new variables to return individual ISSNs in consistent format (8 numbers each)
dimensions$issn <- gsub("-", "", dimensions$issn) # getting rid of hyphens in issns
dimensions$issn1 <- dimensions$issn
dimensions$issn1 <- substr(dimensions$issn1, 3, 10) # because there is extraneous punctuation surrounding the numbers

dimensions$issn2 <- NA
dimensions$issn3 <- NA
dimensions$issn4 <- NA
for (i in 1:nrow(dimensions)) {
if (nchar(dimensions[i, 'issn']) > 12) {dimensions[i, 'issn2'] <- substr(dimensions[i, 'issn'], 14, 21)}
  if (nchar(dimensions[i, 'issn']) > 24) {dimensions[i, 'issn3'] <- substr(dimensions[i, 'issn'], 25, 32)}
  if (nchar(dimensions[i, 'issn']) > 36) {dimensions[i, 'issn4'] <- substr(dimensions[i, 'issn'], 36, 43)}
  else next
} # essentially this for loop identifies where more than one issn is present and returns it into new variables for the second, third and fourth where they exist.This is needed because they can act as matching values for the merge.

# Recoding open_access_categories into simpler form
dimensions$open_access_categories2 <- dimensions$open_access_categories
dimensions$open_access_categories2[dimensions$open_access_categories2 == "Hybrid"] <- "Hybrid gold" # to avoid confusion with journal type
dimensions$open_access_categories2[dimensions$open_access_categories2 == "Green, published" | dimensions$open_access_categories2 == "Green, accepted"] <- "Green"
dimensions$open_access_categories2[dimensions$open_access_categories2 == "Bronze" | dimensions$open_access_categories2 == "Green, submitted" | dimensions$open_access_categories2 == "Closed"] <- "Closed" #since these are all closed from the perspective of UKRI policy

# removing unneccessary columns
dimensions <- dimensions[,!(names(dimensions) %in% c("funder_countries","journal_id", "journal_lists", "research_org_countries", "supporting_grant_ids", "type", "year"))]

# changing column order (if any new columns added they will need to be added here)
col_order <- (c("title", "date", "journal_title", "publisher", "issn", "issn1", "issn2", "issn3", "issn4", "doi", "category_uoa", "category_uoa1", "category_uoa2", "ref_panel", "ref_panel2", "ref_panel_a", "ref_panel_b", "ref_panel_c", "ref_panel_d", "open_access_categories", "open_access_categories2", "funders", "linkout", "research_org_country_names"))
dimensions <- dimensions[, col_order]



###########
# 2. DOWNLOADING AND PREPARING ESAC DATA

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

  # Create new column has_ta = yes. This means that when it is merged into the dimensions data it will highlight all the articles with a publisher with ta.
esac$has_ta <- "yes"

###########
# 3. PREPARING SHERPA ROMEO DATA
  # NB. See code 'Sherpa Romeo API (clean)' for how to download this data'
  # NB2. We were advised by JISC that missing data should be interpreted as 'no requirement' which means missing is actually equivalent to the most permissive policy option.

sherpa <- read.xlsx("sherpa_all_policies.xlsx") # this is data created by the R code file named SHERPA RoMEO API (clean)

  # rename title and id to avoid confusion with title in Dimensions
sherpa <- rename(sherpa, j_title = title, sherpa_id = id, sherpa_publisher = name)

  # Remove unnecessary columns
sherpa <- sherpa[,!(names(sherpa) %in% c("issn_NA","issn_legacy", "pubpol_id"))]

  # Clean up issn codes - this is needed to enable merged with Dimensions on issn
sherpa$issn_print <- gsub("-", "", sherpa$issn_print)
sherpa$issn_electronic <- gsub("-", "", sherpa$issn_electronic)

  # create new column sherpa$embargo which recodes embargo.amount to months where embargo.unit != 'months'
    # NB JISC told us that no data for embargo means no embargo
    # Alternative would be to create a new column using mutate, case_when (this is to do it all in one go and avoid for loop). E.g. #sherpa %>% mutate(embargo = ifelse(embargo.units = "month", TRUE = , FALSE = ,))
sherpa$embargo <- NA
sherpa$embargo.amount[is.na(sherpa$embargo.amount)] <- 0
sherpa$embargo.amount[sherpa$embargo.amount == ""] <- 0
sherpa$embargo.units[is.na(sherpa$embargo.units)] <- "No embargo requirement" # Created this option to differentiate between no data and defined zero embargo though we were told by JISC that they are the same thing
sherpa$embargo.units[sherpa$embargo.units == ""] <- "No embargo requirement"

for (i in 1:nrow(sherpa)) {
  if ((sherpa[i, 'embargo.amount'] == "No embargo requirement") | (sherpa[i, 'embargo.amount'] == 0)) {sherpa[i, 'embargo'] <- 0}
  else if (sherpa[i, 'embargo.units'] == "months") {sherpa[i, 'embargo'] <- sherpa[i, 'embargo.amount']}
  else if (sherpa[i, 'embargo.units'] == "days") {sherpa[i, 'embargo'] <- round((sherpa[i, 'embargo.amount'])/30)} 
  else if (sherpa[i, 'embargo.units'] == "weeks") {sherpa[i, 'embargo'] <- ((sherpa[i, 'embargo.amount'])/4)}
  else if (sherpa[i, 'embargo.units'] == "years") {sherpa[i, 'embargo'] <- sherpa[i, 'embargo.amount']*12}
  else next
} # This loop converts all embargo length into the same unit (months)

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

# Derive license1 to simplify license and only return most permissive license for each policy
sherpa$license1 <- sherpa$license
sherpa$license1[grepl("ccby,", sherpa$license)] <- "cc_by" 
sherpa$license1[grepl("ccbynd,", sherpa$license1)] <- "cc_by_nd"
sherpa$license1[grepl("ccbysa,", sherpa$license1)] <- "cc_by_sa"
sherpa$license1[grepl("ccbync,", sherpa$license1)] <- "cc_by_nc"
sherpa$license1[grepl("ccbyncnd,", sherpa$license1)] <- "cc_by_nc_nd"
sherpa$license1[grepl("ccbyncsa,", sherpa$license1)] <- "cc_by_nc_sa"
sherpa$license1[sherpa$license1 == "en, publisher\'sbespokelicense, bespokelicense, bespokelicense" | sherpa$license1 == "bespokelicense, bespokelicense, publisher'sbespokelicense, en" | sherpa$license1 == "en, bespokelicense, publisher'sbespokelicense, bespokelicense" | sherpa$license1 == "ccgnugpl, en, gnugpl, ccgnugpl" | sherpa$license1 == "ccpublicdomain, pulicdomain, ccpublicdomain, en" | sherpa$license1 == "	en, publisher'sbespokelicense, bespokelicense, bespokelicense" | sherpa$license1 ==  "bespokelicense, en, publisher'sbespokelicense, bespokelicense"] <- "bespoke license"
all_licenses <- sherpa %>% count(license1)
#openxlsx::write.xlsx(as.data.frame(all_licenses), 'all_licenses.xlsx')

    # Simplify license further into new var license2 by merging cc-by and no license requirement AND merging non-compliant licenses
sherpa$license2 <- sherpa$license1
sherpa$license2[sherpa$license2 == "no license requirement"] <- "cc_by"
sherpa$license2[sherpa$license2 != "cc_by" & sherpa$license2 != "cc_by_nd" & sherpa$license2 != "cc_by_sa"] <- "no compliant license"

# Ranking Sherpa policies by license, suitable repository, embargo length etc,

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

    # Create new column to rank Green OA policies

  # Select only policies where green OA is relevant (additional_oa_fee = no). I originally also excluded listed_in_doaj here but then realised that they many pure gold journals still do have green OA policies (this is presumably why they are in sherpa - in fact almost none of the listed_in_doaj policies are associated with a fee). Also remove policies relating to submitted articles (these aren't peer reviewed)
sherpa_green <- sherpa %>%
  filter(additional_oa_fee == "no", article_version != "submitted")

  # Since full/ pure gold journals are not listed as having an additional oa fee their paid policies will incorrectly show up as green oa in many cases. An imperfect work around for this is to say that if a policy for a pure gold journal refers to the published version it should not be seen as green OA.
sherpa_green$license1[sherpa_green$listed_in_doaj == "yes" & sherpa_green$article_version == "published"] <- "NA - policy for published version in full OA journal"
sherpa_green$license2[sherpa_green$listed_in_doaj == "yes" & sherpa_green$article_version == "published"] <- "NA - policy for published version in full OA journal"

  # Create column 'rank_green' to rank policies from most to least permissive - this is basically a series of tests which correspond with a ranking agreed with Rachel Bruce and saved in the project folder.
sherpa_green$rank_green[sherpa_green$license2 == "cc_by" & sherpa_green$embargo == 0 & sherpa_green$compliant_repository == "TRUE" & sherpa_green$copyright != "publishers" & sherpa_green$version_published == "TRUE"] <- "01"
sherpa_green$rank_green[sherpa_green$license2 == "cc_by" & sherpa_green$embargo == 0 & sherpa_green$compliant_repository == "TRUE" & sherpa_green$copyright != "publishers" & sherpa_green$version_published == "FALSE"] <- "01a"

sherpa_green$rank_green[sherpa_green$license2 == "cc_by" & sherpa_green$embargo == 0 & sherpa_green$compliant_repository == "TRUE" & sherpa_green$copyright == "publishers" & sherpa_green$version_published == "TRUE"] <- "02"
sherpa_green$rank_green[sherpa_green$license2 == "cc_by" & sherpa_green$embargo == 0 & sherpa_green$compliant_repository == "TRUE" & sherpa_green$copyright == "publishers" & sherpa_green$version_published == "FALSE"] <- "02a"

sherpa_green$rank_green[sherpa_green$license2 == "cc_by" & sherpa_green$embargo == 0 & sherpa_green$compliant_repository == "FALSE" & sherpa_green$copyright != "publishers" & sherpa_green$version_published == "TRUE"] <- "03"
sherpa_green$rank_green[sherpa_green$license2 == "cc_by" & sherpa_green$embargo == 0 & sherpa_green$compliant_repository == "FALSE" & sherpa_green$copyright != "publishers" & sherpa_green$version_published == "FALSE"] <- "03a"

sherpa_green$rank_green[sherpa_green$license2 == "cc_by" & sherpa_green$embargo == 0 & sherpa_green$compliant_repository == "FALSE" & sherpa_green$copyright == "publishers" & sherpa_green$version_published == "TRUE"] <- "04"
sherpa_green$rank_green[sherpa_green$license2 == "cc_by" & sherpa_green$embargo == 0 & sherpa_green$compliant_repository == "FALSE" & sherpa_green$copyright == "publishers" & sherpa_green$version_published == "FALSE"] <- "04a"

sherpa_green$rank_green[sherpa_green$license2 == "cc_by_nd" & sherpa_green$embargo == 0 & sherpa_green$compliant_repository == "TRUE" & sherpa_green$copyright != "publishers" & sherpa_green$version_published == "TRUE"] <- "05"
sherpa_green$rank_green[sherpa_green$license2 == "cc_by_nd" & sherpa_green$embargo == 0 & sherpa_green$compliant_repository == "TRUE" & sherpa_green$copyright != "publishers" & sherpa_green$version_published == "FALSE"] <- "05a"

sherpa_green$rank_green[sherpa_green$license2 == "cc_by_nd" & sherpa_green$embargo == 0 & sherpa_green$compliant_repository == "TRUE" & sherpa_green$copyright == "publishers" & sherpa_green$version_published == "TRUE"] <- "06"
sherpa_green$rank_green[sherpa_green$license2 == "cc_by_nd" & sherpa_green$embargo == 0 & sherpa_green$compliant_repository == "TRUE" & sherpa_green$copyright == "publishers" & sherpa_green$version_published == "FALSE"] <- "06a"

sherpa_green$rank_green[sherpa_green$license2 == "cc_by_sa" & sherpa_green$embargo == 0 & sherpa_green$compliant_repository == "TRUE" & sherpa_green$copyright != "publishers" & sherpa_green$version_published == "TRUE"] <- "07"
sherpa_green$rank_green[sherpa_green$license2 == "cc_by_sa" & sherpa_green$embargo == 0 & sherpa_green$compliant_repository == "TRUE" & sherpa_green$copyright != "publishers" & sherpa_green$version_published == "FALSE"] <- "07a"

sherpa_green$rank_green[sherpa_green$license2 == "cc_by_sa" & sherpa_green$embargo == 0 & sherpa_green$compliant_repository == "TRUE" & sherpa_green$copyright == "publishers" & sherpa_green$version_published == "TRUE"] <- "08"
sherpa_green$rank_green[sherpa_green$license2 == "cc_by_sa" & sherpa_green$embargo == 0 & sherpa_green$compliant_repository == "TRUE" & sherpa_green$copyright == "publishers" & sherpa_green$version_published == "FALSE"] <- "08a"

sherpa_green$rank_green[sherpa_green$license2 == "cc_by_nd" & sherpa_green$embargo == 0 & sherpa_green$compliant_repository == "FALSE" & sherpa_green$copyright != "publishers" & sherpa_green$version_published == "TRUE"] <- "09"
sherpa_green$rank_green[sherpa_green$license2 == "cc_by_nd" & sherpa_green$embargo == 0 & sherpa_green$compliant_repository == "FALSE" & sherpa_green$copyright != "publishers" & sherpa_green$version_published == "FALSE"] <- "09a"

sherpa_green$rank_green[sherpa_green$license2 == "cc_by_nd" & sherpa_green$embargo == 0 & sherpa_green$compliant_repository == "FALSE" & sherpa_green$copyright == "publishers" & sherpa_green$version_published == "TRUE"] <- "10"
sherpa_green$rank_green[sherpa_green$license2 == "cc_by_nd" & sherpa_green$embargo == 0 & sherpa_green$compliant_repository == "FALSE" & sherpa_green$copyright == "publishers" & sherpa_green$version_published == "FALSE"] <- "10a"

sherpa_green$rank_green[sherpa_green$license2 == "cc_by_sa" & sherpa_green$embargo == 0 & sherpa_green$compliant_repository == "FALSE" & sherpa_green$copyright != "publishers" & sherpa_green$version_published == "TRUE"] <- "11"
sherpa_green$rank_green[sherpa_green$license2 == "cc_by_sa" & sherpa_green$embargo == 0 & sherpa_green$compliant_repository == "FALSE" & sherpa_green$copyright != "publishers" & sherpa_green$version_published == "FALSE"] <- "11a"

sherpa_green$rank_green[sherpa_green$license2 == "cc_by_sa" & sherpa_green$embargo == 0 & sherpa_green$compliant_repository == "FALSE" & sherpa_green$copyright == "publishers" & sherpa_green$version_published == "TRUE"] <- "12"
sherpa_green$rank_green[sherpa_green$license2 == "cc_by_sa" & sherpa_green$embargo == 0 & sherpa_green$compliant_repository == "FALSE" & sherpa_green$copyright == "publishers" & sherpa_green$version_published == "FALSE"] <- "12a"

sherpa_green$rank_green[sherpa_green$license2 == "cc_by" & sherpa_green$embargo != 0 & sherpa_green$compliant_repository == "TRUE" & sherpa_green$copyright != "publishers" & sherpa_green$version_published == "TRUE"] <- "13"
sherpa_green$rank_green[sherpa_green$license2 == "cc_by" & sherpa_green$embargo != 0 & sherpa_green$compliant_repository == "TRUE" & sherpa_green$copyright != "publishers" & sherpa_green$version_published == "FALSE"] <- "13a"

sherpa_green$rank_green[sherpa_green$license2 == "cc_by" & sherpa_green$embargo != 0 & sherpa_green$compliant_repository == "TRUE" & sherpa_green$copyright == "publishers" & sherpa_green$version_published == "TRUE"] <- "14"
sherpa_green$rank_green[sherpa_green$license2 == "cc_by" & sherpa_green$embargo != 0 & sherpa_green$compliant_repository == "TRUE" & sherpa_green$copyright == "publishers" & sherpa_green$version_published == "FALSE"] <- "14a"

sherpa_green$rank_green[sherpa_green$license2 == "cc_by_nd" & sherpa_green$embargo != 0 & sherpa_green$compliant_repository == "TRUE" & sherpa_green$copyright != "publishers" & sherpa_green$version_published == "TRUE"] <- "15"
sherpa_green$rank_green[sherpa_green$license2 == "cc_by_nd" & sherpa_green$embargo != 0 & sherpa_green$compliant_repository == "TRUE" & sherpa_green$copyright != "publishers" & sherpa_green$version_published == "FALSE"] <- "15a"

sherpa_green$rank_green[sherpa_green$license2 == "cc_by_nd" & sherpa_green$embargo != 0 & sherpa_green$compliant_repository == "TRUE" & sherpa_green$copyright == "publishers" & sherpa_green$version_published == "TRUE"] <- "16"
sherpa_green$rank_green[sherpa_green$license2 == "cc_by_nd" & sherpa_green$embargo != 0 & sherpa_green$compliant_repository == "TRUE" & sherpa_green$copyright == "publishers" & sherpa_green$version_published == "FALSE"] <- "16a"

sherpa_green$rank_green[sherpa_green$license2 == "cc_by_sa" & sherpa_green$embargo != 0 & sherpa_green$compliant_repository == "TRUE" & sherpa_green$copyright != "publishers" & sherpa_green$version_published == "TRUE"] <- "17"
sherpa_green$rank_green[sherpa_green$license2 == "cc_by_sa" & sherpa_green$embargo != 0 & sherpa_green$compliant_repository == "TRUE" & sherpa_green$copyright != "publishers" & sherpa_green$version_published == "FALSE"] <- "17a"

sherpa_green$rank_green[sherpa_green$license2 == "cc_by_sa" & sherpa_green$embargo != 0 & sherpa_green$compliant_repository == "TRUE" & sherpa_green$copyright == "publishers" & sherpa_green$version_published == "TRUE"] <- "18"
sherpa_green$rank_green[sherpa_green$license2 == "cc_by_sa" & sherpa_green$embargo != 0 & sherpa_green$compliant_repository == "TRUE" & sherpa_green$copyright == "publishers" & sherpa_green$version_published == "FALSE"] <- "18a"

sherpa_green$rank_green[sherpa_green$license2 == "cc_by" & sherpa_green$embargo != 0 & sherpa_green$compliant_repository == "FALSE" & sherpa_green$copyright != "publishers" & sherpa_green$version_published == "TRUE"] <- "19"
sherpa_green$rank_green[sherpa_green$license2 == "cc_by" & sherpa_green$embargo != 0 & sherpa_green$compliant_repository == "FALSE" & sherpa_green$copyright != "publishers" & sherpa_green$version_published == "FALSE"] <- "19a"

sherpa_green$rank_green[sherpa_green$license2 == "cc_by" & sherpa_green$embargo != 0 & sherpa_green$compliant_repository == "FALSE" & sherpa_green$copyright == "publishers" & sherpa_green$version_published == "TRUE"] <- "20"
sherpa_green$rank_green[sherpa_green$license2 == "cc_by" & sherpa_green$embargo != 0 & sherpa_green$compliant_repository == "FALSE" & sherpa_green$copyright == "publishers" & sherpa_green$version_published == "FALSE"] <- "20a"

sherpa_green$rank_green[sherpa_green$license2 == "cc_by_nd" & sherpa_green$embargo != 0 & sherpa_green$compliant_repository == "FALSE" & sherpa_green$copyright != "publishers" & sherpa_green$version_published == "TRUE"] <- "21"
sherpa_green$rank_green[sherpa_green$license2 == "cc_by_nd" & sherpa_green$embargo != 0 & sherpa_green$compliant_repository == "FALSE" & sherpa_green$copyright != "publishers" & sherpa_green$version_published == "FALSE"] <- "21a"

sherpa_green$rank_green[sherpa_green$license2 == "cc_by_nd" & sherpa_green$embargo != 0 & sherpa_green$compliant_repository == "FALSE" & sherpa_green$copyright == "publishers" & sherpa_green$version_published == "TRUE"] <- "22"
sherpa_green$rank_green[sherpa_green$license2 == "cc_by_nd" & sherpa_green$embargo != 0 & sherpa_green$compliant_repository == "FALSE" & sherpa_green$copyright == "publishers" & sherpa_green$version_published == "FALSE"] <- "22a"

sherpa_green$rank_green[sherpa_green$license2 == "cc_by_sa" & sherpa_green$embargo != 0 & sherpa_green$compliant_repository == "FALSE" & sherpa_green$copyright != "publishers" & sherpa_green$version_published == "TRUE"] <- "23"
sherpa_green$rank_green[sherpa_green$license2 == "cc_by_sa" & sherpa_green$embargo != 0 & sherpa_green$compliant_repository == "FALSE" & sherpa_green$copyright != "publishers" & sherpa_green$version_published == "FALSE"] <- "23a"

sherpa_green$rank_green[sherpa_green$license2 == "cc_by_sa" & sherpa_green$embargo != 0 & sherpa_green$compliant_repository == "FALSE" & sherpa_green$copyright == "publishers" & sherpa_green$version_published == "TRUE"] <- "24"
sherpa_green$rank_green[sherpa_green$license2 == "cc_by_sa" & sherpa_green$embargo != 0 & sherpa_green$compliant_repository == "FALSE" & sherpa_green$copyright == "publishers" & sherpa_green$version_published == "FALSE"] <- "24a"

sherpa_green$rank_green[sherpa_green$license2 == "no compliant license" & sherpa_green$embargo == 0 & sherpa_green$compliant_repository == "TRUE" & sherpa_green$copyright != "publishers" & sherpa_green$version_published == "TRUE"] <- "25"
sherpa_green$rank_green[sherpa_green$license2 == "no compliant license" & sherpa_green$embargo == 0 & sherpa_green$compliant_repository == "TRUE" & sherpa_green$copyright != "publishers" & sherpa_green$version_published == "FALSE"] <- "25a"

sherpa_green$rank_green[sherpa_green$license2 == "no compliant license" & sherpa_green$embargo == 0 & sherpa_green$compliant_repository == "TRUE" & sherpa_green$copyright == "publishers" & sherpa_green$version_published == "TRUE"] <- "26"
sherpa_green$rank_green[sherpa_green$license2 == "no compliant license" & sherpa_green$embargo == 0 & sherpa_green$compliant_repository == "TRUE" & sherpa_green$copyright == "publishers" & sherpa_green$version_published == "FALSE"] <- "26a"

sherpa_green$rank_green[sherpa_green$license2 == "no compliant license" & sherpa_green$embargo == 0 & sherpa_green$compliant_repository == "FALSE" & sherpa_green$copyright != "publishers" & sherpa_green$version_published == "TRUE"] <- "27"
sherpa_green$rank_green[sherpa_green$license2 == "no compliant license" & sherpa_green$embargo == 0 & sherpa_green$compliant_repository == "FALSE" & sherpa_green$copyright != "publishers" & sherpa_green$version_published == "FALSE"] <- "27a"

sherpa_green$rank_green[sherpa_green$license2 == "no compliant license" & sherpa_green$embargo == 0 & sherpa_green$compliant_repository == "FALSE" & sherpa_green$copyright == "publishers" & sherpa_green$version_published == "TRUE"] <- "28"
sherpa_green$rank_green[sherpa_green$license2 == "no compliant license" & sherpa_green$embargo == 0 & sherpa_green$compliant_repository == "FALSE" & sherpa_green$copyright == "publishers" & sherpa_green$version_published == "FALSE"] <- "28a"

sherpa_green$rank_green[sherpa_green$license2 == "no compliant license" & sherpa_green$embargo != 0 & sherpa_green$compliant_repository == "TRUE" & sherpa_green$copyright != "publishers" & sherpa_green$version_published == "TRUE"] <- "29"
sherpa_green$rank_green[sherpa_green$license2 == "no compliant license" & sherpa_green$embargo != 0 & sherpa_green$compliant_repository == "TRUE" & sherpa_green$copyright != "publishers" & sherpa_green$version_published == "FALSE"] <- "29a"

sherpa_green$rank_green[sherpa_green$license2 == "no compliant license" & sherpa_green$embargo != 0 & sherpa_green$compliant_repository == "TRUE" & sherpa_green$copyright == "publishers" & sherpa_green$version_published == "TRUE"] <- "30"
sherpa_green$rank_green[sherpa_green$license2 == "no compliant license" & sherpa_green$embargo != 0 & sherpa_green$compliant_repository == "TRUE" & sherpa_green$copyright == "publishers" & sherpa_green$version_published == "FALSE"] <- "30a"

sherpa_green$rank_green[sherpa_green$license2 == "no compliant license" & sherpa_green$embargo != 0 & sherpa_green$compliant_repository == "FALSE" & sherpa_green$copyright != "publishers" & sherpa_green$version_published == "TRUE"] <- "31"
sherpa_green$rank_green[sherpa_green$license2 == "no compliant license" & sherpa_green$embargo != 0 & sherpa_green$compliant_repository == "FALSE" & sherpa_green$copyright != "publishers" & sherpa_green$version_published == "FALSE"] <- "31a"

sherpa_green$rank_green[sherpa_green$license2 == "no compliant license" & sherpa_green$embargo != 0 & sherpa_green$compliant_repository == "FALSE" & sherpa_green$copyright == "publishers" & sherpa_green$version_published == "TRUE"] <- "32"
sherpa_green$rank_green[sherpa_green$license2 == "no compliant license" & sherpa_green$embargo != 0 & sherpa_green$compliant_repository == "FALSE" & sherpa_green$copyright == "publishers" & sherpa_green$version_published == "FALSE"] <- "32a"

sherpa_green$rank_green[is.na(sherpa_green$rank_green) & sherpa_green$license2 == "NA - policy for published version in full OA journal"] <- "33"

#openxlsx::write.xlsx(as.data.frame(sherpa_green), 'sherpa_green_ranked.xlsx')


# Create new column to rank paid OA policies (based on correspondence with JISC we think this is mostly hybrid journals with APCs - fees associated with pure gold OA are not generally recorded as additional_oa_fee)
  # Cutting out additional_oa_fee = "no" will cut out lots of policies from pure gold journals which don't have additional fees because all their articles are associated with fees. So we need to get these back in (see filter below).

  # Select only policies where paid OA is relevant (additional_oa_fee = yes or it's published only version of a pure gold journal, and remove submitted). 
sherpa_fee <- sherpa %>%
  filter((additional_oa_fee == "yes" | (listed_in_doaj == "yes" & article_version == "published")) & article_version != "submitted")

  # Create column 'rank_fee' to rank policies from most to least permissive
sherpa_fee$rank_fee[sherpa_fee$license2 == "cc_by" & sherpa_fee$copyright_owner != "publishers"] <- 1
sherpa_fee$rank_fee[sherpa_fee$license2 == "cc_by" & sherpa_fee$copyright_owner == "publishers"] <- 2
sherpa_fee$rank_fee[sherpa_fee$license2 == "cc_by_nd" & sherpa_fee$copyright_owner != "publishers"] <- 3
sherpa_fee$rank_fee[sherpa_fee$license2 == "cc_by_nd" & sherpa_fee$copyright_owner == "publishers"] <- 4
sherpa_fee$rank_fee[sherpa_fee$license2 == "cc_by_sa" & sherpa_fee$copyright_owner != "publishers"] <- 5
sherpa_fee$rank_fee[sherpa_fee$license2 == "cc_by_sa" & sherpa_fee$copyright_owner == "publishers"] <- 6
sherpa_fee$rank_fee[sherpa_fee$license2 == "no compliant license" & sherpa_fee$copyright_owner != "publishers"] <- 7
sherpa_fee$rank_fee[sherpa_fee$license2 == "no compliant license" & sherpa_fee$copyright_owner == "publishers"] <- 8

# openxlsx::write.xlsx(as.data.frame(sherpa_fee), 'sherpa_fee_ranked.xlsx')

# Sort both sherpa_fee and sherpa_green by ranks (since we will eventually select only the first/ most permissive policy for each)
sherpa_green <- sherpa_green[order(sherpa_green$rank_green),]
sherpa_fee <- sherpa_fee[order(sherpa_fee$rank_fee),]

# Select only first policy from each (if two policies are in the same rank then this will just pick the first since they are essentially the same for our purposes)
sherpa_green <- sherpa_green[!duplicated(sherpa_green$sherpa_id, incomparables = NA), ]
sherpa_fee <- sherpa_fee[!duplicated(sherpa_fee$sherpa_id, incomparables = NA), ]

# rename columns in sherpa_green and sherpa_fee to avoid confusion to make it clear which policy is which
sherpa_green <- sherpa_green %>% 
  rename(g_article_version = article_version, g_license = license, g_copyright_owner = copyright_owner, g_conditions = conditions, g_location.location = location.location,  g_embargo.amount = embargo.amount, g_embargo.units = embargo.units, g_embargo = embargo, g_license1 = license1, g_license2 = license2, g_compliant_repository = compliant_repository, g_copyright = copyright)

sherpa_fee <- sherpa_fee %>%
  rename(fee_article_version = article_version, fee_license = license, fee_copyright_owner = copyright_owner, fee_conditions = conditions, fee_location.location = location.location,  fee_embargo.amount = embargo.amount, fee_embargo.units = embargo.units, fee_embargo = embargo, fee_license1 = license1, fee_license2 = license2, fee_compliant_repository = compliant_repository, fee_copyright = copyright)


# Final process of reducing sherpa to one row per journal

  # Return only one row for each journal for sherpa and remove columns that vary by policy
sherpa_1row <- sherpa[!duplicated(sherpa$sherpa_id, incomparables = NA), ]
sherpa_1row <- sherpa_1row[,!(names(sherpa) %in% c("additional_oa_fee", "article_version", "license", "copyright_owner", "conditions", "location.location", "embargo.amount", "embargo.units", "embargo", "compliant_license", "license1", "license2", "compliant_repository", "copyright"))]
sherpa_all_policies <- sherpa

  # Merge sherpa_green and sherpa_fee into sherpa (essentially this first chooses the first for each of green and fee then merge fee into green and then that into sherpa)
sherpa_1row <- left_join(sherpa_1row, sherpa_green[ , c("sherpa_id", "g_article_version", "g_license", "g_copyright_owner", "g_conditions", "g_location.location", "g_embargo.amount", "g_embargo.units", "g_embargo", "g_license1", "g_license2", "g_compliant_repository", "g_copyright", "rank_green")], by = "sherpa_id")

sherpa_1row <- left_join(sherpa_1row, sherpa_fee[ , c("sherpa_id", "fee_article_version", "fee_license", "fee_copyright_owner", "fee_conditions", "fee_license1", "fee_license2", "fee_copyright", "rank_fee")], by = "sherpa_id")

# Remove duplicated rows -  I was told by JISC that this had been fixed (i.e. duplicate rows had been removed) but judging from what happened when I reran the code this is not correct. Since this hasn't been fixed I've added another command which sorts by permissibility (i.e. meaning that most permissive of duplicates will be retained)
sherpa_1row <- sherpa_1row[order(sherpa_1row$rank_green, sherpa_1row$rank_fee),]
sherpa_1row <- sherpa_1row[!duplicated(sherpa_1row$issn_print, incomparables = NA), ]
sherpa_1row <- sherpa_1row[!duplicated(sherpa_1row$issn_electronic, incomparables = NA), ]
    
    # renaming duplicate journal titles (as some seperate journals have the same name so you can't just delete duplicates)
sherpa_1row$j_title <- with(sherpa_1row, make.unique(as.character(j_title)))

#openxlsx::write.xlsx(as.data.frame(sherpa_1row), 'sherpa_1row.xlsx') # This is the complete SHERPA database (most permissive policies for each journal)

###########
# 4. LEFT-JOINING ESAC INTO DIMENSIONS MATCHING ON 'PUBLISHER'

merged_pvga <- left_join(dimensions, esac[ , c("Publisher", "has_ta")], by = c("publisher" = "Publisher"))

merged_pvga$has_ta[is.na(merged_pvga$has_ta)] <- "no" # this couldn't be created before the merge (i.e. it's highlighting which rows were not merged with esac entries with TAs)

###########
# 5. LEFT-JOINING SHERPA INTO MERGED DIMENSIONS-ESAC

# Convert ISSNs to remove NAs (just because this was causing issues with the below code- it is changed back below)
merged_pvga$issn2[is.na(merged_pvga$issn2)] <- "0999"
merged_pvga$issn3[is.na(merged_pvga$issn3)] <- "0999"
merged_pvga$issn4[is.na(merged_pvga$issn4)] <- "0999"

# Select rows to keep from sherpa
sherpa_keep_rows <- c("issn_print", "issn_electronic", "j_title", "sherpa_id", "listed_in_doaj", "sherpa_publisher", "system_metadata.uri", "open_access_prohibited", "g_article_version", "g_license", "g_copyright_owner", "g_conditions", "g_location.location", "g_embargo.amount", "g_embargo.units", "g_embargo", "g_license1", "g_license2", "g_compliant_repository", "g_copyright", "rank_green", "fee_article_version", "fee_license", "fee_copyright_owner", "fee_conditions", "fee_license1", "fee_license2", "fee_copyright", "rank_fee")

# Series of inner joins to find all possible matches
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


############
# 6. FINAL EDITS TO MERGED_PVGA

# Return ISSNs to missing
merged_pvga$issn2 <- gsub("0999", "", merged_pvga$issn2)
merged_pvga$issn3 <- gsub("0999", "", merged_pvga$issn3)
merged_pvga$issn4 <- gsub("0999", "", merged_pvga$issn4)

# Merge rank_green categories separated by article version as this doesn't matter in terms of compliance
merged_pvga$rank_green <- gsub("a", "", merged_pvga$rank_green)
merged_pvga$rank_green <- as.numeric(merged_pvga$rank_green)

# Generate new variable for is pure gold OA journal
merged_pvga$pure_gold <- "no"
merged_pvga$pure_gold[merged_pvga$listed_in_doaj == "yes" | merged_pvga$open_access_categories == "Pure gold"] <- "yes"

# Generate new variable for is hybrid journal
merged_pvga$is_hybrid <- "no"
merged_pvga$is_hybrid[!is.na(merged_pvga$fee_article_version) & merged_pvga$pure_gold == "no"] <- "yes"
merged_pvga$is_hybrid[is.na(merged_pvga$sherpa_id)] <- ""
merged_pvga$is_hybrid[merged_pvga$open_access_categories == "Hybrid" & merged_pvga$pure_gold == "no"] <- "yes"

# Generate new variable for is closed journal
merged_pvga$is_closed <- "yes"
merged_pvga$is_closed[is.na(merged_pvga$sherpa_id)] <- ""
merged_pvga$is_closed[merged_pvga$pure_gold == "yes" | merged_pvga$is_hybrid == "yes"] <- "no"

# Generate new variable for journal type
merged_pvga$journal_type <- ""
merged_pvga$journal_type[merged_pvga$pure_gold == "yes"] <- "Pure gold"
merged_pvga$journal_type[merged_pvga$is_hybrid == "yes"] <- "Hybrid"
merged_pvga$journal_type[merged_pvga$is_closed == "yes"] <- "Closed or insufficient information"

#table(merged_pvga$journal_type)

# Generate new variable for compliance (i.e. heirarchy of pure gold > green > ta > reasons for non compliance)
merged_pvga$compliance <- NA
merged_pvga$compliance[merged_pvga$journal_type == "Pure gold"] <- "c: pure gold"
merged_pvga$compliance[merged_pvga$rank_green <= 12 & merged_pvga$journal_type != "Pure gold"] <- "c: green oa"
merged_pvga$compliance[((merged_pvga$rank_green >12 | (is.na(merged_pvga$rank_green)) & merged_pvga$journal_type != "Pure gold" & merged_pvga$journal_type != "Closed or insufficient information") & merged_pvga$has_ta == "yes")] <- "c: only by ta"
merged_pvga$compliance[merged_pvga$journal_type == "Pure gold" & (merged_pvga$g_license2 == "no compliant license" | merged_pvga$fee_license2 == "no compliant license")] <- "nc: pure gold but non-compliant license"
merged_pvga$compliance[merged_pvga$rank_green >=13 & merged_pvga$rank_green <=18 & merged_pvga$has_ta == "no" & merged_pvga$journal_type != "Pure gold"] <- "nc: compliant green license and repository but has embargo"
merged_pvga$compliance[merged_pvga$rank_green >=19 & merged_pvga$rank_green <=24 & merged_pvga$has_ta == "no" & merged_pvga$journal_type != "Pure gold"] <- "nc: compliant green license but has embargo and no suitable repository"
merged_pvga$compliance[merged_pvga$rank_green >=25 & merged_pvga$rank_green <=26 & merged_pvga$has_ta == "no" & merged_pvga$journal_type != "Pure gold"] <- "nc: no compliant green license (zero embargo and suitable repository)"
merged_pvga$compliance[merged_pvga$rank_green >=27 & merged_pvga$rank_green <=28 & merged_pvga$has_ta == "no" & merged_pvga$journal_type != "Pure gold"] <- "nc: no compliant green license or repository (zero embargo)"
merged_pvga$compliance[merged_pvga$rank_green >=29 & merged_pvga$rank_green <=30 & merged_pvga$has_ta == "no" & merged_pvga$journal_type != "Pure gold"] <- "nc: no compliant green license and has embargo (suitable repository)"
merged_pvga$compliance[merged_pvga$rank_green >=31 & merged_pvga$rank_green <=32 & merged_pvga$has_ta == "no" & merged_pvga$journal_type != "Pure gold"] <- "nc: no compliant green license, has embargo and no suitable repository"
merged_pvga$compliance[is.na(merged_pvga$compliance) & !is.na(merged_pvga$sherpa_id) & is.na(merged_pvga$g_license2)] <- "nc: no green policy identified"
merged_pvga$compliance[is.na(merged_pvga$compliance) & merged_pvga$has_ta == "yes" & merged_pvga$journal_type != "Pure gold"] <- "c: only by ta"
merged_pvga$compliance[is.na(merged_pvga$compliance) & merged_pvga$has_ta == "yes" & is.na(merged_pvga$journal_type)] <- "c: only by ta"

# Generate new variable for compliant or not compliant
compliant <- c("c: pure gold", "c: green oa", "c: only by ta")
merged_pvga$compliant <- grepl(paste(compliant, collapse = "|"), merged_pvga$compliance)
#table(merged_pvga$compliant)

# Generate new variable for compliance2 with simplified categories (merging some non compliant) (needed to convert to character first to avoid errors)
merged_pvga$compliance2 <- NA
merged_pvga$compliance2 <- merged_pvga$compliance
merged_pvga$compliance2[merged_pvga$compliance2 == "nc: compliant green license and repository but has embargo" | merged_pvga$compliance2 == "nc: compliant green license but has embargo and no suitable repository"] <- "nc: compliant green license but has embargo"
merged_pvga$compliance2[merged_pvga$compliance2 == "nc: no compliant green license (zero embargo and suitable repository)" | merged_pvga$compliance2 == "nc: no compliant green license or repository (zero embargo)"] <- "nc: zero embargo but no compliant green license"
merged_pvga$compliance2[merged_pvga$compliance2 == "nc: no compliant green license and has embargo (suitable repository)" | merged_pvga$compliance2 == "nc: no compliant green license, has embargo and no suitable repository"] <- "nc: no compliant green license and has embargo"

# Generate new variable for compliance3 with simplified categories (merging all non-compliant)
merged_pvga$compliance3 <- merged_pvga$compliance2
merged_pvga$compliance3[merged_pvga$compliance3 == "nc: pure gold but non-compliant license" | merged_pvga$compliance3 == "nc: compliant green license but has embargo" | merged_pvga$compliance3 == "nc: zero embargo but no compliant green license" | merged_pvga$compliance3 ==  "nc: no compliant green license and has embargo" | merged_pvga$compliance3 ==  "nc: no green policy identified"] <- "not compliant"

# Binary embargo variable
merged_pvga$g_embargo2[merged_pvga$g_embargo == 0] <- TRUE
merged_pvga$g_embargo2[merged_pvga$g_embargo != 0] <- FALSE
merged_pvga$g_embargo2[is.na(merged_pvga$g_embargo)] <- NA

# Binary license variable
merged_pvga$g_license3[merged_pvga$g_license2 != "no compliant license"] <- TRUE
merged_pvga$g_license3[merged_pvga$g_license2 == "no compliant license"] <- FALSE
merged_pvga$g_license3[is.na(merged_pvga$g_license2)] <- NA

# Recode open_access_categories as factor
merged_pvga$open_access_categories <- factor(merged_pvga$open_access_categories, ordered = TRUE, levels = c("Pure gold", "Hybrid", "Green, published", "Green, accepted", "Bronze", "Green, submitted", "Closed"))
merged_pvga$open_access_categories2 <- factor(merged_pvga$open_access_categories2, ordered = TRUE, levels = c("Pure gold", "Hybrid gold", "Green", "Closed"))

# Turn license vars into factor to order by permissiveness (just to make outputs clearer)
merged_pvga$g_license1 <- factor(merged_pvga$g_license1, ordered = TRUE, c("no license requirement", "cc_by", "cc_by_nd", "cc_by_sa", "cc_by_nc", "cc_by_nc_nd", "cc_by_nc_sa", "bespoke license", "NA - policy for published version in full OA journal"))
merged_pvga$g_license2 <- factor(merged_pvga$g_license2, ordered = TRUE, c("cc_by", "cc_by_nd", "cc_by_sa", "no compliant license", "NA - policy for published version in full OA journal"))

# Recode compliance vars as factors
merged_pvga$compliance <- factor(merged_pvga$compliance, ordered = TRUE, levels = c("c: pure gold", "c: green oa", "c: only by ta", "nc: pure gold but non-compliant license", "nc: compliant green license and repository but has embargo", "nc: compliant green license but has embargo and no suitable repository", "nc: no compliant green license (zero embargo and suitable repository)", "nc: no compliant green license or repository (zero embargo)", "nc: no compliant green license and has embargo (suitable repository)", "nc: no compliant green license, has embargo and no suitable repository", "nc: no green policy identified"))

merged_pvga$compliance2 <- factor(merged_pvga$compliance2, ordered = TRUE, levels = c("c: pure gold", "c: green oa", "c: only by ta", "nc: pure gold but non-compliant license", "nc: compliant green license but has embargo", "nc: zero embargo but no compliant green license", "nc: no compliant green license and has embargo", "nc: no green policy identified"))

merged_pvga$compliance3 <- factor(merged_pvga$compliance3, ordered = TRUE, levels = c("c: pure gold", "c: green oa", "c: only by ta", "not compliant"))

# Recode journal_type as factor
merged_pvga$journal_type <- factor(merged_pvga$journal_type, ordered = TRUE, levels = c("Pure gold", "Hybrid", "Closed or insufficient information"))

# Remove unnecessary variables (these were metadata not used for analysis or only needed for testing)
merged_pvga <- subset(merged_pvga, select = -c(category_uoa, funders, linkout, issn_electronic, issn_print, j_title, sherpa_publisher, system_metadata.uri, g_copyright_owner, g_conditions, g_location.location, g_embargo.amount, g_embargo.units, fee_license, fee_copyright_owner, fee_conditions))



############
# 7. WRITE MERGED_PVGA TO EXCEL

openxlsx::write.xlsx(as.data.frame(merged_pvga), 'merged_pvga.xlsx')

# 2017-20: We have a Sherpa link for 98.1% of the publications in the Dimensions sample. We have green (no fee) routes for almost all of these and paid (fee) routes for about three-fifths. This leaves us with 2895 articles in Dimensions not covered by Sherpa data (made up of 919 journals)
# 2018 only: We have a Sherpa link for 98.1% of the publications in the Dimensions sample. We have green (no fee) routes for almost all of these and paid (fee) routes for about three-fifths. This leaves us with 784 articles in Dimensions not covered by Sherpa data (made up of 394 journals). From a check of these journals I don't think it is a matching issue - these are likely gaps in Sherpa's coverage.