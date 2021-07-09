# PREPARING AND MERGING DATA FOR PUBLICATION VENUE GAP ANALYSIS

# Author: Tom Kenny
# Created: August 2020
# Last Updated: April 2021

# Project: this is part of the R Project OA publication venue gap analysis

# Purpose of code: This code merges data from Dimensions, SHERPA RoMEO and ESAC to be used for analysing open access policies.

# Prerequisite: This code requires data from Dimensions and SHERPA RoMEO - the former was sent to TK by Katie Shamash from Dimensions on 1st April 2021 and is imported and cleaned using the script 'Dimensions import and cleaning'. Sherpa was downloaded via API using Tom Kenny's code (SHERPA RoMEO API (clean)). Both data are saved in the Publication Venue Gap Analysis folder in the Policy Analysis drive.

# Clear work space
rm(list=ls())

#library(readr)
library(tidyverse)
library(openxlsx)
library(xml2)
library(rvest)

######### 1. IMPORTING DIMENSIONS DATA----
# NB. This data was downloaded, cleaned and filtered in script "Downloading and cleaning Dimensions data"

load("Data/dimensions_filtered.Rda")

#XXXXXXXXXXX
# 2. DOWNLOADING AND PREPARING ESAC DATA----

library(rvest)

  # Download ESAC data - this downloads the latest version.
esac_url <- "https://esac-initiative.org/about/transformative-agreements/agreement-registry/"
esac <- xml2::read_html(esac_url)
esac <- rvest::html_table(esac)[[1]] %>%
  tibble::as_tibble(.name_repair = "unique")

  # Select only JISC-negotiated deals
esac <- dplyr::filter(esac, Organization == "Jisc")

  # Rename 'publisher' names to match Dimensions data (or else they won't match later on)
# American Physiological Society is fine
esac$Publisher <- gsub("Association for Computing Machinery", "Association for Computing Machinery (ACM)", esac$Publisher)
# Bioscientifica is fine
esac$Publisher <- gsub("BMJ Publishing", "BMJ", esac$Publisher)
esac$Publisher <- gsub("Brill", "Brill Academic Publishers", esac$Publisher)
esac$Publisher <- gsub("Cambridge University Press", "Cambridge University Press (CUP)", esac$Publisher)
esac$Publisher <- gsub("Cold Spring Harbor Laboratory Press", "Cold Spring Harbor Laboratory", esac$Publisher)
esac$Publisher <- gsub("Company of Biologists", "The Company of Biologists", esac$Publisher)
esac$Publisher <- gsub("European Respiratory Society", "European Respiratory Society (ERS)", esac$Publisher)
# Future Science group is fine
# IOP Publishing is fine
# IWA Publishing is fine
esac$Publisher <- gsub("Karger", "Karger Publishers", esac$Publisher)
# Microbiology society is fine
esac$Publisher <- gsub("Oxford University Press", "Oxford University Press (OUP)", esac$Publisher)
# Portland Press is fine
# Rockefeller University Press is fine
# Royal College of General Practitioners is fine
# Royal Irish academy has 0 articles in our sample
esac$Publisher <- gsub("Sage", "SAGE Publications", esac$Publisher)
esac$Publisher <- gsub("Springer Nature", "Springer", esac$Publisher) # this is because I split out Springer and Nature when cleaning Dimensions (since they have different TAs)
esac$Publisher <- gsub("The Geological Society of London", "Geological Society of London", esac$Publisher)
# The Royal Society is fine
# Thieme is fine
esac$Publisher <- gsub("Walter de Gruyter", "De Gruyter", esac$Publisher)
# Wiley is fine

# Add confirmed new agreements not yet on ESACC----
  # ESAC was not updated with these agreements as of 1.04.21 so they need to be added in manually
temp_ta <- data.frame(c("Taylor & Francis", "BMJ", "Royal Society of Chemistry (RSC)"), "United Kingdom", "Jisc", 9999, "09/03/2021", NA, NA)
names(temp_ta) <- c("Publisher", "Country", "Organization", "Annual publications", "Start date", "End date", "Details/ ID")
esac <- bind_rows(esac, temp_ta)

  # Create new column has_ta = yes. This means that when it is merged into the dimensions data it will highlight all the articles with a publisher with ta.
esac$has_ta <- "yes"

  # Remove duplicates (Karger has two agreements which might create duplicates in the merge)
esac <- esac[!duplicated(esac$Publisher, incomparables = NA), ]

#XXXXXXXXXXXX
# 3. PREPARING SHERPA ROMEO DATA----
  # NB. See code 'Sherpa Romeo API (clean)' for how to download this data'
  # NB2. We were advised by JISC that missing data should be interpreted as 'no requirement' which means missing is actually equivalent to the most permissive policy option. However, we were later advised this is not the case for the license.

load("Data/Raw data/sherpa.Rda")

#a. general cleaning of data----
  # rename title and id to avoid confusion with title in Dimensions
sherpa <- rename(sherpa, j_title = title, sherpa_id = id, sherpa_publisher = name)

  # Lowercase j_title to aid matching with Dimensions
sherpa$j_title <- tolower(sherpa$j_title)

  # Remove unnecessary columns
sherpa <- sherpa[,!(names(sherpa) %in% c("issn_NA","issn_legacy", "pubpol_id"))]

#b. recode embargo variable to simplify
  # Recode embargo to simpler form (first 0, 6, 12, >12, then just zero or not)
sherpa$embargo_grouped <- NA
sherpa$embargo_grouped[sherpa$embargo == 0] <- 0
sherpa$embargo_grouped[sherpa$embargo > 0 & sherpa$embargo <= 6] <- 6
sherpa$embargo_grouped[sherpa$embargo > 6 & sherpa$embargo <= 12] <- 12
sherpa$embargo_grouped[sherpa$embargo > 12] <- 13

    # Binary embargo variable
sherpa$embargo_zero[sherpa$embargo == 0] <- "zero embargo"
sherpa$embargo_zero[sherpa$embargo != 0] <- "has embargo"

#c. Work with license variable----
  # Create new columns recoding license into CC-BY, CC-BY-ND, CC-BY-SA and license_not_compliant (return missing data as CC-BY(m))
      # NB Jisc advised us that missing means no requirement making it the most permissive category
      # Alternative: could do a case_when to clean this up and avoid for loop

    # change blank values into no license requirement
sherpa$license[sherpa$license == ""] <- "no license requirement"

    # Derive license_single to simplify license and only return one license for each policy (the most permissive)
sherpa$license_single <- sherpa$license
sherpa$license_single[grepl("cc_by,", sherpa$license_single)| sherpa$license_single == "cc_by"] <- "cc_by"
sherpa$license_single[grepl("public_domain,", sherpa$license_single)| sherpa$license_single == "public_domain" | sherpa$license_single == "cc_public_domain"] <- "cc_by"
sherpa$license_single[grepl("gnu_gpl,", sherpa$license_single)| sherpa$license_single == "gnu_gpl" | sherpa$license_single == "cc_gnu_gpl"] <- "cc_by"
sherpa$license_single[grepl("cc_by_sa,", sherpa$license_single)| sherpa$license_single == "cc_by_sa"] <- "cc_by_sa"
sherpa$license_single[grepl("cc_by_nd,", sherpa$license_single)| sherpa$license_single == "cc_by_nd"] <- "cc_by_nd"
sherpa$license_single[grepl("cc_by_nc,", sherpa$license_single)| sherpa$license_single == "cc_by_nc"] <- "cc_by_nc"
sherpa$license_single[grepl("cc_by_nc_sa,", sherpa$license_single)| sherpa$license_single == "cc_by_nc_sa"] <- "cc_by_nc_sa"
sherpa$license_single[grepl("cc_by_nc_nd,", sherpa$license_single)| sherpa$license_single == "cc_by_nc_nd"] <- "cc_by_nc_nd"
sherpa$license_single[grepl("bespoke_license,", sherpa$license_single)| sherpa$license_single == "bespoke_license"] <- "bespoke_license"
sherpa$license_single[grepl("all_rights_reserved,", sherpa$license_single)| sherpa$license_single == "all_rights_reserved"] <- "all_rights_reserved"
  
    # Simplify license further into new var license_compliant merging all licenses which are never compliant in the old policy or new policy scenarios
sherpa$license_compliant <- sherpa$license_single
sherpa$license_compliant[sherpa$license_compliant != "cc_by" & sherpa$license_compliant != "cc_by_nd" & sherpa$license_compliant != "cc_by_nc"] <- "no compliant license"

    # Binary license variable - is cc_by or isn't
sherpa$license_cc_by <- NA
sherpa$license_cc_by[sherpa$license_single != "cc_by"] <- "not cc_by"
sherpa$license_cc_by[sherpa$license_single == "cc_by"] <- "cc_by"

#d. Preparing other variables to rank Sherpa policies ----

    # Create new column recoding repository into compliant and not compliant. The list of compliant repositories was agreed with Rachel Bruce (essentially it's proper repositories rather than author or institutions homepages) 
sherpa$location.location[sherpa$location.location == ""] <- "no repository requirement"
sherpa$location.location[is.na(sherpa$location.location)] <- "no repository requirement"

compliant_repository <- c("any_website", "any_repository", "institutional_repository", "non_commericial_institutional_repository", "non_commercial_repository", "non_commercial_subject_repository", "subject_repository", "non_commerical_website", "funder_designated_location", "preprint_repository", "no repository requirement")
sherpa$compliant_repository <- grepl(paste(compliant_repository, collapse = "|"), sherpa$location.location)

    # Create new columns recoding article_version into version (only includes published vs does not include published)
sherpa$article_version[sherpa$article_version == ""] <- "no version specified"
sherpa$version_published <- grepl(paste(c("published", "no version specified"), collapse = "|"), sherpa$article_version)

    # Replace empty results for copyright and create new simplified copyright column (either authors or publishers)
sherpa$copyright_owner[sherpa$copyright_owner == "" | is.na(sherpa$copyright_owner)] <- "no copyright requirement"

sherpa$copyright <- ifelse(sherpa$copyright_owner %in% c("authors", "authors_institution", "no copyright requirement"), "authors", "publishers")


#e. Create new column to rank Green OA policies----

  # Select only policies where green OA is relevant (additional_oa_fee = no). I originally also excluded listed_in_doaj here (DOAJ is a listing of gold OA journals) but then realised that they many pure gold journals still do have green OA policies (this is presumably why they are in sherpa - in fact almost none of the listed_in_doaj policies are associated with a fee). Also remove policies relating to submitted articles (these aren't peer reviewed)
sherpa_green <- sherpa %>%
  filter(additional_oa_fee == "no", article_version != "submitted", location.location != "this_journal")

  #Create green_ranking which will be used to select the most permissive green policy for each journal

require(utils)

      # Use expand.grid to create table with every option. NB. The order of both the variables and strings is important here - the most important variables need to come last for the ordering to work and the most permissive options need to come first within each string.
green_ranking <- expand.grid(version_published = c(TRUE, FALSE), copyright = c("authors", "publishers"), compliant_repository = c(TRUE, FALSE), embargo_grouped = c(0, 6, 12, 13), license_single = c("cc_by", "cc_by_nd", "cc_by_nc", "cc_by_sa", "cc_by_nc_nd", "cc_by_nc_sa", "bespoke license", "all rights reserved", "no license requirement"), embargo_zero = c("zero embargo", "has embargo"), license_cc_by = c("cc_by", "not cc_by"))

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
  rename(g_article_version = article_version, g_license = license, g_license_single = license_single, g_license_compliant = license_compliant, g_license_cc_by = license_cc_by, g_copyright_owner = copyright_owner, g_conditions = conditions, g_location.location = location.location,  g_embargo.amount = embargo.amount, g_embargo.units = embargo.units, g_embargo = embargo, g_embargo_grouped = embargo_grouped, g_embargo_zero = embargo_zero, g_compliant_repository = compliant_repository, g_copyright = copyright, g_version_published = version_published)

#f. Create new column to rank paid OA policies----
  #(based on correspondence with JISC we think this is mostly hybrid journals with APCs - fees associated with pure gold OA are not generally recorded as additional_oa_fee)
  # Cutting out additional_oa_fee = "no" will cut out lots of policies from pure gold journals which don't have additional fees because all their articles are associated with fees. So we need to get these back in (see filter below).

  # Select only policies where paid OA is relevant
      #additional_oa_fee = yes or 
      #it's published only version of a pure gold journal (listed_in_doaj is helpful because DOAJ is a listing of pure gold journal) (this is assuming that quite a lot of pure gold policies are not associated with an additional fee because there is always a standard fee for publishing in that journal) or,
      # Article version is submitted or accepted only (there are some policies associated with a fee for accepted/ submitted versions but these can not be compliant and may be errors - I left in ones that also have published version). 
sherpa_fee <- sherpa %>% mutate(unique_id = 1:nrow(sherpa)) %>%
  filter((additional_oa_fee == "yes" | (listed_in_doaj == "yes" & grepl("published", article_version)) | (article_version == "published" & location.location == "this_journal")) &
           !article_version %in% c("submitted", "accepted", "submitted, accepted") &
           !embargo.amount > 0)

    # Create column 'rank_fee' to rank policies from most to least permissive

      # Use expand.grid to create table with every option. NB. The order is important here - the most important variables need to come last for the ordering to work. The only variables we are interested in here are license (which must be cc_by) and copyright retention (authors preferable)
fee_ranking <- expand.grid(copyright = c("authors", "publishers"), license_cc_by = c("cc_by", "not cc_by"))

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
  rename(fee_article_version = article_version, fee_license = license, fee_license_single = license_single, fee_license_compliant = license_compliant, fee_license_cc_by = license_cc_by, fee_copyright_owner = copyright_owner, fee_conditions = conditions, fee_location.location = location.location,  fee_embargo.amount = embargo.amount, fee_embargo.units = embargo.units, fee_embargo = embargo, fee_embargo_grouped = embargo_grouped, fee_embargo_zero = embargo_zero, fee_compliant_repository = compliant_repository, fee_copyright = copyright, g_version_published = version_published)


#g. Final process of reducing sherpa to one row per journal----

  # Return only one row for each journal for sherpa and remove columns that vary by policy
sherpa_1row <- sherpa[!duplicated(sherpa$sherpa_id, incomparables = NA), ]
sherpa_1row <- sherpa_1row[,!(names(sherpa) %in% c("additional_oa_fee", "article_version", "license", "copyright_owner", "conditions", "location.location", "embargo.amount", "embargo.units", "embargo", "embargo1", "embargo_grouped", "embargo_zero",  "compliant_license", "license_single", "license_compliant", "license_cc_by", "compliant_repository", "copyright", "version_published"))]
sherpa_all_policies <- sherpa # this is just renaming it to make it clearer which is which

  # Merge sherpa_green and sherpa_fee into sherpa (essentially this first chooses the first for each of green and fee then merge fee into green and then that into sherpa)
sherpa_1row <- left_join(sherpa_1row, sherpa_green_1row[ , c("sherpa_id", "g_article_version", "g_version_published", "g_license", "g_license_single", "g_license_compliant", "g_license_cc_by", "g_copyright_owner", "g_conditions", "g_location.location", "g_embargo.amount", "g_embargo.units", "g_embargo", "g_embargo_grouped","g_embargo_zero", "g_compliant_repository", "g_copyright", "rank_green")], by = "sherpa_id")

sherpa_1row <- left_join(sherpa_1row, sherpa_fee_1row[ , c("sherpa_id", "fee_article_version", "fee_license", "fee_license_single", "fee_license_compliant", "fee_license_cc_by", "fee_copyright_owner", "fee_conditions",  "fee_copyright", "rank_fee")], by = "sherpa_id")

# Remove duplicated rows -  I was told by JISC that this had been fixed (i.e. duplicate rows had been removed) but judging from what happened when I reran the code (6.4.21) this is not correct. Since this hasn't been fixed I've added another command which sorts by permissibility (i.e. meaning that most permissive of duplicates will be retained)
sherpa_1row <- sherpa_1row[order(sherpa_1row$rank_green, sherpa_1row$rank_fee),]
sherpa_1row <- sherpa_1row[!duplicated(sherpa_1row$issn_print, incomparables = NA), ]
sherpa_1row <- sherpa_1row[!duplicated(sherpa_1row$issn_electronic, incomparables = NA), ]

    # renaming duplicate journal titles (as some seperate journals have the same name so you can't just delete duplicates)
sherpa_1row$j_title <- with(sherpa_1row, make.unique(as.character(j_title)))

#openxlsx::write.xlsx(as.data.frame(sherpa_1row), 'Data/sherpa_1row.xlsx') # This is the complete SHERPA database (most permissive policies for each journal)

#XXXXXXXXXX
# 4. IMPORTING UNPAYWALL DATA ON GREEN OA----
load("Data/unpaywall_green_plus_gold_licences.RData")


#XXXXXXXXXX
# 5. LEFT-JOINING ESAC INTO DIMENSIONS MATCHING ON 'PUBLISHER'----

merged_pvga <- left_join(dimensions, esac[ , c("Publisher", "has_ta")], by = "Publisher")

merged_pvga$has_ta[is.na(merged_pvga$has_ta)] <- "no" # this couldn't be created before the merge (i.e. it's highlighting which rows were not merged with esac entries with TAs)

# TEST tO CHECK IF TAs UP TO DATE
ta_test <- merged_pvga %>% filter(!duplicated(Publisher), has_ta == "yes") # number of observations should be the same as the number of observations in ESAC (assuming every publisher has at least one article in our sample) - if it is lower it probably means a new publisher has been added to ESAC and that publisher has a different name in Dimensions.

#XXXXXXXXXXXX
# 6. JOINING SHERPA INTO MERGED DIMENSIONS-ESAC----

# Select rows to keep from sherpa
sherpa_keep_rows <- c("issn_print", "issn_electronic", "j_title", "sherpa_id", "listed_in_doaj", "sherpa_publisher", "system_metadata.uri", "open_access_prohibited", "g_article_version", "g_license", "g_license_single", "g_license_compliant", "g_license_cc_by", "g_copyright_owner", "g_conditions", "g_location.location", "g_embargo.amount", "g_embargo.units", "g_embargo", "g_embargo_grouped", "g_embargo_zero", "g_compliant_repository", "g_copyright", "rank_green", "fee_article_version", "fee_license", "fee_license_single", "fee_license_compliant", "fee_license_cc_by", "fee_copyright_owner", "fee_conditions", "fee_copyright", "rank_fee")

# Series of inner joins to find all possible matches - the reason this is necessary is because no one variable can identify all the matches (e.g. because the wrong ISSN is listed, or because the title is formatted differently in sherpa and dimensions)
merged_pvga1 <- inner_join(merged_pvga[!is.na(merged_pvga$ISSN),], sherpa_1row[ , sherpa_keep_rows], by = c("ISSN" = "issn_print"))
merged_pvga2 <- inner_join(merged_pvga[!is.na(merged_pvga$e.ISSN),], sherpa_1row[ , sherpa_keep_rows], by = c("e.ISSN" = "issn_electronic"))
merged_pvga3 <- inner_join(merged_pvga, sherpa_1row[ , sherpa_keep_rows], by = c("Source.title" = "j_title"))

# Bind rows to bring together all matching rows, then keep only first match for each article
merged_pvga4 <- bind_rows(merged_pvga1, merged_pvga2, merged_pvga3) %>%
  filter(!duplicated(DOI))
rm(merged_pvga1, merged_pvga2, merged_pvga3)

# Identify rows with no match
merged_pvga0 <- left_join(merged_pvga, merged_pvga4[ , c("DOI", "sherpa_id")], by = "DOI") %>%
  filter(is.na(sherpa_id))

# Merge these non-matched rows with the matched rows
merged_pvga <- bind_rows(merged_pvga0, merged_pvga4)

# 7. JOINING UNPAYWALL INTO MERGED DIMENSIONS-ESAC-SHERPA----

merged_pvga <- merged_pvga %>%
  left_join(unpaywall_green_plus_gold_licences, by = c("DOI" = "doi"))

#XXXXXXXXXXXXX
# 8. EDITS TO MERGED_PVGA----

# Refine Open Access variable to match UKRI policy----
  # Dimensions ranks Bronze OA > Green OA, but UKRI ranks Green OA > Bronze OA and considers Bronze equivalent to closed. This means that rather than just assigning Bronze OA as 'Closed', we need to reallocate it to Green and Closed depending on whether AAM/VoR green route is available
  # Dimensions has a green: submitted category which we would consider closed

merged_pvga$Open.Access_ukri <- merged_pvga$Open.Access
merged_pvga$Open.Access_ukri[merged_pvga$Open.Access_ukri == "Hybrid"] <- "Hybrid gold" # to avoid confusion with journal type
merged_pvga$Open.Access_ukri[merged_pvga$Open.Access_ukri == "Bronze" | merged_pvga$Open.Access_ukri == "Green, Submitted" | merged_pvga$Open.Access_ukri == "Closed"] <- "Closed" #since these are all closed from the perspective of UKRI policy
merged_pvga$Open.Access_ukri[merged_pvga$Open.Access_ukri %in% c("Green, Published", "Green, Accepted") |
                              (merged_pvga$Open.Access_ukri == "Closed" & !is.na(merged_pvga$upw_green_version))] <- "Green"

# Generate new variables for journal type----
    # NB when using journal_type variable we are looking forward at potential compliance. This means that if journals were not open access when article was published, but are now open access (i.e. listed in DOAJ but Open.Access != Pure Gold) then we should count them as open access. On the other hand there are some journals which are recorded as Pure Gold in Dimensions, but are not listed in DOAJ, but manual checking of 5 of these journals showed them to be either incorrectly labelled as not in DOAJ, or journals which were listed in DOAJ but are now closed. This means it is reasonable to class both listed in DOAJ and Open.Access = Pure Gold as signifying pure gold.
merged_pvga$journal_type <- NA

merged_pvga$journal_type[merged_pvga$listed_in_doaj == "yes" | merged_pvga$Open.Access == "Pure Gold"] <- "Pure Gold"
merged_pvga$journal_type[is.na(merged_pvga$journal_type) & (!is.na(merged_pvga$fee_article_version) | merged_pvga$Open.Access == "Hybrid")] <- "Hybrid"
merged_pvga$journal_type[is.na(merged_pvga$journal_type)] <- "Closed or insufficient information"

# Create variable for number of articles per journal----
articles_per_journal = merged_pvga %>% group_by(Source.title) %>% count()
merged_pvga <- left_join(merged_pvga, articles_per_journal, by = "Source.title") %>%
  rename(articles_in_journal = n)

# Create value TA_split to give breakdown of proportion of articles covered by current and target TAs
# NB this code was previously in the analysis script but it makes more sense to have it here

# List of when TAs started
ta_2019 = c("Springer")
ta_2020 = c("European Respiratory Society (ERS)", "IOP Publishing", "IWA Publishing", "Karger Publishers", "Microbiology Society", "Portland Press", "Rockefeller University Press", "SAGE Publications", "Thieme", "Wiley")
ta_2021 = c("American Physiological Society", "Association for Computing Machinery (ACM)", "Bioscientifica", "Brill Academic Publishers", "Cambridge University Press (CUP)", "Cold Spring Harbor Laboratory", "The Company of Biologists", "Future Science Group", "Royal College of General Practitioners", "Royal Irish Academy", "Geological Society of London", "The Royal Society", "De Gruyter", "Oxford University Press (OUP)", "Taylor & Francis", "BMJ", "Royal Society of Chemistry (RSC)") 
target_tas <- c("Elsevier", "American Chemical Society (ACS)", "Wolters Kluwer", "Nature")

# Create new variable to split up TAs
merged_pvga$ta_split <- "No TA"
merged_pvga$ta_split[merged_pvga$Publisher %in% ta_2019] <- "2019"
merged_pvga$ta_split[merged_pvga$Publisher %in% ta_2020] <- "2020"
merged_pvga$ta_split[merged_pvga$Publisher %in% ta_2021] <- "2021"
merged_pvga$ta_split[merged_pvga$Publisher %in% target_tas] <- "target TAs"

# Remove unnecessary variables----
  # (these were mostly metadata not used for analysis or only needed for testing)
merged_pvga <- subset(merged_pvga,
                      select = -c(issn_electronic, issn_print, j_title, sherpa_publisher, system_metadata.uri, g_copyright_owner,
                                  g_conditions, g_location.location, g_embargo.amount, fee_license, fee_copyright_owner, fee_conditions))

#XXXXXXXXXXXXX
#9. CREATE COMPLIANCE VARIABLES TO INDICATE COMPLIANCE WITH DIFFERENT POLICY SCENARIOS----

  # NB. NOTE FOR METHODOLOGY: Since we don't have Sherpa matches for all the articles, around 2.5% do not have data on license or embargo. 30% of these are recorded as Pure Gold OA in Dimensions and 25% as Hybrid  Gold, so we assume these have a cc_by license for purposes of estimating compliance (given this is true for the vast majority of these journals where we do have data). In practice, we do this by introducing 'NA' higher in the heirarchy than "not cc_by" in the compliance tables below. While some of the remaining 45% are recorded as Green by Dimensions, we can not reasonably record these as being green for potential compliance purposes because we have no data on the license or embargos. 

#a. Create Cartesian tables for compliance with current policy.----
  # These basically list out every possible combination of the key variables we are interested in for each element of the policy. We need to do several separate tables to make it simpler.
    
    #Table 1: Compliance with gold OA
compliance_fee <- expand.grid(
  journal_type = c("Pure Gold", "Hybrid", "Closed or insufficient information"),
  fee_license_cc_by = c("cc_by", NA, "not cc_by"))

compliance_fee <- compliance_fee %>%
  mutate(num_fee = 1:nrow(compliance_fee))

  # Table 2: compliance with green OA in current policy

    # There is an exception in the current policy for researchers funded by AHRC and ESRC, so need to derive a temporary variable for this
merged_pvga$embargo_exception <- NA
merged_pvga$embargo_exception[merged_pvga$ukri_funders %in% c("AHRC", "ESRC")] <- TRUE # not using grepl her as if other ukri funders are involved then exception shouldn't apply

compliance_current_green <- expand.grid(
  g_embargo_grouped = c(0, 6, 12, 13),
  embargo_exception = c(TRUE, NA),
  g_license_compliant = c("cc_by", "cc_by_nd", "cc_by_nc", "no compliant license"))

compliance_current_green <- compliance_current_green %>%
  mutate(num_current_green = 1:nrow(compliance_current_green))

  # Table 3: compliance with green OA in new policy
compliance_new_green <- expand.grid(
  g_embargo_zero = c("zero embargo", "has embargo"),
  g_license_compliant = c("cc_by", "cc_by_nd", "cc_by_nc", "no compliant license")) # this will lead to one article with NA as there is one cc_by_nd, but including it would make the table much more complex)

compliance_new_green <- compliance_new_green %>%
  mutate(num_new_green = 1:nrow(compliance_new_green))

  # Then merge all these in to create new columns showing the ranks in merged_pvga
merged_pvga <- left_join(merged_pvga, compliance_fee)
merged_pvga <- left_join(merged_pvga, compliance_current_green)
merged_pvga <- left_join(merged_pvga, compliance_new_green)

merged_pvga$num_current_green[is.na(merged_pvga$num_fee)] <- 99
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
merged_pvga$compliance_current[is.na(merged_pvga$compliance_current)] <- "not compliant"

    # simplified binary version of compliance_current (to make analysis tables/ charts easier)
merged_pvga$compliance_current2 <- merged_pvga$compliance_current
merged_pvga$compliance_current2[merged_pvga$compliance_current2 != "not compliant"] <- "compliant"


#c. Potential compliance with new policy scenario 1 (hybrid gold allowed)----
  # This looks at whether the most permissive policies for each journal/ article should provide a route to compliance with the current UKRI policy.   Routes to compliance with this policy are:
    # a. published gold OA (pure or hybrid) with a CC_BY license
    # b. published green OA with a cc_by license and zero embargo

merged_pvga$compliance_new_hybrid <- NA
merged_pvga$compliance_new_hybrid[merged_pvga$num_fee %in% c(1,4)] <- "c: pure gold"
merged_pvga$compliance_new_hybrid[merged_pvga$num_fee %in% c(2,5)] <- "c: hybrid gold"
merged_pvga$compliance_new_hybrid[!merged_pvga$num_fee %in% c(1,2,4,5) & merged_pvga$num_new_green %in% c(1,3)] <- "c: confirmed green oa"
merged_pvga$compliance_new_hybrid[is.na(merged_pvga$compliance_new_hybrid)] <- "not compliant"

    # simplified binary version of compliance_new_hybrid

merged_pvga$compliance_new_hybrid2 <- merged_pvga$compliance_new_hybrid
merged_pvga$compliance_new_hybrid2[merged_pvga$compliance_new_hybrid2 != "not compliant"] <- "compliant"

#d. Potential compliance with new policy scenario 2 (no hybrid gold)----
# This looks at whether the most permissive policies for each journal/ article should provide a route to compliance with the current UKRI policy. Routes to compliance with this policy are:
# a. published gold OA (pure or hybrid covered by a TA) with a CC_BY license
# b. published green OA with a cc_by license and zero embargo

merged_pvga$compliance_new <- NA
merged_pvga$compliance_new[merged_pvga$num_fee %in% c(1,4)] <- "c: pure gold"
merged_pvga$compliance_new[merged_pvga$has_ta == "yes" & merged_pvga$num_fee %in% c(2,5)] <- "c: hybrid gold with a TA"
merged_pvga$compliance_new[!merged_pvga$num_fee %in% c(1,4) & !(merged_pvga$has_ta == "yes" & merged_pvga$num_fee %in% c(2,5)) & merged_pvga$num_new_green %in% c(1,3)] <- "c: confirmed green oa"
merged_pvga$compliance_new[is.na(merged_pvga$compliance_new)] <- "not compliant"

    # simplified binary version of compliance_new

merged_pvga$compliance_new2 <- merged_pvga$compliance_new
merged_pvga$compliance_new2[merged_pvga$compliance_new2 != "not compliant"] <- "compliant"

#e. Potential compliance with new policy scenario 3 (funding for full OA only)----
# This looks at whether the most permissive policies for each journal/ article should provide a route to compliance with the current UKRI policy. Routes to compliance with this policy are:
# a. published in full gold OA journal with a CC_BY license
# b. published green OA with a cc_by license and zero embargo

merged_pvga$compliance_new_pure <- NA
merged_pvga$compliance_new_pure[merged_pvga$num_fee %in% c(1,4)] <- "c: pure gold"
merged_pvga$compliance_new_pure[!merged_pvga$num_fee %in% c(1,4) & merged_pvga$num_new_green %in% c(1,3)] <- "c: confirmed green oa"
merged_pvga$compliance_new_pure[is.na(merged_pvga$compliance_new_pure)] <- "not compliant"

#f. Compliance_new, if JISC achieves target TAs----

merged_pvga$compliance_new_target_TAs <- NA
merged_pvga$compliance_new_target_TAs[merged_pvga$num_fee %in% c(1,4)] <- "c: pure gold"
merged_pvga$compliance_new_target_TAs[merged_pvga$ta_split %in% c("2019","2020","2021", "target TAs") & merged_pvga$num_fee %in% c(2,5)] <- "c: hybrid gold with a TA"
merged_pvga$compliance_new_target_TAs[!merged_pvga$num_fee %in% c(1,4) & !(merged_pvga$ta_split %in% c("2019","2020","2021", "target TAs") &
                                                                             merged_pvga$num_fee %in% c(2,5)) & merged_pvga$num_new_green %in% c(1,3)] <- "c: confirmed green oa"
merged_pvga$compliance_new_target_TAs[is.na(merged_pvga$compliance_new_target_TAs)] <- "No supported route"


#####8. FINAL EDITS TO MERGED PVGA----

# a. Record vars as factors and order them----

  # Recode Open.Access as factor
merged_pvga$Open.Access <- factor(merged_pvga$Open.Access, ordered = TRUE, levels = c("Pure Gold", "Hybrid", "Green, Published", "Green, Accepted", "Bronze", "Green, Submitted", "Closed"))
merged_pvga$Open.Access_ukri <- factor(merged_pvga$Open.Access_ukri, ordered = TRUE, levels = c("Pure Gold", "Hybrid gold", "Green", "Closed"))

  # Recode g_embargo_zero as a factor
merged_pvga$g_embargo <- factor(merged_pvga$g_embargo, ordered = TRUE, levels = c(0,3,6,12,18,24,36,48,NA))
merged_pvga$g_embargo_grouped <- factor(merged_pvga$g_embargo_grouped, ordered = TRUE, levels = c(0,6,12,13))
merged_pvga$g_embargo_zero <- factor(merged_pvga$g_embargo_zero, ordered = TRUE, levels = c("zero embargo", "has embargo"))

  # Recode g_compliant_repository as a factor
merged_pvga$g_compliant_repository <- factor(merged_pvga$g_compliant_repository, ordered = TRUE, levels = c(TRUE, FALSE))

  # Turn license vars into factor to order by permissiveness (just to make outputs clearer)
merged_pvga$g_license_single <- factor(merged_pvga$g_license_single, ordered = TRUE, c("cc_by", "cc_by_sa", "cc_by_nd", "cc_by_nc", "cc_by_nc_sa", "cc_by_nc_nd", "bespoke_license", "all_rights_reserved", "no license requirement", NA))
merged_pvga$g_license_compliant <- factor(merged_pvga$g_license_compliant, ordered = TRUE, c("cc_by", "cc_by_nd", "cc_by_nc", "no compliant license", NA))

  # Recode compliance vars as factors
merged_pvga$compliance_current <- factor(merged_pvga$compliance_current, ordered = TRUE, levels = c("c: pure gold", "c: hybrid gold", "c: confirmed green oa", "not compliant"))

merged_pvga$compliance_new_hybrid <- factor(merged_pvga$compliance_new_hybrid, ordered = TRUE, levels = c("c: pure gold", "c: hybrid gold", "c: confirmed green oa", "not compliant"))

merged_pvga$compliance_new <- factor(merged_pvga$compliance_new, ordered = TRUE, levels = c("c: pure gold", "c: hybrid gold with a TA", "c: confirmed green oa", "not compliant"))

merged_pvga$compliance_new_pure <- factor(merged_pvga$compliance_new_pure, ordered = TRUE, levels = c("c: pure gold", "c: confirmed green oa", "not compliant"))

merged_pvga$compliance_new_target_TAs <- factor(merged_pvga$compliance_new_target_TAs, ordered = TRUE, levels = c("c: pure gold", "c: hybrid gold with a TA", "c: confirmed green oa", "No supported route"))

  # Recode journal_type as factor
merged_pvga$journal_type <- factor(merged_pvga$journal_type, ordered = TRUE, levels = c("Pure Gold", "Hybrid", "Closed or insufficient information"))


# Code which will allow for reproducible random reordering of rows----
  # this is needed because of code in the analysis script which removes duplicates on journal_title to select only one row for each journal, and thus approximate results at a journal level. The randomisation removes risk of systematic bias in which article is selected for each journal (though the measure is still a proxy at best)
set.seed(42)
rows <- sample(nrow(merged_pvga))
merged_pvga <- merged_pvga[rows, ]


#XXXXXXXXXX
# 7. SELECTING ONLY VARIABLES USED IN THE ANALYSIS----
# this only includes variables which are used in the analysis
merged_pvga <- merged_pvga %>%
  select(-c(Units.of.Assessment, open_access_prohibited, rank_green, rank_fee, num_current_green, compliance_current2, compliance_new_hybrid2,
            fee_article_version, listed_in_doaj, best_oa_status_upw, journal_is_oa, journal_is_in_doaj))


# 8. WRITE MERGED_PVGA TO EXCEL AND RDA ----

openxlsx::write.xlsx(as.data.frame(merged_pvga), 'Data/merged_pvga.xlsx')
write_tsv(merged_pvga, file = "Data/merged_pvga.tsv")
save(merged_pvga, file = "Data/merged_pvga.Rda")

# Merged PVGA: We have a Sherpa link for 97.5% of the publications in the Dimensions sample. We have green (no fee) routes for almost all of these and paid (fee) routes for about three-fifths.
