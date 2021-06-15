# Working out licenses for all UKRI-funded articles

# Clear work space
rm(list=ls())

# Load packages
library('stringdist')
library('callr')
library(dplyr)
library(readr)
library(tidyverse)
library(RODBC)
library(odbc)

# Load Dimensions UKRI-funded articles data----
dimensions_ukri <- read_tsv(file = "Data/Raw data/20210401 Dimensions export pubs UKRI funded 2014-2020.tsv") # this is all articles associated with UKRI funding 2014-2020

    # Make column names tidy - this is needed because the version sent by Dimensions had spaces in column titles
names(dimensions_ukri) <- make.names(names(dimensions_ukri), unique = TRUE)

    # Filter for years we're interested in
dimensions_ukri <- dimensions_ukri %>%
  filter(PubYear %in% c(2017:2020))

save(dimensions_ukri,     file = "Data/Unpaywall licenses/dimensions_ukri.RData")

# CLEANING DIMENSIONS DATA AND DERIVING NEW VARIABLES ----

# Remove duplicate rows---- 

# Download unpaywall data for those DOIs

  # Get the references of interest ----
references_list <- dimensions_ukri %>%
  filter(!duplicated(DOI), !is.na(DOI), DOI != "")
  # Connect to Azure Data Hub

# Open an ODBC connection to the Data Hub ----
con <- DBI::dbConnect(odbc::odbc(),
                      Driver = "ODBC Driver 17 for SQL Server",
                      Server = "ausorizmidb01.fbf0f5dae340.database.windows.net",
                      Authentication = "ActiveDirectoryIntegrated")

# con <- DBI::dbConnect(odbc::odbc(), "Azure_UKRI_DataHub", UID = "tom.kenny@ukri.org") # this needs to be updated depending on the user

# Save the references to the database UKRIGrants_Reporting ----

# define the table to which you want to save the data
table_id <- DBI::Id(catalog = "SandBoxEndUsers", schema = "dbo", table = "unpaywall_refs_all_ukri_from_dimensions")

# write the table to the Data Hub Sandbox
odbc::dbWriteTable(conn = con,
                   value = references_list,
                   table_id,
                   overwrite = TRUE)


# Get the data ----

# NB. This can take a while to run

# First for doi
dois_w_info_doi <- odbc::dbGetQuery(conn = con,
                                "SELECT
                           refs.*,
                           other.*
                         FROM [SandboxEndUsers].dbo.[unpaywall_refs_all_ukri_from_dimensions] AS refs
                          LEFT JOIN [UnPaywall_DOI].[dbo].[doi] AS other
                            ON refs.doi = other.doi")

# Then for location
dois_w_location <- odbc::dbGetQuery(conn = con,
                                    "SELECT
                           refs.*,
                           other.*
                         FROM [SandboxEndUsers].dbo.[unpaywall_refs_all_ukri_from_dimensions] AS refs
                          LEFT JOIN [UnPaywall_DOI].[dbo].[oa_locations] AS other
                            ON refs.doi = other.doi
                        WHERE
                        other.is_best = 1")


# Write it on a csv file
write_csv(dois_w_info, "Data/Unpaywall licenses/dois_w_info.csv")
write_csv(dois_w_location, "Data/Unpaywall licenses/dois_w_location.csv")

# Save out as .Rdata

upw_location <- dois_w_location %>% select(-1) # this is to remove duplicate doi column
upw_data  <- dois_w_info %>% select(-1)

# save output
save(upw_data,     file = "Data/Unpaywall licenses/upw_data.RData")
save(upw_location, file = "Data/Unpaywall licenses/upw_location.RData")


Merging----
# If starting from here
#load("Data/Unpaywall licenses/upw_data.RData")
#load("Data/Unpaywall licenses/upw_location.RData")
#load("Data/Unpaywall licenses/dimensions_ukri.RData")

# Join Dimensions and Unpaywall----

dimensions_upw <- dimensions_ukri %>%
  select(DOI, Title, PubYear, Open.Access) %>%
  filter(PubYear %in% c(2017:2020)) %>%
  left_join(upw_data %>% filter(!is.na(doi)) %>% select(doi, is_oa, journal_is_oa, journal_is_in_doaj), by = c("DOI" = "doi")) %>%
  left_join(upw_location %>% filter(!is.na(doi)) %>% select(doi, version, host_type, license, evidence), by = c("DOI" = "doi"))

# Tidy up upw license variable
dimensions_upw$license[grepl("specific", dimensions_upw$license)] <- "Publisher-specific license"
dimensions_upw$license[dimensions_upw$license %in% c("pd", "cc0")] <- "Publisher-specific license"

save(dimensions_upw, file = "Data/Unpaywall licenses/dimensions_upw.RData")


# Analysis----

# If starting from here
load("Data/Unpaywall licenses/dimensions_upw.RData")

# General count licenses
dimensions_upw_licenses <- dimensions_upw %>% 
  count(license) %>%
  mutate(percent = round(n/sum(n)*100,1)) %>%
  mutate(percent_w_license_info = round(n/sum(n[!is.na(license)])*100,1))


# Count licenses by OA status

    # Recode OAC to match our categories
dimensions_upw <- dimensions_upw %>%
  mutate(Open.Access = gsub("All OA; ", "", Open.Access))
dimensions_upw$Open.Access2 <- dimensions_upw$Open.Access
dimensions_upw$Open.Access2[dimensions_upw$Open.Access2 == "Hybrid"] <- "Hybrid gold" # to avoid confusion with journal type
dimensions_upw$Open.Access2[dimensions_upw$Open.Access2 == "Green, Published" | dimensions_upw$Open.Access2 == "Green, Accepted"] <- "Green"
dimensions_upw$Open.Access2[dimensions_upw$Open.Access2 == "Bronze" | dimensions_upw$Open.Access2 == "Green, Submitted" | dimensions_upw$Open.Access2 == "Closed"] <- "Closed" #since these are all closed from the perspective of UKRI policy

dimensions_upw_oac_licenses <- dimensions_upw %>%
  group_by(Open.Access2) %>%
  count(license) %>%
  mutate(percent = round(n/sum(n)*100,1)) %>%
  mutate(percent_w_license_info = round(n/sum(n[!is.na(license)])*100,1))

# Add totals by merging tables
dimensions_upw_oac_licenses2 <- dimensions_upw_oac_licenses %>%
  bind_rows(dimensions_upw_licenses %>% mutate(Open.Access2 = " All articles") %>% relocate(Open.Access2))

# Save out
write.xlsx(dimensions_upw_oac_licenses2, file = "Data/Unpaywall licenses/dimensions_upw_oac_licenses.xlsx")

