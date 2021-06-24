# Testing Bronze OA in Dimensions

# Clear work space
rm(list=ls())

#library(readr)
library(tidyverse)
library(openxlsx)

#XXXXXXXXX
# Import Dimensions/ merged data
load("Data/merged_pvga.Rda")

# Open an ODBC connection to the Data Hub ----
con <- DBI::dbConnect(odbc::odbc(),
                      Driver = "ODBC Driver 17 for SQL Server",
                      Server = "ausorizmidb01.fbf0f5dae340.database.windows.net",
                      Authentication = "ActiveDirectoryIntegrated")

# con <- DBI::dbConnect(odbc::odbc(), "Azure_UKRI_DataHub", UID = "tom.kenny@ukri.org") # this needs to be updated depending on the user

# Get the data ----

upw_2018 <- odbc::dbGetQuery(conn = con,
                                  "SELECT doi, year, title, oa_locations, best_oa_location
                         FROM [UnPaywall_DOI].dbo.[doi]
                            WHERE year IN ('2018')")

save(upw_2018, file = "Testing/upw_2018.Rda")

# Load Dimensions data


bronze_oa <- merged_pvga %>%
  filter(Open.Access == "Bronze") %>%
  select(Title, DOI, PubYear, Source.title, Research.Organizations...standardized, for_division, Open.Access, g_article_version, g_license)

bronze_oa_upw <- bronze_oa %>%
  left_join(upw_2018 %>% select(doi, best_oa_location, oa_locations), by = c("DOI" = "doi")) %>%
  separate(oa_locations, into = c("loc1", "loc2", "loc3", "loc4", "loc5", "loc6", "loc7", "loc8", "loc9", "loc10", "loc11", "loc12", "loc13", "loc14", "loc15",
                                  "loc16", "loc17", "loc18", "loc19", "loc20", "loc21", "loc22"), sep = "}, {") %>%
  pivot_longer(c(loc1, loc2, loc3, loc4, loc5, loc6, loc7, loc8, loc9, loc10, loc11, loc12, loc13, loc14, loc15, loc16, loc17,
                 loc18, loc19, loc20, loc21, loc22), names_to = "loc_all", values_to ="locations") %>%
  filter(grepl("\"host_type\": \"repository\"", locations) & grepl("publishedVersion|acceptedVersion", locations)) %>%
  mutate(published = grepl("publishedVersion", locations), accepted = grepl("acceptedVersion", locations)) %>%
  arrange(desc(published)) %>%
  distinct(DOI, .keep_all = TRUE)

# I.e. 2493/ 4115 (61%) of  bronze OA articles have a green version of VoR or AAM

