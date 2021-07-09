# Uploading final merged_pvga to DataHub

# Data last uploaded 7th July 2021

# Load packages
library(odbc)
library(readr)

# load merged_pvga
load("Data/merged_pvga.Rda")

# Load final merged_pvga data (all updated w/c 29th March 2021)

# Open an ODBC connection to the Data Hub
con <- DBI::dbConnect(odbc::odbc(),
                      Driver = "ODBC Driver 17 for SQL Server",
                      Server = "ausorizmidb01.fbf0f5dae340.database.windows.net",
                      Authentication = "ActiveDirectoryIntegrated")

# define the table to which you want to save the data
table_id <- DBI::Id(catalog = "SandBoxEndUsers", schema = "dbo", table = "OA_Publication_Venue_Gap_Analysis")

# write the table to the Data Hub Sandbox
odbc::dbWriteTable(conn = con,
                   name = table_id, 
                   value = merged_pvga,
                   overwrite = TRUE,
                   field.types=c(
                     Title = "varchar",
                     DOI = "varchar",
                     Publication.Date = "varchar",
                     PubYear = "int",
                     Source.title = "varchar",
                     Publisher = "varchar",
                     ISSN = "varchar",
                     e.ISSN = "varchar",
                     Publication.Type = "varchar",
                     Country.of.Research.organization = "varchar",
                     Research.Organizations...standardized = "varchar",
                     FOR..ANZSRC..Categories = "varchar",
                     for_division = "varchar",
                     for_group = "varchar",
                     discipline = "varchar",
                     Open.Access = "varchar",
                     Funder = "varchar",
                     ukri_funders = "varchar",
                     has_ta = "varchar",
                     sherpa_id = "int",
                     g_article_version = "varchar",
                     g_license = "varchar",
                     g_license_single = "varchar",
                     g_license_compliant = "varchar",
                     g_license_cc_by = "varchar",
                     g_embargo.units = "varchar",
                     g_embargo = "varchar",
                     g_embargo_grouped = "varchar",
                     g_embargo_zero = "varchar",
                     g_compliant_repository = "varchar",
                     g_copyright = "varchar",
                     fee_license_single = "varchar",
                     fee_license_compliant = "varchar",
                     fee_license_cc_by = "varchar",
                     fee_copyright = "varchar",
                     upw_green_licence = "varchar",
                     upw_green_version = "varchar",
                     upw_gold_licence = "varchar",
                     Open.Access_ukri = "varchar",
                     journal_type = "varchar",
                     articles_in_journal = "int",
                     ta_split = "varchar",
                     num_fee = "int",
                     num_new_green = "int",
                     compliance_current = "varchar",
                     compliance_new_hybrid = "varchar",
                     compliance_new = "varchar",
                     compliance_new2 = "varchar",
                     compliance_new_pure = "varchar",
                     compliance_new_target_TAs = "varchar"))
