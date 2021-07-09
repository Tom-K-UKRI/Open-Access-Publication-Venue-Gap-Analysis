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
                   value = merged_pvga,
                   table_id)
