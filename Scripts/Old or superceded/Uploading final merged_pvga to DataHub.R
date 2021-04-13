# Uploading final merged_pvga to DataHub

# Load packages
library(odbc)
library(readr)

# Load final merged_pvga data (all updated w/c 29th March 2021)

# Open an ODBC connection to the Data Hub
con <- DBI::dbConnect(odbc::odbc(), "Azure_UKRI_DataHub") # “Azure_UKRI_DataHub” is what I called my ODBC connection

# define the table to which you want to save the data
table_id <- DBI::Id(catalog = "SandBoxEndUsers", schema = "dbo", table = "OA_Publication_Venue_Gap_Analysis")

# write the table to the Data Hub Sandbox
odbc::dbWriteTable(conn = con,
                   value = merged_pvga,
                   table_id)
