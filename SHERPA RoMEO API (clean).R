# SHERPA ROMEO CODE FOR VENUE GAP ANALYSIS

# Author: Tom Kenny
# Created: August 2020
# Last Updated: November 2020
# Last ran: 24.11.202

# Purpose of code: to download information on journals' open access policies from the SHERPA API

#XXXXXXXXXXXXXXXXX
# Clear work space
rm(list=ls())

# Set working directory
mainDir <- "C:/Users/TKen02/UKRI/Policy Analysis - Documents/Open Access/Projects/Publication Venue Gap Analysis/Data/Original data"
setwd(mainDir)

#XXXXXXXXXXXXXXXX
#1. DOWNLOAD FROM API----

# Load packages
library(httr)
library(jsonlite)
library(tidyverse)

# Set number of times to query api here (limit = 100, total of 30,969 records as of 25/09/20 so need 310 API calls to download all data - the reasons it is not literally the highest id number/ 100 is because there are quite a lot of id numbers with no journal, where presumably the journal has been deleted)
max_api_calls <- 310 # if Sherpa add more records this may need to be increased (this gets you up to ID = 37990)

# Add API key/ access token - this is Tom Kenny's token but easy to generate your own
api_key <- "E46056F4-F72C-11EA-B80C-3822122D6054"

# Creating the API url - the URL needs to be paged using limit (number per call) and offset (number to start call from). So the first call should be limit = 100, offset = 0 and the second should be limit = 100, offset = 100. You can also use filters though none are included here. Information is here: https://v2.sherpa.ac.uk/api/object-retrieval.html
base_api_url <- "https://v2.sherpa.ac.uk/cgi/retrieve?item-type=publication&format=Json"
limit <- 100
api_url <- paste0(base_api_url, "&api-key=", api_key, "&limit=", limit, "&offset=")

# Creating a loop to page requests, and then flattening each page of the JSON file which is returned
pages <- list()
for(i in 1:max_api_calls){
  offset <- limit*(i-1) # i.e. for the first page the offset will be (1-1)*100 = 0 and for the second it will be (2-1)*100 = 100
  page <- fromJSON(paste0(api_url, offset), flatten = TRUE)
  page <- page$items # this essentially jumps us to the first level of the JSON heirarchy we are interested in
  message("Querying api for ", limit, " publications at offset ", offset)
  pages[[i]] <- page
}

# Combining queries into a single JSON file
message("Combining queries")
publications <- rbind_pages(pages)
save(publications, file = "sherpa_publications")

#XXXXXXXXXXXXXXXXXXX

# 2. FILTERING JSON TO RETURN RELEVANT VARIABLES IN ACCESSIBLE FORM----
  #the original json data is very nested so needs work before it is useful - the goal here is to return one observation per journal policy

# we need to run through journals one at a time so first set up page as one record
page <- publications[1,] # i.e. page becomes the first journal rather than the first 100

#a. initialise first record, selecting all relevant variables----
title <- unnest(page, 'title') %>% select(title) %>% head(1)
issn <- as.data.frame(page$issns) %>% mutate(row = row_number()) %>% select(type, issn, row) %>% pivot_wider(names_prefix='issn_', names_from = type, values_from = issn) %>% slice(1:2) %>% select(-row) %>% summarise_all(funs(first(na.omit(.)))) # this is needed because a few iterations of issn have duplicates
doaj <- select(page, 'listed_in_doaj')
sherpa_id <- select(page, 'id')
sherpa_web <- select(page, 'system_metadata.uri')
oa_prohibited <- as.data.frame(page$publisher_policy) %>% select(open_access_prohibited) %>% mutate(pubpol_id = seq_len(n()), title=title$title)
publisher <- unnest(as.data.frame(page$publishers), 'publisher.name') %>% select(name)
    
    # this loop goes into the list of policies under permitted oa. For each one it returns all the elements of the policy we might be interested in
if ('permitted_oa' %in% names(as.data.frame(page$publisher_policy))){
  pubpol_list <- list()
  for (k in 1:nrow(oa_prohibited)){
    
    pubpol_temp <- unnest(as.data.frame(page$publisher_policy)[k, ], 'permitted_oa') %>%
      select(any_of(c('rowname', 'additional_oa_fee', 'article_version', 'additional_oa_fee', 'embargo.amount', 'embargo.units', 'license', 'copyright_owner', 'conditions', 'location.location'))) %>%
      mutate(pubpol_id = k)
    
    pubpol_list[[k]] <- pubpol_temp
    
  }
  
  pubpol <- do.call(bind_rows, pubpol_list)
  
  pubpol_oa <- merge(oa_prohibited, pubpol, by='pubpol_id')
  
} else {
    pubpol_oa <- oa_prohibited 
  
}
    # we then merge the publisher policy information with the article metadata so that each policy has one row
record <- merge(cbind(sherpa_id, title, issn, doaj, publisher, sherpa_web), pubpol_oa, by='title')
records <- record

#b. Run loop to repeat for all policies-----------------------------------
  # NB - some journals are skipped because their data is formatted in a way which does not allow the loop to run (see code with next for issn and publisher policy). However, this is a relative small number of cases.

for(i in (1:30100)){ # The number of records may need to change if this is run again, however exclude was not working with an nrow command
  
  page <- publications[i,]
  
  title <- unnest(page, 'title') %>% select(title) %>% head(1)
  
  issn <- as.data.frame(page$issns)
  if(is.null(issn$type)) {next} # this is needed because a number of rows return null values and break the loop
  issn <- as.data.frame(page$issns) %>% mutate(row = row_number()) %>% select(type, issn, row) %>% pivot_wider(names_prefix='issn_', names_from = type, values_from = issn) %>% slice(1:2) %>% select(-row) %>% summarise_all(funs(first(na.omit(.)))) # this is needed because a few iterations of issn have duplicates
  
  
  doaj <- select(page, 'listed_in_doaj')
  
  sherpa_id <- select(page, 'id')
  
  sherpa_web <- select(page, 'system_metadata.uri')
  
  oa_prohibited <- as.data.frame(page$publisher_policy)
  if(is.null(oa_prohibited$open_access_prohibited)) {next} # this is needed because a number of rows return null values and break the loop
  oa_prohibited <- as.data.frame(page$publisher_policy) %>% select(open_access_prohibited) %>% mutate(pubpol_id = seq_len(n()), title=title$title)
  
  publisher <- unnest(as.data.frame(page$publishers), 'publisher.name') %>% select(name)
  
  if ('permitted_oa' %in% names(as.data.frame(page$publisher_policy))){
    pubpol_list <- list()
    for (k in 1:nrow(oa_prohibited)){
      
      pubpol_temp <- unnest(as.data.frame(page$publisher_policy)[k, ], 'permitted_oa') %>%
        select(any_of(c('rowname', 'additional_oa_fee', 'article_version', 'additional_oa_fee', 'embargo.amount', 'embargo.units', 'license', 'copyright_owner', 'conditions', 'location.location'))) %>%
        mutate(pubpol_id = k)
      
      pubpol_list[[k]] <- pubpol_temp
      
    }
    
    pubpol <- do.call(bind_rows, pubpol_list)
    
    pubpol_oa <- merge(oa_prohibited, pubpol, by='pubpol_id')
    
  } else {
    pubpol_oa <- oa_prohibited 
    
  }
  
  record <- merge(cbind(sherpa_id, title, issn, doaj, publisher, sherpa_web), pubpol_oa, by='title')
  records <- bind_rows(records, record)
  
  message(i)
  
}

# EDITING EMBARGO VARIABLE----
  # This code begins cleaning the sherpa data - it is only in this script because it takes a while to run so it slows down the other script


# create new column sherpa$embargo which recodes embargo.amount to months where embargo.unit != 'months'
# NB JISC told us that no data for embargo means no embargo
# Alternative would be to create a new column using mutate, case_when (this is to do it all in one go and avoid for loop). E.g. #sherpa %>% mutate(embargo = ifelse(embargo.units = "month", TRUE = , FALSE = ,))

sherpa <- records

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

#XXXXXXXXXXXXX
# WRITE TO EXCEL----
openxlsx::write.xlsx(as.data.frame(sherpa), 'sherpa_all_policies.xlsx')
