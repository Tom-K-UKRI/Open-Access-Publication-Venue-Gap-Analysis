# SHERPA ROMEO CODE FOR VENUE GAP ANALYSIS

# Author: Tom Kenny
# Created: August 2020
# Last Updated: March 2021
# Last ran: 29.03.2021

# Purpose of code: to download information on journals' open access policies from the SHERPA API

#XXXXXXXXXXXXXXXXX
# Clear work space
rm(list=ls())

#XXXXXXXXXXXXXXXX
#1. DOWNLOAD FROM API----

# Load packages
library(httr)
library(jsonlite)
library(tidyverse)
library(readr)

# Set number of times to query api here (limit = 100 per page) - the reasons it is not literally the highest id number/ 100 is because there are quite a lot of id numbers with no journal, where presumably the journal has been deleted). as of 29.03.21 there are 317 pages of results available

max_api_calls <- 400 # if Sherpa add more records this may need to be increased. You can always set it too high and them remove pages later on.

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

# Remove additional pages not filled
#pages <- pages[1:317] # if updating replace 317 with final page which has records

# Combining queries into a single JSON file and saving out as .Rda
message("Combining queries")
publications <- rbind_pages(pages)
save(publications, file = "Data/Raw data/sherpa_publications_json.Rda") # TK: to preserve this since download takes so long

#XXXXXXXXXXXXXXXXXXX

# 2. FILTERING JSON TO RETURN RELEVANT VARIABLES IN ACCESSIBLE FORM----
  #the original json data is very nested so needs work before it is useful - the goal here is to return one observation per journal policy
  # NB - some journals are skipped because their data is formatted in a way which does not allow the loop to run (see code with next for issn and publisher policy). However, this is a relative small number of cases.

records <- NULL

for(i in (31147:nrow(publications))){ # The number of records may need to change if this is run again, however exclude was not working with an nrow command
  
  page <- publications[i,]
  
  title <- unnest(page, 'title') %>% select(title) %>% head(1)
  
  issn <- as.data.frame(page$issns)
  if(is.null(issn$type)) {next} # this is needed because a number of rows return null values and break the loop

  issn <- as.data.frame(page$issns) %>% mutate(row = row_number()) 

  if(!"issn" %in% colnames(issn)) {next} # this is needed as one of the records didn't have an issn column
  
  issn <- issn %>% select(type, issn, row) %>% pivot_wider(names_prefix='issn_', names_from = type, values_from = issn) %>% slice(1:2) %>% select(-row) %>% summarise_all(funs(first(na.omit(.)))) # this is needed because a few iterations of issn have duplicates
  
  
  doaj <- select(page, 'listed_in_doaj')
  
  sherpa_id <- select(page, 'id')
  
  sherpa_web <- select(page, 'system_metadata.uri')
  
  oa_prohibited <- as.data.frame(page$publisher_policy)
  if(is.null(oa_prohibited$open_access_prohibited)) {next} # this is needed because a number of rows return null values and break the loop
  oa_prohibited <- as.data.frame(page$publisher_policy) %>% select(open_access_prohibited) %>% mutate(pubpol_id = seq_len(n()), title=title$title)
  
  if(page$publishers == "NULL") {next} # this is needed because one row returns a null value and breaks the loop
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

# Editing license variable to change from nested list to string
for (i in 1:nrow(sherpa)) {
  license <- as.data.frame(sherpa[i,'license'])
  if(is.null(license[i,'license'])) {sherpa[i, 'license'] = ""}
  sherpa[i,'license'] = paste(license$license, collapse = ", ")
  if(i %% 250 == 0) {message(i)}
  next
}

sherpa$license <- as.character(sherpa$license)

# Editing article version variable to change from list to string
for (i in 1:nrow(sherpa)) {
  if(sherpa[i,'article_version'] == "NULL") {sherpa[i, 'article_version'] = ""}
  version <- as.data.frame(sherpa[i,'article_version'])
  sherpa[i,'article_version'] = paste(version[,1], collapse = ", ")
  if(i %% 250 == 0) {message(i)}
  next
}

sherpa$article_version <- as.character(sherpa$article_version)

# Editing conditions variable to change from list to string
for (i in 1:nrow(sherpa)) {
  if(sherpa[i,'conditions'] == "NULL") {sherpa[i,'conditions'] = ""}
  condition <- as.data.frame(sherpa[i,'conditions'])
  sherpa[i,'conditions'] = paste(condition[,1], collapse = ", ")
  if(i %% 250 == 0) {message(i)}
  next
}

sherpa$conditions <- as.character(sherpa$conditions)


# Editing location.location variable to change from list to string
for (i in 1:nrow(sherpa)) {
  if(sherpa[i,'location.location'] == "NULL") {sherpa[i, 'location.location'] = ""}
  location <- as.data.frame(sherpa[i,'location.location'])
  sherpa[i,'location.location'] = paste(location[,1], collapse = ", ")
  if(i %% 250 == 0) {message(i)}
  next
}

sherpa$location.location <- as.character(sherpa$location.location)

#XXXXXXXXXXXXX
# WRITE TO TSV (can't be CSV as commas in values)----
write_tsv(sherpa, file = 'Data/Raw Data/sherpa_all_policies.tsv')

openxlsx::write.xlsx(as.data.frame(sherpa), 'Data/Raw Data/sherpa_all_policies.xlsx')

# SAVE AS .Rda
save(sherpa, file = "Data/Raw Data/sherpa.Rda")


