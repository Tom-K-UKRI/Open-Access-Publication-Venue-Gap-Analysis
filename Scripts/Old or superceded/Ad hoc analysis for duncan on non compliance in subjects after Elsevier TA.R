# Ad hoc analysis for duncan on non compliance in subjects after Elsevier TA

# Clear work space
rm(list=ls())

#library(readr)
library(tidyverse)
library(openxlsx)
library(janitor)
library(knitr)
library(ggplot2)
library(psych)

#XXXXXXXXX
# Import data
load("Data/merged_pvga.Rda")


# Deriving new versions of merged_pvga----
  
  # one for if Elsevier had a TA and one for all 2021 target TAs - these need to be run seperately depending on which analysis is being done

merged_pvga_elsevierta <- merged_pvga
merged_pvga_elsevierta$has_ta[merged_pvga_elsevierta$Publisher %in% c("Elsevier", "Nature", "Wolters Kluwer", "American Chemical Society (ACS)")] <- "yes"
# merged_pvga_elsevierta$has_ta[merged_pvga_elsevierta$Publisher  == "Elsevier"] <- "yes"

# Re-deriving compliance variables for the new version of merged_pvga
merged_pvga_elsevierta$compliance_new <- NA
merged_pvga_elsevierta$compliance_new[merged_pvga_elsevierta$num_fee %in% c(1,4)] <- "c: pure gold"
merged_pvga_elsevierta$compliance_new[merged_pvga_elsevierta$has_ta == "yes" & merged_pvga_elsevierta$num_fee %in% c(2,5)] <- "c: hybrid gold with a TA"
merged_pvga_elsevierta$compliance_new[!merged_pvga_elsevierta$num_fee %in% c(1,4) & !(merged_pvga_elsevierta$has_ta == "yes" & merged_pvga_elsevierta$num_fee %in% c(2,5)) & merged_pvga_elsevierta$num_new_green %in% c(1,3)] <- "c: confirmed green oa"
merged_pvga_elsevierta$compliance_new[!merged_pvga_elsevierta$num_fee %in% c(1,4) & !(merged_pvga_elsevierta$has_ta == "yes" & merged_pvga_elsevierta$num_fee %in% c(2,5)) & !merged_pvga_elsevierta$num_new_green %in% c(1,3) & (merged_pvga_elsevierta$g_embargo == 0 & merged_pvga_elsevierta$g_license1 == "no license requirement") & merged_pvga_elsevierta$g_compliant_repository == "TRUE"] <- "nc: unconfirmed green oa"
merged_pvga_elsevierta$compliance_new[is.na(merged_pvga_elsevierta$compliance_new)] <- "not compliant"

# simplified binary version of compliance_new

merged_pvga_elsevierta$compliance_new2 <- merged_pvga_elsevierta$compliance_new
merged_pvga_elsevierta$compliance_new2[merged_pvga_elsevierta$compliance_new2 != "not compliant" & merged_pvga_elsevierta$compliance_new2 != "nc: unconfirmed green oa"] <- "compliant"
merged_pvga_elsevierta$compliance_new2[merged_pvga_elsevierta$compliance_new2 == "not compliant" | merged_pvga_elsevierta$compliance_new2 == "nc: unconfirmed green oa"] <- "not compliant"


#  Top Publishers and oac----
top_publishers_oac <- merged_pvga %>%
  group_by(Publisher) %>%
  count(Open.Access2) %>%
  pivot_wider(names_from = Open.Access2, values_from = n) %>%
  adorn_totals("col") %>%
  rename(pure_gold = "Pure Gold", hybrid_gold = "Hybrid gold") %>%
  mutate(pure_percent = round(pure_gold/Total*100,1), hybrid_gold_percent = round(hybrid_gold/Total*100,1), green_percent = round(Green/Total*100,1), closed_percent = round(Closed/Total*100,1)) %>%
  select(-c(2,3,4,5)) %>%
  rename(Total_n_articles = Total) %>%
  mutate(percent_of_total_articles = round(Total_n_articles/sum(Total_n_articles)*100,1)) %>%
  arrange(desc(Total_n_articles)) %>%
  mutate(cml = round(cumsum(percent_of_total_articles),0)) %>%
  relocate(1,2,7,8,3,4,5,6) %>%
  adorn_totals("row")

openxlsx::write.xlsx(as.data.frame(top_publishers_oac), 'Output/Tables/Duncan/top_publishers_oac.xlsx')


#Analysis of non-supported articles in subjects----

# Create weight for articles with multiple subjects
merged_pvga_elsevierta$subject_weight <- ifelse(grepl(";", merged_pvga_elsevierta$for_division), 1/(str_count(merged_pvga_elsevierta$for_division, pattern = ";")+1), 1)

divisions <- merged_pvga_elsevierta %>%
  separate(for_division, into = c("division1", "division2", "division3", "division4"), sep = "; ") %>%
  pivot_longer(c(division1, division2, division3, division4), names_to = "division_all", values_to ="division") %>%   filter(!is.na(division))

not_compliance_new_subject <- divisions %>%
  filter(compliance_new2 == "not compliant") %>%
  group_by(division) %>%
  summarise(n = n(), weight = sum(subject_weight)/n, weighted_n = n*weight) %>% 
  mutate(total = sum(weighted_n)) %>%
  ungroup() %>%
  # mutate(n = n/  (sum(n)/ nrow(merged_pvga_elsevierta %>% filter(compliance_new2 == "not compliant")))) %>% this is a simple weight based on reducing everything by 17%
  arrange(desc(weighted_n)) %>%
  mutate(percent_all_articles = weighted_n/nrow(merged_pvga_elsevierta)*100) %>% # percent of all articles
  mutate(percent_not_supported = weighted_n/total*100) %>%
  adorn_totals("row")
# add in total articles per subject
articles_per_subject <- divisions %>%
  count(division, sort = TRUE)

not_compliance_new_subject <- left_join(not_compliance_new_subject, articles_per_subject, by = "division") %>%
  mutate(percent_of_subject_total = round(n.x / n.y * 100,1)) %>%
  select(-c('total', 'n.y'))

openxlsx::write.xlsx(as.data.frame(not_compliance_new_subject), 'Output/Tables/Duncan/With all target TAs - not_compliant_new_subject.xlsx')
# openxlsx::write.xlsx(as.data.frame(not_compliance_new_subject), 'Output/Tables/Duncan/With Elsevier TA - not_compliant_new_subject.xlsx')




# Top 10 publishers by number of UKRI-funded articles (S2)
not_compliance_new_publisher_s2 <- merged_pvga %>%
  filter(compliance_new2 == "not compliant") %>%
  count(Publisher, sort = TRUE) %>%
  mutate(percent = (n / nrow(merged_pvga)) * 100) %>%
  mutate(percent_unsupported = n/sum(n)*100) %>%
  mutate(cml = round(cumsum(percent_unsupported),0)) %>%
  adorn_totals("row")
# add in total articles per publisher
articles_per_publisher <- merged_pvga %>%
  count(Publisher, sort = TRUE)

not_compliance_new_publisher_s2 <- left_join(not_compliance_new_publisher_s2, articles_per_publisher, by = "Publisher") %>%
  mutate(percent_of_publisher_total = round(n.x / n.y * 100,1))
openxlsx::write.xlsx(as.data.frame(not_compliance_new_publisher_s2), 'Output/Tables/Duncan/not_compliant_new_publisher_s2.xlsx')

# Top 10 publishers by number of UKRI-funded articles (no TAs)
not_compliance_new_publisher_pure <- merged_pvga %>%
  filter(compliance_new_pure %in% c("not compliant", "nc: unconfirmed green oa")) %>%
  count(Publisher, sort = TRUE) %>%
  mutate(percent = (n / nrow(merged_pvga)) * 100) %>%
  mutate(percent_unsupported = n/sum(n)*100) %>%
  mutate(cml = round(cumsum(percent_unsupported),0)) %>%
  adorn_totals("row")
# add in total articles per publisher
articles_per_publisher <- merged_pvga %>%
  count(Publisher, sort = TRUE)

not_compliance_new_publisher_pure <- left_join(not_compliance_new_publisher_pure, articles_per_publisher, by = "Publisher") %>%
  mutate(percent_of_publisher_total = round(n.x / n.y * 100,1))
openxlsx::write.xlsx(as.data.frame(not_compliance_new_publisher_pure), 'Output/Tables/Duncan/not_compliant_new_publisher_noTAs.xlsx')

# Top 10 publishers by number of UKRI-funded articles (target TAs)
not_compliance_new_publisher_target <- merged_pvga_elsevierta %>%
  filter(compliance_new2 == "not compliant") %>%
  count(Publisher, sort = TRUE) %>%
  mutate(percent = (n / nrow(merged_pvga)) * 100) %>%
  mutate(percent_unsupported = n/sum(n)*100) %>%
  mutate(cml = round(cumsum(percent_unsupported),0)) %>%
  adorn_totals("row")
# add in total articles per publisher
articles_per_publisher <- merged_pvga %>%
  count(Publisher, sort = TRUE)

not_compliance_new_publisher_target <- left_join(not_compliance_new_publisher_target, articles_per_publisher, by = "Publisher") %>%
  mutate(percent_of_publisher_total = round(n.x / n.y * 100,1))
openxlsx::write.xlsx(as.data.frame(not_compliance_new_publisher_target), 'Output/Tables/Duncan/not_compliant_new_publisher_targetTAs.xlsx')
