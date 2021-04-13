# PUBLICATION VENUE GAP ANALYSIS IN REPORT ORDER

# Author: Tom Kenny
# Created: August 2020
# Last Updated: November 2020

# Purpose of code: This code produces tables and charts for the publication venue gap analysis - it forms the basis of an early findings presentation.

# Prerequisite: this relies on the dataset merged_pvga produced with Tom Kenny's code 'Preparing and Merging PVGA data' which in turn is dependent on data from Dimensions downloaded using google sheets API and from Sherpa (downloaded using code SHERPA RoMEO API and cleaning) See (C:\Users\TKen02\UKRI\Policy Analysis - Documents\Open Access\Projects\Publication Venue Gap Analysis\Data\).

# NB. Just before merged_pvga was created (as imported below), the observations were randomised with set.seed(42). This is necessary because of a number of analyses which used !duplicated(Source.title) to focus on analysis of journals.Other random orders were tested and they make only small changes to some of the n columns (not making major changes to percentages). This approach is a simplification but seems a strong proxy.

#XXXXXXXXXXXXXXX
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


####

#build UKRI colour palette - # UKRI brand guidelines are saved here https://www.ukri.org/files/brand-guidelines-a-guide-to-understanding-and-using-our-brand/

ukri_pal <- c("#2E2D62","#FBBB10", "#F08900","#E355EC", "#923D9D","#34D5AE", "#16978A","#FF5A5A", "#CB3564","#00BED5", "#008AAD","#67c04D", "#3E863E","#1E5DF8", "#003088")

colour_ukri <- c("#2E2D62")
grey_ukri <- c("#676767")

# colour function for box plots, bar plots etc
scale_fill_manual(values = ukri_pal)
# colour function for lines and points
scale_color_manual(values = ukri_pal)

# Set black-and-white colour scheme and add UKRI font (Arial)

theme_set(theme_bw())
theme_update(text= element_text(family="arial", size=12), 
             rect = element_blank(),
             plot.title = element_text(family="arial", hjust = 0.5),
             axis.text = element_text(family = "arial", size = 12))


#XXXXXXX

# ACTUAL OPEN ACCESS STATUS----

# 1. Dimensions' Open Access Categories ----

Open.Access_a <- merged_pvga %>% # article level
  count(Open.Access2) %>%
  mutate(percent = round(n / sum(n) * 100),1) %>%
  adorn_totals("row")

Open.Access_j <- merged_pvga %>% # journal level
  filter(!duplicated(Source.title)) %>%
  count(Open.Access2) %>%
  mutate(percent = round(n / sum(n) * 100),1) %>%
  adorn_totals("row")

    # basic statistics of number of articles per journal within each oac (mean, median, sd, max)

sd_apj_pure <- psych::describe(merged_pvga$articles_in_journal[!duplicated(merged_pvga$Source.title) & merged_pvga$Open.Access2 == "Pure Gold"])
sd_apj_hybrid <- psych::describe(merged_pvga$articles_in_journal[!duplicated(merged_pvga$Source.title) & merged_pvga$Open.Access2 == "Hybrid gold"])
sd_apj_green <- psych::describe(merged_pvga$articles_in_journal[!duplicated(merged_pvga$Source.title) & merged_pvga$Open.Access2 == "Green"])
sd_apj_closed <- psych::describe(merged_pvga$articles_in_journal[!duplicated(merged_pvga$Source.title) & merged_pvga$Open.Access2 == "Closed"])
sd_apj_total <- psych::describe(merged_pvga$articles_in_journal[!duplicated(merged_pvga$Source.title)])
sd_apj <- bind_rows(sd_apj_pure, sd_apj_hybrid, sd_apj_green, sd_apj_closed, sd_apj_total)
rm(sd_apj_pure, sd_apj_hybrid, sd_apj_green, sd_apj_closed, sd_apj_total)

  # bind together for open access categories table in report
Open.Access <- bind_cols(Open.Access_a, Open.Access_j, sd_apj) %>%
  rename("Open access categories" = Open.Access2...1, n_articles = n...2, n_journals = n...6, standard_deviation = sd) %>%
  mutate(mean = round(mean,0), standard_deviation = round(standard_deviation,0)) %>%
  select(-c(4, 5, 8, 9, 10, 14, 15, 16, 18, 19, 20, 21))
rm(Open.Access_a, Open.Access_j, sd_apj)

openxlsx::write.xlsx(as.data.frame(Open.Access), 'Output/Tables/Open.Access.xlsx')

  # open access categories bar chart (not currently in report)
Open.Access_bar <- merged_pvga %>%
  count(Open.Access2) %>%
  mutate(placeholder = "", percent = round((n / sum(n) * 100)),0) %>%
  ggplot(aes(x = placeholder, y = percent, fill = Open.Access2)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("#F08900", "#FBBB10", "#16978A", "#FF5A5A"), name = "Impact of TA") +
  # ggtitle("Dimensions' Open Access Categories") +
  theme(axis.title.x = element_text(face="bold", size=14, margin = margin(t=10)),
        axis.text.x  = element_text(vjust=0.5, size=14),
        title =  element_text(face="bold", size=15),
        legend.text = element_text(size=14)) +
  labs(x="Articles associated with UKRI funding", y= "Percent of articles")

ggsave(Open.Access_bar, filename = "Output/Charts/Open.Access_bar.png")

# Bar chart for breakdown of articles in hybrid journals with TA and green
impact_of_ta <- merged_pvga %>%
  filter(journal_type == "Hybrid")

impact_of_ta$impact_of_ta <- if_else(impact_of_ta$has_ta == "yes", "Hybrid gold in journal covered by TA", 
                                     if_else(impact_of_ta$num_new_green %in% c(1,3), "No TA but certainly has compliant green route",
                                             if_else((impact_of_ta$g_license1 == "no license requirement" & impact_of_ta$g_embargo == 0 & impact_of_ta$g_compliant_repository == "TRUE"), "No TA and unconfirmed green route", "No TA or compliant green route")))

impact_of_ta$impact_of_ta[is.na(impact_of_ta$impact_of_ta)] <-  "No TA or compliant green route"

impact_of_ta <- impact_of_ta %>%
  count(impact_of_ta) %>%
  mutate(hybrid_articles="Hybrid articles", percent_of_hybrid = round(n/nrow(merged_pvga[merged_pvga$journal_type == "Hybrid", ])*100,1)) %>%
  mutate(impact_of_ta = factor(impact_of_ta, ordered = TRUE, levels = c("Hybrid gold in journal covered by TA", "No TA but certainly has compliant green route", "No TA and unconfirmed green route", "No TA or compliant green route"))) %>%
  arrange(impact_of_ta)

openxlsx::write.xlsx(as.data.frame(impact_of_ta), 'Output/Tables/impact_of_ta.xlsx')

impact_of_ta_chart <- ggplot(impact_of_ta, aes(x = hybrid_articles, y = percent_of_hybrid, fill=impact_of_ta)) +
  geom_bar(position = "stack", stat = "identity", width = 6) +
  scale_fill_manual(values = c("#FBBB10", "#16978A", "#99d8c9", "#FF5A5A"), name = "Impact of TA") +
  # ggtitle("Compliance of articles in hybrid \n journals if funding restricted \n to journals covered by a TA") +
  theme(axis.title.x = element_text(face="bold", size=14, margin = margin(t=10)),
        axis.text.x  = element_text(vjust=0.5, size=14),
        title =  element_text(face="bold", size=15),
        legend.text = element_text(size=14)) +
  labs(x="Articles in Hybrid journals", y= "% of articles in hybrid journals")

ggsave(impact_of_ta_chart, filename = "Output/Charts/hybrid_ta_breakdown.png")


# 2. Top publishers and oac----
top_publishers_oac <- merged_pvga %>%
  group_by(publisher) %>%
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

openxlsx::write.xlsx(as.data.frame(top_publishers_oac), 'Output/Tables/top_publishers_oac.xlsx')


#3. Dimensions Open Access Categories x Discipline ----

disciplines <- merged_pvga %>% 
  separate(discipline, into = c("disc1", "disc2", "disc3", "disc4"), sep = "; ") %>%
  pivot_longer(c(disc1, disc2, disc3, disc4), names_to = "disc_all", values_to ="disc")

Open.Access_a_disc <- disciplines %>%
  filter(!is.na(disc)) %>%
  group_by(disc) %>%
  count(Open.Access2, .drop = FALSE) %>%
  mutate(percent = (n / sum(n)) * 100) %>%
  mutate(cml = round(cumsum(percent),0)) %>%
  mutate(percent = round(percent, 0)) %>%
  mutate(disc = factor(disc, ordered = TRUE, levels = c("Arts & Humanities", "Health Sciences", "Life Sciences", "Physical Sciences", "Social Sciences", "Missing")), 
        Open.Access2 = factor(Open.Access2, ordered = TRUE, levels = c("Pure Gold", "Hybrid gold", "Green", "Closed"))) %>%
  arrange(disc, Open.Access2)

openxlsx::write.xlsx(as.data.frame(Open.Access_a_disc), 'Output/Tables/Open.Access_a_disc.xlsx')

# Bar
Open.Access_a_disc_bar <- Open.Access_a_disc %>%
  ggplot(aes(x=disc, y=percent)) +
  geom_bar(aes(fill=Open.Access2), stat = "identity", position = "stack", width = 0.9) +
  scale_fill_manual(values = c("#F08900","#FBBB10", "#16978A","#FF5A5A")) +
  # ggtitle("Open Access Categories by discipline") +
  theme(axis.title.x = element_text(face="bold", size=14, margin = margin(t=10)),
        axis.text.x  = element_text(vjust=0.5, size=12),
        title =  element_text(face="bold", size=16),
        legend.text = element_text(size=14)) +
  labs(x="Disciplines", y= "% of all articles in panel", fill = "Open access") +
  scale_x_discrete(labels = c("Arts &\nHumanities", "Health\nSciences","Life\nSciences", "Physical\nSciences", "Social\nSciences", "Missing"))

ggsave(Open.Access_a_disc_bar, filename = "Output/Charts/Open.Access_a_disc_bar.png")


# 3. Dimensions open acccess categories by Fields of Research ----
# To run this analysis we first need to create tables for Fields of Research Divisions and Groups

  # First Divisions
divisions <- merged_pvga %>% 
  separate(for_division, into = c("division1", "division2", "division3", "division4"), sep = "; ") %>%
  pivot_longer(c(division1, division2, division3, division4), names_to = "division_all", values_to ="division")

summary_divisions <- divisions %>%
  filter(!is.na(division)) %>% 
  group_by(division) %>% summarize(n=n()) %>%
  mutate(percent = round(n/(nrow(merged_pvga))*100,1))

openxlsx::write.xlsx(as.data.frame(summary_divisions), 'Output/Tables/summary_for_divisions.xlsx')

Open.Access_for_division <- divisions %>%
  group_by(division) %>%
  count(Open.Access2, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1))
openxlsx::write.xlsx(as.data.frame(Open.Access_for_division), 'Output/Tables/Open.Access_for_division.xlsx')
  
  # Then Groups
groups <- merged_pvga %>% 
  separate(for_group, into = c("group1", "group2", "group3", "group4"), sep = "; ") %>%
  pivot_longer(c(group1, group2, group3, group4), names_to = "group_all", values_to ="group")

summary_groups <- groups %>%
  filter(!is.na(group)) %>% 
  group_by(group) %>% summarize(n=n()) %>%
  mutate(percent = round(n/(nrow(merged_pvga))*100,1))

openxlsx::write.xlsx(as.data.frame(summary_groups), 'Output/Tables/summary_for_groups.xlsx')

Open.Access_for_group <- groups %>%
  group_by(group) %>%
  count(Open.Access2, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1))
openxlsx::write.xlsx(as.data.frame(Open.Access_for_group), 'Output/Tables/Open.Access_for_group.xlsx')

  # Then merge them together to look at fields of research together
summary_fields_of_research <- left_join(divisions[!is.na(divisions$division),], groups[!is.na(groups$group), c("group", "doi")], by = "doi")
      # Remove rows where there is not a match between division and group (i.e. duplicates, or empty)
summary_fields_of_research$division2 <- substr(summary_fields_of_research$division, start = 1, stop = 2)
summary_fields_of_research$group2 <- substr(summary_fields_of_research$group, start = 1, stop = 2)
summary_fields_of_research <- summary_fields_of_research[!(summary_fields_of_research$division2 != summary_fields_of_research$group2),]

      # Then summarise
summary_fields_of_research <- summary_fields_of_research %>%
  group_by(division, group) %>% summarize(n=n()) %>%
  mutate(percent = round(n/(nrow(merged_pvga))*100,1))

openxlsx::write.xlsx(as.data.frame(summary_fields_of_research), 'Output/Tables/summary_fields_of_research.xlsx')



# 4. Journal type ----

journal_type_a <- merged_pvga %>%
  filter(!is.na(journal_type)) %>%
  count(journal_type) %>%
  mutate(percent = (n / sum(n) * 100)) %>%
  mutate(cml = round(cumsum(percent),0)) %>%
  mutate(percent = round(percent,0)) %>%
  adorn_totals("row")
journal_type_a

journal_type_j <- merged_pvga %>%
  filter(!is.na(journal_type)) %>%
  filter(!duplicated(Source.title)) %>%
  count(journal_type) %>%
  mutate(percent = (n / sum(n) * 100)) %>%
  mutate(cml = round(cumsum(percent),0)) %>%
  mutate(percent = round(percent,0)) %>%
  adorn_totals("row")
journal_type_j

journal_type <- bind_cols(journal_type_a, journal_type_j) %>%
  mutate(articles_per_journal = round(n...2 / n...6),1) %>%
  select(-5, -10)
journal_type
rm(journal_type_a, journal_type_j)
openxlsx::write.xlsx(as.data.frame(journal_type), 'Output/Tables/journal_type.xlsx')

# Journal type by discipline

disciplines <- merged_pvga %>% 
  separate(discipline, into = c("disc1", "disc2", "disc3", "disc4"), sep = "; ") %>%
  pivot_longer(c(disc1, disc2, disc3, disc4), names_to = "disc_all", values_to ="disc")

journal_type_a_disc <- disciplines %>%
  filter(!is.na(disc)) %>%
  group_by(disc) %>%
  count(journal_type, .drop = FALSE) %>%
  mutate(percent = (n / sum(n)) * 100) %>%
  mutate(cml = round(cumsum(percent),0)) %>%
  mutate(percent = round(percent, 0))

journal_type_j_disc <- disciplines %>%
  filter(!is.na(disc), !duplicated(Source.title)) %>%
  group_by(disc) %>%
  count(journal_type, .drop = FALSE) %>%
  mutate(percent = (n / sum(n)) * 100) %>%
  mutate(cml = round(cumsum(percent),0)) %>%
  mutate(percent = round(percent, 0))

journal_type_disc <- bind_cols(journal_type_a_disc, journal_type_j_disc) %>%
  mutate(disc = factor(disc...1, ordered = TRUE, levels = c("Arts & Humanities", "Health Sciences", "Life Sciences", "Physical Sciences", "Social Sciences", "Missing"))) %>%
  select(-c(5,6,7,10)) %>%
  rename(n_articles = n...3, n_journals = n...8) %>%
    arrange(disc)

openxlsx::write.xlsx(as.data.frame(journal_type_disc), 'Output/Tables/journal_type_disc.xlsx')

# Stacked bar chart
journal_type_a_disc_bar <- journal_type_a_disc %>%
  mutate(journal_type = factor(journal_type, ordered = TRUE, levels = c("Pure Gold", "Hybrid", "Closed or insufficient information")),
         disc = factor(disc, ordered = TRUE, levels = c("Arts & Humanities", "Health Sciences", "Life Sciences", "Physical Sciences", "Social Sciences", "Missing"))) %>%
  ggplot(aes(x=disc, y=percent)) +
  geom_bar(aes(fill=journal_type), stat = "identity", position = "stack", width = 0.9) +
  scale_fill_manual(values = c("#F08900","#FBBB10", "#FF5A5A"), labels = c("Pure Gold\n", "Hybrid\n", "Closed or insufficient\ninformation")) +
  # ggtitle("Journal Type by Discipline") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme(axis.title.x = element_text(face="bold", size=12, margin = margin(t=10)),
        axis.text.x  = element_text(vjust=0.5, size=10),
        title =  element_text(face="bold", size=16),
        legend.text = element_text(size=14)) +
  labs(x="Discipline", y= "% of all articles in panel", fill = "Journal Type") 

ggsave(journal_type_a_disc_bar, filename = "Output/Charts/journal_type_a_disc_bar.png")


#XXXXXXXXXXXXXXXXX
# 5. Transformative agreements----

# What number and proportion of articles journals are covered by TAs
TA_articles <- merged_pvga %>% filter(journal_type == "Hybrid") %>% count(has_ta) %>% mutate(percent = round(n / sum(n) * 100, 1)) %>%  adorn_totals("row")
TA_journals <- merged_pvga %>%  filter(!duplicated(Source.title), journal_type == "Hybrid") %>% count(has_ta) %>% mutate(percent = round(n / sum(n) * 100, 1)) %>%  adorn_totals("row")
TA <- bind_cols(TA_articles, TA_journals) %>% rename(number_articles = n...2, number_journals = n...5) %>% select(-4)

openxlsx::write.xlsx(as.data.frame(TA), 'Output/Tables/transformative_agreements.xlsx')


# Transformative agreements coverage in hybrid journals by Discipline

disciplines <- merged_pvga %>% 
  separate(discipline, into = c("disc1", "disc2", "disc3", "disc4"), sep = "; ") %>%
  pivot_longer(c(disc1, disc2, disc3, disc4), names_to = "disc_all", values_to ="disc")

TA_a_disc <- disciplines %>%
  filter(!is.na(disc), journal_type == "Hybrid") %>%
  group_by(disc) %>%
  count(has_ta, .drop = FALSE) %>%
  mutate(percent = round(n/sum(n)*100),1) %>%
  adorn_totals("row")

TA_j_disc <- disciplines %>%
  filter(!is.na(disc), !duplicated(Source.title), journal_type == "Hybrid") %>%
  group_by(disc) %>%
  count(has_ta, .drop = FALSE) %>%
  mutate(percent = round(n/sum(n)*100),1) %>%
  adorn_totals("row")

TA_disc <- bind_cols(TA_a_disc, TA_j_disc) %>%
  select(-c(5,6,7,10)) %>%
  rename(n_articles = n...3, n_journals = n...8) %>%
  arrange(factor(disc...1, levels = c("Arts & Humanities", "Health Sciences", "Life Sciences", "Physical Sciences", "Social Sciences"))) %>%
  filter(has_ta...2 == "yes")

TA_disc2 <- bind_cols(TA_a_disc, TA_j_disc) %>% # this one shows has ta and doesn't have ta
  select(-c(5,6,7,10)) %>%
  rename(n_articles = n...3, n_journals = n...8) %>%
  arrange(factor(disc...1, levels = c("Arts & Humanities", "Health Sciences", "Life Sciences", "Physical Sciences", "Social Sciences")))

  # Adding Totals row at the bottom (i.e. figures for all hybrid journals)
    # this involves generating totals for articles and then journals, then merging those columns, renaming them, and binding rows with previous table
TA_hybrid_a <- merged_pvga %>% filter(journal_type == "Hybrid") %>% count(has_ta) %>% mutate(percent = n/sum(n)*100) %>%
  filter(has_ta == "yes")
TA_hybrid_j <- merged_pvga %>% filter(!duplicated(Source.title), journal_type == "Hybrid") %>% count(has_ta) %>% mutate(percent = n/sum(n)*100) %>%
  filter(has_ta == "yes")
TA_hybrid <- bind_cols(TA_hybrid_a, TA_hybrid_j) %>%
  mutate(disc = "ALL") %>%
  select(-has_ta...4) %>%
  relocate(disc) %>%
  rename(has_ta...2 = has_ta...1, n_articles = n...2, percent...4 = percent...3, n_journals = n...5, percent...9 = percent...6)

TA_disc <- bind_rows(TA_disc, TA_hybrid)
rm(TA_hybrid, TA_hybrid_a, TA_hybrid_j)

openxlsx::write.xlsx(as.data.frame(TA_disc), 'Output/Tables/TA_disc.xlsx')


# Transformative agreements coverage in hybrid journals by subject

divisions <- merged_pvga %>% 
  separate(for_division, into = c("div1", "div2", "div3", "div4"), sep = "; ") %>%
  pivot_longer(c(div1, div2, div3, div4), names_to = "div_all", values_to ="div")

TA_a_div <- divisions %>%
  filter(!is.na(div), journal_type == "Hybrid") %>%
  group_by(div) %>%
  count(has_ta, .drop = FALSE) %>%
  mutate(percent = round(n/sum(n)*100),1) %>%
  adorn_totals("row")

TA_j_div <- divisions %>%
  filter(!is.na(div), !duplicated(Source.title), journal_type == "Hybrid") %>%
  group_by(div) %>%
  count(has_ta, .drop = FALSE) %>%
  mutate(percent = round(n/sum(n)*100),1) %>%
  adorn_totals("row")

TA_div <- bind_cols(TA_a_div, TA_j_div) %>%
  select(-c(5,6,7,10)) %>%
  rename(n_articles = n...3, n_journals = n...8) %>%
  filter(has_ta...2 == "yes")
rm(TA_j_div, TA_a_div)

openxlsx::write.xlsx(as.data.frame(TA_div), 'Output/Tables/TA_div.xlsx')



# Bar chart for breakdown of articles in hybrid journals with TA and green
impact_of_ta <- merged_pvga %>%
  filter(journal_type == "Hybrid")

impact_of_ta$impact_of_ta <- if_else(impact_of_ta$has_ta == "yes", "Hybrid gold in\njournal covered by TA", 
                              if_else(impact_of_ta$num_new_green %in% c(1,3), "No TA but certainly\nhas compliant green route",
                                if_else((impact_of_ta$g_license1 == "no license requirement" & impact_of_ta$g_embargo == 0 & impact_of_ta$g_compliant_repository == "TRUE"), "No TA and unconfirmed\ngreen route", "No TA or compliant\ngreen route")))

impact_of_ta$impact_of_ta[is.na(impact_of_ta$impact_of_ta)] <-  "No TA or compliant\ngreen route"

impact_of_ta <- impact_of_ta %>%
  count(impact_of_ta) %>%
  mutate(hybrid_articles="Hybrid articles", percent_of_hybrid = round(n/nrow(merged_pvga[merged_pvga$journal_type == "Hybrid", ])*100,1)) %>%
  arrange(factor(impact_of_ta, ordered = TRUE, levels = c("Hybrid gold in\njournal covered by TA", "No TA but certainly\nhas compliant green route", "No TA and unconfirmed\ngreen route", "No TA or compliant\ngreen route")))

openxlsx::write.xlsx(as.data.frame(impact_of_ta), 'Output/Tables/impact_of_ta.xlsx')

impact_of_ta_chart <- ggplot(impact_of_ta, aes(x = hybrid_articles, y = percent_of_hybrid, fill=impact_of_ta)) +
         geom_bar(position = "stack", stat = "identity") +
         scale_fill_manual(values = c("#FBBB10", "#16978A", "#99d8c9", "#FF5A5A"), name = "Impact of TA") +
         # ggtitle("Compliance of articles in hybrid \n journals if funding restricted \n to journals covered by a TA") +
  theme(axis.title.x = element_text(face="bold", size=14, margin = margin(t=10)),
        axis.text.x  = element_text(vjust=0.5, size=14),
        title =  element_text(face="bold", size=15),
        legend.text = element_text(size=14)) +
  labs(x="Articles in Hybrid journals", y= "% of articles in hybrid journals")

ggsave(impact_of_ta_chart, filename = "Output/Charts/impact_of_ta_chart.png")

# Actual open access status of articles with and without a TA
TA_oac <- merged_pvga %>% filter(journal_type =="Hybrid", has_ta == "yes") %>% count(Open.Access2) %>% mutate(percent = n/sum(n)*100)
NoTA_oac <- merged_pvga %>% filter(journal_type =="Hybrid", has_ta == "no") %>% count(Open.Access2) %>% mutate(percent = n/sum(n)*100)
TA_oac <- bind_cols(TA_oac, NoTA_oac)
  rm(NoTA_oac)

# Green OA compliance of articles with and without a TA
merged_pvga$num_new_green <- factor(merged_pvga$num_new_green, ordered = TRUE, c(1,2,3,4,5,6,7,8,99)) # this is needed as otherwise bind_cols wont work.
  TA_green <- merged_pvga %>% filter(journal_type =="Hybrid", has_ta == "yes") %>% count(num_new_green, .drop = FALSE) %>% mutate(percent = n/sum(n)*100)
  NoTA_green <- merged_pvga %>% filter(journal_type =="Hybrid", has_ta == "no") %>% count(num_new_green, .drop = FALSE) %>% mutate(percent = n/sum(n)*100)
  TA_green <- bind_cols(TA_green, NoTA_green)
  rm(NoTA_green)         
                                   
#XXXXXXXXXXX
# POTENTIAL COMPLIANCE WITH CURRENT POLICY----

# 1. # Potential compliance with old policy
compliance_current_a <- merged_pvga %>%
  count(compliance_current, .drop = FALSE) %>%
  mutate(percent = round((n / sum(n) * 100),1), cml = cumsum(percent)) %>%
  adorn_totals("row")

compliance_current_j <- merged_pvga %>%
  filter(!duplicated(Source.title)) %>% # NB code for creating merged data pseudo-randomly orders observations using set.seed(42), but any random order should work with only small variations in the n column.
  count(compliance_current, .drop = FALSE) %>%
  mutate(percent = round((n / sum(n) * 100),1), cml = cumsum(percent)) %>%
  adorn_totals("row")
compliance_current_j

compliance_current <- bind_cols(compliance_current_a, compliance_current_j) %>%
  select(-5)
rm(compliance_current_a, compliance_current_j)
openxlsx::write.xlsx(as.data.frame(compliance_current), 'Output/Tables/compliance_current.xlsx')

# 2. Potential compliance with current policy by discipline

  # There are two ref columns and we need to know if each subject exists in either of them for each article.
disciplines <- merged_pvga %>% 
  separate(discipline, into = c("disc1", "disc2", "disc3", "disc4"), sep = "; ") %>%
  pivot_longer(c(disc1, disc2, disc3, disc4), names_to = "disc_all", values_to ="disc")

compliance_current_disc_a <- disciplines %>%
  filter(!is.na(disc)) %>% 
  group_by(disc) %>%
  count(compliance_current, .drop = FALSE) %>%
  pivot_wider(names_from = disc, values_from = n) %>%
  rename(Arts = "Arts & Humanities", Health = "Health Sciences", Life = "Life Sciences", Physical = "Physical Sciences", Social = "Social Sciences") %>%
  mutate(percent_arts = round(Arts/sum(Arts)*100,1), percent_health = round(Health/sum(Health)*100,1), percent_life = round(Life/sum(Life)*100,1), percent_physical = round(Physical/sum(Physical)*100,1), percent_social = round(Social/sum(Social)*100,1), percent_missing = round(Missing/sum(Missing)*100,1)) %>%
  relocate(compliance_current, Arts, percent_arts, Health, percent_health, Life, percent_life, Physical, percent_physical, Social, percent_social, Missing, percent_missing)

compliance_current_disc_j <- disciplines %>%
  filter(!is.na(disc), !duplicated(Source.title)) %>%
  group_by(disc) %>%
  count(compliance_current, .drop = FALSE) %>%
  pivot_wider(names_from = disc, values_from = n) %>%
  rename(Arts = "Arts & Humanities", Health = "Health Sciences", Life = "Life Sciences", Physical = "Physical Sciences", Social = "Social Sciences") %>%
  mutate(percent_arts = round(Arts/sum(Arts)*100,1), percent_health = round(Health/sum(Health)*100,1), percent_life = round(Life/sum(Life)*100,1), percent_physical = round(Physical/sum(Physical)*100,1), percent_social = round(Social/sum(Social)*100,1), percent_missing = round(Missing/sum(Missing)*100,1)) %>%
  relocate(compliance_current, Arts, percent_arts, Health, percent_health, Life, percent_life, Physical, percent_physical, Social, percent_social, Missing, percent_missing)

compliance_current_disc <- bind_cols(compliance_current_disc_a, compliance_current_disc_j) %>%
  rename(Arts_articles = Arts...2, Health_articles = Health...4, Life_articles = Life...6, Physical_articles = Physical...8, Social_articles = Social...10, Missing_articles = Missing...12, Arts_journals = Arts...15, Health_journals = Health...17, Life_journals = Life...19, Physical_journals = Physical...21, Social_journals = Social...23, Missing_journals = Missing...25) %>%
  adorn_totals("row") %>%
  select(-'compliance_current...14')

openxlsx::write.xlsx(as.data.frame(compliance_current_disc), 'Output/Tables/compliance_current_disc.xlsx')



#XXXXXXXXXXX
# POTENTIAL COMPLIANCE WITH NEW POLICY SCENARIO 1 SUPPORTING HYBRID GOLD----

# 1. Potential compliance with new policy (if hybrid gold allowed) ----
compliance_h_a <- merged_pvga %>%
  count(compliance_new_hybrid, .drop = FALSE) %>%
  mutate(percent = round((n / sum(n) * 100),1), cml = cumsum(percent)) %>%
  adorn_totals("row")

compliance_h_j <- merged_pvga %>%
  filter(!duplicated(Source.title)) %>%
  count(compliance_new_hybrid, .drop = FALSE) %>%
  mutate(percent = round((n / sum(n) * 100),1), cml = cumsum(percent)) %>%
  adorn_totals("row")

compliance_h <- bind_cols(compliance_h_a, compliance_h_j) %>%
  select(-5)
rm(compliance_h_a, compliance_h_j)
openxlsx::write.xlsx(as.data.frame(compliance_h), 'Output/Tables/compliance_new_hybrid.xlsx')


# 2. Potential compliance with new policy (S1) by ref_panel

# There are two ref columns and we need to know if each subject exists in either of them for each article.
disciplines <- merged_pvga %>% 
  separate(discipline, into = c("disc1", "disc2", "disc3", "disc4"), sep = "; ") %>%
  pivot_longer(c(disc1, disc2, disc3, disc4), names_to = "disc_all", values_to ="disc")

compliance_new_hybrid_disc_a <- disciplines %>%
  filter(!is.na(disc)) %>% 
  group_by(disc) %>%
  count(compliance_new_hybrid, .drop = FALSE) %>%
  pivot_wider(names_from = disc, values_from = n) %>%
  rename(Arts = "Arts & Humanities", Health = "Health Sciences", Life = "Life Sciences", Physical = "Physical Sciences", Social = "Social Sciences") %>%
  mutate(percent_arts = round(Arts/sum(Arts)*100,1), percent_health = round(Health/sum(Health)*100,1), percent_life = round(Life/sum(Life)*100,1), percent_physical = round(Physical/sum(Physical)*100,1), percent_social = round(Social/sum(Social)*100,1), percent_missing = round(Missing/sum(Missing)*100,1)) %>%
  relocate(compliance_new_hybrid, Arts, percent_arts, Health, percent_health, Life, percent_life, Physical, percent_physical, Social, percent_social, Missing, percent_missing)

compliance_new_hybrid_disc_j <- disciplines %>%
  filter(!is.na(disc), !duplicated(Source.title)) %>%
  group_by(disc) %>%
  count(compliance_new_hybrid, .drop = FALSE) %>%
  pivot_wider(names_from = disc, values_from = n) %>%
  rename(Arts = "Arts & Humanities", Health = "Health Sciences", Life = "Life Sciences", Physical = "Physical Sciences", Social = "Social Sciences") %>%
  mutate(percent_arts = round(Arts/sum(Arts)*100,1), percent_health = round(Health/sum(Health)*100,1), percent_life = round(Life/sum(Life)*100,1), percent_physical = round(Physical/sum(Physical)*100,1), percent_social = round(Social/sum(Social)*100,1), percent_missing = round(Missing/sum(Missing)*100,1)) %>%
  relocate(compliance_new_hybrid, Arts, percent_arts, Health, percent_health, Life, percent_life, Physical, percent_physical, Social, percent_social, Missing, percent_missing)

compliance_new_hybrid_disc <- bind_cols(compliance_new_hybrid_disc_a, compliance_new_hybrid_disc_j)  %>%
  rename(Arts_articles = Arts...2, Health_articles = Health...4, Life_articles = Life...6, Physical_articles = Physical...8, Social_articles = Social...10, Missing_articles = Missing...12, Arts_journals = Arts...15, Health_journals = Health...17, Life_journals = Life...19, Physical_journals = Physical...21, Social_journals = Social...23, Missing_journals = Missing...25) %>%
  adorn_totals("row") %>%
  select(-'compliance_new_hybrid...14')
rm(compliance_new_hybrid_disc_a, compliance_new_hybrid_disc_j)

openxlsx::write.xlsx(as.data.frame(compliance_new_hybrid_disc), 'Output/Tables/compliance_new_hybrid_disc.xlsx')


#XXXXXXXXXXX
# POTENTIAL COMPLIANCE WITH NEW POLICY SCENARIO 2 (NO SUPPORT FOR HYBRID GOLD OTHER THAN THROUGH TA)----

# 1. Full break down of potential compliance with new policy (no Hybrid gold allowed) ----
compliance_new_a <- merged_pvga %>%
  count(compliance_new, .drop = FALSE) %>%
  mutate(percent = (n / sum(n) * 100)) %>%
  mutate(cml = round(cumsum(percent),0)) %>%
  mutate(percent = round(percent,0)) %>%
  adorn_totals("row")

compliance_new_j <- merged_pvga %>%
  filter(!duplicated(Source.title)) %>%
  count(compliance_new, .drop = FALSE) %>%
  mutate(percent = (n / sum(n) * 100)) %>%
  mutate(cml = round(cumsum(percent),0)) %>%
  mutate(percent = round(percent,0)) %>%
  adorn_totals("row")

compliance_new <- bind_cols(compliance_new_a, compliance_new_j) %>%
  select(-5)
rm(compliance_new_a, compliance_new_j)
openxlsx::write.xlsx(as.data.frame(compliance_new), 'Output/Tables/compliance_new.xlsx')


# 2. Compliance with new policy S2 by discipline ----

disciplines <- merged_pvga %>% 
  separate(discipline, into = c("disc1", "disc2", "disc3", "disc4"), sep = "; ") %>%
  pivot_longer(c(disc1, disc2, disc3, disc4), names_to = "disc_all", values_to ="disc")

compliance_new_disc_a <- disciplines %>%
  filter(!is.na(disc)) %>% 
  group_by(disc) %>%
  count(compliance_new, .drop = FALSE) %>%
  pivot_wider(names_from = disc, values_from = n) %>%
  rename(Arts = "Arts & Humanities", Health = "Health Sciences", Life = "Life Sciences", Physical = "Physical Sciences", Social = "Social Sciences") %>%
  mutate(percent_arts = round(Arts/sum(Arts)*100,1), percent_health = round(Health/sum(Health)*100,1), percent_life = round(Life/sum(Life)*100,1), percent_physical = round(Physical/sum(Physical)*100,1), percent_social = round(Social/sum(Social)*100,1), percent_missing = round(Missing/sum(Missing)*100,1)) %>%
  relocate(compliance_new, Arts, percent_arts, Health, percent_health, Life, percent_life, Physical, percent_physical, Social, percent_social, Missing, percent_missing)

compliance_new_disc_j <- disciplines %>%
  filter(!is.na(disc), !duplicated(Source.title)) %>%
  group_by(disc) %>%
  count(compliance_new, .drop = FALSE) %>%
  pivot_wider(names_from = disc, values_from = n) %>%
  rename(Arts = "Arts & Humanities", Health = "Health Sciences", Life = "Life Sciences", Physical = "Physical Sciences", Social = "Social Sciences") %>%
  mutate(percent_arts = round(Arts/sum(Arts)*100,1), percent_health = round(Health/sum(Health)*100,1), percent_life = round(Life/sum(Life)*100,1), percent_physical = round(Physical/sum(Physical)*100,1), percent_social = round(Social/sum(Social)*100,1), percent_missing = round(Missing/sum(Missing)*100,1)) %>%
  relocate(compliance_new, Arts, percent_arts, Health, percent_health, Life, percent_life, Physical, percent_physical, Social, percent_social, Missing, percent_missing)

compliance_new_disc <- bind_cols(compliance_new_disc_a, compliance_new_disc_j)  %>%
  rename(Arts_articles = Arts...2, Health_articles = Health...4, Life_articles = Life...6, Physical_articles = Physical...8, Social_articles = Social...10, Missing_articles = Missing...12, Arts_journals = Arts...15, Health_journals = Health...17, Life_journals = Life...19, Physical_journals = Physical...21, Social_journals = Social...23, Missing_journals = Missing...25) %>%
  adorn_totals("row") %>%
  select(-'compliance_new...14')

openxlsx::write.xlsx(as.data.frame(compliance_new_disc), 'Output/Tables/compliance_new_disc.xlsx')

# Stacked bar chart for Compliance by discipline
compliance_new_disc_a <- disciplines %>%
  filter(!is.na(disc)) %>% 
  group_by(disc) %>%
  count(compliance_new, .drop = FALSE) %>%
  mutate(percent = round(n/sum(n)*100,1))

compliance_new_disc_a$disc[1:5] <- "Arts &\nHumanities"
compliance_new_disc_a$disc[6:10] <- "Health\nSciences"
compliance_new_disc_a$disc[11:15] <- "Life\nSciences"
compliance_new_disc_a$disc[16:20] <- "Missing"
compliance_new_disc_a$disc[21:25] <- "Physical\nSciences"
compliance_new_disc_a$disc[26:30] <- "Social\nSciences"

compliance_new_disc_a$disc <- factor(compliance_new_disc_a$disc, ordered = TRUE, c("Arts &\nHumanities", "Health\nSciences", "Life\nSciences", "Physical\nSciences", "Social\nSciences", "Missing"))

compliance_new_disc_chart <- ggplot(compliance_new_disc_a, aes(fill=compliance_new, y = percent, x = disc)) +
  geom_bar(position = "stack", stat = "identity", width = 0.9) +
  scale_fill_manual(values = c("#F08900", "#FBBB10", "#16978A", "#99d8c9", "#FF5A5A"), name = "Route to Compliance") +
  # ggtitle("Compliance by Discipline in S2") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme(axis.title.x = element_text(face="bold", size=14, margin = margin(t=10)),
        axis.text.x  = element_text(vjust=0.5, size=12),
        title =  element_text(face="bold", size=16),
        legend.text = element_text(size=14)) +
  labs(x="Discipline", y= "% of articles")

ggsave(compliance_new_disc_chart, filename = "Output/Charts/compliance_new_disc_chart.png")
  

# 3. compliant by publisher ----

# This code creates a table which shows all publishers ranked by the number of compliant articles they have in our sample, with several further breakdowns
compliance_new_publisher <- NA
compliance_new_publisher <- merged_pvga %>%
  filter(compliance_new2 != "not compliant") %>%
  count(publisher, sort = TRUE) %>%
  mutate(percent_of_compliant_articles = (n / sum(n)) * 100, cml = round(cumsum(percent_of_compliant_articles),0), percent_of_compliant_articles = round(percent_of_compliant_articles,0))
# add in total articles per publisher
articles_per_publisher <- merged_pvga %>%
  count(publisher, sort = TRUE)
# add in total articles compliant through pure gold
pure_gold_per_publisher <- merged_pvga %>%
  filter(compliance_new == "c: pure gold") %>%
  count(publisher, sort = TRUE)
# add in total articles compliant through TA
ta_per_publisher <- merged_pvga %>%
  filter(compliance_new == "c: hybrid gold with a TA") %>%
  count(publisher, sort = TRUE)
# add in total articles compliant through green
green_per_publisher <- merged_pvga %>%
  filter(compliance_new == "c: confirmed green oa") %>%
  count(publisher, sort = TRUE)

compliance_new_publisher <- left_join(compliance_new_publisher, articles_per_publisher, by = "publisher") %>%
  mutate(percent_of_publishers_articles = round(n.x/n.y*100,1)) %>%
  select(-c(5))

compliance_new_publisher <- left_join(compliance_new_publisher, pure_gold_per_publisher, by = "publisher") %>%
  mutate(percent_pure = round((n/n.x)*100,1)) %>%
  rename(n_pure = n)

compliance_new_publisher <- left_join(compliance_new_publisher, ta_per_publisher, by = "publisher") %>%
  mutate(proportion_c_by_ta = round(n/n.x * 100, 0)) %>%
  rename(n_c_by_ta = n)

  compliance_new_publisher <- left_join(compliance_new_publisher, green_per_publisher, by = "publisher") %>%
  mutate(percent_green = round(n/n.x*100,1)) %>%
    rename(n_compliant = n.x, n_green = n) %>%
  select(-c(6,8,10)) %>%
    adorn_totals("row")

rm(ta_per_publisher, articles_per_publisher, pure_gold_per_publisher, green_per_publisher)

openxlsx::write.xlsx(as.data.frame(compliance_new_publisher), 'Output/Tables/compliant_new_publisher.xlsx')



# 4. Non-compliant by publisher ----

not_compliance_new_publisher <- merged_pvga %>%
  filter(compliance_new2 == "not compliant") %>%
  count(publisher, sort = TRUE) %>%
  mutate(percent = (n / sum(n)) * 100) %>%
  mutate(cml = round(cumsum(percent),0)) %>%
  mutate(percent = round(percent, 0)) %>%
  adorn_totals("row")
# add in total articles per publisher
articles_per_publisher <- merged_pvga %>%
  count(publisher, sort = TRUE)

not_compliance_new_publisher <- left_join(not_compliance_new_publisher, articles_per_publisher, by = "publisher") %>%
  mutate(percent_of_publisher_total = round(n.x / n.y * 100),1) %>%
  select(-5)
openxlsx::write.xlsx(as.data.frame(not_compliance_new_publisher), 'Output/Tables/not_compliant_new_publisher.xlsx')



# 5. Non compliant by FOR Division and Group ----
# to work out percentages you need to generate a list of totals for each uoc then filter that so as to only include the categories that still exist after the filter.

  # FIRST DIVISION
divisions <- merged_pvga %>% 
  separate(for_division, into = c("division1", "division2", "division3", "division4"), sep = "; ") %>%
  pivot_longer(c(division1, division2, division3, division4), names_to = "division_all", values_to ="division")

# Working out denominators (since we won't be able to do this after filtering the data)
summary_division <- divisions %>%
  filter(!is.na(division)) %>% 
  group_by(division) %>% summarize(n=n())

# Highest number and proportion of non-compliant articles
not_compliant_new_division <- divisions %>%
  filter(compliance_new2 == "not compliant", !is.na(division)) %>%
  group_by(division) %>%
  summarise(count = n()) %>%
  mutate(percent = round(count / sum(count) * 100, 1), percent_not_compliant = round((count / c(summary_division$n)*100), digits = 1)) %>%
  arrange(desc(count)) %>%
  mutate(cml = round(cumsum(percent),1)) %>%
  adorn_totals("row")
openxlsx::write.xlsx(as.data.frame(not_compliant_new_division), 'Output/Tables/not_compliant_new_division.xlsx')

    # WITH ELSEVIER TA - to show impact if Jisc negotiated TA with Elsevier

merged_pvga$has_ta_elsevier <- ifelse(merged_pvga$publisher == "Elsevier", "yes", merged_pvga$has_ta)

merged_pvga$compliance_new_elsevier_ta <- NA
merged_pvga$compliance_new_elsevier_ta[merged_pvga$num_fee %in% c(1,4)] <- "c: pure gold"
merged_pvga$compliance_new_elsevier_ta[merged_pvga$has_ta_elsevier == "yes" & merged_pvga$num_fee %in% c(2,5)] <- "c: hybrid gold with a TA"
merged_pvga$compliance_new_elsevier_ta[!merged_pvga$num_fee %in% c(1,4) & !(merged_pvga$has_ta_elsevier == "yes" & merged_pvga$num_fee %in% c(2,5)) & merged_pvga$num_new_green %in% c(1,3)] <- "c: confirmed green oa"
merged_pvga$compliance_new_elsevier_ta[!merged_pvga$num_fee %in% c(1,4) & !(merged_pvga$has_ta_elsevier == "yes" & merged_pvga$num_fee %in% c(2,5)) & !merged_pvga$num_new_green %in% c(1,3) & (merged_pvga$g_embargo == 0 & merged_pvga$g_license1 == "no license requirement") & merged_pvga$g_compliant_repository == "TRUE"] <- "nc: unconfirmed green oa"
merged_pvga$compliance_new_elsevier_ta[is.na(merged_pvga$compliance_new_elsevier_ta)] <- "not compliant"

not_compliant_new_division_elsevier_ta <- divisions %>%
  filter(compliance_new_elsevier_ta == "not compliant", !is.na(division)) %>%
  group_by(division) %>%
  summarise(count = n()) %>%
  mutate(percent = round(count / sum(count) * 100, 1), percent_not_compliant = round((count / c(summary_division$n)*100), digits = 1)) %>%
  arrange(desc(count)) %>%
  mutate(cml = round(cumsum(percent),1)) %>%
  adorn_totals("row")
openxlsx::write.xlsx(as.data.frame(not_compliant_new_division), 'Output/Tables/not_compliant_new_division_elsevier_ta.xlsx')
      
  #THEN GROUP
groups <- merged_pvga %>% 
  separate(for_group, into = c("group1", "group2", "group3", "group4"), sep = "; ") %>%
  pivot_longer(c(group1, group2, group3, group4), names_to = "group_all", values_to ="group")

# Working out denominators (since we won't be able to do this after filtering the data)
summary_group <- groups %>%
  filter(!is.na(group)) %>% 
  group_by(group) %>% summarize(n=n())
# openxlsx::write.xlsx(as.data.frame(summary_uoa), 'summary_uoa.xlsx')

# Highest number and proportion of non-compliant articles
not_compliant_new_group <- groups %>%
  filter(compliance_new2 == "not compliant", !is.na(group)) %>%
  group_by(group) %>%
  summarise(count = n()) %>%
  mutate(percent = round(count / sum(count) * 100, 1), percent_not_compliant = round((count / c(summary_group$n)*100), digits = 1)) %>%
  arrange(desc(count)) %>%
  mutate(cml = round(cumsum(percent),1)) %>%
  adorn_totals("row")
openxlsx::write.xlsx(as.data.frame(not_compliant_new_group), 'Output/Tables/not_compliant_new_group.xlsx')

# 6. Compliance by Fields of Research----

  # FIRST DIVISIONS
divisions <- merged_pvga %>% 
  separate(for_division, into = c("division1", "division2", "division3", "division4"), sep = "; ") %>%
  pivot_longer(c(division1, division2, division3, division4), names_to = "division_all", values_to ="division")

compliance_new_division <- divisions %>%
  filter(!is.na(division)) %>%
  group_by(division) %>%
  count(compliance_new) %>%
  mutate(percent = round(n/sum(n)*100,1)) %>%
  mutate(total_n = sum(n)) %>%
  select(-n) %>%
  pivot_wider(names_from = compliance_new, values_from = c(percent, total_n)) %>%
  rename(total_n = "total_n_c: pure gold") %>%
  select(-c(8,9,10,11))

compliance_new_division$division[is.na(compliance_new_division$division)] <- "Missing"

openxlsx::write.xlsx(as.data.frame(compliance_new_division), 'Output/Tables/compliance_new_division.xlsx')

  # THEN GROUPS
groups <- merged_pvga %>% 
  separate(for_group, into = c("group1", "group2", "group3", "group4"), sep = "; ") %>%
  pivot_longer(c(group1, group2, group3, group4), names_to = "group_all", values_to ="group")

compliance_new_group <- groups %>%
  filter(!is.na(group)) %>%
  group_by(group) %>%
  count(compliance_new, .drop = FALSE) %>%
  mutate(percent = round(n/sum(n)*100,1)) %>%
  mutate(total_n = sum(n)) %>%
  select(-n) %>%
  pivot_wider(names_from = compliance_new, values_from = c(percent, total_n)) %>%
  rename(total_n = "total_n_c: pure gold") %>%
  select(-c(8,9,10,11))

compliance_new_group$group[is.na(compliance_new_group$group)] <- "Missing"

openxlsx::write.xlsx(as.data.frame(compliance_new_group), 'Output/Tables/compliance_new_group.xlsx')


#7. Compliance by funding council----
ukri_funders <- merged_pvga %>% 
  separate(ukri_funders, into = c("ukri_funder1", "ukri_funder2", "ukri_funder3", "ukri_funder4","ukri_funder5", "ukri_funder6", "ukri_funder7", "ukri_funder8"), sep = ", ") %>%
  pivot_longer(c(ukri_funder1, ukri_funder2, ukri_funder3, ukri_funder4, ukri_funder5, ukri_funder6, ukri_funder7, ukri_funder8), names_to = "ukri_funder_all", values_to ="ukri_funder")

summary_ukri_funders <- ukri_funders %>%
  filter(!is.na(ukri_funder)) %>% 
  group_by(ukri_funder) %>% summarize(n=n())

compliance_new_ukri_funder <- ukri_funders  %>%
  group_by(ukri_funder) %>%
  count(compliance_new) %>%
  mutate(percent = round(n/sum(n)*100,1)) %>%
  mutate(total_n = sum(n)) %>%
  select(-n) %>%
  pivot_wider(names_from = compliance_new, values_from = c(percent, total_n)) %>%
  rename(total_n = "total_n_c: pure gold") %>%
  select(-c(8,9,10,11))

openxlsx::write.xlsx(as.data.frame(compliance_new_ukri_funder), 'Output/Tables/compliance_new_ukri_funder.xlsx')

# GREEN LICENSES AND EMBARGOS ----
  # first compliant only, then all except pure gold, then all, then bind them all together, rename columns, and remove unnecessary rows and columns. Pure Gold needs to be excluded since most default pure gold policies are listed as not being associated with an additional_oa_fee in Sherpa, however, they are associated with a fee in practice.

# 3. Mix of green compliance and non-compliance ----


# Create new var g_article_version2 to change into published vs accepted
merged_pvga$g_article_version2[merged_pvga$g_article_version == "submitted, accepted" | merged_pvga$g_article_version == "accepted"] <- "accepted_any"
merged_pvga$g_article_version2[merged_pvga$g_article_version == "accepted, published" | merged_pvga$g_article_version == "submitted, accepted, published" | merged_pvga$g_article_version == "submitted, published" | merged_pvga$g_article_version == "published"] <- "published_any"
merged_pvga$g_article_version2 <- factor(merged_pvga$g_article_version2, ordered = TRUE, levels = c("published_any", "accepted_any"))


#a. Green Compliance----

# Simple breakdown of green compliance
simple_green <- merged_pvga %>% 
  filter(journal_type != "Pure Gold") %>%
  group_by(g_license3, g_embargo3) %>% 
  count() %>%
  ungroup() %>%
  mutate(percent = round(n / sum(n) * 100,1))

openxlsx::write.xlsx(as.data.frame(simple_green), 'Output/Tables/simple_green_breakdown.xlsx')
  
# Table to show mix of compliance for green Oa
green_breakdown_a <- merged_pvga %>%
  filter(journal_type != "Pure Gold", !is.na(g_license2), !is.na(g_embargo2), !is.na(g_compliant_repository), !is.na(g_article_version)) %>%
  count(g_license3, g_embargo3, g_compliant_repository, g_article_version) %>%
  mutate(percent = round(n / sum(n) * 100,1)) %>%
  adorn_totals("row")

green_breakdown_j <- merged_pvga %>%
  filter(journal_type != "Pure Gold", !is.na(g_license2), !is.na(g_embargo2), !is.na(g_compliant_repository), !is.na(g_article_version)) %>%
  filter(!duplicated(Source.title)) %>%
  count(g_license3, g_embargo3, g_compliant_repository, g_article_version) %>%
  mutate(percent = round(n / sum(n) * 100,1)) %>%
  adorn_totals("row")

green_breakdown <- bind_cols(green_breakdown_a, green_breakdown_j) %>%
  rename(article_n = n...5, article_percent = percent...6, journal_n = n...11, journal_percent = percent...12) %>%
  select(-7, -8, -9, -10)
rm(green_breakdown_a, green_breakdown_j)

openxlsx::write.xlsx(as.data.frame(green_breakdown), 'Output/Tables/green_breakdown.xlsx')

#b. green licenses----

compliant_green_licenses <- merged_pvga %>%
  filter(num_new_green %in% c(1,3), !is.na(g_license1), journal_type != "Pure Gold") %>%
  count(g_license1, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1)) %>%
  adorn_totals("row")

all_green_licenses_exc_pure <- merged_pvga %>%
  filter(!is.na(g_license1), journal_type != "Pure Gold") %>%
  count(g_license1, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1)) %>%
  adorn_totals("row")

all_green_licenses <- merged_pvga %>%
  filter(!is.na(g_license1)) %>%
  count(g_license1, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1)) %>%
  adorn_totals("row")

licenses <- bind_cols(compliant_green_licenses, all_green_licenses_exc_pure, all_green_licenses) %>%
  rename(License = "g_license1...1", compliant_n = n...2, "c%" = percent...3, all_green_no_pure_n = n...5, "agnp%" = percent...6, all_green_n = n...8, "ag%" = percent...9) %>%
  select(-4,-7) %>%
  subset(License != "NA - policy for published version in full OA journal" & License != "bespoke license")
rm(compliant_green_licenses, all_green_licenses_exc_pure, all_green_licenses)

openxlsx::write.xlsx(as.data.frame(licenses), 'Output/Tables/licenses.xlsx')

#c. green embargos----
compliant_green_embargos <- merged_pvga %>%
  filter(num_new_green %in% c(1,3), !is.na(g_embargo), journal_type != "Pure Gold") %>%
  count(g_embargo, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1)) %>%
  adorn_totals("row")

all_green_embargos_exc_pure <- merged_pvga %>%
  filter(!is.na(g_embargo), journal_type != "Pure Gold") %>%
  count(g_embargo, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1)) %>%
  adorn_totals("row")

all_green_embargos <- merged_pvga %>%
  filter(!is.na(g_embargo)) %>%
  count(g_embargo, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1)) %>%
  adorn_totals("row")

embargos <- bind_cols(compliant_green_embargos, all_green_embargos_exc_pure, all_green_embargos) %>% # NB embargo3 has to be a factor for this to work
  rename(Embargo = "g_embargo...1", compliant_n = n...2, "c%" = percent...3, all_green_no_pure_n = n...5, "agnp%" = percent...6, all_green_n = n...8, "ag%" = percent...9) %>%
  select(-4,-7)
rm(compliant_green_embargos, all_green_embargos_exc_pure, all_green_embargos)

openxlsx::write.xlsx(as.data.frame(embargos), 'embargos.xlsx')

all_green_embargos_exc_pure_disc <- merged_pvga %>%
  filter(!is.na(g_embargo), journal_type != "Pure Gold", g_embargo.units != "No embargo requirement") %>%
  group_by(discipline) %>%
  count(g_embargo, .drop = FALSE) %>%
  arrange(discipline, desc(n)) %>%
  mutate(percent = round(n / sum(n) * 100,1)) %>%
  adorn_totals("row")

all_green_embargos_exc_pure_disc <- merged_pvga %>%
  filter(grepl("Arts|Social", discipline) & !is.na(g_embargo), journal_type != "Pure Gold") %>%
  group_by(discipline) %>%
  count(g_embargo, .drop = FALSE) %>%
  arrange(discipline, desc(n)) %>%
  mutate(percent = round(n / sum(n) * 100,1)) %>%
  adorn_totals("row")

#d. Unconfirmed Green OA----
unconfirmed_green_oa_publisher <- merged_pvga %>% filter(compliance_new == "nc: unconfirmed green oa") %>% count(publisher) %>% mutate(percent=round(n/sum(n)*100,1))%>% arrange(desc(n)) %>% mutate(cml = cumsum(percent))

openxlsx::write.xlsx(as.data.frame(unconfirmed_green_oa_publisher), 'Output/Tables/unconfirmed_green_oa_publisher.xlsx')

#XXXXXXXX
# IMPACT OF DIFFERENT POLICY SCENARIOS----

# Create stacked bar chart comparing actual OA and all three potential compliance scenarios (current, with HG, without HG)

    # Current OA status
Open.Access_a <- merged_pvga %>% # article level
  count(Open.Access2) %>%
  mutate(percent = round(n / sum(n) * 100,1), cml = round(cumsum(percent)))

Open.Access_j <- merged_pvga %>% # journal level
  filter(!duplicated(Source.title)) %>%
  count(Open.Access2) %>%
  mutate(percent = round(n / sum(n) * 100,1), cml = round(cumsum(percent)))

    # Potential compliance with current policy
compliance_current_a <- merged_pvga %>%
  count(compliance_current, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1), cml = round(cumsum(percent)))

compliance_current_j <- merged_pvga %>%
  filter(!duplicated(Source.title)) %>%
  count(compliance_current, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1), cml = round(cumsum(percent)))
         

    # Scenario 1
compliance_h_a <- merged_pvga %>%
  count(compliance_new_hybrid, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1), cml = round(cumsum(percent)))

compliance_h_j <- merged_pvga %>%
  filter(!duplicated(Source.title)) %>%
  count(compliance_new_hybrid, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1), cml = round(cumsum(percent)))

    # Scenario 2
compliance_new_a <- merged_pvga %>%
  count(compliance_new, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1), cml = round(cumsum(percent)))

compliance_new_j <- merged_pvga %>%
  filter(!duplicated(Source.title)) %>%
  count(compliance_new, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1), cml = round(cumsum(percent)))

# Scenario 3

compliance_new_pure_a <- merged_pvga %>%
  count(compliance_new_pure, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1), cml = round(cumsum(percent)))

compliance_new_pure_j <- merged_pvga %>%
  filter(!duplicated(Source.title)) %>%
  count(compliance_new_pure, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1), cml = round(cumsum(percent)))

      # Bind together figures for articles
policy_impact_a <- bind_rows(Open.Access_a, compliance_current_a, compliance_h_a, compliance_new_a, compliance_new_pure_a) %>%
  mutate(Scenario = "Current\n(actual)") %>%
  unite(Compliance_Route, c('Open.Access2', 'compliance_current', 'compliance_new_hybrid', 'compliance_new', 'compliance_new_pure'), na.rm = TRUE) %>%
  relocate(5,1,2,3,4)

policy_impact_a[5:9,'Scenario'] <- "Current\n(potential)"
policy_impact_a[10:14,'Scenario'] <- "S1"
policy_impact_a[15:19,'Scenario'] <- "S2"
policy_impact_a[20:23,'Scenario'] <- "S3"
policy_impact_a[c(1,5,10,15,20), 'Compliance_Route'] <- "Full gold OA"
policy_impact_a[c(2,6,11,16), 'Compliance_Route'] <- "Hybrid gold\n(with TA for S2)"
policy_impact_a[c(3,7,12,17,21), 'Compliance_Route'] <- "Confirmed green OA"
policy_impact_a[c(8,13,18,22), 'Compliance_Route'] <- "Unconfirmed green OA"
policy_impact_a[c(4,9,14,19,23), 'Compliance_Route'] <- "No potential supported\nroute to open access"

policy_impact_a$Compliance_Route <- factor(policy_impact_a$Compliance_Route, ordered = TRUE, levels = c("Full gold OA", "Hybrid gold\n(with TA for S2)", "Confirmed green OA", "Unconfirmed green OA", "No potential supported\nroute to open access"))

openxlsx::write.xlsx(as.data.frame(policy_impact_a), 'Output/Tables/Impact of each policy scenario (articles).xlsx')

      # Create stacked bar chart for articles

policy_impact_a_bar <- ggplot(policy_impact_a, aes(fill=Compliance_Route, y=percent, x=Scenario)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("#F08900", "#FBBB10", "#16978A", "#99d8c9", "#FF5A5A"), name = "Route to Open Access") +
  # ggtitle("Impact of each policy scenario (articles)") +
  theme(axis.title.x = element_text(face="bold", size=14, margin = margin(t=10)),
        axis.text.x  = element_text(vjust=0.5, size=14),
        title =  element_text(face="bold", size=16),
        legend.text = element_text(size=14)) +
  labs(x="Policy Scenarios", y= "% of articles")

ggsave(policy_impact_a_bar, filename = "Output/Charts/Impact of each policy scenario (articles).png")

      # Subset percentages only and pivot to create comparison table for articles
policy_impact_a_summary <- policy_impact_a %>%
  select(-c(n, cml)) %>%
  pivot_wider(names_from = Scenario, values_from = percent) %>%
  arrange(factor(Compliance_Route, levels = c("Full gold OA", "Hybrid gold\n(with TA for S2)", "Confirmed green OA", "Unconfirmed green OA", "No potential supported\nroute to open access")))

policy_impact_a_summary[is.na(policy_impact_a_summary)] <- 0

openxlsx::write.xlsx(as.data.frame(policy_impact_a_summary), 'Output/Tables/policy_impact_a_summary.xlsx')

    # Bind together figures for journals
policy_impact_j <- bind_rows(Open.Access_j, compliance_current_j, compliance_h_j, compliance_new_j, compliance_new_pure_j) %>%
  mutate(Scenario = "Current\n(actual)") %>%
  unite(Compliance_Route, c('Open.Access2', 'compliance_current', 'compliance_new_hybrid', 'compliance_new', 'compliance_new_pure'), na.rm = TRUE) %>%
  relocate(5,1,2,3,4)

policy_impact_j[5:9,'Scenario'] <- "Current\n(potential)"
policy_impact_j[10:14,'Scenario'] <- "S1"
policy_impact_j[15:19,'Scenario'] <- "S2"
policy_impact_j[20:23,'Scenario'] <- "S3"
policy_impact_j[c(1,5,10,15,20), 'Compliance_Route'] <- "Full gold OA"
policy_impact_j[c(2,6,11,16), 'Compliance_Route'] <- "Hybrid gold\n(with TA for S2)"
policy_impact_j[c(3,7,12,17,21), 'Compliance_Route'] <- "Confirmed green OA"
policy_impact_j[c(8,13,18,22), 'Compliance_Route'] <- "Unconfirmed green OA"
policy_impact_j[c(4,9,14,19,23), 'Compliance_Route'] <- "No potential supported\nroute to open access"

policy_impact_j$Compliance_Route <- factor(policy_impact_j$Compliance_Route, ordered = TRUE, levels = c("Full gold OA", "Hybrid gold\n(with TA for S2)", "Confirmed green OA", "Unconfirmed green OA", "No potential supported\nroute to open access"))

openxlsx::write.xlsx(as.data.frame(policy_impact_j), 'Output/Tables/Impact of each policy scenario (journals).xlsx')

# Create stacked bar chart for journals

policy_impact_j_bar <- ggplot(policy_impact_j, aes(fill=Compliance_Route, y=percent, x=Scenario)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("#F08900", "#FBBB10", "#16978A", "#99d8c9", "#FF5A5A"), name = "Route to open access") +
  # ggtitle("Impact of each policy scenario (journals)") +
  theme(axis.title.x = element_text(face="bold", size=14, margin = margin(t=10)),
        axis.text.x  = element_text(vjust=0.5, size=14),
        title =  element_text(face="bold", size=16),
        legend.text = element_text(size=14)) +
  labs(x="Policy Scenarios", y= "% of journals")

ggsave(policy_impact_j_bar, filename = "Output/Charts/Impact of each policy scenario (journals).png")

# Subset percentages only and pivot to create comparison table for journals
policy_impact_j_summary <- policy_impact_j %>%
  select(-c(n, cml)) %>%
  pivot_wider(names_from = Scenario, values_from = percent) %>%
  arrange(factor(Compliance_Route, levels = c("Full gold OA", "Hybrid gold\n(with TA for S2)", "Confirmed green OA", "Unconfirmed green OA", "No potential supported\nroute to open access")))

policy_impact_j_summary[is.na(policy_impact_j_summary)] <- 0

openxlsx::write.xlsx(as.data.frame(policy_impact_j_summary), 'Output/Tables/policy_impact_j_summary.xlsx')

