# PUBLICATION VENUE GAP ANALYSIS IN REPORT ORDER

# Author: Tom Kenny
# Created: August 2020
# Last Updated: November 2020

# Purpose of code: This code produces tables and charts for the publication venue gap analysis - it forms the basis of an early findings presentation.

# Prerequisite: this relies on the dataset merged_pvga produced with Tom Kenny's code 'Preparing and Merging PVGA data' which in turn is dependent on data from Dimensions downloaded using google sheets API and from Sherpa (downloaded using code SHERPA RoMEO API and cleaning) See (C:\Users\TKen02\UKRI\Policy Analysis - Documents\Open Access\Projects\Publication Venue Gap Analysis\Data\).

# NB. Just before merged_pvga was created (as imported below), the observations were randomised with set.seed(42). This is necessary because of a number of analyses which used !duplicated(journal_title) to focus on analysis of journals.Other random orders were tested and they make only small changes to some of the n columns (not making major changes to percentages). This approach is a simplification but seems a strong proxy.

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


# Code which will allow for reproducible random reordering of rows (e.g. when getting rid of duplicates)
set.seed(42)
rows <- sample(nrow(merged_pvga))
merged_pvga <- merged_pvga[rows, ]

# Not yet run in other code



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

open_access_categories_a <- merged_pvga %>% # article level
  count(open_access_categories2) %>%
  mutate(percent = round(n / sum(n) * 100),1) %>%
  adorn_totals("row")

open_access_categories_j <- merged_pvga %>% # journal level
  filter(!duplicated(journal_title)) %>%
  count(open_access_categories2) %>%
  mutate(percent = round(n / sum(n) * 100),1) %>%
  adorn_totals("row")

    # basic statistics of number of articles per journal within each oac (mean, median, sd, max)

sd_apj_pure <- psych::describe(merged_pvga$articles_in_journal[!duplicated(merged_pvga$journal_title) & merged_pvga$open_access_categories2 == "Pure gold"])
sd_apj_hybrid <- psych::describe(merged_pvga$articles_in_journal[!duplicated(merged_pvga$journal_title) & merged_pvga$open_access_categories2 == "Hybrid gold"])
sd_apj_green <- psych::describe(merged_pvga$articles_in_journal[!duplicated(merged_pvga$journal_title) & merged_pvga$open_access_categories2 == "Green"])
sd_apj_closed <- psych::describe(merged_pvga$articles_in_journal[!duplicated(merged_pvga$journal_title) & merged_pvga$open_access_categories2 == "Closed"])
sd_apj_total <- psych::describe(merged_pvga$articles_in_journal[!duplicated(merged_pvga$journal_title)])
sd_apj <- bind_rows(sd_apj_pure, sd_apj_hybrid, sd_apj_green, sd_apj_closed, sd_apj_total)
rm(sd_apj_pure, sd_apj_hybrid, sd_apj_green, sd_apj_closed, sd_apj_total)

  # bind together for open access categories table in report
open_access_categories <- bind_cols(open_access_categories_a, open_access_categories_j, sd_apj) %>%
  rename("Open access categories" = open_access_categories2...1, n_articles = n...2, n_journals = n...6, standard_deviation = sd) %>%
  mutate(mean = round(mean,0), standard_deviation = round(standard_deviation,0)) %>%
  select(-c(4, 5, 8, 9, 10, 14, 15, 16, 18, 19, 20, 21))
rm(open_access_categories_a, open_access_categories_j, sd_apj)

openxlsx::write.xlsx(as.data.frame(open_access_categories), 'Output/Tables/open_access_categories.xlsx')

  # open access categories bar chart (not currently in report)
open_access_categories_bar <- merged_pvga %>%
  count(open_access_categories2) %>%
  mutate(percent = round((n / sum(n) * 100)),0) %>%
  ggplot(aes(x = open_access_categories2, y = n, fill = open_access_categories2)) +
  geom_col(fill = c("#F08900","#FBBB10", "#16978A","#FF5A5A")) +
  ggtitle("Dimensions' Open Access Categories") +
  labs(x="Open Access Categories", y= "Number of articles")

ggsave(open_access_categories_bar, filename = "Output/Charts/open_access_categories_bar.png")


# 2. Top publishers and oac----
top_publishers_oac <- merged_pvga %>%
  group_by(publisher) %>%
  count(open_access_categories2) %>%
  pivot_wider(names_from = open_access_categories2, values_from = n) %>%
  adorn_totals("col") %>%
  rename(pure_gold = "Pure gold", hybrid_gold = "Hybrid gold") %>%
  mutate(pure_percent = round(pure_gold/Total*100,1), hybrid_gold_percent = round(hybrid_gold/Total*100,1), green_percent = round(Green/Total*100,1), closed_percent = round(Closed/Total*100,1)) %>%
  select(-c(2,3,4,5)) %>%
    rename(Total_n_articles = Total) %>%
  mutate(percent_of_total_articles = round(Total_n_articles/sum(Total_n_articles)*100,1)) %>%
  arrange(desc(Total_n_articles)) %>%
  mutate(cml = round(cumsum(percent_of_total_articles),0)) %>%
  relocate(1,2,7,8,3,4,5,6)

openxlsx::write.xlsx(as.data.frame(top_publishers_oac), 'Output/Tables/top_publishers_oac.xlsx')


#3. Dimensions Open Access Categories x Discipline ----

disciplines <- merged_pvga %>% 
  separate(discipline, into = c("disc1", "disc2", "disc3", "disc4"), sep = ", ") %>%
  pivot_longer(c(disc1, disc2, disc3, disc4), names_to = "disc_all", values_to ="disc")

open_access_categories_a_disc <- disciplines %>%
  filter(!is.na(disc)) %>%
  group_by(disc) %>%
  count(open_access_categories2, .drop = FALSE) %>%
  mutate(percent = (n / sum(n)) * 100) %>%
  mutate(cml = round(cumsum(percent),0)) %>%
  mutate(percent = round(percent, 0)) %>%
  mutate(disc = factor(disc, ordered = TRUE, levels = c("Arts & Humanities", "Health Sciences", "Life Sciences", "Physical Sciences", "Social Sciences", "Missing")), 
        open_access_categories2 = factor(open_access_categories2, ordered = TRUE, levels = c("Pure gold", "Hybrid gold", "Green", "Closed"))) %>%
  arrange(disc, open_access_categories2)

openxlsx::write.xlsx(as.data.frame(open_access_categories_a_disc), 'Output/Tables/open_access_categories_a_disc.xlsx')

# Bar
open_access_categories_a_disc_bar <- open_access_categories_a_disc %>%
  ggplot(aes(x=disc, y=percent)) +
  geom_bar(aes(fill=open_access_categories2), stat = "identity", position = "stack", width = 0.9) +
  scale_fill_manual(values = c("#F08900","#FBBB10", "#16978A","#FF5A5A")) +
  ggtitle("Open Access Categories by discipline") +
  theme(axis.title.x = element_text(face="bold", size=14, margin = margin(t=10)),
        axis.text.x  = element_text(vjust=0.5, size=12),
        title =  element_text(face="bold", size=16),
        legend.text = element_text(size=14)) +
  labs(x="Disciplines", y= "% of all articles in panel", fill = "Open access") +
  scale_x_discrete(labels = c("Arts &\nHumanities", "Health\nSciences","Life\nSciences", "Physical\nSciences", "Social\nSciences", "Missing"))

ggsave(open_access_categories_a_disc_bar, filename = "Output/Charts/open_access_categories_a_disc_bar.png")


# 3. Dimensions open acccess categories by Fields of Research ----
# To run this analysis we first need to create tables for Fields of Research Divisions and Groups

  # First Divisions
divisions <- merged_pvga %>% 
  separate(for_division, into = c("division1", "division2", "division3", "division4"), sep = ", ") %>%
  pivot_longer(c(division1, division2, division3, division4), names_to = "division_all", values_to ="division")

summary_divisions <- divisions %>%
  filter(!is.na(division)) %>% 
  group_by(division) %>% summarize(n=n()) %>%
  mutate(percent = round(n/(nrow(merged_pvga))*100,1))

openxlsx::write.xlsx(as.data.frame(summary_for_divisions), 'Output/Tables/summary_for_divisions.xlsx')

open_access_categories_for_division <- divisions %>%
  group_by(division) %>%
  count(open_access_categories2, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1))
openxlsx::write.xlsx(as.data.frame(open_access_categories_for_division), 'Output/Tables/open_access_categories_for_division.xlsx')
  
  # Then Groups
groups <- merged_pvga %>% 
  separate(for_group, into = c("group1", "group2", "group3", "group4"), sep = ", ") %>%
  pivot_longer(c(group1, group2, group3, group4), names_to = "group_all", values_to ="group")

summary_groups <- groups %>%
  filter(!is.na(group)) %>% 
  group_by(group) %>% summarize(n=n()) %>%
  mutate(percent = round(n/(nrow(merged_pvga))*100,1))

openxlsx::write.xlsx(as.data.frame(summary_for_groups), 'Output/Tables/summary_for_groups.xlsx')

open_access_categories_for_group <- groups %>%
  group_by(group) %>%
  count(open_access_categories2, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1))
openxlsx::write.xlsx(as.data.frame(open_access_categories_for_group), 'Output/Tables/open_access_categories_for_group.xlsx')

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
  filter(!duplicated(journal_title)) %>%
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
  separate(discipline, into = c("disc1", "disc2", "disc3", "disc4"), sep = ", ") %>%
  pivot_longer(c(disc1, disc2, disc3, disc4), names_to = "disc_all", values_to ="disc")

journal_type_a_disc <- disciplines %>%
  filter(!is.na(disc)) %>%
  group_by(disc) %>%
  count(journal_type, .drop = FALSE) %>%
  mutate(percent = (n / sum(n)) * 100) %>%
  mutate(cml = round(cumsum(percent),0)) %>%
  mutate(percent = round(percent, 0))

journal_type_j_disc <- disciplines %>%
  filter(!is.na(disc), !duplicated(journal_title)) %>%
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
  mutate(journal_type = factor(journal_type, ordered = TRUE, levels = c("Pure gold", "Hybrid", "Closed or insufficient information")),
         disc = factor(disc, ordered = TRUE, levels = c("Arts & Humanities", "Health Sciences", "Life Sciences", "Physical Sciences", "Social Sciences", "Missing"))) %>%
  ggplot(aes(x=disc, y=percent)) +
  geom_bar(aes(fill=journal_type), stat = "identity", position = "stack", width = 0.9) +
  scale_fill_manual(values = c("#F08900","#FBBB10", "#FF5A5A"), labels = c("Pure gold\n", "Hybrid\n", "Closed or insufficient\ninformation")) +
  ggtitle("Journal Type by Discipline") +
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
TA_journals <- merged_pvga %>%  filter(!duplicated(journal_title), journal_type == "Hybrid") %>% count(has_ta) %>% mutate(percent = round(n / sum(n) * 100, 1)) %>%  adorn_totals("row")
TA <- bind_cols(TA_articles, TA_journals) %>% rename(number_articles = n...2, number_journals = n...5) %>% select(-4)

openxlsx::write.xlsx(as.data.frame(TA), 'Output/Tables/transformative_agreements.xlsx')


# Transformative agreements by Discipline

disciplines <- merged_pvga %>% 
  separate(discipline, into = c("disc1", "disc2", "disc3", "disc4"), sep = ", ") %>%
  pivot_longer(c(disc1, disc2, disc3, disc4), names_to = "disc_all", values_to ="disc")

TA_a_disc <- disciplines %>%
  filter(!is.na(disc), journal_type == "Hybrid") %>%
  group_by(disc) %>%
  count(has_ta, .drop = FALSE) %>%
  mutate(percent = round(n/sum(n)*100),1) %>%
  adorn_totals("row")

TA_j_disc <- disciplines %>%
  filter(!is.na(disc), !duplicated(journal_title), journal_type == "Hybrid") %>%
  group_by(disc) %>%
  count(has_ta, .drop = FALSE) %>%
  mutate(percent = round(n/sum(n)*100),1) %>%
  adorn_totals("row")

TA_disc <- bind_cols(TA_a_disc, TA_j_disc) %>%
  select(-c(5,6,7,10)) %>%
  rename(n_articles = n...3, n_journals = n...8)
rm(TA_j_disc, TA_a_disc)

openxlsx::write.xlsx(as.data.frame(TA_disc), 'Output/Tables/TA_disc.xlsx')

# Bar chart for breakdown of articles in hybrid journals with TA and green
impact_of_ta <- merged_pvga %>%
  filter(journal_type == "Hybrid")

impact_of_ta$impact_of_ta <- if_else(impact_of_ta$has_ta == "yes", "Hybrid gold in journal covered by TA", 
                              if_else(impact_of_ta$num_new_green %in% c(1,3), "No TA but certainly has compliant green route",
                                if_else((impact_of_ta$g_license1 == "no license requirement" & impact_of_ta$g_embargo == 0), "No TA and unconfirmed green route", "No TA or compliant green route")))

impact_of_ta$impact_of_ta[is.na(impact_of_ta$impact_of_ta)] <-  "No TA or compliant green route"

impact_of_ta <- impact_of_ta %>%
  count(impact_of_ta) %>%
  mutate(hybrid_articles="Hybrid articles", percent_of_hybrid = round(n/nrow(merged_pvga[merged_pvga$journal_type == "Hybrid", ])*100,1)) %>%
  mutate(impact_of_ta = factor(impact_of_ta, ordered = TRUE, levels = c("Hybrid gold in journal covered by TA", "No TA but certainly has compliant green route", "No TA and unconfirmed green route", "No TA or compliant green route"))) %>%
  arrange(impact_of_ta)

openxlsx::write.xlsx(as.data.frame(impact_of_ta), 'Output/Tables/impact_of_ta.xlsx')

impact_of_ta_chart <- ggplot(impact_of_ta, aes(x = hybrid_articles, y = percent_of_hybrid, fill=impact_of_ta)) +
         geom_bar(position = "stack", stat = "identity") +
         scale_fill_manual(values = c("#FBBB10", "#16978A", "#99d8c9", "#FF5A5A"), name = "Impact of TA") +
         ggtitle("Compliance of articles in hybrid \n journals if funding restricted \n to journals covered by a TA") +
  theme(axis.title.x = element_text(face="bold", size=14, margin = margin(t=10)),
        axis.text.x  = element_text(vjust=0.5, size=14),
        title =  element_text(face="bold", size=15),
        legend.text = element_text(size=14)) +
  labs(x="Articles in Hybrid journals", y= "% of articles in hybrid journals")

ggsave(impact_of_ta_chart, filename = "Output/Charts/impact_of_ta_chart.png")

# Actual open access status of articles with and without a TA
TA_oac <- merged_pvga %>% filter(journal_type =="Hybrid", has_ta == "yes") %>% count(open_access_categories2) %>% mutate(percent = n/sum(n)*100)
NoTA_oac <- merged_pvga %>% filter(journal_type =="Hybrid", has_ta == "no") %>% count(open_access_categories2) %>% mutate(percent = n/sum(n)*100)
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
  filter(!duplicated(journal_title)) %>% # NB code for creating merged data pseudo-randomly orders observations using set.seed(42), but any random order should work with only small variations in the n column.
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
  separate(discipline, into = c("disc1", "disc2", "disc3", "disc4"), sep = ", ") %>%
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
  filter(!is.na(disc), !duplicated(journal_title)) %>%
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
rm(compliance_current_disc_a, compliance_current_disc_j)

openxlsx::write.xlsx(as.data.frame(compliance_current_disc), 'Output/Tables/compliance_current_disc.xlsx')



#XXXXXXXXXXX
# POTENTIAL COMPLIANCE WITH NEW POLICY SCENARIO 1 SUPPORTING HYBRID GOLD----

# 1. Potential compliance with new policy (if hybrid gold allowed) ----
compliance_h_a <- merged_pvga %>%
  count(compliance_new_hybrid, .drop = FALSE) %>%
  mutate(percent = round((n / sum(n) * 100),1), cml = cumsum(percent)) %>%
  adorn_totals("row")

compliance_h_j <- merged_pvga %>%
  filter(!duplicated(journal_title)) %>%
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
  separate(discipline, into = c("disc1", "disc2", "disc3", "disc4"), sep = ", ") %>%
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
  filter(!is.na(disc), !duplicated(journal_title)) %>%
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
  filter(!duplicated(journal_title)) %>%
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
  separate(discipline, into = c("disc1", "disc2", "disc3", "disc4"), sep = ", ") %>%
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
  filter(!is.na(disc), !duplicated(journal_title)) %>%
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
  ggtitle("Compliance by Discipline in S2") +
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
  filter((compliance_new2 == "not compliant" | compliance_new == "c: confirmed green oa"), !is.na(publisher)) %>%
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
  separate(for_division, into = c("division1", "division2", "division3", "division4"), sep = ", ") %>%
  pivot_longer(c(division1, division2, division3, division4), names_to = "division_all", values_to ="division")

# Working out denominators (since we won't be able to do this after filtering the data)
summary_division <- divisions %>%
  filter(!is.na(division)) %>% 
  group_by(division) %>% summarize(n=n())
# openxlsx::write.xlsx(as.data.frame(summary_uoa), 'summary_uoa.xlsx')

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

  #THEN GROUP
groups <- merged_pvga %>% 
  separate(for_group, into = c("group1", "group2", "group3", "group4"), sep = ", ") %>%
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
  separate(for_division, into = c("division1", "division2", "division3", "division4"), sep = ", ") %>%
  pivot_longer(c(division1, division2, division3, division4), names_to = "division_all", values_to ="division")

compliance_new_division <- divisions %>%
  filter(!is.na(division)) %>%
  group_by(division) %>%
  count(compliance_new2) %>%
  mutate(percent = round(n/sum(n)*100,1))

openxlsx::write.xlsx(as.data.frame(compliance_new_division), 'Output/Tables/compliance_new_division.xlsx')

  # THEN GROUPS
groups <- merged_pvga %>% 
  separate(for_group, into = c("group1", "group2", "group3", "group4"), sep = ", ") %>%
  pivot_longer(c(group1, group2, group3, group4), names_to = "group_all", values_to ="group")

compliance_new_group <- groups %>%
  filter(!is.na(group)) %>%
  group_by(group) %>%
  count(compliance_new) %>%
  mutate(percent = round(n/sum(n)*100,1))

openxlsx::write.xlsx(as.data.frame(compliance_new_group), 'Output/Tables/compliance_new_group.xlsx')

# GREEN LICENSES AND EMBARGOS ----
  # first compliant only, then all except pure gold, then all, then bind them all together, rename columns, and remove unnecessary rows and columns. Pure gold needs to be excluded since most default pure gold policies are listed as not being associated with an additional_oa_fee in Sherpa, however, they are associated with a fee in practice.

# 3. Mix of green compliance and non-compliance ----


# Create new var g_article_version2 to change into published vs accepted
merged_pvga$g_article_version2[merged_pvga$g_article_version == "submitted, accepted" | merged_pvga$g_article_version == "accepted"] <- "accepted_any"
merged_pvga$g_article_version2[merged_pvga$g_article_version == "accepted, published" | merged_pvga$g_article_version == "submitted, accepted, published" | merged_pvga$g_article_version == "submitted, published" | merged_pvga$g_article_version == "published"] <- "published_any"
merged_pvga$g_article_version2 <- factor(merged_pvga$g_article_version2, ordered = TRUE, levels = c("published_any", "accepted_any"))


#a. Green Compliance----

# Table to show mix of compliance for green Oa
green_breakdown_a <- merged_pvga %>%
  filter(journal_type != "Pure gold", !is.na(g_license2), !is.na(g_embargo2), !is.na(g_compliant_repository), !is.na(g_article_version)) %>%
  count(g_license3, g_embargo3, g_compliant_repository, g_article_version2) %>%
  mutate(percent = round(n / sum(n) * 100,1)) %>%
  adorn_totals("row")

green_breakdown_j <- merged_pvga %>%
  filter(journal_type != "Pure gold", !is.na(g_license2), !is.na(g_embargo2), !is.na(g_compliant_repository), !is.na(g_article_version)) %>%
  filter(!duplicated(journal_title)) %>%
  count(g_license3, g_embargo3, g_compliant_repository, g_article_version2) %>%
  mutate(percent = round(n / sum(n) * 100,1)) %>%
  adorn_totals("row")

green_breakdown <- bind_cols(green_breakdown_a, green_breakdown_j) %>%
  rename(article_n = n...5, article_percent = percent...6, journal_n = n...11, journal_percent = percent...12) %>%
  select(-7, -8, -9, -10)
rm(green_breakdown_a, green_breakdown_j)

openxlsx::write.xlsx(as.data.frame(green_breakdown), 'Output/Tables/green_breakdown.xlsx')

#b. green licenses----

compliant_green_licenses <- merged_pvga %>%
  filter(num_new_green %in% c(1,3), !is.na(g_license1), journal_type != "Pure gold") %>%
  count(g_license1, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1)) %>%
  adorn_totals("row")

all_green_licenses_exc_pure <- merged_pvga %>%
  filter(!is.na(g_license1), journal_type != "Pure gold") %>%
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
  filter(num_new_green %in% c(1,3), !is.na(g_embargo), journal_type != "Pure gold") %>%
  count(g_embargo, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1)) %>%
  adorn_totals("row")

all_green_embargos_exc_pure <- merged_pvga %>%
  filter(!is.na(g_embargo), journal_type != "Pure gold") %>%
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

  # To figure out what proportion of the 0 embargos are explicitly zero embargo as opposed to no requirement. I couldn't figure out a clever way to edit the above table so some of the calculation needs to be done in excel
number_explicit_zero_embargo <- merged_pvga %>%
  filter(num_new_green %in% c(1,3), !is.na(g_embargo), journal_type != "Pure gold", g_embargo == 0, g_embargo.units != "No embargo requirement") %>%
  count(g_embargo2)
number_explicit_zero_embargo

embargos <- embargos %>%
  mutate(number_explicit_zero = number_explicit_zero_embargo$n)

openxlsx::write.xlsx(as.data.frame(embargos), 'embargos.xlsx')


#XXXXXXXX
# IMPACT OF DIFFERENT POLICY SCENARIOS----

# Create stacked bar chart comparing actual OA and all three potential compliance scenarios (current, with HG, without HG)

    # Current OA status
open_access_categories_a <- merged_pvga %>% # article level
  count(open_access_categories2) %>%
  mutate(percent = round(n / sum(n) * 100),1)

open_access_categories_j <- merged_pvga %>% # journal level
  filter(!duplicated(journal_title)) %>%
  count(open_access_categories2) %>%
  mutate(percent = round(n / sum(n) * 100),1)

    # Potential compliance with current policy
compliance_current_a <- merged_pvga %>%
  count(compliance_current, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1), cml = round(cumsum(percent)))

compliance_current_j <- merged_pvga %>%
  filter(!duplicated(journal_title)) %>%
  count(compliance_current, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1), cml = round(cumsum(percent)))
         

    # Scenario 1
compliance_h_a <- merged_pvga %>%
  count(compliance_new_hybrid, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1), cml = round(cumsum(percent)))

compliance_h_j <- merged_pvga %>%
  filter(!duplicated(journal_title)) %>%
  count(compliance_new_hybrid, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1), cml = round(cumsum(percent)))

    # Scenario 2
compliance_new_a <- merged_pvga %>%
  count(compliance_new, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1), cml = round(cumsum(percent)))

compliance_new_j <- merged_pvga %>%
  filter(!duplicated(journal_title)) %>%
  count(compliance_new, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1), cml = round(cumsum(percent)))


      # Bind together figures for articles
policy_impact_a <- bind_rows(compliance_current_a, compliance_h_a, compliance_new_a) %>%
  mutate(Scenario = "Current") %>%
  unite(Compliance_Route, c('compliance_current', 'compliance_new_hybrid', 'compliance_new'), na.rm = TRUE) %>%
  relocate(5,1,2,3,4)

policy_impact_a[6:10,'Scenario'] <- "S1"
policy_impact_a[11:15,'Scenario'] <- "S2"
policy_impact_a[c(1,6,11), 'Compliance_Route'] <- "Pure gold"
policy_impact_a[c(2,7,12), 'Compliance_Route'] <- "Hybrid gold (with TA for S2)"
policy_impact_a[c(3,8,13), 'Compliance_Route'] <- "Confirmed green OA"
policy_impact_a[c(4,9,14), 'Compliance_Route'] <- "Unconfirmed green OA"
policy_impact_a[c(5,10,15), 'Compliance_Route'] <- "Not compliant"

policy_impact_a$Compliance_Route <- factor(policy_impact_a$Compliance_Route, ordered = TRUE, levels = c("Pure gold", "Hybrid gold (with TA for S2)", "Confirmed green OA", "Unconfirmed green OA", "Not compliant"))

openxlsx::write.xlsx(as.data.frame(policy_impact_a), 'Output/Tables/policy_impact_a.xlsx')

      # Create stacked bar chart for articles

policy_impact_a_bar <- ggplot(policy_impact_a, aes(fill=Compliance_Route, y=percent, x=Scenario)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("#F08900", "#FBBB10", "#16978A", "#99d8c9", "#FF5A5A"), name = "Route to Compliance") +
  ggtitle("Impact of each policy scenario") +
  theme(axis.title.x = element_text(face="bold", size=14, margin = margin(t=10)),
        axis.text.x  = element_text(vjust=0.5, size=14),
        title =  element_text(face="bold", size=16),
        legend.text = element_text(size=14)) +
  labs(x="Policy Scenarios", y= "% of articles")

ggsave(policy_impact_a_bar, filename = "Output/Charts/policy_impact_a_bar.png")

    # Bind together figures for journals
policy_impact_j <- bind_rows(compliance_current_j, compliance_h_j, compliance_new_j) %>%
  mutate(Scenario = "Current") %>%
  unite(Compliance_Route, c('compliance_current', 'compliance_new_hybrid', 'compliance_new'), na.rm = TRUE) %>%
  relocate(5,1,2,3,4)

policy_impact_j[6:10,'Scenario'] <- "S1"
policy_impact_j[11:15,'Scenario'] <- "S2"
policy_impact_j[c(1,6,11), 'Compliance_Route'] <- "Pure gold"
policy_impact_j[c(2,7,12), 'Compliance_Route'] <- "Hybrid gold (with TA for S2)"
policy_impact_j[c(3,8,13), 'Compliance_Route'] <- "Confirmed green OA"
policy_impact_j[c(4,9,14), 'Compliance_Route'] <- "Unconfirmed green OA"
policy_impact_j[c(5,10,15), 'Compliance_Route'] <- "Not compliant"

policy_impact_j$Compliance_Route <- factor(policy_impact_a$Compliance_Route, ordered = TRUE, levels = c("Pure gold", "Hybrid gold (with TA for S2)", "Confirmed green OA", "Unconfirmed green OA", "Not compliant"))

openxlsx::write.xlsx(as.data.frame(policy_impact_j), 'Output/Tables/policy_impact_j.xlsx')

# Create stacked bar chart for journals

policy_impact_j_bar <- ggplot(policy_impact_j, aes(fill=Compliance_Route, y=percent, x=Scenario)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("#F08900", "#FBBB10", "#16978A", "#99d8c9", "#FF5A5A"), name = "Route to Compliance") +
  ggtitle("Impact of each policy scenario") +
  theme(axis.title.x = element_text(face="bold", size=14, margin = margin(t=10)),
        axis.text.x  = element_text(vjust=0.5, size=14),
        title =  element_text(face="bold", size=16),
        legend.text = element_text(size=14)) +
  labs(x="Policy Scenarios", y= "% of journals")

ggsave(policy_impact_j_bar, filename = "Output/Charts/policy_impact_j_bar.png")








#XXXXXXXX NOTHING FROM THIS POINT ON REQUIRES QA----
# ADDITIONAL ANALYSES NOT STARTED----

# Analysis by funding council
# Analysis by country of research instittue (e.g. filtering for UK-led)

# TESTING SHERPA RECORDS WITH NO REQUIREMENTS----

#1. Articles listed as hybrid in Dimensions with no paid OA option in Sherpa
test <- merged_pvga %>%
  filter(journal_type == "Hybrid" & is.na(fee_license1)) # I picked 5 journals at random - for the first two Sherpa was wrong, the third Sherpa had updated it to be correct since I last ran the API, the fourth and fifth (Oncotarget and Microbiology Resource Announcements) were actually Pure Gold journals which hadn't been picked up by Sherpa or Dimensions. So all five were indeed cc-by, but 2 were incorrectly classified by our methodology.



#XXXXXXXXXX
# EXPLORING GROUPS OF NON-COMPLIANT ARTICLES/ JOURNALS AND SUITABLE ALTERNATIVES----

# 1. Key subjects 

  # Chemistry
merged_pvga$chemistry[merged_pvga$category_uoa1 == "B08 Chemistry" | merged_pvga$ref_panel2 == "B08 Chemistry"] <- TRUE
table(merged_pvga$chemistry)

chemistry_publisher <- merged_pvga %>%
  filter(compliance_new2 == "not compliant", chemistry == TRUE, !is.na(publisher)) %>%
  count(publisher, sort = TRUE) %>%
  mutate(percent = round(n / sum(n) * 100, digits=2)) %>%
  adorn_totals("row")
chemistry_publisher
openxlsx::write.xlsx(as.data.frame(chemistry_publisher), 'chemistry_publisher.xlsx')


chemistry_journals <- merged_pvga %>%
  filter(compliance_new2 == "not compliant", chemistry == TRUE) %>%
  count(journal_title, publisher, sort = TRUE) %>%
  mutate(percent = round(n / sum(n) * 100, digits=2)) %>%
  adorn_totals("row")
chemistry_journals

# Architecture
merged_pvga$architecture[merged_pvga$category_uoa1 == "C13 Architecture, Built Environment and Planning" | merged_pvga$ref_panel2 == "C13 Architecture, Built Environment and Planning"] <- TRUE
table(merged_pvga$architecture)

architecture_publisher <- merged_pvga %>%
  filter(compliance_new2 == "not compliant", architecture == TRUE, !is.na(publisher)) %>%
  count(publisher, sort = TRUE) %>%
  mutate(percent = round(n / sum(n) * 100, digits=2)) %>%
  adorn_totals("row")
architecture_publisher
openxlsx::write.xlsx(as.data.frame(architecture_publisher), 'architecture_publisher.xlsx')


architecture_journals <- merged_pvga %>%
  filter(compliance_new2 == "not compliant", architecture == TRUE) %>%
  count(journal_title, publisher, sort = TRUE) %>%
  mutate(percent = round(n / sum(n) * 100, digits=2)) %>%
  mutate(cml = 100*cumsum(percent)/sum(percent)) %>%
  adorn_totals("row")
architecture_journals

# 2. Key journals

merged_pvga %>%
  filter(is.na(rank_green)) %>%
  count()









########## NOT IN REPORT DOES NOT NEED QA  ################
# GOLD OA IN HYBRID JOURNALS - DEFINED AS ASSOCIATED WITH ADDITIONAL OA FEE OR LISTED AS HYBRID IN DIMENSIONS AND NOT PURE GOLD

# 1. Overall breakdown of most permissive policies associated with a fee (excluding pure OA)
rank_fee_a <- merged_pvga %>%
  filter(journal_type == "Hybrid", !is.na(sherpa_id)) %>%
  count(rank_fee) %>%
  mutate(percent = round(n / sum(n) * 100, digits=2)) %>%
  adorn_totals("row")
rank_fee_a

rank_fee_j <- merged_pvga %>%
  filter(journal_type == "Hybrid", !duplicated(journal_title), !is.na(sherpa_id)) %>%
  count(rank_fee) %>%
  mutate(percent = round(n / sum(n) * 100, digits=2)) %>%
  adorn_totals("row")
rank_fee_j

rank_fee <- bind_cols(rank_fee_a, rank_fee_j)
openxlsx::write.xlsx(as.data.frame(rank_fee), 'rank_fee.xlsx')


# Test: is it reasonable to exclude articles with no Sherpa data - test results are that the distribution of open_access_categories isn't that different from the overall sample - more bronze, closed, and pure gold. Less hybrid and green (though not a huge difference)
sherpa_missing_test_a <- merged_pvga %>%
  filter(is.na(sherpa_id)) %>%
  count(open_access_categories2) %>%
  mutate(percent = (n / sum(n)) * 100) %>%
  mutate(cml = round(cumsum(percent),0)) %>%
  mutate(percent = round(percent, 0)) %>%
  adorn_totals("row")
sherpa_missing_test_a

sherpa_missing_test_j <- merged_pvga %>%
  filter(is.na(sherpa_id), !duplicated(journal_title)) %>%
  count(open_access_categories2) %>%
  mutate(percent = (n / sum(n)) * 100) %>%
  mutate(cml = round(cumsum(percent),0)) %>%
  mutate(percent = round(percent, 0)) %>%
  adorn_totals("row")
sherpa_missing_test_j

sherpa_missing_test_a2 <- merged_pvga %>%
  filter(!is.na(sherpa_id)) %>%
  count(open_access_categories2) %>%
  mutate(percent = (n / sum(n)) * 100) %>%
  mutate(cml = round(cumsum(percent),0)) %>%
  mutate(percent = round(percent, 0)) %>%
  adorn_totals("row")
sherpa_missing_test_a2

sherpa_missing_test_ref <- merged_pvga %>%
  filter(is.na(sherpa_id)) %>%
  count(ref_panel) %>%
  mutate(percent = (n / sum(n)) * 100) %>%
  mutate(cml = round(cumsum(percent),0)) %>%
  mutate(percent = round(percent, 0)) %>%
  adorn_totals("row")
sherpa_missing_test_ref

sherpa_missing_test <- bind_cols(open_access_categories, sherpa_missing_test_a, sherpa_missing_test_a2)
sherpa_missing_test2 <- bind_cols(sherpa_missing_test_a, sherpa_missing_test_j)
rm(sherpa_missing_test_a, sherpa_missing_test_j, sherpa_missing_test_a2)
openxlsx::write.xlsx(as.data.frame(sherpa_missing_test), 'sherpa_missing_test.xlsx')
openxlsx::write.xlsx(as.data.frame(sherpa_missing_test2), 'sherpa_missing_test2.xlsx')
openxlsx::write.xlsx(as.data.frame(sherpa_missing_test_ref), 'sherpa_missing_test_ref.xlsx')

# Looks like the main differences are that the journals missing sherpa data are much smaller on average (fewer UKRI articles per journal). In particular we're losing quite a lot of small pure gold journals (n articles = 852, n journals = 291), and quite a lot of small journals which are probably closed (articles = 2895, journals = 916).


# CLUSTER ANALYSIS
# Cluster analysis of Units of Assessment x open access categories
library(factoextra)

open_access_categories_uoa_grouped <- pivot_wider(open_access_categories_uoa, id_cols = uoa, names_from = open_access_categories2, values_from = percent)

# recode NA to zero as kmeans doesn't work with NA
open_access_categories_uoa_grouped[is.na(open_access_categories_uoa_grouped)] <- 0

# work out optimal number of clusters
factoextra::fviz_nbclust(open_access_categories_uoa_grouped[, c("Pure gold","Hybrid gold","Green","Closed")], kmeans, method = "gap_stat") # optimal number of clusters = 3

# Explore clusters

kmeans(open_access_categories_uoa_grouped[, c("Pure gold","Hybrid gold","Green","Closed")], 3) # So you have three clusters which are basically 
# 1. low pure gold (10%), low hybrid gold (17%), low green (14), high closed (60) - 
# 2. high pure gold (32%), high hybrid gold (30%), low green (13%), low closed (24%))
# 3. medium-low pure gold (13%), medium-high hybrid gold (27%), high green (19%), medium closed (40%)

# 4 dimensional cluster plot
dim_oa_cluster_graph <- kmeans(open_access_categories_uoa_grouped[, c("Pure gold","Hybrid gold","Green","Closed")], 3) %>%
  fviz_cluster(data = open_access_categories_uoa_grouped[, c("Pure gold","Hybrid gold","Green","Closed")], palette = ukri_pal, ggtheme = theme_minimal())


# Working out clusters of compliance 

library(factoextra)
# Pivot wider to get each UoA on one row
compliance_uoa2_grouped <- pivot_wider(compliance_uoa2, id_cols = uoa, names_from = compliance_new2, values_from = percent)

# work out optimal number of clusters
compliance_uoa2_grouped %>% ungroup() %>% select(-1, -4) %>%
  factoextra::fviz_nbclust(kmeans, method = "gap_stat") # optimal number of clusters = 4

# Explore clusters
uoa_compliance_clusters <- compliance_uoa2_grouped %>% ungroup() %>% select(-1, -4) %>%
  kmeans(4) # So you have four clusters which are basically 
# 1. High pure gold (32%), low green (21%), medium not compliant (30%) 
# 2. Medium pure gold (14%), medium green (46%), medium non compliatn (27%)
# 3. Low pure gold (11%), low green (26%), high non compliant (48%)
# 4. Low pure gold (9%), high green(64%), low non compliant (16%)

# 4 dimensional cluster plot
compliance_uoa2_grouped %>% select(-1) %>% kmeans(method = "gap_stat", 4) %>% fviz_cluster(data = compliance_uoa_grouped, palette = ukri_pal, ggtheme = theme_minimal())

kmeans(compliance_uoa2_grouped[, c(2,3, 5)], 4) %>%
  fviz_cluster(data = compliance_uoa2_grouped[, c(2,3,4)], palette = ukri_pal, ggtheme = theme_minimal())



# Tree map for Compliance by discipline 
compliance_new_disc_a <- disciplines %>%
  filter(disc != "Missing" & disc != "") %>% 
  group_by(disc) %>%
  count(compliance_new, .drop = FALSE)

compliance_new_disc_chart <- compliance_new_disc_a %>%
  group_by(disc, compliance_new) %>%
  head(16) %>%
  summarise_all(funs(sum))

library(treemap)

compliance_new_disc_tree_disca <- compliance_new_disc_chart %>%
  filter(disc == "A") %>%
  treemap(index="compliance_new",
          vSize="n",
          type="index",
          palette=c("#F08900", "#2E2D62", "#16978A", "#FF5A5A"),
          title="Panel A: Medicine & Life Sciences",
          fontsize.title=18,
          fontsize.labels = 16,
          algorithm="pivotSize",
          sortID = "compliance_new"
  )

compliance_new_disc_tree_discb <- compliance_new_disc_chart %>%
  filter(disc == "B") %>%
  treemap(index="compliance_new",
          vSize="n",
          type="index",
          palette=c("#F08900", "#2E2D62", "#16978A", "#FF5A5A"),
          title="Panel B: Physical Sciences, Engingeering & Maths",
          fontsize.title=18,
          fontsize.labels = 16,
          algorithm="pivotSize",
          sortID = "compliance_new"
  )

compliance_new_disc_tree_discc <- compliance_new_disc_chart %>%
  filter(disc == "C") %>%
  treemap(index="compliance_new",
          vSize="n",
          type="index",
          palette=c("#F08900", "#2E2D62", "#16978A", "#FF5A5A"),
          title="Panel C: Social Sciences",
          fontsize.title=18,
          fontsize.labels = 16,
          algorithm="pivotSize",
          sortID = "compliance_new"
  )

compliance_new_disc_tree_discd <- compliance_new_disc_chart %>%
  filter(disc == "D") %>%
  treemap(index="compliance_new",
          vSize="n",
          type="index",
          palette=c("#F08900", "#2E2D62", "#16978A", "#FF5A5A"),
          title="Panel D: Arts & Humanities",
          fontsize.title=18,
          fontsize.labels = 16,
          algorithm="pivotSize",
          sortID = "compliance_new"
  )

rm(compliance_new_disc_a, compliance_new_disc_j)
