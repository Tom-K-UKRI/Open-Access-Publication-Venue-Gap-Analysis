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

# Set working directory
mainDir <- "C:\\Users\\TKen02\\UKRI\\Policy Analysis - Documents\\Open Access\\Projects\\Publication Venue Gap Analysis\\Analysis" # assuming an updated version of the merged_pvga data is saved here
setwd(mainDir)

#library(readr)
library(tidyverse)
library(openxlsx)
library(janitor)
library(knitr)
library(ggplot2)
library(psych)


#XXXXXXXXX
# Import data
merged_pvga <- read.xlsx("merged_pvga.xlsx")

#####e. Final edits to merged_pvga (including creating factors)----
  # NB THIS IS ALSO IN THE OTHER SCRIPT BUT FOR SOME REASON THE CREATION OF FACTORS DOESN'T SURVIVE THE JOURNEY TO EXCEL AND BACK SO IT NEEDS TO BE RUN AGAIN
# Recode open_access_categories as factor
merged_pvga$open_access_categories <- factor(merged_pvga$open_access_categories, ordered = TRUE, levels = c("Pure gold", "Hybrid", "Green, published", "Green, accepted", "Bronze", "Green, submitted", "Closed"))
merged_pvga$open_access_categories2 <- factor(merged_pvga$open_access_categories2, ordered = TRUE, levels = c("Pure gold", "Hybrid gold", "Green", "Closed"))

# Recode g_embargo3 as a factor
merged_pvga$g_embargo <- factor(merged_pvga$g_embargo, ordered = TRUE, levels = c(0,3,6,12,18,24,36,48,NA))
merged_pvga$g_embargo2 <- factor(merged_pvga$g_embargo2, ordered = TRUE, levels = c(0,6,12,13))
merged_pvga$g_embargo3 <- factor(merged_pvga$g_embargo3, ordered = TRUE, levels = c("zero embargo", "has embargo"))

# Recode g_compliant_repository as a factor
merged_pvga$g_compliant_repository <- factor(merged_pvga$g_compliant_repository, ordered = TRUE, levels = c(TRUE, FALSE))

# Turn license vars into factor to order by permissiveness (just to make outputs clearer)
merged_pvga$g_license1 <- factor(merged_pvga$g_license1, ordered = TRUE, c("no license requirement", "cc_by", "cc_by_nd", "cc_by_sa", "cc_by_nc", "cc_by_nc_nd", "cc_by_nc_sa", "bespoke license", NA))
merged_pvga$g_license2 <- factor(merged_pvga$g_license2, ordered = TRUE, c("cc_by", "cc_by_nd", "cc_by_nc", "no compliant license", NA))

# Recode compliance vars as factors
merged_pvga$compliance_current <- factor(merged_pvga$compliance_current, ordered = TRUE, levels = c("c: pure gold", "c: hybrid gold", "c: green oa", "not compliant"))

merged_pvga$compliance_new_hybrid <- factor(merged_pvga$compliance_new_hybrid, ordered = TRUE, levels = c("c: pure gold", "c: hybrid gold", "c: green oa",  "not compliant"))

merged_pvga$compliance_new <- factor(merged_pvga$compliance_new, ordered = TRUE, levels = c("c: pure gold", "c: hybrid gold with a TA", "c: green oa", "not compliant"))

# Recode journal_type as factor
merged_pvga$journal_type <- factor(merged_pvga$journal_type, ordered = TRUE, levels = c("Pure gold", "Hybrid", "Closed or insufficient information"))

# Code which will allow for reproducible random reordering of rows (e.g. when getting rid of duplicates)
set.seed(42)
rows <- sample(nrow(merged_pvga))
merged_pvga <- merged_pvga[rows, ]

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
  mutate(percent = round(n / sum(n) * 100),0) %>%
  adorn_totals("row")

open_access_categories_j <- merged_pvga %>% # journal level
  filter(!duplicated(journal_title)) %>%
  count(open_access_categories2) %>%
  mutate(percent = round(n / sum(n) * 100),0) %>%
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

openxlsx::write.xlsx(as.data.frame(open_access_categories), 'open_access_categories.xlsx')

  # open access categories bar chart (not currently in report)
open_access_categories_bar <- merged_pvga %>%
  count(open_access_categories2) %>%
  mutate(percent = round((n / sum(n) * 100)),0) %>%
  ggplot(aes(x = open_access_categories2, y = n, fill = open_access_categories2)) +
  geom_col(fill = c("#F08900","#FBBB10", "#16978A","#FF5A5A")) +
  ggtitle("Dimensions' Open Access Categories") +
  labs(x="Open Access Categories", y= "Number of articles")

# 2. Top publishers and oac----
top_publishers_oac <- merged_pvga %>%
  group_by(publisher) %>%
  count(open_access_categories2) %>%
  pivot_wider(names_from = open_access_categories2, values_from = n) %>%
  adorn_totals("col") %>%
  rename(pure_gold = "Pure gold", hybrid_gold = "Hybrid gold") %>%
  mutate(pure_percent = round(pure_gold/Total*100,0), hybrid_gold_percent = round(hybrid_gold/Total*100,0), green_percent = round(Green/Total*100,0), closed_percent = round(Closed/Total*100,0)) %>%
  select(-c(2,3,4,5)) %>%
    rename(Total_n_articles = Total) %>%
  mutate(percent_of_total_articles = round(Total_n_articles/sum(Total_n_articles)*100,0)) %>%
  arrange(desc(Total_n_articles)) %>%
  mutate(cml = round(cumsum(percent_of_total_articles),0)) %>%
  relocate(1,2,7,8,3,4,5,6)

openxlsx::write.xlsx(as.data.frame(top_publishers_oac), 'top_publishers_oac.xlsx')


#3. Dimensions Open Access Categories x REF Panel ----

# Dimensions open_access_categories x ref panel (not possible to do at journal level). NB. This has to be done four times and bound because ref_panel1 only captures the first ref panel attributed to an article. This means it will underestimate how many articles are in each ref panel and this will have a particular affect on panels B,C, and D since the panels are ordered alphabetically.

ref_panels <- merged_pvga %>% pivot_longer(c(ref_panel, ref_panel2), names_to = "ref_all", values_to ="ref")

open_access_categories_a_ref <- ref_panels %>%
  filter(ref != "", ref != "Missing") %>%
  group_by(ref) %>%
  count(open_access_categories2, .drop = FALSE) %>%
  mutate(percent = (n / sum(n)) * 100) %>%
  mutate(cml = round(cumsum(percent),0)) %>%
  mutate(percent = round(percent, 0)) %>%
  adorn_totals("row")
openxlsx::write.xlsx(as.data.frame(open_access_categories_a_ref), 'open_access_categories_a_ref.xlsx')

# Bar
open_access_categories_a_ref$open_access_categories2 <- factor(open_access_categories_a_ref$open_access_categories2, ordered = TRUE, levels = c("Pure gold", "Hybrid gold", "Green", "Closed"))
open_access_categories_a_ref_bar <- open_access_categories_a_ref %>%
  filter(open_access_categories2 != "Total") %>%
  ggplot(aes(x=ref, y=percent)) +
  geom_bar(aes(fill=open_access_categories2), stat = "identity", position = position_dodge2()) +
  scale_fill_manual(values = c("#F08900","#FBBB10", "#16978A","#FF5A5A")) +
  ggtitle("Open Access Categories by REF Panel") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme(axis.title.x = element_text(face="bold", size=12, margin = margin(t=10)),
             axis.text.x  = element_text(vjust=0.5, size=10)) +
  labs(x="REF Panels", y= "% of all articles in panel", fill = "Open access") +
  scale_x_discrete(labels = c('A: M&LS', 'B: PS,E&M','C: SS', 'D: A&H'))

  
ggsave(open_access_categories_a_ref_bar, filename = "open_access_categories_a_ref_bar.png")


# 3. Dimensions open acccess categories by Unit of Assessment ----

  # First pivot longer to create one column with all the UoAs
units_of_assessment <- merged_pvga %>% pivot_longer(c(category_uoa1, category_uoa2), names_to = "uoa_all", values_to ="uoa")

summary_uoa <- units_of_assessment %>% # could probably just filter out missing
  filter(uoa != "Missing" & uoa != "") %>% 
  group_by(uoa) %>% summarize(n=n())
# openxlsx::write.xlsx(as.data.frame(summary_uoa), 'summary_uoa.xlsx')

#summary_uoa_not_compliant <- units_of_assessment %>% filter(compliant)

open_access_categories_uoa <- units_of_assessment %>%
  filter(uoa != "Missing" & uoa != "") %>% 
  group_by(uoa) %>%
  count(open_access_categories2, .drop = FALSE) %>%
  mutate(percent = (n / sum(n)) * 100) %>%
  mutate(cml = round(cumsum(percent),0)) %>%
  mutate(percent = round(percent, 0)) %>%
  select(-5)
openxlsx::write.xlsx(as.data.frame(open_access_categories_uoa), 'open_access_categories_uoa.xlsx')



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
openxlsx::write.xlsx(as.data.frame(journal_type), 'journal_type.xlsx')

# Journal type by ref panel

ref_panels <- merged_pvga %>% pivot_longer(c(ref_panel, ref_panel2), names_to = "ref_all", values_to ="ref")

journal_type_a_ref <- ref_panels %>%
  filter(ref != "", ref != "Missing") %>%
  group_by(ref) %>%
  count(journal_type, .drop = FALSE) %>%
  mutate(percent = (n / sum(n)) * 100) %>%
  mutate(cml = round(cumsum(percent),0)) %>%
  mutate(percent = round(percent, 0)) %>%
  adorn_totals("row")

journal_type_j_ref <- ref_panels %>%
  filter(ref != "", ref != "Missing", !duplicated(journal_title)) %>%
  group_by(ref) %>%
  count(journal_type, .drop = FALSE) %>%
  mutate(percent = (n / sum(n)) * 100) %>%
  mutate(cml = round(cumsum(percent),0)) %>%
  mutate(percent = round(percent, 0)) %>%
  adorn_totals("row")

journal_type_ref <- bind_cols(journal_type_a_ref, journal_type_j_ref) %>%
  select(-c(5,6,7,10)) %>%
  rename(n_articles = n...3, n_journals = n...8)

openxlsx::write.xlsx(as.data.frame(journal_type_ref), 'journal_type_ref.xlsx')

# Stacked bar chart
journal_type_a_ref$Journal_Type <- factor(journal_type_a_ref$Journal_Type, ordered = TRUE, levels = c("pure gold", "hybrid", "closed", "Total"))
journal_type_a_ref_bar <- journal_type_a_ref %>%
  filter(Journal_Type != "Total") %>%
  rename("A: M & LS" = "% A", "B: PS, E & M" = "% B", "C: SS" = "% C", "D: A & H" = "% D") %>%
  pivot_longer(c("A: M & LS", "B: PS, E & M", "C: SS", "D: A & H")) %>%
  ggplot(aes(x=name, y=value)) +
  geom_bar(aes(fill=Journal_Type), stat = "identity", position = position_dodge2()) +
  scale_fill_manual(values = c("#F08900","#FBBB10", "#FF5A5A")) +
  ggtitle("Journal Type by REF Panel") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme(axis.title.x = element_text(face="bold", size=12, margin = margin(t=10)),
        axis.text.x  = element_text(vjust=0.5, size=10)) +
  labs(x="REF Panels", y= "% of all articles in panel")
ggsave(journal_type_a_ref_bar, filename = "journal_type_a_ref_bar.png")


#XXXXXXXXXXXXXXXXX
# 5. Transformative agreements----

# What number and proportion of articles journals are covered by TAs
TA_articles <- merged_pvga %>% count(has_ta) %>% mutate(percent = round(n / sum(n) * 100, 1)) %>%  adorn_totals("row")
TA_journals <- merged_pvga %>% filter(!duplicated(journal_title)) %>% count(has_ta) %>% mutate(percent = round(n / sum(n) * 100, 1)) %>%  adorn_totals("row")
TA <- bind_cols(TA_articles, TA_journals) %>% rename(number_articles = n...2, number_journals = n...5) %>% select(-4)

openxlsx::write.xlsx(as.data.frame(TA), 'transformative_agreements.xlsx')

# Transformative agreements by REF panel

ref_panels <- merged_pvga %>% pivot_longer(c(ref_panel, ref_panel2), names_to = "ref_all", values_to ="ref")

TA_a_ref <- ref_panels %>%
  filter(ref != "", ref != "Missing") %>%
  group_by(ref) %>%
  count(has_ta, .drop = FALSE) %>%
  mutate(percent = round(n/sum(n)*100),0) %>%
  adorn_totals("row")

TA_j_ref <- ref_panels %>%
  filter(ref != "", ref != "Missing", !duplicated(journal_title)) %>%
  group_by(ref) %>%
  count(has_ta, .drop = FALSE) %>%
  mutate(percent = round(n/sum(n)*100),0) %>%
  adorn_totals("row")

TA_ref <- bind_cols(TA_a_ref, TA_j_ref) %>%
  select(-c(5,6,7,10)) %>%
  rename(n_articles = n...3, n_journals = n...8)
rm(TA_j_ref, TA_a_ref)

openxlsx::write.xlsx(as.data.frame(TA_ref), 'TA_ref.xlsx')

#XXXXXXXXXXX
# POTENTIAL COMPLIANCE WITH CURRENT POLICY----

# 1. # Potential compliance with old policy
compliance_current_a <- merged_pvga %>%
  count(compliance_current, .drop = FALSE) %>%
  mutate(percent = (n / sum(n) * 100)) %>%
  mutate(cml = round(cumsum(percent),0)) %>%
  mutate(percent = round(percent,0)) %>%
  adorn_totals("row")
compliance_current_a

compliance_current_j <- merged_pvga %>%
  filter(!duplicated(journal_title)) %>% # NB code for creating merged data pseudo-randomly orders observations using set.seed(42), but any random order should work with only small variations in the n column.
  count(compliance_current, .drop = FALSE) %>%
  mutate(percent = (n / sum(n) * 100)) %>%
  mutate(cml = round(cumsum(percent),0)) %>%
  mutate(percent = round(percent,0)) %>%
  adorn_totals("row")
compliance_current_j

compliance_current <- bind_cols(compliance_current_a, compliance_current_j) %>%
  select(-5)
rm(compliance_current_a, compliance_current_j)
openxlsx::write.xlsx(as.data.frame(compliance_current), 'compliance_current.xlsx')

# 2. Potential compliance with current policy by ref_panel

  # There are two ref columns and we need to know if each subject exists in either of them for each article.
ref_panels <- merged_pvga %>% pivot_longer(c(ref_panel, ref_panel2), names_to = "ref_all", values_to ="ref")

compliance_current_ref_a <- ref_panels %>%
  filter(ref != "Missing" & ref != "") %>% 
  group_by(ref) %>%
  count(compliance_current, .drop = FALSE) %>%
  pivot_wider(names_from = ref, values_from = n) %>%
  mutate(percent_a = round(A/sum(A)*100,0), percent_b = round(B/sum(B)*100,0), percent_c = round(C/sum(C)*100,0), percent_d = round(D/sum(D)*100,0)) %>%
  relocate(compliance_current, A, percent_a, B, percent_b, C, percent_c, D, percent_d)

compliance_current_ref_j <- ref_panels %>%
  filter(!duplicated(journal_title)) %>%
  filter(ref != "Missing" & ref != "") %>% 
  group_by(ref) %>%
  count(compliance_current, .drop = FALSE) %>%
  pivot_wider(names_from = ref, values_from = n) %>%
  mutate(percent_a = round(A/sum(A)*100,0), percent_b = round(B/sum(B)*100,0), percent_c = round(C/sum(C)*100,0), percent_d = round(D/sum(D)*100,0)) %>%
  relocate(compliance_current, A, percent_a, B, percent_b, C, percent_c, D, percent_d)

compliance_current_ref <- bind_cols(compliance_current_ref_a, compliance_current_ref_j) %>%
  rename(A_articles = A...2, B_articles = B...4, C_articles = C...6, D_articles = D...8, A_journals = A...11, B_journals = B...13, C_journals = C...15, D_journals= D...17) %>%
  adorn_totals("row") %>%
  relocate(1,2,3,11,12,4,5,13,14,6,7,15,16,8,9,17,18) %>%
  select(-'compliance_current...10')
rm(compliance_current_ref_a, compliance_current_ref_j)

openxlsx::write.xlsx(as.data.frame(compliance_current_ref), 'compliance_current_ref.xlsx')



#XXXXXXXXXXX
# POTENTIAL COMPLIANCE WITH NEW POLICY SCENARIO 1 SUPPORTING HYBRID GOLD----

# 1. Potential compliance with new policy (if hybrid gold allowed) ----
compliance_h_a <- merged_pvga %>%
  count(compliance_new_hybrid, .drop = FALSE) %>%
  mutate(percent = (n / sum(n) * 100)) %>%
  mutate(cml = round(cumsum(percent),0)) %>%
  mutate(percent = round(percent,0)) %>%
  adorn_totals("row")
compliance_h_a

compliance_h_j <- merged_pvga %>%
  filter(!duplicated(journal_title)) %>%
  count(compliance_new_hybrid, .drop = FALSE) %>%
  mutate(percent = (n / sum(n) * 100)) %>%
  mutate(cml = round(cumsum(percent),0)) %>%
  mutate(percent = round(percent,0)) %>%
  adorn_totals("row")
compliance_h_j

compliance_h <- bind_cols(compliance_h_a, compliance_h_j) %>%
  select(-5)
rm(compliance_h_a, compliance_h_j)
openxlsx::write.xlsx(as.data.frame(compliance_h), 'compliance_new_hybrid_permitted.xlsx')


# 2. Potential compliance with new policy (S1) by ref_panel

# There are two ref columns and we need to know if each subject exists in either of them for each article.
ref_panels <- merged_pvga %>% pivot_longer(c(ref_panel, ref_panel2), names_to = "ref_all", values_to ="ref")

compliance_new_hybrid_ref_a <- ref_panels %>%
  filter(ref != "Missing" & ref != "") %>% 
  group_by(ref) %>%
  count(compliance_new_hybrid, .drop = FALSE) %>%
  pivot_wider(names_from = ref, values_from = n) %>%
  mutate(percent_a = round(A/sum(A)*100,0), percent_b = round(B/sum(B)*100,0), percent_c = round(C/sum(C)*100,0), percent_d = round(D/sum(D)*100,0)) %>%
  relocate(compliance_new_hybrid, A, percent_a, B, percent_b, C, percent_c, D, percent_d)

compliance_new_hybrid_ref_j <- ref_panels %>%
  filter(!duplicated(journal_title)) %>%
  filter(ref != "Missing" & ref != "") %>% 
  group_by(ref) %>%
  count(compliance_new_hybrid, .drop = FALSE) %>%
  pivot_wider(names_from = ref, values_from = n) %>%
  mutate(percent_a = round(A/sum(A)*100,0), percent_b = round(B/sum(B)*100,0), percent_c = round(C/sum(C)*100,0), percent_d = round(D/sum(D)*100,0)) %>%
  relocate(compliance_new_hybrid, A, percent_a, B, percent_b, C, percent_c, D, percent_d)

compliance_new_hybrid_ref <- bind_cols(compliance_new_hybrid_ref_a, compliance_new_hybrid_ref_j)  %>%
  rename(A_articles = A...2, B_articles = B...4, C_articles = C...6, D_articles = D...8, A_journals = A...11, B_journals = B...13, C_journals = C...15, D_journals= D...17) %>%
  adorn_totals("row") %>%
  relocate(1,2,3,11,12,4,5,13,14,6,7,15,16,8,9,17,18) %>%
  select(-'compliance_new_hybrid...10')
rm(compliance_new_hybrid_ref_a, compliance_new_hybrid_ref_j)

openxlsx::write.xlsx(as.data.frame(compliance_new_hybrid_ref), 'compliance_new_hybrid_ref.xlsx')


#XXXXXXXXXXX
# POTENTIAL COMPLIANCE WITH NEW POLICY SCENARIO 2 (NO SUPPORT FOR HYBRID GOLD OTHER THAN THROUGH TA)----

# 1. Full break down of potential compliance with new policy (no Hybrid gold allowed) ----
compliance_new_a <- merged_pvga %>%
  count(compliance_new, .drop = FALSE) %>%
  mutate(percent = (n / sum(n) * 100)) %>%
  mutate(cml = round(cumsum(percent),0)) %>%
  mutate(percent = round(percent,0)) %>%
  adorn_totals("row")
compliance_new_a

compliance_new_j <- merged_pvga %>%
  filter(!duplicated(journal_title)) %>%
  count(compliance_new, .drop = FALSE) %>%
  mutate(percent = (n / sum(n) * 100)) %>%
  mutate(cml = round(cumsum(percent),0)) %>%
  mutate(percent = round(percent,0)) %>%
  adorn_totals("row")
compliance_new_j

compliance_new <- bind_cols(compliance_new_a, compliance_new_j) %>%
  select(-5)
rm(compliance_new_a, compliance_new_j)
openxlsx::write.xlsx(as.data.frame(compliance_new), 'compliance_new.xlsx')


# 2. Compliance with new policy S2 by REF panel ----

ref_panels <- merged_pvga %>% pivot_longer(c(ref_panel, ref_panel2), names_to = "ref_all", values_to ="ref")

compliance_new_ref_a <- ref_panels %>%
  filter(ref != "Missing" & ref != "") %>% 
  group_by(ref) %>%
  count(compliance_new, .drop = FALSE) %>%
  pivot_wider(names_from = ref, values_from = n) %>%
  mutate(percent_a = round(A/sum(A)*100,0), percent_b = round(B/sum(B)*100,0), percent_c = round(C/sum(C)*100,0), percent_d = round(D/sum(D)*100,0)) %>%
  relocate(compliance_new, A, percent_a, B, percent_b, C, percent_c, D, percent_d)

compliance_new_ref_j <- ref_panels %>%
  filter(!duplicated(journal_title)) %>%
  filter(ref != "Missing" & ref != "") %>% 
  group_by(ref) %>%
  count(compliance_new, .drop = FALSE) %>%
  pivot_wider(names_from = ref, values_from = n) %>%
  mutate(percent_a = round(A/sum(A)*100,0), percent_b = round(B/sum(B)*100,0), percent_c = round(C/sum(C)*100,0), percent_d = round(D/sum(D)*100,0)) %>%
  relocate(compliance_new, A, percent_a, B, percent_b, C, percent_c, D, percent_d)

compliance_new_ref <- bind_cols(compliance_new_ref_a, compliance_new_ref_j)  %>%
  rename(A_articles = A...2, B_articles = B...4, C_articles = C...6, D_articles = D...8, A_journals = A...11, B_journals = B...13, C_journals = C...15, D_journals= D...17) %>%
  adorn_totals("row") %>%
  relocate(1,2,3,11,12,4,5,13,14,6,7,15,16,8,9,17,18) %>%
  select(-'compliance_new...10')
rm(compliance_new_ref_a, compliance_new_ref_j)

openxlsx::write.xlsx(as.data.frame(compliance_new_ref), 'compliance_new_ref.xlsx')

# Tree map for Compliance by REF panel 
compliance_new_ref_a <- ref_panels %>%
  filter(ref != "Missing" & ref != "") %>% 
  group_by(ref) %>%
  count(compliance_new, .drop = FALSE)

compliance_new_ref_chart <- compliance_new_ref_a %>%
  group_by(ref, compliance_new) %>%
  head(16) %>%
  summarise_all(funs(sum))

library(treemap)

compliance_new_ref_tree_refa <- compliance_new_ref_chart %>%
  filter(ref == "A") %>%
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

compliance_new_ref_tree_refb <- compliance_new_ref_chart %>%
  filter(ref == "B") %>%
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

compliance_new_ref_tree_refc <- compliance_new_ref_chart %>%
  filter(ref == "C") %>%
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

compliance_new_ref_tree_refd <- compliance_new_ref_chart %>%
  filter(ref == "D") %>%
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



# 3. compliant by publisher ----
# Might want to think about just doing top publishers rather than top compliant vs non compliant

compliance_new_publisher <- NA
compliance_new_publisher <- merged_pvga %>%
  filter(compliance_new != "not compliant", !is.na(publisher)) %>%
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
  filter(compliance_new == "c: green oa") %>%
  count(publisher, sort = TRUE)

compliance_new_publisher <- left_join(compliance_new_publisher, articles_per_publisher, by = "publisher") %>%
  mutate(percent_of_publishers_articles = round(n.x/n.y*100,0)) %>%
  select(-c(5))

compliance_new_publisher <- left_join(compliance_new_publisher, pure_gold_per_publisher, by = "publisher") %>%
  mutate(percent_pure = round((n/n.x)*100,0)) %>%
  rename(n_pure = n)

compliance_new_publisher <- left_join(compliance_new_publisher, ta_per_publisher, by = "publisher") %>%
  mutate(proportion_c_by_ta = round(n/n.x * 100, 0)) %>%
  rename(n_c_by_ta = n)

  compliance_new_publisher <- left_join(compliance_new_publisher, green_per_publisher, by = "publisher") %>%
  mutate(percent_green = round(n/n.x*100,0)) %>%
    rename(n_compliant = n.x, n_green = n) %>%
  select(-c(6,8,10)) %>%
    adorn_totals("row")

rm(ta_per_publisher, articles_per_publisher, pure_gold_per_publisher, green_per_publisher)

openxlsx::write.xlsx(as.data.frame(compliance_new_publisher), 'compliant_new_publisher.xlsx')



# 4. Non-compliant by publisher ----

not_compliance_new_publisher <- merged_pvga %>%
  filter(compliance_new == "not compliant", !is.na(publisher)) %>%
  count(publisher, sort = TRUE) %>%
  mutate(percent = (n / sum(n)) * 100) %>%
  mutate(cml = round(cumsum(percent),0)) %>%
  mutate(percent = round(percent, 0)) %>%
  adorn_totals("row")
# add in total articles per publisher
articles_per_publisher <- merged_pvga %>%
  count(publisher, sort = TRUE)

not_compliance_new_publisher <- left_join(not_compliance_new_publisher, articles_per_publisher, by = "publisher") %>%
  mutate(percent_of_publisher_total = round(n.x / n.y * 100, digits=0)) %>%
  select(-5)
openxlsx::write.xlsx(as.data.frame(not_compliance_new_publisher), 'not_compliance_new_publisher.xlsx')



# 5. Non compliant by REF UoC ----
# to work out percentages you need to generate a list of totals for each uoc then filter that so as to only include the categories that still exist after the filter.

# There are two units of assessment columns and we need to know if each subject exists in either of them for each article.

# First pivot longer to create one column with all the UoAs
units_of_assessment <- merged_pvga %>% pivot_longer(c(category_uoa1, category_uoa2), names_to = "uoa_all", values_to ="uoa")

# Working out denominators (since we won't be able to do this after filtering the data)
summary_uoa <- units_of_assessment %>%
  filter(uoa != "Missing" & uoa != "") %>% 
  group_by(uoa) %>% summarize(n=n())
# openxlsx::write.xlsx(as.data.frame(summary_uoa), 'summary_uoa.xlsx')

# Highest number and proportion of non-compliant articles
not_compliant_uoa <- units_of_assessment %>%
  filter(compliance_new == "not compliant", uoa != "Missing" & uoa != "") %>%
  group_by(uoa) %>%
  summarise(count = n()) %>%
  mutate(percent = round(count / sum(count) * 100, 0), percent_not_compliant = round((count / c(summary_uoa$n)*100), digits = 0)) %>%
  arrange(desc(count)) %>%
  mutate(cml = round(cumsum(percent),0)) %>%
  adorn_totals("row")
openxlsx::write.xlsx(as.data.frame(not_compliant_uoa), 'not_compliant_uoa.xlsx')

# 6. Compliance by UOA ----
units_of_assessment <- merged_pvga %>% pivot_longer(c(category_uoa1, category_uoa2), names_to = "uoa_all", values_to ="uoa")

compliance_new_uoa <- units_of_assessment %>%
  filter(uoa != "Missing" & uoa != "") %>% 
  group_by(uoa) %>%
  count(compliance_new, .drop = FALSE) %>%
  mutate(percent = (n / sum(n)) * 100) %>%
  mutate(cml = round(cumsum(percent),0)) %>%
  mutate(percent = round(percent, 0))
openxlsx::write.xlsx(as.data.frame(compliance_new_uoa), 'compliance_new_uoa.xlsx')




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
  mutate(percent = round(n / sum(n) * 100, digits=0)) %>%
  adorn_totals("row")

green_breakdown_j <- merged_pvga %>%
  filter(journal_type != "Pure gold", !is.na(g_license2), !is.na(g_embargo2), !is.na(g_compliant_repository), !is.na(g_article_version)) %>%
  filter(!duplicated(journal_title)) %>%
  count(g_license3, g_embargo3, g_compliant_repository, g_article_version2) %>%
  mutate(percent = round(n / sum(n) * 100, digits=0)) %>%
  adorn_totals("row")

green_breakdown <- bind_cols(green_breakdown_a, green_breakdown_j) %>%
  rename(article_n = n...5, article_percent = percent...6, journal_n = n...11, journal_percent = percent...12) %>%
  select(-7, -8, -9, -10)
rm(green_breakdown_a, green_breakdown_j)

openxlsx::write.xlsx(as.data.frame(green_breakdown), 'green_breakdown.xlsx')

#b. green licenses----

compliant_green_licenses <- merged_pvga %>%
  filter(num_new_green == 1, !is.na(g_license1), journal_type != "Pure gold") %>%
  count(g_license1, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100, digits=0)) %>%
  adorn_totals("row")

all_green_licenses_exc_pure <- merged_pvga %>%
  filter(!is.na(g_license1), journal_type != "Pure gold") %>%
  count(g_license1, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100, digits=0)) %>%
  adorn_totals("row")

all_green_licenses <- merged_pvga %>%
  filter(!is.na(g_license1)) %>%
  count(g_license1, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100, digits=0)) %>%
  adorn_totals("row")

licenses <- bind_cols(compliant_green_licenses, all_green_licenses_exc_pure, all_green_licenses) %>%
  rename(License = "g_license1...1", compliant_n = n...2, "c%" = percent...3, all_green_no_pure_n = n...5, "agnp%" = percent...6, all_green_n = n...8, "ag%" = percent...9) %>%
  select(-4,-7) %>%
  subset(License != "NA - policy for published version in full OA journal" & License != "bespoke license")
rm(compliant_green_licenses, all_green_licenses_exc_pure, all_green_licenses)

openxlsx::write.xlsx(as.data.frame(licenses), 'licenses.xlsx')

#c. green embargos----
compliant_green_embargos <- merged_pvga %>%
  filter(num_new_green == 1, !is.na(g_embargo), journal_type != "Pure gold") %>%
  count(g_embargo, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100, digits=0)) %>%
  adorn_totals("row")

all_green_embargos_exc_pure <- merged_pvga %>%
  filter(!is.na(g_embargo), journal_type != "Pure gold") %>%
  count(g_embargo, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100, digits=0)) %>%
  adorn_totals("row")

all_green_embargos <- merged_pvga %>%
  filter(!is.na(g_embargo)) %>%
  count(g_embargo, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100, digits=0)) %>%
  adorn_totals("row")

embargos <- bind_cols(compliant_green_embargos, all_green_embargos_exc_pure, all_green_embargos) %>% # NB embargo3 has to be a factor for this to work
  rename(Embargo = "g_embargo...1", compliant_n = n...2, "c%" = percent...3, all_green_no_pure_n = n...5, "agnp%" = percent...6, all_green_n = n...8, "ag%" = percent...9) %>%
  select(-4,-7)
rm(compliant_green_embargos, all_green_embargos_exc_pure, all_green_embargos)

  # To figure out what proportion of the 0 embargos are explicitly zero embargo as opposed to no requirement. I couldn't figure out a clever way to edit the above table so some of the calculation needs to be done in excel
number_explicit_zero_embargo <- merged_pvga %>%
  filter(num_new_green == 1, !is.na(g_embargo), journal_type != "Pure gold", g_embargo == 0, g_embargo.units != "No embargo requirement") %>%
  count(g_embargo2)
number_explicit_zero_embargo

embargos <- embargos %>%
  mutate(number_explicit_zero = number_explicit_zero_embargo$n)

openxlsx::write.xlsx(as.data.frame(embargos), 'embargos.xlsx')


#XXXXXXXX
# IMPACT OF DIFFERENT POLICY SCENARIOS----

# Identify articles which are compliant in different scenarios

  
  # Compliant with current policy but not Scenario 1 (new policy, hybrid supported)
current_s1 <- merged_pvga %>%
  filter(!(is.na(sherpa_id) & has_ta == "no"), compliance_current == "not compliant", compliance_new_hybrid != "not compliant")

  # Compliant with current policy but not Scenario 2 (new policy, hybrid not supported)


  # Compliant with Scenario 1 but not Scenario 2


#XXXXXXXX NOTHING FROM THIS POINT ON REQUIRES QA----
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
  filter(compliance_new == "not compliant", chemistry == TRUE, !is.na(publisher)) %>%
  count(publisher, sort = TRUE) %>%
  mutate(percent = round(n / sum(n) * 100, digits=2)) %>%
  adorn_totals("row")
chemistry_publisher
openxlsx::write.xlsx(as.data.frame(chemistry_publisher), 'chemistry_publisher.xlsx')


chemistry_journals <- merged_pvga %>%
  filter(compliance_new == "not compliant", chemistry == TRUE) %>%
  count(journal_title, publisher, sort = TRUE) %>%
  mutate(percent = round(n / sum(n) * 100, digits=2)) %>%
  adorn_totals("row")
chemistry_journals

# Architecture
merged_pvga$architecture[merged_pvga$category_uoa1 == "C13 Architecture, Built Environment and Planning" | merged_pvga$ref_panel2 == "C13 Architecture, Built Environment and Planning"] <- TRUE
table(merged_pvga$architecture)

architecture_publisher <- merged_pvga %>%
  filter(compliance_new == "not compliant", architecture == TRUE, !is.na(publisher)) %>%
  count(publisher, sort = TRUE) %>%
  mutate(percent = round(n / sum(n) * 100, digits=2)) %>%
  adorn_totals("row")
architecture_publisher
openxlsx::write.xlsx(as.data.frame(architecture_publisher), 'architecture_publisher.xlsx')


architecture_journals <- merged_pvga %>%
  filter(compliance_new == "not compliant", architecture == TRUE) %>%
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
