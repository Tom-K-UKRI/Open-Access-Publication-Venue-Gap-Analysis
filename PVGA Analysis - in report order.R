# PUBLICATION VENUE GAP ANALYSIS IN REPORT ORDER ERROR V

# Author: Tom Kenny
# Created: August 2020
# Last Updated: November 2020

# Purpose of code: This code produces tables and charts for the publication venue gap analysis - it forms the basis of an early findings presentation.

# Prerequisite: this relies on the dataset merged_pvga produced with Tom Kenny's code 'Preparing and Merging PVGA data' which in turn is dependent on data from Dimensions downloaded using google sheets API and from Sherpa (downloaded using code SHERPA RoMEO API and cleaning) See (C:\Users\TKen02\UKRI\Policy Analysis - Documents\Open Access\Projects\Publication Venue Gap Analysis\Data\).


#################
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

#####
# Import data
merged_pvga <- read.xlsx("merged_pvga.xlsx")


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


#####

# ACTUAL OPEN ACCESS STATUS

# 1. Dimensions' Open Access Categories

open_access_categories <- merged_pvga %>%
  count(open_access_categories2) %>%
  mutate(percent = (n / sum(n) * 100), cml = round(cumsum(percent),0), percent = round(percent,0)) %>%
  adorn_totals("row")
open_access_categories

openxlsx::write.xlsx(as.data.frame(open_access_categories), 'open_access_categories.xlsx')

open_access_categories_bar <- merged_pvga %>%
  count(open_access_categories2) %>%
  mutate(percent = round((n / sum(n) * 100)),0) %>%
  ggplot(aes(x = open_access_categories2, y = n, fill = open_access_categories2)) +
  geom_col(fill = c("#F08900","#FBBB10", "#16978A","#FF5A5A")) +
  ggtitle("Dimensions' Open Access Categories") +
  labs(x="Open Access Categories", y= "Number of articles")

# 2. Dimensions Open Access Categories x REF Panel

# Dimensions open_access_categories x ref panel (not possible to do at journal level). NB. This has to be done four times and bound because ref_panel1 only captures the first ref panel attributed to an article. This means it will underestimate how many articles are in each ref panel and this will have a particular affect on panels B,C, and D since the panels are ordered alphabetically.
open_access_categories_a_refa <- merged_pvga %>%
  filter(ref_panel_a == TRUE) %>%
  count(open_access_categories2) %>%
  mutate(percent = (n / sum(n) * 100)) %>%
  mutate(cml = round(cumsum(percent),0)) %>%
  mutate(percent = round(percent,0)) %>%
  adorn_totals("row")

open_access_categories_a_refb <- merged_pvga %>%
  filter(ref_panel_b == TRUE) %>%
  count(open_access_categories2) %>%
  mutate(percent = (n / sum(n) * 100)) %>%
  mutate(cml = round(cumsum(percent),0)) %>%
  mutate(percent = round(percent,0)) %>%
  adorn_totals("row")

open_access_categories_a_refc <- merged_pvga %>%
  filter(ref_panel_c == TRUE) %>%
  count(open_access_categories2) %>%
  mutate(percent = (n / sum(n) * 100)) %>%
  mutate(cml = round(cumsum(percent),0)) %>%
  mutate(percent = round(percent,0)) %>%
  adorn_totals("row")

open_access_categories_a_refd <- merged_pvga %>%
  filter(ref_panel_d == TRUE) %>%
  count(open_access_categories2) %>%
  mutate(percent = (n / sum(n) * 100)) %>%
  mutate(cml = round(cumsum(percent),0)) %>%
  mutate(percent = round(percent,0)) %>%
  adorn_totals("row")

open_access_categories_a_ref <- bind_cols(open_access_categories_a_refa, open_access_categories_a_refb, open_access_categories_a_refc, open_access_categories_a_refd) %>%
  rename(n_Panel_A = n...2, n_Panel_B = n...6, n_Panel_C = n...10, n_Panel_D = n...14, "% A" = percent...3, "% B" = percent...7, "% C" = percent...11, "% D" = percent...15, Open_Access_Categories = open_access_categories2...1) %>%
  select(-5, -9, -13)
rm(open_access_categories_a_refa, open_access_categories_a_refb, open_access_categories_a_refc, open_access_categories_a_refd)
openxlsx::write.xlsx(as.data.frame(open_access_categories_a_ref), 'open_access_categories_a_ref.xlsx')

# Bar
open_access_categories_a_ref$Open_Access_Categories <- factor(open_access_categories_a_ref$Open_Access_Categories, ordered = TRUE, levels = c("Pure gold", "Hybrid gold", "Green", "Closed", "Total"))
open_access_categories_a_ref_bar <- open_access_categories_a_ref %>%
  filter(Open_Access_Categories != "Total") %>%
  rename("A: M & LS" = "% A", "B: PS, E & M" = "% B", "C: SS" = "% C", "D: A & H" = "% D") %>%
  pivot_longer(c("A: M & LS", "B: PS, E & M", "C: SS", "D: A & H")) %>%
  ggplot(aes(x=name, y=value)) +
  geom_bar(aes(fill=Open_Access_Categories), stat = "identity", position = position_dodge2()) +
  scale_fill_manual(values = c("#F08900","#FBBB10", "#16978A","#FF5A5A")) +
  ggtitle("Open Access Categories by REF Panel") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme(axis.title.x = element_text(face="bold", size=12, margin = margin(t=10)),
             axis.text.x  = element_text(vjust=0.5, size=10)) +
  labs(x="REF Panels", y= "% of all articles in panel")
ggsave(open_access_categories_a_ref_bar, filename = "open_access_categories_a_ref_bar.png")


# 3. Dimensions open acccess categories by Unit of Assessment
uoa_names <- c("A01 Clinical Medicine", "A02 Public Health, Health Services and Primary Care", "A03 Allied Health Professions, Dentistry, Nursing and Pharmacy", "A04 Psychology, Psychiatry and Neuroscience", "A05 Biological Sciences", "A06 Agriculture, Veterinary and Food Science", "B07 Earth Systems and Environmental Sciences", "B08 Chemistry", "B09 Physics", "B10 Mathematical Sciences", "B11 Computer Science and Informatics", "B12 Engineering", "C13 Architecture, Built Environment and Planning", "C14 Geography and Environmental Studies", "C15 Archaeology", "C16 Economics and Econometrics", "C17 Business and Management Studies", "C18 Law", "C19 Politics and International Studies", "C20 Social Work and Social Policy", "C21 Sociology", "C22 Anthropology and Development Studies", "C23 Education", "C24 Sport and Exercise Sciences, Leisure and Tourism", "D25 Area Studies", "D26 Modern Languages and Linguistics", "D27 English Language and Literature", "D28 History", "D29 Classics", "D30 Philosophy", "D31 Theology and Religious Studies", "D32 Art and Design: History, Practice and Theory", "D33 Music, Drama, Dance, Performing Arts, Film and Screen Studies", "D34 Communication, Cultural and Media Studies, Library and Information Management")

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
  mutate(percent = round(percent, 0))
open_access_categories_uoa
openxlsx::write.xlsx(as.data.frame(open_access_categories_uoa), 'open_access_categories_uoa.xlsx')

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


# 4. Journal type

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
journal_type_a_refa <- merged_pvga %>%
  filter(ref_panel_a == TRUE, !is.na(sherpa_id), !is.na(journal_type)) %>%
  count(journal_type) %>%
  mutate(percent = round(n / sum(n) * 100),1) %>%
  adorn_totals("row")
journal_type_a_refa

journal_type_a_refb <- merged_pvga %>%
  filter(ref_panel_b == TRUE, !is.na(sherpa_id), !is.na(journal_type)) %>%
  count(journal_type) %>%
  mutate(percent = round(n / sum(n) * 100),1) %>%
  adorn_totals("row")
journal_type_a_refb

journal_type_a_refc <- merged_pvga %>%
  filter(ref_panel_c == TRUE, !is.na(sherpa_id), !is.na(journal_type)) %>%
  count(journal_type) %>%
  mutate(percent = round(n / sum(n) * 100),1) %>%
  adorn_totals("row")
journal_type_a_refc

journal_type_a_refd <- merged_pvga %>%
  filter(ref_panel_d == TRUE, !is.na(sherpa_id), !is.na(journal_type)) %>%
  count(journal_type) %>%
  mutate(percent = round(n / sum(n) * 100),1) %>%
  adorn_totals("row")
journal_type_a_refd

journal_type_a_ref <- bind_cols(journal_type_a_refa, journal_type_a_refb, journal_type_a_refc, journal_type_a_refd) %>%
  rename(n_Panel_A = n...2, n_Panel_B = n...6, n_Panel_C = n...10, n_Panel_D = n...14, "% A" = percent...3, "% B" = percent...7, "% C" = percent...11, "% D" = percent...15, Journal_Type = journal_type...1) %>%
  select(-4, -5, -8, -9, -12, -13, -16)
rm(journal_type_a_refa, journal_type_a_refb, journal_type_a_refc, journal_type_a_refd)
openxlsx::write.xlsx(as.data.frame(journal_type_a_ref), 'journal_type_a_ref.xlsx')

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

##########
# HYPOTHETICAL OPEN ACCESS UNDER NEW UKRI POLICY


# 1. # Full break down of compliance
compliance_a <- merged_pvga %>%
  filter(!(is.na(sherpa_id) & has_ta == "no")) %>%
  count(compliance2, .drop = FALSE) %>%
  mutate(percent = (n / sum(n) * 100)) %>%
  mutate(cml = round(cumsum(percent),0)) %>%
  mutate(percent = round(percent,0)) %>%
  adorn_totals("row")
compliance_a

compliance_j <- merged_pvga %>%
  filter(!duplicated(journal_title)) %>%
  filter(!(is.na(sherpa_id) & has_ta == "no")) %>%
  count(compliance2, .drop = FALSE) %>%
  mutate(percent = (n / sum(n) * 100)) %>%
  mutate(cml = round(cumsum(percent),0)) %>%
  mutate(percent = round(percent,0)) %>%
  adorn_totals("row")
compliance_j

compliance <- bind_cols(compliance_a, compliance_j) %>%
  select(-5)
rm(compliance_a, compliance_j)
openxlsx::write.xlsx(as.data.frame(compliance), 'compliance.xlsx')


# 2. Green licenses - first compliant only, then all except pure gold, then all, then bind them all together, rename columns, and remove unnecessary rows and columns
compliant_green_licenses <- merged_pvga %>%
  filter(compliance == "c: green oa") %>%
  count(g_license1, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100, digits=0)) %>%
  adorn_totals("row")

all_green_licenses_exc_pure <- merged_pvga %>%
  filter(!is.na(g_license), journal_type != "Pure gold") %>%
  count(g_license1, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100, digits=0)) %>%
  adorn_totals("row")

all_green_licenses <- merged_pvga %>%
  filter(!is.na(g_license), g_license1 != "NA - policy for published version in full OA journal") %>%
  count(g_license1, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100, digits=0)) %>%
  adorn_totals("row")

licenses <- bind_cols(compliant_green_licenses, all_green_licenses_exc_pure, all_green_licenses) %>%
  rename(License = "g_license1...1", compliant_n = n...2, "c%" = percent...3, all_green_no_pure_n = n...5, "agnp%" = percent...6, all_green_n = n...8, "ag%" = percent...9) %>%
  select(-4,-7) %>%
  subset(License != "NA - policy for published version in full OA journal" & License != "bespoke license")
rm(compliant_green_licenses, all_green_licenses_exc_pure, all_green_licenses)

openxlsx::write.xlsx(as.data.frame(licenses), 'licenses.xlsx')

# 3. Mix of green compliance

# Create new var g_article_version2 to change into published vs accepted
merged_pvga$g_article_version2[merged_pvga$g_article_version == "submitted, accepted" | merged_pvga$g_article_version == "accepted"] <- "accepted_any"
merged_pvga$g_article_version2[merged_pvga$g_article_version == "accepted, published" | merged_pvga$g_article_version == "submitted, accepted, published" | merged_pvga$g_article_version == "submitted, published" | merged_pvga$g_article_version == "published"] <- "published_any"
table(merged_pvga$g_article_version2)

# Table to show mix of compliance for green Oa
compliant_reason_a <- merged_pvga %>%
  filter(compliance == "c: green oa", !is.na(g_license2), !is.na(g_embargo2), !is.na(g_compliant_repository), !is.na(g_article_version)) %>%
  count(g_license3, g_embargo2, g_compliant_repository, g_article_version2) %>%
  mutate(percent = round(n / sum(n) * 100, digits=0)) %>%
  adorn_totals("row")
compliant_reason_a

compliant_reason_j <- merged_pvga %>%
  filter(compliance == "c: green oa", !is.na(g_license2), !is.na(g_embargo2), !is.na(g_compliant_repository), !is.na(g_article_version)) %>%
  filter(!duplicated(journal_title)) %>%
  count(g_license3, g_embargo2, g_compliant_repository, g_article_version2) %>%
  mutate(percent = round(n / sum(n) * 100, digits=0)) %>%
  adorn_totals("row")
compliant_reason_j

compliant_reason <- bind_cols(compliant_reason_a, compliant_reason_j) %>%
  rename(article_n = n...5, article_percent = percent...6, journal_n = n...11, journal_percent = percent...12) %>%
  select(-7, -8, -9, -10)
rm(compliant_reason_a, compliant_reason_j)

openxlsx::write.xlsx(as.data.frame(compliant_reason), 'compliant_reason.xlsx')

# 4. Mix of green non-compliance

non_compliant_reason_a <- merged_pvga %>%
  filter(compliant == FALSE, !is.na(g_license2), !is.na(g_embargo2), !is.na(g_compliant_repository)) %>%
  count(g_license2, g_embargo2, g_compliant_repository) %>%
  mutate(percent = round(n / sum(n) * 100, digits=2)) %>%
  adorn_totals("row")
non_compliant_reason_a %>%
  
  openxlsx::write.xlsx(as.data.frame(non_compliant_reason_a), 'non_compliant_reason_a.xlsx')


# 4. Compliant by publisher
compliant_publisher <- merged_pvga %>%
  filter(compliant == TRUE, !is.na(publisher)) %>%
  count(publisher, sort = TRUE) %>%
  mutate(percent = (n / sum(n)) * 100, cml = round(cumsum(percent),0))
compliant_publisher
# add in total articles per publisher
articles_per_publisher <- merged_pvga %>%
  count(publisher, sort = TRUE)
articles_per_publisher
# add in total articles only compliant through TA
ta_per_publisher <- merged_pvga %>%
  filter(compliance == "c: only by ta") %>%
  count(publisher, sort = TRUE)
ta_per_publisher

compliant_publisher <- left_join(compliant_publisher, articles_per_publisher, by = "publisher") %>%
  mutate(percent_of_publisher_total = round(n.x / n.y * 100, digits=0), percent = round(percent,0)) %>%
  select(-5) %>%
  adorn_totals("row")

compliant_publisher <- left_join(compliant_publisher, ta_per_publisher, by = "publisher") %>%
  mutate(proportion_c_by_ta = round(n/n.x * 100, 0)) %>%
  rename(n_c_by_ta = n, n_compliant = n.x, percent_of_all_compliant = percent)

rm(ta_per_publisher, articles_per_publisher)

openxlsx::write.xlsx(as.data.frame(compliant_publisher), 'compliant_publisher.xlsx')



# 5. Non-compliant by publisher

not_compliant_publisher <- merged_pvga %>%
  filter(compliant == FALSE, !is.na(publisher)) %>%
  count(publisher, sort = TRUE) %>%
  mutate(percent = (n / sum(n)) * 100) %>%
  mutate(cml = round(cumsum(percent),0)) %>%
  mutate(percent = round(percent, 0)) %>%
  adorn_totals("row")
not_compliant_publisher
# add in total articles per publisher
articles_per_publisher <- merged_pvga %>%
  filter(!is.na(sherpa_id)) %>%
  count(publisher, sort = TRUE)
articles_per_publisher

not_compliant_publisher <- left_join(not_compliant_publisher, articles_per_publisher, by = "publisher") %>%
  mutate(percent_of_publisher_total = round(n.x / n.y * 100, digits=0)) %>%
  select(-5)
openxlsx::write.xlsx(as.data.frame(not_compliant_publisher), 'not_compliant_publisher.xlsx')


      
# 6. Non compliant by REF panel (# I don't think this analysis is possible at journal level since a lot of journals publish across different REF panels)

# Compliance var by ref_panel
compliance_ref_a <- merged_pvga %>%
  filter(ref_panel_a == TRUE, !(is.na(sherpa_id) & has_ta == "no")) %>%
  count(compliance2) %>%
  mutate(percent = (n / sum(n)) * 100) %>%
  mutate(cml = round(cumsum(percent),0)) %>%
  mutate(percent = round(percent, 0)) %>%
  adorn_totals("row")
compliance_ref_a

compliance_ref_b <- merged_pvga %>%
  filter(ref_panel_b == TRUE, !(is.na(sherpa_id) & has_ta == "no")) %>%
  count(compliance2) %>%
  mutate(percent = (n / sum(n)) * 100) %>%
  mutate(cml = round(cumsum(percent),0)) %>%
  mutate(percent = round(percent, 0)) %>%
  adorn_totals("row")
compliance_ref_b

compliance_ref_c <- merged_pvga %>%
  filter(ref_panel_c == TRUE, !(is.na(sherpa_id) & has_ta == "no")) %>%
  count(compliance2) %>%
  mutate(percent = (n / sum(n)) * 100) %>%
  mutate(cml = round(cumsum(percent),0)) %>%
  mutate(percent = round(percent, 0)) %>%
  adorn_totals("row")
compliance_ref_c

compliance_ref_d <- merged_pvga %>%
  filter(ref_panel_d == TRUE, !(is.na(sherpa_id) & has_ta == "no")) %>%
  count(compliance2, .drop = FALSE) %>%
  mutate(percent = (n / sum(n)) * 100) %>%
  mutate(cml = round(cumsum(percent),0)) %>%
  mutate(percent = round(percent, 0)) %>%
  adorn_totals("row")
compliance_ref_d

compliance_ref <- bind_cols(compliance_ref_a, compliance_ref_b, compliance_ref_c, compliance_ref_d) %>%
  rename(n_Panel_A = n...2, n_Panel_B = n...6, n_Panel_C = n...10, n_Panel_D = n...14, "% A" = percent...3, "% B" = percent...7, "% C" = percent...11, "% D" = percent...15, Compliance = compliance2...1) %>%
  select(-5, -9, -13)
rm(compliance_ref_a, compliance_ref_b, compliance_ref_c, compliance_ref_d)

openxlsx::write.xlsx(as.data.frame(compliance_ref), 'compliance_ref.xlsx')

  # Tree map for Compliance by REF panel 
  # recode all nc categories into one
compliance_ref$Compliance[compliance_ref$Compliance == "nc: pure gold but non-compliant license" | compliance_ref$Compliance == "nc: compliant green license but has embargo" | compliance_ref$Compliance == "nc: zero embargo but no compliant green license" | compliance_ref$Compliance ==  "nc: no compliant green license and has embargo" | compliance_ref$Compliance ==  "nc: no green policy identified"] <- "not compliant"

compliance_ref <- compliance_ref %>%
  group_by(Compliance) %>%
  summarise_all(funs(sum))

compliance_ref$Compliance <- factor(compliance_ref$Compliance, ordered = TRUE, levels = c("c: pure gold", "c: green oa", "c: only by ta", "not compliant", "Total"))

compliance_ref_bar <- compliance_ref %>%
  filter(Compliance != "Total") %>%
  rename("A: M & LS" = "% A", "B: PS, E & M" = "% B", "C: SS" = "% C", "D: A & H" = "% D") %>%
  pivot_longer(c("A: M & LS", "B: PS, E & M", "C: SS", "D: A & H")) %>%
  ggplot(aes(x=name, y=value)) +
  geom_bar(aes(fill=Compliance), stat = "identity", position = position_dodge2()) +
  scale_fill_manual(values = c("#F08900","#16978A", "#2E2D62", "#FF5A5A")) +
  ggtitle("Compliance by REF Panel") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme(axis.title.x = element_text(face="bold", size=12, margin = margin(t=10)),
        axis.text.x  = element_text(vjust=0.5, size=10)) +
  labs(x="REF Panels", y= "% of all articles in panel")
ggsave(compliance_ref_bar, filename = "compliance_ref_bar.png")

# compliance_ref_pie <- compliance_ref %>%
#   filter(Compliance != "Total") %>%
#   rename("A: M & LS" = "% A", "B: PS, E & M" = "% B", "C: SS" = "% C", "D: A & H" = "% D") %>%
#   pivot_longer(c("A: M & LS", "B: PS, E & M", "C: SS", "D: A & H")) %>%
#   ggplot(aes(x=name, y=value)) +
#   geom_bar(aes(fill=Compliance), stat = "identity") +
#   geom_text(aes(label = value), position = position_stack(vjust = 0.5)) +
#   coord_polar(theta = "y") +
#   facet_wrap(~ name)  +
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank()) + 
#   theme(legend.position='bottom') + 
#   guides(fill=guide_legend(nrow=2, byrow=TRUE))
  
library(treemap)

compliance_ref_tree_refa <- compliance_ref %>%
  filter(Compliance != "Total") %>%
  rename("A: M & LS" = "% A", "B: PS, E & M" = "% B", "C: SS" = "% C", "D: A & H" = "% D") %>%
  pivot_longer(c("A: M & LS", "B: PS, E & M", "C: SS", "D: A & H")) %>%
  filter(name == "A: M & LS") %>%
  treemap(index="Compliance",
          vSize="value",
          type="index",
          palette=c("#F08900","#16978A", "#2E2D62", "#FF5A5A"),
          title="Panel A: Medicine & Life Sciences",
          fontsize.title=18,
          fontsize.labels = 16
          )

compliance_ref_tree_refb <- compliance_ref %>%
  filter(Compliance != "Total") %>%
  rename("A: M & LS" = "% A", "B: PS, E & M" = "% B", "C: SS" = "% C", "D: A & H" = "% D") %>%
  pivot_longer(c("A: M & LS", "B: PS, E & M", "C: SS", "D: A & H")) %>%
  filter(name == "B: PS, E & M") %>%
  treemap(index="Compliance",
          vSize="value",
          type="index",
          palette=c("#F08900","#16978A", "#2E2D62", "#FF5A5A"),
          title="Panel B: Physical Sciences, Engingeering & Maths",
          fontsize.title=18,
          fontsize.labels = 16
  )

compliance_ref_tree_refc <- compliance_ref %>%
  filter(Compliance != "Total") %>%
  rename("A: M & LS" = "% A", "B: PS, E & M" = "% B", "C: SS" = "% C", "D: A & H" = "% D") %>%
  pivot_longer(c("A: M & LS", "B: PS, E & M", "C: SS", "D: A & H")) %>%
  filter(name == "C: SS") %>%
  treemap(index="Compliance",
          vSize="value",
          type="index",
          palette=c("#F08900","#16978A", "#2E2D62", "#FF5A5A"),
          title="Panel C: Social Sciences",
          fontsize.title=18,
          fontsize.labels = 16
  )

compliance_ref_tree_refd <- compliance_ref %>%
  filter(Compliance != "Total") %>%
  rename("A: M & LS" = "% A", "B: PS, E & M" = "% B", "C: SS" = "% C", "D: A & H" = "% D") %>%
  pivot_longer(c("A: M & LS", "B: PS, E & M", "C: SS", "D: A & H")) %>%
  filter(name == "D: A & H") %>%
  treemap(index="Compliance",
          vSize="value",
          type="index",
          palette=c("#F08900","#16978A", "#2E2D62", "#FF5A5A"),
          title="Panel D: Arts & Humanities",
          fontsize.title=18,
          fontsize.labels = 16
  )


# 7. Non compliant by REF UoC - to work out percentages you need to generate a list of totals for each uoc then filter that so as to only include the categories that still exist after the filter.

# There are two units of assessment columns and we need to know if each subject exists in either of them for each article.

  # First pivot longer to create one column with all the UoAs
units_of_assessment <- merged_pvga %>% pivot_longer(c(category_uoa1, category_uoa2), names_to = "uoa_all", values_to ="uoa")

    # Working out denominators (since we won't be able to do this after filtering the data)
summary_uoa <- units_of_assessment %>%
  filter(uoa != "Missing" & uoa != "") %>% 
  group_by(uoa) %>% summarize(n=n())
# openxlsx::write.xlsx(as.data.frame(summary_uoa), 'summary_uoa.xlsx')

# Highest number and proportion of non-compliant articles
not_compliant_uoa2 <- units_of_assessment %>%
  filter(compliant == FALSE, uoa != "Missing" & uoa != "") %>%
  group_by(uoa) %>%
  summarise(count = n()) %>%
  mutate(percent = round(count / sum(count) * 100, 0), percent_not_compliant = round((count / c(summary_uoa$n)*100), digits = 0)) %>%
  arrange(desc(count)) %>%
  mutate(cml = round(cumsum(percent),0))
  adorn_totals("row")
not_compliant_uoa2
openxlsx::write.xlsx(as.data.frame(not_compliant_uoa2), 'not_compliant_uoa2.xlsx')

# 8. Compliance by UOA
units_of_assessment <- merged_pvga %>% pivot_longer(c(category_uoa1, category_uoa2), names_to = "uoa_all", values_to ="uoa")

compliance_uoa2 <- units_of_assessment %>%
  filter(!(is.na(sherpa_id) & has_ta == "no")) %>%
  filter(uoa != "Missing" & uoa != "") %>% 
  group_by(uoa) %>%
  count(compliance3, .drop = FALSE) %>%
  mutate(percent = (n / sum(n)) * 100) %>%
  mutate(cml = round(cumsum(percent),0)) %>%
  mutate(percent = round(percent, 0))
compliance_uoa2
openxlsx::write.xlsx(as.data.frame(compliance_uoa2), 'compliance_uoa2.xlsx')

# Working out clusters of compliance 

library(factoextra)
  # Pivot wider to get each UoA on one row
compliance_uoa2_grouped <- pivot_wider(compliance_uoa2, id_cols = uoa, names_from = compliance3, values_from = percent)

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




##########
# EXPLORING GROUPS OF NON-COMPLIANT ARTICLES/ JOURNALS AND SUITABLE ALTERNATIVES

# 1. Key subjects 

  # Chemistry
merged_pvga$chemistry[merged_pvga$category_uoa1 == "B08 Chemistry" | merged_pvga$ref_panel2 == "B08 Chemistry"] <- TRUE
table(merged_pvga$chemistry)

chemistry_publisher <- merged_pvga %>%
  filter(compliant == FALSE, chemistry == TRUE, !is.na(publisher)) %>%
  count(publisher, sort = TRUE) %>%
  mutate(percent = round(n / sum(n) * 100, digits=2)) %>%
  adorn_totals("row")
chemistry_publisher
openxlsx::write.xlsx(as.data.frame(chemistry_publisher), 'chemistry_publisher.xlsx')


chemistry_journals <- merged_pvga %>%
  filter(compliant == FALSE, chemistry == TRUE) %>%
  count(journal_title, publisher, sort = TRUE) %>%
  mutate(percent = round(n / sum(n) * 100, digits=2)) %>%
  adorn_totals("row")
chemistry_journals

# Architecture
merged_pvga$architecture[merged_pvga$category_uoa1 == "C13 Architecture, Built Environment and Planning" | merged_pvga$ref_panel2 == "C13 Architecture, Built Environment and Planning"] <- TRUE
table(merged_pvga$architecture)

architecture_publisher <- merged_pvga %>%
  filter(compliant == FALSE, architecture == TRUE, !is.na(publisher)) %>%
  count(publisher, sort = TRUE) %>%
  mutate(percent = round(n / sum(n) * 100, digits=2)) %>%
  adorn_totals("row")
architecture_publisher
openxlsx::write.xlsx(as.data.frame(architecture_publisher), 'architecture_publisher.xlsx')


architecture_journals <- merged_pvga %>%
  filter(compliant == FALSE, architecture == TRUE) %>%
  count(journal_title, publisher, sort = TRUE) %>%
  mutate(percent = round(n / sum(n) * 100, digits=2)) %>%
  mutate(cml = 100*cumsum(percent)/sum(percent)) %>%
  adorn_totals("row")
architecture_journals

# 2. Key journals

merged_pvga %>%
  filter(is.na(rank_green)) %>%
  count()









##########
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

#####


uoa_numbers <- units_of_assessment %>%
  filter(uoa != "Missing" & uoa != "") %>% 
  group_by(uoa) %>%
  count() %>%
  mutate(percent = round((n / sum(n)) * 100),0)
openxlsx::write.xlsx(as.data.frame(uoa_numbers), 'uoa_numbers.xlsx')

