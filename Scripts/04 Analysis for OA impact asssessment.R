# PVGA Analysis for impact assessment

# Goal: To produce tables and charts which are part of annex to OA Policy Impact Assessment.

# NB. This only includes analysis which was used in the report - for other analysis (e.g. at a journal level) see superceded code 'PVGA Analysis - in report order'

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

# SET UP STYLES FOR CHARTS etc.----
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

# LIMITATIONS----

# Breakdown of license information 

  # for green policies (excluding Pure Gold since we believe that these are not green policies per se)
all_green_licenses_exc_pure <- merged_pvga %>%
  filter(!is.na(g_license1), journal_type != "Pure Gold") %>%
  count(g_license1, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1)) %>%
  mutate(g_license1 = factor(g_license1, ordered = TRUE, levels = c("cc_by", "cc_by_nd", "cc_by_sa", "cc_by_nc", "cc_by_nc_nd", "cc_by_nc_sa", "bespoke_license", "all_rights_reserved", "no license requirement"))) %>%
  arrange(g_license1) %>%
  adorn_totals("row")

  # for gold policies (excluding Pure Gold since we believe that these are not green policies per se)
all_fee_licenses <- merged_pvga %>%
  filter(!is.na(fee_license1)) %>%
  count(fee_license1, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1)) %>%
  mutate(fee_license1 = factor(fee_license1, ordered = TRUE, levels = c("cc_by", "cc_by_nd", "cc_by_sa", "cc_by_nc", "cc_by_nc_nd", "cc_by_nc_sa", "bespoke_license", "all_rights_reserved", "no license requirement"))) %>%
  arrange(fee_license1) %>%
  adorn_totals("row")

  # bind green and gold
all_licenses <- bind_cols(all_green_licenses_exc_pure, all_fee_licenses)

write.xlsx(all_licenses, file = "Output/Tables/All licenses.xlsx")

# IMPACT OF EACH POLICY SCENARIO----

# Ultimately creating a stacked bar chart comparing actual OA, potential compliance with the current policy and all three potential compliance scenarios (current, funding for all, funding for hybrid with TAs, funding for fully OA only).

  # Current OA status
Open.Access_a <- merged_pvga %>% # article level
  count(Open.Access2) %>%
  mutate(percent = round(n / sum(n) * 100,1), cml = round(cumsum(percent)))

  # Potential compliance with current policy
compliance_current_a <- merged_pvga %>%
  count(compliance_current, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1), cml = round(cumsum(percent)))

  # Potential compliance with Scenario 1
compliance_h_a <- merged_pvga %>%
  count(compliance_new_hybrid, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1), cml = round(cumsum(percent)))

  # Potential compliance with Scenario 2
compliance_new_a <- merged_pvga %>%
  count(compliance_new, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1), cml = round(cumsum(percent)))

  # Potential compliance with Scenario 3
compliance_new_pure_a <- merged_pvga %>%
  count(compliance_new_pure, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1), cml = round(cumsum(percent)))

# Bind together figures for each scenario
policy_impact_a <- bind_rows(Open.Access_a, compliance_current_a, compliance_h_a, compliance_new_a, compliance_new_pure_a) %>%
  mutate(Scenario = "Current\n(actual)") %>%
  unite(Compliance_Route, c('Open.Access2', 'compliance_current', 'compliance_new_hybrid', 'compliance_new', 'compliance_new_pure'), na.rm = TRUE) %>%
  relocate(5,1,2,3,4)

  # Label each scenario and compliance route
policy_impact_a[5:9,'Scenario'] <- "Current\n(potential)"
policy_impact_a[10:14,'Scenario'] <- "S1"
policy_impact_a[15:19,'Scenario'] <- "S2"
policy_impact_a[20:23,'Scenario'] <- "S3"
policy_impact_a[c(1,5,10,15,20), 'Compliance_Route'] <- "Full gold OA"
policy_impact_a[c(2,6,11,16), 'Compliance_Route'] <- "Hybrid gold\n(with TA for S2)"
policy_impact_a[c(3,7,12,17,21), 'Compliance_Route'] <- "Confirmed green OA"
policy_impact_a[c(8,13,18,22), 'Compliance_Route'] <- "Unconfirmed green OA"
policy_impact_a[c(4,9,14,19,23), 'Compliance_Route'] <- "No potential supported\nroute to open access"

policy_impact_a$Compliance_Route <- factor(policy_impact_a$Compliance_Route, ordered = TRUE, levels = c("Full gold OA", "Hybrid gold\n(with TA for S2)", "Confirmed green OA", "Unconfirmed green OA", "No potential supported\nroute to open access")) # this is needed so they appear in order in table/chart

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

# Create summary table to compare scenarios - subset percentages only and
policy_impact_a_summary <- policy_impact_a %>%
  select(-c(n, cml)) %>%
  pivot_wider(names_from = Scenario, values_from = percent) %>%
  arrange(factor(Compliance_Route, levels = c("Full gold OA", "Hybrid gold\n(with TA for S2)", "Confirmed green OA", "Unconfirmed green OA", "No potential supported\nroute to open access")))

policy_impact_a_summary[is.na(policy_impact_a_summary)] <- 0

openxlsx::write.xlsx(as.data.frame(policy_impact_a_summary), 'Output/Tables/policy_impact_a_summary.xlsx')

# BREAKDOWN OF ARTICLES WITHOUT SUPPORTED ROUTE TO OA IN NEW POLICy----
why_unsupported <- merged_pvga %>%
  filter(compliance_new2 == "not compliant") %>%
  count(journal_type, fee_license3, has_ta) %>%
  mutate(percent=round(n/sum(n)*100,1))

write.xlsx(why_unsupported, file = "Output/Tables/why unsupported by new policy.xlsx")


# ACTUAL OPEN ACCESS STATUS----
  # this is using Dimensions variable Open.Access (simplified to Open.Access2 which merges categories to match main UKRI categories)
  # there is a more complex version of this table showing average number of articles per OA type but not used in current version of finding so only including simple version here.

Open.Access_a <- merged_pvga %>% # article level
  count(Open.Access2) %>%
  mutate(percent = round(n / sum(n) * 100),1) %>%
  adorn_totals("row")

openxlsx::write.xlsx(as.data.frame(Open.Access_a), 'Output/Tables/Open.Access.xlsx')


# Actual Open Access x Discipline ----

disciplines <- merged_pvga %>% 
  separate(discipline, into = c("disc1", "disc2", "disc3", "disc4"), sep = "; ") %>%
  pivot_longer(c(disc1, disc2, disc3, disc4), names_to = "disc_all", values_to ="disc")

Open.Access_a_disc <- disciplines %>%
  filter(!is.na(disc)) %>%
  group_by(disc) %>%
  count(Open.Access2, .drop = FALSE) %>%
  mutate(percent = (n / sum(n)) * 100) %>%
  mutate(cml = round(cumsum(percent),1)) %>%
  mutate(percent = round(percent, 1)) %>%
  mutate(disc = factor(disc, ordered = TRUE, levels = c("Arts & Humanities", "Health Sciences", "Life Sciences", "Physical Sciences", "Social Sciences", "Missing")), 
         Open.Access2 = factor(Open.Access2, ordered = TRUE, levels = c("Pure Gold", "Hybrid gold", "Green", "Closed"))) %>% # this is needed to get everything in correct order
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


# VARIATION OF POLICY IMPACT BY GROUP----

# 1. Variation by UKRI funder----
ukri_funders <- merged_pvga %>% 
  separate(ukri_funders, into = c("ukri_funder1", "ukri_funder2", "ukri_funder3", "ukri_funder4","ukri_funder5", "ukri_funder6", "ukri_funder7", "ukri_funder8"), sep = ", ") %>%
  pivot_longer(c(ukri_funder1, ukri_funder2, ukri_funder3, ukri_funder4, ukri_funder5, ukri_funder6, ukri_funder7, ukri_funder8), names_to = "ukri_funder_all", values_to ="ukri_funder")

compliance_new_ukri_funder <- ukri_funders  %>%
  filter(!is.na(ukri_funder)) %>% 
  group_by(ukri_funder) %>%
  count(compliance_new) %>%
  mutate(percent = round(n/sum(n)*100,1)) %>%
  mutate(total_n = sum(n)) %>%
  select(-n) %>%
  pivot_wider(names_from = compliance_new, values_from = c(percent, total_n)) %>%
  mutate(total_supported = sum(across(1:3), na.rm = TRUE)) %>% # I couldn't get rowsum to work - for some weird reason this code seems to requires 0 indexing of columns (i.e. I'm adding columns 2:4 in this line)
  rename(total_n = "total_n_c: pure gold") %>%
  select(-c(8,9,10,11)) %>%
  relocate(1,8)

# Add column to show proportion of each division covered by TA
TA_ukri_funder <- ukri_funders %>%
  filter(!is.na(ukri_funder), journal_type == "Hybrid") %>%
  group_by(ukri_funder) %>%
  count(has_ta, .drop = FALSE) %>%
  mutate(percent = round(n/sum(n)*100,1)) %>%
  filter(has_ta == "yes")

# Merge in with compliance_new_ukri_funder to add TA column
compliance_new_ukri_funder_ta <- bind_cols(compliance_new_ukri_funder, TA_ukri_funder) %>%
  select(-c(9,10,11)) %>%
  rename(percent_hybrid_with_TA = percent)

# Add all articles row (this feels very elaborate but I couldn't think of a simpler way to do it)
compliance_all_articles <- as.data.frame(t(merged_pvga %>%
                                             count(compliance_new) %>%
                                             mutate(percent = round(n/sum(n)*100),1))) %>%
  mutate(total_n = nrow(merged_pvga)) %>%
  slice(3)

compliance_all_articles <- as.data.frame(lapply(compliance_all_articles,as.numeric))
compliance_all_articles <- compliance_all_articles %>%
  mutate(total_supported = sum(across(1:3))) %>%
  relocate(7)

ta_all_hybrid <- merged_pvga %>% filter(journal_type == "Hybrid") %>% count(has_ta) %>% mutate(percent_hybrid_with_TA = round(n/sum(n)*100),1) %>% filter(has_ta == "yes")

compliance_ta_all_articles <- bind_cols(compliance_all_articles, ta_all_hybrid) %>%
  select(-c(8,9,11))

colnames(compliance_ta_all_articles) <- colnames(compliance_new_ukri_funder_ta[2:9])

# Merge in with compliance_new_ukri_funder_ta to create final table
compliance_new_ukri_funder_ta <- bind_rows(compliance_new_ukri_funder_ta, compliance_ta_all_articles)
compliance_new_ukri_funder_ta[12,'ukri_funder'] <- "All articles"

openxlsx::write.xlsx(as.data.frame(compliance_new_ukri_funder_ta), 'Output/Tables/compliance_new_ukri_funder_ta.xlsx')
rm(compliance_new_ukri_funder, compliance_all_articles, ta_all_hybrid, TA_ukri_funder, ukri_funders)

# 2. Variation by discipline ----

disciplines <- merged_pvga %>% 
  separate(discipline, into = c("disc1", "disc2", "disc3", "disc4"), sep = "; ") %>%
  pivot_longer(c(disc1, disc2, disc3, disc4), names_to = "disc_all", values_to ="disc")

compliance_new_disc <- disciplines %>%
  filter(!is.na(disc)) %>%
  group_by(disc) %>%
  count(compliance_new) %>%
  mutate(percent = round(n/sum(n)*100,1)) %>%
  mutate(total_n = sum(n)) %>%
  select(-n) %>%
  pivot_wider(names_from = compliance_new, values_from = c(percent, total_n)) %>%
  mutate(total_supported = sum(across(1:3), na.rm = TRUE)) %>% # I couldn't get rowsum to work - for some weird reason this code seems to requires 0 indexing of columns (i.e. I'm adding columns 2:4 in this line)
  rename(total_n = "total_n_c: pure gold") %>%
  select(-c(8,9,10,11)) %>%
  relocate(1,8)

compliance_new_disc$disc[is.na(compliance_new_disc$disc)] <- "Missing"

    # Add column to show proportion of each division covered by TA
TA_disc <- disciplines %>%
  filter(!is.na(disc), journal_type == "Hybrid") %>%
  group_by(disc) %>%
  count(has_ta, .drop = FALSE) %>%
  mutate(percent = round(n/sum(n)*100),1) %>%
  filter(has_ta == "yes")

    # Merge in with compliance_new_disc to add TA column
compliance_new_disc_ta <- bind_cols(compliance_new_disc, TA_disc) %>%
  select(-c(9,10,11,13)) %>%
  rename(percent_hybrid_with_TA = percent)
  
# Merge in with compliance_new_disc_ta to create final table
compliance_new_disc_ta <- bind_rows(compliance_new_disc_ta, compliance_ta_all_articles)
compliance_new_disc_ta[7,'disc'] <- "All articles"

openxlsx::write.xlsx(as.data.frame(compliance_new_disc_ta), 'Output/Tables/compliance_new_disc_ta.xlsx')

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



# 3. Variation by Fields of Research Division (i.e. subject)----

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
  mutate(total_supported = sum(across(1:3), na.rm = TRUE)) %>% # I couldn't get rowsum to work - for some weird reason this code seems to requires 0 indexing of columns (i.e. I'm adding columns 2:4 in this line)
  rename(total_n = "total_n_c: pure gold") %>%
  select(-c(8,9,10,11)) %>%
  relocate(1,8)

compliance_new_division$division[is.na(compliance_new_division$division)] <- "Missing"

      # Add column to show proportion of each division covered by TA

TA_div <- divisions %>%
  filter(!is.na(division), journal_type == "Hybrid") %>%
  group_by(division) %>%
  count(has_ta, .drop = FALSE) %>%
  mutate(percent = round(n/sum(n)*100),1) %>%
  filter(has_ta == "yes")

  # Merge in with compliance_new_division
compliance_new_division_ta <- bind_cols(compliance_new_division, TA_div) %>%
  select(-c(9,10,11,13)) %>%
  rename(percent_hybrid_with_TA = percent)

# Merge in with all articles row to create final table
compliance_new_division_ta <- bind_rows(compliance_new_division_ta, compliance_ta_all_articles)
compliance_new_division_ta[24,'division'] <- "All articles"

openxlsx::write.xlsx(as.data.frame(compliance_new_division_ta), 'Output/Tables/compliance_new_division_ta.xlsx')


# 3. Variation by Fields of Research Group (i.e. specialism)----
groups <- merged_pvga %>% 
  separate(for_group, into = c("group1", "group2", "group3", "group4"), sep = "; ") %>%
  pivot_longer(c(group1, group2, group3, group4), names_to = "group_all", values_to ="group")

compliance_new_group <- groups %>%
  filter(!is.na(group)) %>%
  group_by(group) %>%
  count(compliance_new) %>%
  mutate(percent = round(n/sum(n)*100,1)) %>%
  mutate(total_n = sum(n)) %>%
  select(-n) %>%
  pivot_wider(names_from = compliance_new, values_from = c(percent, total_n)) %>%
  mutate(total_supported = sum(across(1:3), na.rm = TRUE)) %>% # I couldn't get rowsum to work - for some weird reason this code seems to requires 0 indexing of columns (i.e. I'm adding columns 2:4 in this line)
  rename(total_n = "total_n_c: pure gold") %>%
  select(-c(8,9,10,11)) %>%
  relocate(1,8)

compliance_new_group$group[is.na(compliance_new_group$group)] <- "Missing"

    # Proportion of specialisms covered by TAs
TA_group <- groups %>%
  mutate(has_ta = factor(has_ta)) %>% # this is needed to stop specialisms with 0% TA coverage being dropped
  filter(!is.na(group), journal_type == "Hybrid") %>%
  group_by(group) %>%
  count(has_ta, .drop = FALSE) %>%
  mutate(percent = round(n/sum(n)*100),1) %>%
  filter(has_ta == "yes")

# Merge in with compliance_new_group
compliance_new_group_ta <- bind_cols(compliance_new_group, TA_group) %>%
  select(-c(9,10,11,13)) %>%
  rename(percent_hybrid_with_TA =percent)

# Merge in with all articles row to create final table
compliance_new_group_ta <- bind_rows(compliance_new_group_ta, compliance_ta_all_articles)
compliance_new_group_ta[154,'group'] <- "All articles"

openxlsx::write.xlsx(as.data.frame(compliance_new_group_ta), 'Output/Tables/compliance_new_group_ta.xlsx')


# 4. Variation by Publisher----

#  Top Publishers and oac
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

openxlsx::write.xlsx(as.data.frame(top_publishers_oac), 'Output/Tables/top_publishers_oac.xlsx')


# Non-compliant by publisher

not_compliance_new_publisher <- merged_pvga %>%
  filter(compliance_new2 == "not compliant") %>%
  count(Publisher, sort = TRUE) %>%
  mutate(percent = (n / sum(n)) * 100) %>%
  mutate(cml = round(cumsum(percent),0)) %>%
  mutate(percent = round(percent, 0)) %>%
  adorn_totals("row")
# add in total articles per publisher
articles_per_publisher <- merged_pvga %>%
  count(Publisher, sort = TRUE)

not_compliance_new_publisher <- left_join(not_compliance_new_publisher, articles_per_publisher, by = "Publisher") %>%
  mutate(percent_of_publisher_total = round(n.x / n.y * 100,1)) %>%
  select(-5)
openxlsx::write.xlsx(as.data.frame(not_compliance_new_publisher), 'Output/Tables/not_compliant_new_publisher.xlsx')

# Publisher by discipline
disciplines <- merged_pvga %>% 
  separate(discipline, into = c("disc1", "disc2", "disc3", "disc4"), sep = "; ") %>%
  pivot_longer(c(disc1, disc2, disc3, disc4), names_to = "disc_all", values_to ="disc")

publisher_discipline <- disciplines %>%
  filter(!is.na(disc)) %>%
  group_by(disc) %>%
  count(Publisher) %>%
  arrange(desc(n)) %>%
  arrange(disc) %>%
  mutate(percent = round(n/sum(n)*100,1), cml = cumsum(percent)) %>%
  filter(cml < 80)

openxlsx::write.xlsx(as.data.frame(publisher_discipline), 'Output/Tables/publisher_discipline.xlsx')

# CURRENT SUPPORT FOR GREEN OA IN JOURNAL POLICIES----

# Simple breakdown of green compliance
simple_green <- merged_pvga %>% 
  filter(journal_type != "Pure Gold") %>%
  group_by(g_license3, g_embargo3) %>% 
  count() %>%
  ungroup() %>%
  mutate(percent = round(n / sum(n) * 100,1))

openxlsx::write.xlsx(as.data.frame(simple_green), 'Output/Tables/simple_green_breakdown.xlsx')


# TRANSFORMATIVE AGREEMENTS----

# 1. Impact of new TAs on potential compliance with UKRI policy----

# List of when TAs started
ta_2019 = c("Springer")
ta_2020 = c("European Respiratory Society (ERS)", "IOP Publishing", "IWA Publishing", "Karger Publishers", "Microbiology Society", "Portland Press", "Rockefeller University Press", "SAGE Publications", "Thieme", "Wiley")
ta_2021 = c("American Physiological Society", "Association for Computing Machinery (ACM)", "Bioscientifica", "Brill Academic Publishers", "Cambridge University Press (CUP)", "Cold Spring Harbor Laboratory", "The Company of Biologists", "Future Science Group", "Royal College of General Practitioners", "Royal Irish Academy", "Geological Society of London", "The Royal Society", "De Gruyter", "Oxford University Press (OUP)", "Taylor & Francis", "BMJ", "Royal Society of Chemistry (RSC)") 

# Create new variable to split up TAs
merged_pvga$ta_split <- "No TA"
merged_pvga$ta_split[merged_pvga$Publisher %in% ta_2019] <- "2019"
merged_pvga$ta_split[merged_pvga$Publisher %in% ta_2020] <- "2020"
merged_pvga$ta_split[merged_pvga$Publisher %in% ta_2021] <- "2021"

# Create stacked bar chart comparing potential compliance with several different assumptions about coverage of Transformative Agreements

  # No Transformative Agreement option/ none negotiated
merged_pvga$compliance_new_ta_noTA <- NA
merged_pvga$compliance_new_ta_noTA[merged_pvga$num_fee %in% c(1,4)] <- "c: pure gold"
merged_pvga$compliance_new_ta_noTA[merged_pvga$ta_split == "noTA" & merged_pvga$num_fee %in% c(2,5)] <- "c: hybrid gold with a TA"
merged_pvga$compliance_new_ta_noTA[!merged_pvga$num_fee %in% c(1,4) & merged_pvga$num_new_green %in% c(1,3)] <- "c: confirmed green oa"
merged_pvga$compliance_new_ta_noTA[!merged_pvga$num_fee %in% c(1,4) & !merged_pvga$num_new_green %in% c(1,3) & (merged_pvga$g_embargo == 0 & merged_pvga$g_license1 == "no license requirement")] <- "nc: unconfirmed green oa"
merged_pvga$compliance_new_ta_noTA[is.na(merged_pvga$compliance_new_ta_noTA)] <- "No supported route"

merged_pvga$compliance_new_ta_noTA <- factor(merged_pvga$compliance_new_ta_noTA, ordered = TRUE, levels = c("c: pure gold", "c: hybrid gold with a TA", "c: confirmed green oa", "nc: unconfirmed green oa", "No supported route"))

compliance_new_ta_noTA_a <- merged_pvga %>%
  count(compliance_new_ta_noTA, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1), cml = round(cumsum(percent)))

  # Transformative agreements to end of 2019
merged_pvga$compliance_new_ta_2019 <- NA
merged_pvga$compliance_new_ta_2019[merged_pvga$num_fee %in% c(1,4)] <- "c: pure gold"
merged_pvga$compliance_new_ta_2019[merged_pvga$ta_split == "2019" & merged_pvga$num_fee %in% c(2,5)] <- "c: hybrid gold with a TA"
merged_pvga$compliance_new_ta_2019[!merged_pvga$num_fee %in% c(1,4) & !(merged_pvga$ta_split == "2019" & merged_pvga$num_fee %in% c(2,5)) & merged_pvga$num_new_green %in% c(1,3)] <- "c: confirmed green oa"
merged_pvga$compliance_new_ta_2019[!merged_pvga$num_fee %in% c(1,4) & !(merged_pvga$ta_split == "2019" & merged_pvga$num_fee %in% c(2,5)) & !merged_pvga$num_new_green %in% c(1,3) & (merged_pvga$g_embargo == 0 & merged_pvga$g_license1 == "no license requirement")] <- "nc: unconfirmed green oa"
merged_pvga$compliance_new_ta_2019[is.na(merged_pvga$compliance_new_ta_2019)] <- "No supported route"

merged_pvga$compliance_new_ta_2019 <- factor(merged_pvga$compliance_new_ta_2019, ordered = TRUE, levels = c("c: pure gold", "c: hybrid gold with a TA", "c: confirmed green oa", "nc: unconfirmed green oa", "No supported route"))

compliance_new_ta_2019_a <- merged_pvga %>%
  count(compliance_new_ta_2019, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1), cml = round(cumsum(percent)))


  # Transformative agreements to end of 2020
merged_pvga$compliance_new_ta_2020 <- NA
merged_pvga$compliance_new_ta_2020[merged_pvga$num_fee %in% c(1,4)] <- "c: pure gold"
merged_pvga$compliance_new_ta_2020[merged_pvga$ta_split %in% c("2019","2020") & merged_pvga$num_fee %in% c(2,5)] <- "c: hybrid gold with a TA"
merged_pvga$compliance_new_ta_2020[!merged_pvga$num_fee %in% c(1,4) & !((merged_pvga$ta_split == "2019" | merged_pvga$ta_split == "2020") & merged_pvga$num_fee %in% c(2,5)) & merged_pvga$num_new_green %in% c(1,3)] <- "c: confirmed green oa"
merged_pvga$compliance_new_ta_2020[!merged_pvga$num_fee %in% c(1,4) & !(merged_pvga$ta_split %in% c("2019","2020") & merged_pvga$num_fee %in% c(2,5)) & !merged_pvga$num_new_green %in% c(1,3) & (merged_pvga$g_embargo == 0 & merged_pvga$g_license1 == "no license requirement")] <- "nc: unconfirmed green oa"
merged_pvga$compliance_new_ta_2020[is.na(merged_pvga$compliance_new_ta_2020)] <- "No supported route"

merged_pvga$compliance_new_ta_2020 <- factor(merged_pvga$compliance_new_ta_2020, ordered = TRUE, levels = c("c: pure gold", "c: hybrid gold with a TA", "c: confirmed green oa", "nc: unconfirmed green oa", "No supported route"))

compliance_new_ta_2020_a <- merged_pvga %>%
  count(compliance_new_ta_2020, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1), cml = round(cumsum(percent)))


  # Transformative agreements to end of 2021
merged_pvga$compliance_new_ta_2021 <- NA
merged_pvga$compliance_new_ta_2021[merged_pvga$num_fee %in% c(1,4)] <- "c: pure gold"
merged_pvga$compliance_new_ta_2021[merged_pvga$ta_split %in% c("2019","2020","2021") & merged_pvga$num_fee %in% c(2,5)] <- "c: hybrid gold with a TA"
merged_pvga$compliance_new_ta_2021[!merged_pvga$num_fee %in% c(1,4) & !(merged_pvga$ta_split %in% c("2019","2020","2021") & merged_pvga$num_fee %in% c(2,5)) & merged_pvga$num_new_green %in% c(1,3)] <- "c: confirmed green oa"
merged_pvga$compliance_new_ta_2021[!merged_pvga$num_fee %in% c(1,4) & !(merged_pvga$ta_split %in% c("2019","2020","2021") & merged_pvga$num_fee %in% c(2,5)) & !merged_pvga$num_new_green %in% c(1,3) & (merged_pvga$g_embargo == 0 & merged_pvga$g_license1 == "no license requirement")] <- "nc: unconfirmed green oa"
merged_pvga$compliance_new_ta_2021[is.na(merged_pvga$compliance_new_ta_2021)] <- "No supported route"

merged_pvga$compliance_new_ta_2021 <- factor(merged_pvga$compliance_new_ta_2021, ordered = TRUE, levels = c("c: pure gold", "c: hybrid gold with a TA", "c: confirmed green oa", "nc: unconfirmed green oa", "No supported route"))

compliance_new_ta_2021_a <- merged_pvga %>%
  count(compliance_new_ta_2021, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1), cml = round(cumsum(percent)))


  # If Elsevier negotiated a TA

merged_pvga$compliance_new_ta_Elsevier <- NA
merged_pvga$compliance_new_ta_Elsevier[merged_pvga$num_fee %in% c(1,4)] <- "c: pure gold"
merged_pvga$compliance_new_ta_Elsevier[(merged_pvga$ta_split %in% c("2019","2020","2021") | merged_pvga$Publisher == "Elsevier") & merged_pvga$num_fee %in% c(2,5)] <- "c: hybrid gold with a TA"
merged_pvga$compliance_new_ta_Elsevier[!merged_pvga$num_fee %in% c(1,4) & !((merged_pvga$ta_split %in% c("2019","2020","2021") | merged_pvga$Publisher == "Elsevier") & merged_pvga$num_fee %in% c(2,5)) & merged_pvga$num_new_green %in% c(1,3)] <- "c: confirmed green oa"
merged_pvga$compliance_new_ta_Elsevier[!merged_pvga$num_fee %in% c(1,4) & !((merged_pvga$ta_split %in% c("2019","2020","2021") | merged_pvga$Publisher == "Elsevier") & merged_pvga$num_fee %in% c(2,5)) & !merged_pvga$num_new_green %in% c(1,3) & (merged_pvga$g_embargo == 0 & merged_pvga$g_license1 == "no license requirement")] <- "nc: unconfirmed green oa"
merged_pvga$compliance_new_ta_Elsevier[is.na(merged_pvga$compliance_new_ta_Elsevier)] <- "No supported route"

merged_pvga$compliance_new_ta_Elsevier <- factor(merged_pvga$compliance_new_ta_Elsevier, ordered = TRUE, levels = c("c: pure gold", "c: hybrid gold with a TA", "c: confirmed green oa", "nc: unconfirmed green oa", "No supported route"))

compliance_new_ta_Elsevier_a <- merged_pvga %>%
  count(compliance_new_ta_Elsevier, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1), cml = round(cumsum(percent)))


  # If TA coverage was 100%
merged_pvga$compliance_new_ta_100 <- NA
merged_pvga$compliance_new_ta_100[merged_pvga$num_fee %in% c(1,4)] <- "c: pure gold"
merged_pvga$compliance_new_ta_100[merged_pvga$num_fee %in% c(2,5)] <- "c: hybrid gold with a TA"
merged_pvga$compliance_new_ta_100[!merged_pvga$num_fee %in% c(1,4) & !merged_pvga$num_fee %in% c(2,4) & merged_pvga$num_new_green %in% c(1,3)] <- "c: confirmed green oa"
merged_pvga$compliance_new_ta_100[!merged_pvga$num_fee %in% c(1,4) & !merged_pvga$num_fee %in% c(2,4) & !merged_pvga$num_new_green %in% c(1,3) & (merged_pvga$g_embargo == 0 & merged_pvga$g_license1 == "no license requirement")] <- "nc: unconfirmed green oa"
merged_pvga$compliance_new_ta_100[is.na(merged_pvga$compliance_new_ta_100)] <- "No supported route"

merged_pvga$compliance_new_ta_100 <- factor(merged_pvga$compliance_new_ta_100, ordered = TRUE, levels = c("c: pure gold", "c: hybrid gold with a TA", "c: confirmed green oa", "nc: unconfirmed green oa", "No supported route"))

compliance_new_ta_100_a <- merged_pvga %>%
  count(compliance_new_ta_100, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1), cml = round(cumsum(percent)))

# Bind together articles
ta_impact_a <- bind_rows(compliance_new_ta_noTA_a, compliance_new_ta_2019_a, compliance_new_ta_2020_a, compliance_new_ta_2021_a, compliance_new_ta_Elsevier_a, compliance_new_ta_100_a) %>%
  mutate(ta_coverage = "No TAs") %>%
  unite(Compliance_Route, c('compliance_new_ta_noTA', 'compliance_new_ta_2019', 'compliance_new_ta_2020', 'compliance_new_ta_2021', 'compliance_new_ta_Elsevier', 'compliance_new_ta_100'), na.rm = TRUE) %>%
  relocate(5,1,2,3,4)

ta_impact_a[6:10,'ta_coverage'] <- "TAs by\n2019"
ta_impact_a[11:15,'ta_coverage'] <- "TAs by\n2020"
ta_impact_a[16:20,'ta_coverage'] <- "TAs by\n2021"
ta_impact_a[21:25,'ta_coverage'] <- "+ Elsevier\nTA"
ta_impact_a[26:30,'ta_coverage'] <- "Full TA\ncoverage"
ta_impact_a[c(1,6,11,16,21,26), 'Compliance_Route'] <- "Pure gold"
ta_impact_a[c(2,7,12,17,22,27), 'Compliance_Route'] <- "Hybrid gold with TA"
ta_impact_a[c(3,8,13,18,23,28), 'Compliance_Route'] <- "Confirmed green OA"
ta_impact_a[c(4,9,14,19,24,29), 'Compliance_Route'] <- "Unconfirmed green OA"
ta_impact_a[c(5,10,15,20,25,30), 'Compliance_Route'] <- "No supported route"

ta_impact_a$Compliance_Route <- factor(ta_impact_a$Compliance_Route, ordered = TRUE, levels = c("Pure gold", "Hybrid gold with TA", "Confirmed green OA", "Unconfirmed green OA", "No supported route"))
ta_impact_a$ta_coverage <- factor(ta_impact_a$ta_coverage, ordered = TRUE, levels = c("No TAs", "TAs by\n2019", "TAs by\n2020", "TAs by\n2021", "+ Elsevier\nTA", "Full TA\ncoverage"))

openxlsx::write.xlsx(as.data.frame(ta_impact_a), 'Output/Tables/ta_impact_a.xlsx')

# Create stacked bar chart for articles

ta_impact_a_bar <- ggplot(ta_impact_a, aes(fill=Compliance_Route, y=percent, x=ta_coverage)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("#F08900", "#FBBB10", "#16978A", "#99d8c9", "#FF5A5A"), name = "Route to Compliance") +
  theme(axis.title.x = element_text(face="bold", size=14, margin = margin(t=10)),
        axis.text.x  = element_text(vjust=0.5, size=14, angle = 90),
        axis.title.y = element_text(face="bold", size=14, margin = margin(t=10)),
        legend.text = element_text(size=14)) +
  labs(x="Introduction of new TAs", y= "% of articles")

ggsave(ta_impact_a_bar, filename = "Output/Charts/ta_impact_a_bar.png")
