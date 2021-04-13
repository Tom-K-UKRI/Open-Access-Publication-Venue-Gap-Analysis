# Additional analyses of transformative agreements

# Goal of this analysis is to really pull out the impact of TAs - method is to look at what would happen with no deals, then deals negotiated by 2017, 2018, 2019, 2020, 2021. Then add in other potential deals e.g. Elsevier

# Prerequisite - you need merged_pvga which is produced using code 'Preparing and merging PVGA data' or you can import it as described below

# Clear work space
rm(list=ls())

#library(readr)
library(tidyverse)
library(openxlsx)

#XXXXXXXXX
# Import data----
load("Data/merged_pvga.Rda")


# IMPACT OF DIFFERENT COVERAGE OF TAs ON ARTICLES & JOURNALS----

# List of when TAs started
ta_2019 = c("Springer")
ta_2020 = c("European Respiratory Society (ERS)", "IOP Publishing", "IWA Publishing", "Karger Publishers", "Microbiology Society", "Portland Press", "Rockefeller University Press", "SAGE Publications", "Thieme", "Wiley")
ta_2021 = c("American Physiological Society", "Association for Computing Machinery (ACM)", "Bioscientifica", "Brill Academic Publishers", "Cambridge University Press (CUP)", "Cold Spring Harbor Laboratory", "The Company of Biologists", "Future Science Group", "Royal College of General Practitioners", "Royal Irish Academy", "Geological Society of London", "The Royal Society", "De Gruyter") 

# Create new variable to split up TAs
merged_pvga$ta_split <- "No TA"
merged_pvga$ta_split[merged_pvga$publisher %in% ta_2019] <- "2019"
merged_pvga$ta_split[merged_pvga$publisher %in% ta_2020] <- "2020"
merged_pvga$ta_split[merged_pvga$publisher %in% ta_2021] <- "2021"
merged_pvga$ta_split[merged_pvga$publisher == "Oxford University Press (OUP)"] <- "Oxford University Press (OUP)"
merged_pvga$ta_split[merged_pvga$publisher == "Elsevier"] <- "Elsevier"
# merged_pvga$ta_split[merged_pvga$publisher == "American Chemical Society (ACS)"] <- "American Chemical Society (ACS)"
# merged_pvga$ta_split[merged_pvga$publisher == "Taylor & Francis"] <- "Taylor & Francis"
# merged_pvga$ta_split[merged_pvga$publisher == "American Physical Society (APS)"] <- "American Physical Society (APS)"

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

compliance_new_ta_noTA_j <- merged_pvga %>%
  filter(!duplicated(journal_title)) %>%
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

compliance_new_ta_2019_j <- merged_pvga %>%
  filter(!duplicated(journal_title)) %>%
  count(compliance_new_ta_2019, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1), cml = round(cumsum(percent)))

  # Transformative agreements to end of 2020
merged_pvga$compliance_new_ta_2020 <- NA
merged_pvga$compliance_new_ta_2020[merged_pvga$num_fee %in% c(1,4)] <- "c: pure gold"
merged_pvga$compliance_new_ta_2020[(merged_pvga$ta_split == "2019" | merged_pvga$ta_split == "2020") & merged_pvga$num_fee %in% c(2,5)] <- "c: hybrid gold with a TA"
merged_pvga$compliance_new_ta_2020[!merged_pvga$num_fee %in% c(1,4) & !((merged_pvga$ta_split == "2019" | merged_pvga$ta_split == "2020") & merged_pvga$num_fee %in% c(2,5)) & merged_pvga$num_new_green %in% c(1,3)] <- "c: confirmed green oa"
merged_pvga$compliance_new_ta_2020[!merged_pvga$num_fee %in% c(1,4) & !((merged_pvga$ta_split == "2019" | merged_pvga$ta_split == "2020") & merged_pvga$num_fee %in% c(2,5)) & !merged_pvga$num_new_green %in% c(1,3) & (merged_pvga$g_embargo == 0 & merged_pvga$g_license1 == "no license requirement")] <- "nc: unconfirmed green oa"
merged_pvga$compliance_new_ta_2020[is.na(merged_pvga$compliance_new_ta_2020)] <- "No supported route"

merged_pvga$compliance_new_ta_2020 <- factor(merged_pvga$compliance_new_ta_2020, ordered = TRUE, levels = c("c: pure gold", "c: hybrid gold with a TA", "c: confirmed green oa", "nc: unconfirmed green oa", "No supported route"))

compliance_new_ta_2020_a <- merged_pvga %>%
  count(compliance_new_ta_2020, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1), cml = round(cumsum(percent)))

compliance_new_ta_2020_j <- merged_pvga %>%
  filter(!duplicated(journal_title)) %>%
  count(compliance_new_ta_2020, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1), cml = round(cumsum(percent)))


  # Transformative agreements to end of 2021
merged_pvga$compliance_new_ta_2021 <- NA
merged_pvga$compliance_new_ta_2021[merged_pvga$num_fee %in% c(1,4)] <- "c: pure gold"
merged_pvga$compliance_new_ta_2021[(merged_pvga$ta_split == "2019" | merged_pvga$ta_split == "2020" | merged_pvga$ta_split == "2021") & merged_pvga$num_fee %in% c(2,5)] <- "c: hybrid gold with a TA"
merged_pvga$compliance_new_ta_2021[!merged_pvga$num_fee %in% c(1,4) & !((merged_pvga$ta_split == "2019" | merged_pvga$ta_split == "2020" | merged_pvga$ta_split == "2021") & merged_pvga$num_fee %in% c(2,5)) & merged_pvga$num_new_green %in% c(1,3)] <- "c: confirmed green oa"
merged_pvga$compliance_new_ta_2021[!merged_pvga$num_fee %in% c(1,4) & !((merged_pvga$ta_split == "2019" | merged_pvga$ta_split == "2020" | merged_pvga$ta_split == "2021") & merged_pvga$num_fee %in% c(2,5)) & !merged_pvga$num_new_green %in% c(1,3) & (merged_pvga$g_embargo == 0 & merged_pvga$g_license1 == "no license requirement")] <- "nc: unconfirmed green oa"
merged_pvga$compliance_new_ta_2021[is.na(merged_pvga$compliance_new_ta_2021)] <- "No supported route"

merged_pvga$compliance_new_ta_2021 <- factor(merged_pvga$compliance_new_ta_2021, ordered = TRUE, levels = c("c: pure gold", "c: hybrid gold with a TA", "c: confirmed green oa", "nc: unconfirmed green oa", "No supported route"))

compliance_new_ta_2021_a <- merged_pvga %>%
  count(compliance_new_ta_2021, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1), cml = round(cumsum(percent)))

compliance_new_ta_2021_j <- merged_pvga %>%
  filter(!duplicated(journal_title)) %>%
  count(compliance_new_ta_2021, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1), cml = round(cumsum(percent)))


  # If OUP negotiated a TA

merged_pvga$compliance_new_ta_OUP <- NA
merged_pvga$compliance_new_ta_OUP[merged_pvga$num_fee %in% c(1,4)] <- "c: pure gold"
merged_pvga$compliance_new_ta_OUP[(merged_pvga$ta_split == "2016" | merged_pvga$ta_split == "2017" | merged_pvga$ta_split == "2018" | merged_pvga$ta_split == "2019" | merged_pvga$ta_split == "2020" | merged_pvga$ta_split == "2021" | merged_pvga$ta_split %in% c("Oxford University Press (OUP)", "BMJ", "Taylor & Francis")) & merged_pvga$num_fee %in% c(2,5)] <- "c: hybrid gold with a TA"
merged_pvga$compliance_new_ta_OUP[!merged_pvga$num_fee %in% c(1,4) & !((merged_pvga$ta_split == "2016" | merged_pvga$ta_split == "2017" | merged_pvga$ta_split == "2018" | merged_pvga$ta_split == "2019" | merged_pvga$ta_split == "2020" | merged_pvga$ta_split == "2021" | merged_pvga$ta_split %in% c("Oxford University Press (OUP)", "BMJ", "Taylor & Francis")) & merged_pvga$num_fee %in% c(2,5)) & merged_pvga$num_new_green %in% c(1,3)] <- "c: confirmed green oa"
merged_pvga$compliance_new_ta_OUP[!merged_pvga$num_fee %in% c(1,4) & !((merged_pvga$ta_split == "2016" | merged_pvga$ta_split == "2017" | merged_pvga$ta_split == "2018" | merged_pvga$ta_split == "2019" | merged_pvga$ta_split == "2020" | merged_pvga$ta_split == "2021" | merged_pvga$ta_split %in% c("Oxford University Press (OUP)", "BMJ", "Taylor & Francis")) & merged_pvga$num_fee %in% c(2,5)) & !merged_pvga$num_new_green %in% c(1,3) & (merged_pvga$g_embargo == 0 & merged_pvga$g_license1 == "no license requirement")] <- "nc: unconfirmed green oa"
merged_pvga$compliance_new_ta_OUP[is.na(merged_pvga$compliance_new_ta_OUP)] <- "No supported route"

merged_pvga$compliance_new_ta_OUP <- factor(merged_pvga$compliance_new_ta_OUP, ordered = TRUE, levels = c("c: pure gold", "c: hybrid gold with a TA", "c: confirmed green oa", "nc: unconfirmed green oa", "No supported route"))

compliance_new_ta_OUP_a <- merged_pvga %>%
  count(compliance_new_ta_OUP, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1), cml = round(cumsum(percent)))

compliance_new_ta_OUP_j <- merged_pvga %>%
  filter(!duplicated(journal_title)) %>%
  count(compliance_new_ta_OUP, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1), cml = round(cumsum(percent)))

  # If Elsevier negotiated a TA
merged_pvga$compliance_new_ta_Elsevier <- NA
merged_pvga$compliance_new_ta_Elsevier[merged_pvga$num_fee %in% c(1,4)] <- "c: pure gold"
merged_pvga$compliance_new_ta_Elsevier[(merged_pvga$ta_split == "2016" | merged_pvga$ta_split == "2017" | merged_pvga$ta_split == "2018" | merged_pvga$ta_split == "2019" | merged_pvga$ta_split == "2020" | merged_pvga$ta_split == "2021" | merged_pvga$ta_split == "Elsevier" | merged_pvga$ta_split %in% c("Oxford University Press (OUP)", "BMJ", "Taylor & Francis")) & merged_pvga$num_fee %in% c(2,5)] <- "c: hybrid gold with a TA"
merged_pvga$compliance_new_ta_Elsevier[!merged_pvga$num_fee %in% c(1,4) & !((merged_pvga$ta_split == "2016" | merged_pvga$ta_split == "2017" | merged_pvga$ta_split == "2018" | merged_pvga$ta_split == "2019" | merged_pvga$ta_split == "2020" | merged_pvga$ta_split == "2021" | merged_pvga$ta_split == "Elsevier" | merged_pvga$ta_split %in% c("Oxford University Press (OUP)", "BMJ", "Taylor & Francis")) & merged_pvga$num_fee %in% c(2,5)) & merged_pvga$num_new_green %in% c(1,3)] <- "c: confirmed green oa"
merged_pvga$compliance_new_ta_Elsevier[!merged_pvga$num_fee %in% c(1,4) & !((merged_pvga$ta_split == "2016" | merged_pvga$ta_split == "2017" | merged_pvga$ta_split == "2018" | merged_pvga$ta_split == "2019" | merged_pvga$ta_split == "2020" | merged_pvga$ta_split == "2021" | merged_pvga$ta_split == "Elsevier" | merged_pvga$ta_split %in% c("Oxford University Press (OUP)", "BMJ", "Taylor & Francis")) & merged_pvga$num_fee %in% c(2,5)) & !merged_pvga$num_new_green %in% c(1,3) & (merged_pvga$g_embargo == 0 & merged_pvga$g_license1 == "no license requirement")] <- "nc: unconfirmed green oa"
merged_pvga$compliance_new_ta_Elsevier[is.na(merged_pvga$compliance_new_ta_Elsevier)] <- "No supported route"

merged_pvga$compliance_new_ta_Elsevier <- factor(merged_pvga$compliance_new_ta_Elsevier, ordered = TRUE, levels = c("c: pure gold", "c: hybrid gold with a TA", "c: confirmed green oa", "nc: unconfirmed green oa", "No supported route"))

compliance_new_ta_Elsevier_a <- merged_pvga %>%
  count(compliance_new_ta_Elsevier, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1), cml = round(cumsum(percent)))

compliance_new_ta_Elsevier_j <- merged_pvga %>%
  filter(!duplicated(journal_title)) %>%
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

compliance_new_ta_100_j <- merged_pvga %>%
  filter(!duplicated(journal_title)) %>%
  count(compliance_new_ta_100, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1), cml = round(cumsum(percent)))


  # Bind together articles
ta_impact_a <- bind_rows(compliance_new_ta_noTA_a, compliance_new_ta_2019_a, compliance_new_ta_2020_a, compliance_new_ta_2021_a, compliance_new_ta_OUP_a, compliance_new_ta_Elsevier_a, compliance_new_ta_100_a) %>%
  mutate(ta_coverage = "No TAs") %>%
  unite(Compliance_Route, c('compliance_new_ta_noTA', 'compliance_new_ta_2019', 'compliance_new_ta_2020', 'compliance_new_ta_2021', 'compliance_new_ta_OUP', 'compliance_new_ta_Elsevier', 'compliance_new_ta_100'), na.rm = TRUE) %>%
  relocate(5,1,2,3,4)

ta_impact_a[6:10,'ta_coverage'] <- "TAs by\n2019"
ta_impact_a[11:15,'ta_coverage'] <- "TAs by\n2020"
ta_impact_a[16:20,'ta_coverage'] <- "TAs by\n2021"
ta_impact_a[21:25,'ta_coverage'] <- "OUP, BMJ,\nT&F TAs"
ta_impact_a[26:30,'ta_coverage'] <- "+ Elsevier\nTA"
ta_impact_a[31:35,'ta_coverage'] <- "Full TA\ncoverage"
ta_impact_a[c(1,6,11,16,21,26,31), 'Compliance_Route'] <- "Pure gold"
ta_impact_a[c(2,7,12,17,22,27,32), 'Compliance_Route'] <- "Hybrid gold with TA"
ta_impact_a[c(3,8,13,18,23,28,33), 'Compliance_Route'] <- "Confirmed green OA"
ta_impact_a[c(4,9,14,19,24,29,34), 'Compliance_Route'] <- "Unconfirmed green OA"
ta_impact_a[c(5,10,15,20,25,30,35), 'Compliance_Route'] <- "No supported route"

ta_impact_a$Compliance_Route <- factor(ta_impact_a$Compliance_Route, ordered = TRUE, levels = c("Pure gold", "Hybrid gold with TA", "Confirmed green OA", "Unconfirmed green OA", "No supported route"))
ta_impact_a$ta_coverage <- factor(ta_impact_a$ta_coverage, ordered = TRUE, levels = c("No TAs", "TAs by\n2019", "TAs by\n2020", "TAs by\n2021", "OUP, BMJ,\nT&F TAs", "+ Elsevier\nTA", "Full TA\ncoverage"))

openxlsx::write.xlsx(as.data.frame(ta_impact_a), 'Output/Tables/ta_impact_a.xlsx')

# Create stacked bar chart for articles

ta_impact_a_bar <- ggplot(ta_impact_a, aes(fill=Compliance_Route, y=percent, x=ta_coverage)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("#F08900", "#FBBB10", "#16978A", "#99d8c9", "#FF5A5A"), name = "Route to Compliance") +
  # ggtitle("Impact of new Transformative Agreements on \nsupported OA routes in new UKRI policy") +
  theme(axis.title.x = element_text(face="bold", size=14, margin = margin(t=10)),
        axis.text.x  = element_text(vjust=0.5, size=14, angle = 90),
        title =  element_text(face="bold", size=16),
        legend.text = element_text(size=14)) +
  labs(x="Introduction of new TAs", y= "% of articles")

ggsave(ta_impact_a_bar, filename = "Output/Charts/ta_impact_a_bar.png")



# Bind together journals
ta_impact_j <- bind_rows(compliance_new_ta_noTA_j, compliance_new_ta_2019_j, compliance_new_ta_2020_j, compliance_new_ta_2021_j, compliance_new_ta_OUP_j, compliance_new_ta_Elsevier_j, compliance_new_ta_100_j) %>%
  mutate(ta_coverage = "No TAs") %>%
  unite(Compliance_Route, c('compliance_new_ta_noTA', 'compliance_new_ta_2019', 'compliance_new_ta_2020', 'compliance_new_ta_2021', 'compliance_new_ta_OUP', 'compliance_new_ta_Elsevier', 'compliance_new_ta_100'), na.rm = TRUE) %>%
  relocate(5,1,2,3,4)

ta_impact_j[6:10,'ta_coverage'] <- "TAs by\n2019"
ta_impact_j[11:15,'ta_coverage'] <- "TAs by\n2020"
ta_impact_j[16:20,'ta_coverage'] <- "TAs by\n2021"
ta_impact_j[21:25,'ta_coverage'] <- "OUP TA"
ta_impact_j[26:30,'ta_coverage'] <- "Elsevier &\nOUP TAs"
ta_impact_j[31:35,'ta_coverage'] <- "Full TA\ncoverage"
ta_impact_j[c(1,6,11,16,21,26,31), 'Compliance_Route'] <- "Pure gold"
ta_impact_j[c(2,7,12,17,22,27,32), 'Compliance_Route'] <- "Hybrid gold with TA"
ta_impact_j[c(3,8,13,18,23,28,33), 'Compliance_Route'] <- "Confirmed green OA"
ta_impact_j[c(4,9,14,19,24,29,34), 'Compliance_Route'] <- "Unconfirmed green OA"
ta_impact_j[c(5,10,15,20,25,30,35), 'Compliance_Route'] <- "No supported route"

ta_impact_j$Compliance_Route <- factor(ta_impact_j$Compliance_Route, ordered = TRUE, levels = c("Pure gold", "Hybrid gold with TA", "Confirmed green OA", "Unconfirmed green OA", "No supported route"))
ta_impact_j$ta_coverage <- factor(ta_impact_j$ta_coverage, ordered = TRUE, levels = c("No TAs", "TAs by\n2019", "TAs by\n2020", "TAs by\n2021", "OUP TA", "Elsevier &\nOUP TAs", "Full TA\ncoverage"))

openxlsx::write.xlsx(as.data.frame(ta_impact_j), 'Output/Tables/ta_impact_j.xlsx')

# Create stacked bar chart for articles

ta_impact_j_bar <- ggplot(ta_impact_j, aes(fill=Compliance_Route, y=percent, x=ta_coverage)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("#F08900", "#FBBB10", "#16978A", "#99d8c9", "#FF5A5A"), name = "Route to Compliance") +
  # ggtitle("Impact of new Transformative Agreements on \npotential compliance with new UKRI policy") +
  theme(axis.title.x = element_text(face="bold", size=14, margin = margin(t=10)),
        axis.text.x  = element_text(vjust=0.5, size=14, angle = 90),
        title =  element_text(face="bold", size=16),
        legend.text = element_text(size=14)) +
  labs(x="Introduction of new TAs", y= "% of articles")

ggsave(ta_impact_j_bar, filename = "Output/Charts/ta_impact_j_bar.png")



# MARCH WORK----

# Pie chart breaking down all publications by proportion compliant at each stage of TA development----

# First split up TAs into groups


merged_pvga$ta_split <- "No TA"
merged_pvga$ta_split[merged_pvga$publisher %in% ta_2019] <- "2019"
merged_pvga$ta_split[merged_pvga$publisher %in% ta_2020] <- "2020"
merged_pvga$ta_split[merged_pvga$publisher %in% ta_2021] <- "2021"
merged_pvga$ta_split[merged_pvga$publisher == "Oxford University Press (OUP)"] <- "Oxford University Press (OUP)"
merged_pvga$ta_split[merged_pvga$publisher == "Elsevier"] <- "Elsevier"

# Stage 1: No TAs so compliance only through full OA and confirmed green
merged_pvga$gold_noTA <- NA
merged_pvga$gold_noTA[merged_pvga$num_fee %in% c(1,4)] <- "Supported Gold OA"
merged_pvga$gold_noTA[is.na(merged_pvga$gold_noTA)] <- "No supported Gold OA"

merged_pvga$gold_noTA <- factor(merged_pvga$gold_noTA, ordered = TRUE, levels = c("Supported Gold OA", "No supported Gold OA"))

gold_noTA_a <- merged_pvga %>%
  count(gold_noTA, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1), cml = round(cumsum(percent)))

gold_noTA_j <- merged_pvga %>%
  filter(!duplicated(journal_title)) %>%
  count(gold_noTA, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1), cml = round(cumsum(percent)))

    # publisher by number of non-compliant articles
publishers_noTA <- merged_pvga %>%
  filter(compliance_new_pure == "not compliant" | compliance_new_pure == "nc: unconfirmed green oa") %>%
  count(publisher, sort = TRUE) %>%
  mutate(percent = round(n / sum(n) * 100,1), cml=cumsum(percent))

# Stage 2: Current set of TAs
ta_current = c(paste(esac$Publisher)) # this requires running the ESAC code in Preparing and Merging PVGA data, so I have also pasted the string below (as of 12.03.21))
#ta_current = c("American Physiological Society", "Association for Computing Machinery (ACM)", "Bioscientifica", "BMJ", "Brill Academic Publishers", "Cambridge University Press (CUP)", "Cold Spring Harbor Laboratory", "The Company of Biologists", "European Respiratory Society (ERS)", "Future Science Group", "IOP Publishing", "IWA Publishing", "Karger Publishers", "Microbiology Society", "Oxford University Press (OUP)", "Portland Press", "Rockefeller University Press", "Royal College of General Practitioners", "Royal Irish Academy", "SAGE Publications", "Springer", "Geological Society of London", "The Royal Society", "Thieme", "De Gruyter", "Wiley", "Taylor & Francis", "Royal Society of Chemistry (RSC)")

merged_pvga$gold_currentTA <- NA
merged_pvga$gold_currentTA[merged_pvga$num_fee %in% c(1,4)] <- "Supported Gold OA"
merged_pvga$gold_currentTA[merged_pvga$num_fee %in% c(2,5) & merged_pvga$publisher %in% ta_current] <- "Supported Gold OA"
merged_pvga$gold_currentTA[is.na(merged_pvga$gold_currentTA)] <- "No supported Gold OA"

merged_pvga$gold_currentTA <- factor(merged_pvga$gold_currentTA, ordered = TRUE, levels = c("Supported Gold OA", "No supported Gold OA"))

gold_currentTA_a <- merged_pvga %>%
  count(gold_currentTA, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1), cml = round(cumsum(percent)))

gold_currentTA_j <- merged_pvga %>%
  filter(!duplicated(journal_title)) %>%
  count(gold_currentTA, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1), cml = round(cumsum(percent)))

# publisher by number of non-compliant articles
publishers_currentTA <- merged_pvga %>%
  filter(compliance_new2 == "not compliant") %>%
  count(publisher, sort = TRUE) %>%
  mutate(percent = round(n / sum(n) * 100,1), cml=cumsum(percent))

# stage 3: TAs targeted to be in place by end of 2021
ta_targets = c("Elsevier", "American Chemical Society (ACS)", "Wolters Kluwer", "Nature")

merged_pvga$gold_targetsTA <- NA
merged_pvga$gold_targetsTA[merged_pvga$num_fee %in% c(1,4)] <- "Supported Gold OA"
merged_pvga$gold_targetsTA[merged_pvga$num_fee %in% c(2,5) & merged_pvga$publisher %in% c(ta_current, "Elsevier")] <- "Supported Gold OA"
merged_pvga$gold_targetsTA[is.na(merged_pvga$gold_targetsTA)] <- "No supported Gold OA"

merged_pvga$gold_targetsTA <- factor(merged_pvga$gold_targetsTA, ordered = TRUE, levels = c("Supported Gold OA", "No supported Gold OA"))

gold_targetsTA_a <- merged_pvga %>%
  count(gold_targetsTA, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1), cml = round(cumsum(percent)))

gold_targetsTA_j <- merged_pvga %>%
  filter(!duplicated(journal_title)) %>%
  count(gold_targetsTA, .drop = FALSE) %>%
  mutate(percent = round(n / sum(n) * 100,1), cml = round(cumsum(percent)))

# publisher by number of non-compliant articles
publishers_targetsTA <- merged_pvga %>%
  filter(gold_targetsTA == "No supported Gold OA" & !num_new_green %in% c(1,3)) %>%
  count(publisher, sort = TRUE) %>%
  mutate(percent = round(n / sum(n) * 100,1), cml=cumsum(percent))

# Subject example: History - no TAs, before recent TAs, recent TAs, target TAs----

publishers_history <- merged_pvga %>%
  filter(grepl("History", for_division)) %>%
  count(Publisher, sort = TRUE) %>%
  arrange(desc(n)) %>%
  mutate(percent = round(n / sum(n) * 100,1), cml=cumsum(percent))


# Output all tables to excel
list_of_datasets <- list("Gold no TA" = gold_noTA_a, "Pubs no TA" = publishers_noTA,  "Gold current TAs" = gold_currentTA_a, "Pubs current TAs" = publishers_currentTA,   "Gold target TAs" = gold_targetsTA_a, "Pubs target TAs" = publishers_targetsTA)
write.xlsx(list_of_datasets, file = "Output/TA tables_raw.xlsx", append = TRUE)

