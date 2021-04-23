# Ad hoc analysis for duncan on non compliance in subjects after Elsevier TA

merged_pvga_elsevierta <- merged_pvga
merged_pvga_elsevierta$has_ta[merged_pvga_elsevierta$Publisher %in% c("Elsevier", "Nature", "Wolters Kluwer", "American Chemical Society (ACS)")] <- "yes"
# merged_pvga_elsevierta$has_ta[merged_pvga_elsevierta$Publisher  == "Elsevier"] <- "yes"

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
  select(-4)

openxlsx::write.xlsx(as.data.frame(not_compliance_new_subject), 'Output/Tables/With all target TAs - not_compliant_new_subject.xlsx')
# openxlsx::write.xlsx(as.data.frame(not_compliance_new_subject), 'Output/Tables/With Elsevier TA - not_compliant_new_subject.xlsx')







# Top 10 publishers by number of UKRI-funded articels
not_compliance_new_publisher <- merged_pvga %>%
  filter(compliance_new2 == "not compliant") %>%
  count(Publisher, sort = TRUE) %>%
  mutate(percent = (n / nrow(merged_pvga)) * 100) %>%
  mutate(cml = round(cumsum(percent),0)) %>%
  adorn_totals("row")
# add in total articles per publisher
articles_per_publisher <- merged_pvga %>%
  count(Publisher, sort = TRUE)

not_compliance_new_publisher <- left_join(not_compliance_new_publisher, articles_per_publisher, by = "Publisher") %>%
  mutate(percent_of_publisher_total = round(n.x / n.y * 100,1)) %>%
  select(-5)
openxlsx::write.xlsx(as.data.frame(not_compliance_new_publisher), 'Output/Tables/not_compliant_new_publisher_duncan.xlsx')
