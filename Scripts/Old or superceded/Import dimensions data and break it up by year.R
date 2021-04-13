# Import dimensions data and break it up by year

library(dplyr)
library(openxlsx)


test <- read.csv("Output/all_dim_2015_2020.csv")

# Import data
all_dim_2015_2020_part1 <- read.xlsx("Output/Dim_UKRI_2015_2020 part 1.xlsx")
all_dim_2015_2020_part2 <- read.xlsx("Output/Dim_UKRI_2015_2020 part 2.xlsx")

all_dim_2015_2020 <- bind_rows(all_dim_2015_2020_part1, all_dim_2015_2020_part2)

all_dim_2015_2020_2 <- all_dim_2015_2020 %>%
  select(c('DOI', 'Title', 'Source.title', 'Publisher', 'PubYear', 'Open.Access', 'Publication.Type', 'Authors.Affiliations', 'Research.Organizations.-.standardized', 'Funder', 'Funder.Group', 'FOR.(ANZSRC).Categories', 'Units.of.Assessment')) %>%
  filter(PubYear == "2015" | PubYear == "2016" | PubYear == "2017" | PubYear == "2018" | PubYear == "2019" | PubYear == "2020")
write.xlsx(all_dim_2015_2020_2, "Output/all_dim_2015_2020_2.xlsx")

# Break up by year and save out to excel
all_dim_2015 <- all_dim_2015_2020_2 %>%
  filter(PubYear == "2015")
write.xlsx(all_dim_2015, "Output/all_dim_2015.xlsx")

all_dim_2016 <- all_dim_2015_2020_2 %>%
  filter(PubYear == "2016")
write.xlsx(all_dim_2016, "Output/all_dim_2016.xlsx")

all_dim_2017 <- all_dim_2015_2020_2 %>%
  filter(PubYear == "2017")
write.xlsx(all_dim_2017, "Output/all_dim_2017.xlsx")

all_dim_2018 <- all_dim_2015_2020_2 %>%
  filter(PubYear == "2018")
write.xlsx(all_dim_2018, "Output/all_dim_2018.xlsx")

all_dim_2019 <- all_dim_2015_2020_2 %>%
  filter(PubYear == "2019")
write.xlsx(all_dim_2019, "Output/all_dim_2019.xlsx")

all_dim_2020 <- all_dim_2015_2020_2 %>%
  filter(PubYear == "2020")
write.xlsx(all_dim_2020, "Output/all_dim_2020.xlsx")