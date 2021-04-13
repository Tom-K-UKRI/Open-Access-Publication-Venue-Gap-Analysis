# JISC subscriptions data analysis

library(dplyr)

jisc_subs <- read.csv("Data/All_subscriptions_data_20201222.csv")

# Number of ROs and number of TAs they are signed up to
number_ROs <- jisc_subs %>% count(Organisation.Name)

# Number signed up to each publisher / TA
number_perTA <- jisc_subs %>% count(Publisher..of.product.)

