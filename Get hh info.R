library(tidyverse)
source("Utils.R")

# --------------------- Create variable for household income ---------------------
df18q1<- get_data_for_quarter(2018, 1) %>%
  get_dummies() %>% aggregate_sum()
df19q1<- get_data_for_quarter(2019, 1) %>%
  get_dummies() %>% aggregate_sum()
df20q1<- get_data_for_quarter(2020, 1) %>%
  get_dummies() %>% aggregate_sum()
df21q1<- get_data_for_quarter(2021, 1) %>%
  get_dummies() %>% aggregate_sum()

# For each household we estimate the total household income as follows:
#  - For each household, we have the number earners in each income bracket
# To estimate total household income we proceed as follows:
# For each household, we multiply the number of people that are in a given income bracket 
# by the value in the middle of the range of the given income bracket. Then, we 
# add up the values that we for each bracket for a given household.

df18q1 <- df18q1 %>% select(hh_id, Ano, Trimestre, income_1,
                            income_2, income_3, income_4,
                            income_5, income_6, income_7,
                            income_8) %>%
  filter(hh_id %in% df19q1$hh_id) %>%
  mutate(tot_income=(income_1*0.25+income_2*0.75+income_3*1.5
                     +income_4*2.5+income_5*4+income_6*7.5
                     +income_7*15+income_8*20)
         )
df19q1 <- df19q1 %>% select(hh_id, Ano, Trimestre, income_1,
                            income_2, income_3, income_4,
                            income_5, income_6, income_7,
                            income_8) %>%
  filter(hh_id %in% df20q1$hh_id) %>%
  mutate(tot_income=(income_1*0.25+income_2*0.75+income_3*1.5
                     +income_4*2.5+income_5*4+income_6*7.5
                     +income_7*15+income_8*20)
  )
df20q1 <- df20q1 %>% select(hh_id, Ano, Trimestre, income_1,
                            income_2, income_3, income_4,
                            income_5, income_6, income_7,
                            income_8) %>%
  filter(hh_id %in% df21q1$hh_id) %>%
  mutate(tot_income=(income_1*0.25+income_2*0.75+income_3*1.5
                     +income_4*2.5+income_5*4+income_6*7.5
                     +income_7*15+income_8*20)
  )

write.csv(bind_rows(df18q1, df19q1, df20q1), "data/pnad_income_2018_2019_2020.csv")
rm(df18q1, df19q1, df20q1, df21q1)

# --------------------- Get info on education level of head of the household ---------------------
df18q1<- get_data_for_quarter(2018, 1) 
df19q1<- get_data_for_quarter(2019, 1) 
df20q1<- get_data_for_quarter(2020, 1)
df21q1<- get_data_for_quarter(2021, 1)

df18q1 <- df18q1 %>% filter(
  hh_id %in% df19q1$hh_id,
  role_in_hh == "Pessoa responsável pelo domicílio") %>%
  mutate(hi_edu_level_hh_head=hi_lvl_attended) %>%
  select(hh_id, Ano, Trimestre, hi_edu_level_hh_head)
df19q1 <- df19q1 %>% filter(
  hh_id %in% df20q1$hh_id,
  role_in_hh == "Pessoa responsável pelo domicílio") %>%
  mutate(hi_edu_level_hh_head=hi_lvl_attended) %>%
  select(hh_id, Ano, Trimestre, hi_edu_level_hh_head)
df20q1 <- df20q1 %>% filter(
  hh_id %in% df21q1$hh_id,
  role_in_hh == "Pessoa responsável pelo domicílio") %>%
  mutate(hi_edu_level_hh_head=hi_lvl_attended) %>%
  select(hh_id, Ano, Trimestre, hi_edu_level_hh_head)

write.csv(bind_rows(df18q1, df19q1, df20q1), "data/pnad_edu_head_2018_2019_2020.csv")
rm(df18q1, df19q1, df20q1, df21q1)

# --------------------- merge data above to the current dataset ---------------------
edu <- read_csv("data/pnad_edu_head_2018_2019_2020.csv")
inc <- read_csv("data/pnad_income_2018_2019_2020.csv")
df <- read_csv("data/pnad_q1_2018_2019_2020_matched.csv")

df <- merge(df, edu, by="hh_id")
df <- merge(df, inc, by="hh_id")

write.csv(df, "data/pnad_q1_2018_2019_2020_matched_v2.csv")

rm(df, inc, edu)