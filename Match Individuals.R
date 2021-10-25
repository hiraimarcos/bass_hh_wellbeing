library(tidyverse)
library(PNADcIBGE)

q12 <- read_csv("data/pnad_2020_q1q2_individual_bothpresent.csv")
q12$Trimestre <- as.character(q12$Trimestre)
q12$hh_id <- as.character(q12$hh_id)

q1_20 <- q12 %>% filter(Trimestre == "1") %>% filter(birth_year_n < 2011 & birth_year_n >= 2000)
q2_20 <- q12 %>% filter(Trimestre == "2") %>% filter(age < 20 & age >= 10)
rm(q12)

q12_20 <- merge(q1_20, q2_20, by=c('hh_id', "birth_month_n", "birth_year_n", "sex"))
q12_20 <- merge(q1_20, q2_20, by=c('hh_id', "birth_month_n", "birth_year_n", "sex", "birth_day_n"))
rm(q1_20, q2_20)

q12_20 <- q12_20 %>% mutate(dropout = ifelse(x.goes_to_school_y==1 & y.goes_to_school_y==0, 1, 0))

q12 <- read_csv("data/pnad_2019_q1q2_individual_bothpresent.csv")
q12$Trimestre <- as.character(q12$Trimestre)
q12$hh_id <- as.character(q12$hh_id)

q1_19 <- q12 %>% filter(Trimestre == "1") %>% filter(age < 20 & age >= 10)
q2_19 <- q12 %>% filter(Trimestre == "2") %>% filter(age < 20 & age >= 10)
rm(q12)

q12_19 <- merge(q1_19, q2_19, by=c('hh_id', "birth_month_n", "birth_year_n", "sex"))
q12_19 <- merge(q1_19, q2_19, by=c('hh_id', "birth_month_n", "birth_year_n", "sex", "birth_day_n"))