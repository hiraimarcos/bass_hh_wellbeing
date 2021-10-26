library(tidyverse)
library(PNADcIBGE)

# ---------- Match first and second quarter of 2020 ----------
q12 <- read_csv("data/pnad_2020_q1q2_individual_bothpresent.csv")
q12$Trimestre <- as.character(q12$Trimestre)
q12$hh_id <- as.character(q12$hh_id)

# limit sample to people aged approx. 10 to 20
q1_20 <- q12 %>% filter(Trimestre == "1") %>% filter(birth_year_n < 2011 & birth_year_n >= 2000)
q2_20 <- q12 %>% filter(Trimestre == "2") %>% filter(birth_year_n < 2011 & birth_year_n >= 2000)
rm(q12)

# here we merge the quarters on the exact birth date, on gender and on hh_id
# we chose to merge on the exact birth date because the difference was small
# if we excluded the day (~40 fewer observations) and it is possible that we
# would double count some people if we excluded the day (if two people were
# born in the same month, which could happen if cousins or relatives live
# in the same household) 
q12_20 <- merge(q1_20, q2_20,
                by=c('hh_id', "birth_month_n", "birth_year_n", "sex", "birth_day_n"),
                suffixes = c('_q1', '_q2'))
q12_20 <- q12_20 %>% mutate(
  dropout = ifelse(goes_to_school_q1=="Sim" & goes_to_school_q2=="Não", 1, 0)
)
rm(q1_20, q2_20)



# ---------- Match first and second quarter of 2019 ----------
q12 <- read_csv("data/pnad_2019_q1q2_individual_bothpresent.csv")
q12$Trimestre <- as.character(q12$Trimestre)
q12$hh_id <- as.character(q12$hh_id)

q1_19 <- q12 %>% filter(Trimestre == "1") %>% filter(birth_year_n < 2011 & birth_year_n >= 2000)
q2_19 <- q12 %>% filter(Trimestre == "2") %>% filter(birth_year_n < 2011 & birth_year_n >= 2000)
rm(q12)

q12_19 <- merge(q1_19, q2_19, 
                by=c('hh_id', "birth_month_n", "birth_year_n", "sex", "birth_day_n"),
                suffixes = c('_q1', '_q2'))
q12_19 <- q12_19 %>% mutate(
  dropout = ifelse(goes_to_school_q1=="Sim" & goes_to_school_q2=="Não", 1, 0)
)
rm(q1_19, q2_19)

q12 <- rbind(q12_19, q12_20) %>% rename(Gender=sex, Quarter=Trimestre_q1, Year=Ano_q1)
q12$Year <- as.character(q12$Year)
q12$hh_id <- as.character(q12$hh_id)
rm(q12_19, q12_20)




# ---------- Analyze dropout first and second quarters ----------

q12  %>% group_by(Year) %>%
  summarise(Dropout_rate=sum(dropout)/n()) %>%
  ggplot(aes(x=Year, y=Dropout_rate)) +
  geom_bar(position='dodge', stat='identity')

q12%>% filter(goes_to_school_q1=="Sim")  %>% group_by(Year) %>%
  summarise(Dropout_rate=sum(dropout)/n()) %>%
  ggplot(aes(x=Year, y=Dropout_rate)) +
  geom_bar(position='dodge', stat='identity')

q12  %>% group_by(Year, Gender) %>%
  summarise(Dropout_rate=sum(dropout)/n()) %>%
  ggplot(aes(x=Year, y=Dropout_rate, fill=Gender)) +
  geom_bar(position='dodge', stat='identity')

q12  %>% group_by(Year, edu_level_q1) %>%
  summarise(Dropout_rate=sum(dropout)/n()) %>%
  ggplot(aes(x=edu_level_q1, y=Dropout_rate, fill=Year)) +
  geom_bar(position='dodge', stat='identity') + 
  theme(axis.text.x = element_text(angle = 65, hjust=1))




# ---------- Match first and third quarter of 2020 ----------
q13 <- read_csv("data/pnad_2020_q1q3_individual_bothpresent.csv")
q13$Trimestre <- as.character(q13$Trimestre)
q13$hh_id <- as.character(q13$hh_id)

# limit sample to people aged approx. 10 to 20
q1_20 <- q13 %>% filter(Trimestre == "1") %>% filter(birth_year_n < 2011 & birth_year_n >= 2000)
q3_20 <- q13 %>% filter(Trimestre == "3") %>% filter(birth_year_n < 2011 & birth_year_n >= 2000)
rm(q13)

q13_20 <- merge(q1_20, q3_20,
                by=c('hh_id', "birth_month_n", "birth_year_n", "sex", "birth_day_n"),
                suffixes = c('_q1', '_q3'))
q13_20 <- q13_20 %>% mutate(
  dropout = ifelse(goes_to_school_q1=="Sim" & goes_to_school_q3=="Não", 1, 0)
)
rm(q1_20, q3_20)

# ---------- Match first and third quarter of 2019 ----------
q13_19 <- read_csv("data/pnad_2019_q1q3_individual_bothpresent.csv")
q13_19$Trimestre <- as.character(q13_19$Trimestre)
q13_19$hh_id <- as.character(q13_19$hh_id)

# limit sample to people aged approx. 10 to 20
q1_19 <- q13_19 %>% filter(Trimestre == "1") %>% filter(birth_year_n < 2011 & birth_year_n >= 2000)
q3_19 <- q13_19 %>% filter(Trimestre == "3") %>% filter(birth_year_n < 2011 & birth_year_n >= 2000)
rm(q13_19)

q13_19 <- merge(q1_19, q3_19,
                by=c('hh_id', "birth_month_n", "birth_year_n", "sex", "birth_day_n"),
                suffixes = c('_q1', '_q3'))
q13_19 <- q13_19 %>% mutate(
  dropout = ifelse(goes_to_school_q1=="Sim" & goes_to_school_q3=="Não", 1, 0)
)
rm(q1_19, q3_19)

q13 <- rbind(q13_19, q13_20) %>% rename(Gender=sex, Quarter=Trimestre_q1, Year=Ano_q1)
q13$Trimestre <- as.character(q13$Trimestre)
q13$hh_id <- as.character(q13$hh_id)
rm(q13_19, q13_20)



# ---------- Analyze dropout first and third quarters ----------

q13 <- q13 %>% rename(Gender=sex, Quarter=Trimestre_q1, Year=Ano_q1)
q13$Year <- as.character(q13$Year)

q13  %>% group_by(Year) %>%
  summarise(Dropout_rate=sum(dropout)/n()) %>%
  ggplot(aes(x=Year, y=Dropout_rate)) +
  geom_bar(position='dodge', stat='identity')

q13%>% filter(goes_to_school_q1=="Sim")  %>% group_by(Year) %>%
  summarise(Dropout_rate=sum(dropout)/n()) %>%
  ggplot(aes(x=Year, y=Dropout_rate)) +
  geom_bar(position='dodge', stat='identity')

q13  %>% group_by(Year, Gender) %>%
  summarise(Dropout_rate=sum(dropout)/n()) %>%
  ggplot(aes(x=Year, y=Dropout_rate, fill=Gender)) +
  geom_bar(position='dodge', stat='identity')

q13  %>% group_by(Year, edu_level_q1) %>%
  summarise(Dropout_rate=sum(dropout)/n()) %>%
  ggplot(aes(x=edu_level_q1, y=Dropout_rate, fill=Year)) +
  geom_bar(position='dodge', stat='identity') + 
  theme(axis.text.x = element_text(angle = 65, hjust=1))

q13  %>% group_by(Year, edu_level_q1) %>%
  summarise(count=n()) %>%
  ggplot(aes(x=edu_level_q1, y=count, fill=Year)) +
  geom_bar(position='dodge', stat='identity') + 
  theme(axis.text.x = element_text(angle = 65, hjust=1))


# ---------- Merge matched data to hh data to run some regressions ----------

q12_20_hh <- read_csv("data/2020_q12_aggregated.csv")
q12_19_hh <- read_csv("data/2019_q12_aggregated.csv")

q12_hh <- rbind(q12_19_hh, q12_20_hh) %>% rename(Year=Ano) %>%
          filter(Trimestre==1) %>%
          select(
            Year, hh_id, num_range("income_", 1:6),
          )
q12_hh$hh_id <- as.character(q12_hh$hh_id)
q12_hh$Year <- as.character(q12_hh$Year)
rm(q12_20_hh, q12_19_hh)

main12 <- left_join(q12, q12_hh, by=c("hh_id", "Year"))
write.csv(main12, "data/matched_2020_2019_q12_hh.csv")





