library(tidyverse)
library(PNADcIBGE)

# get data for fiurst and second quarters of 2020
q12 <- read_csv("pnad_q1q2_individual_bothpresent.csv")
q12$Trimestre <- as.character(q12$Trimestre)

# plot number of people going to school per quarter
q12 %>% group_by(Trimestre, goes_to_school) %>%
  summarise(count=n()) %>%
  ggplot(aes(x=goes_to_school, y=count, fill=Trimestre)) +
  geom_bar(position='dodge', stat='identity')

# plot number of people going to schoo per quarter by sex
q12 %>% group_by(Trimestre, goes_to_school, sex) %>%
  summarise(count=n()) %>%
  filter(goes_to_school=="Sim") %>%
  ggplot(aes(fill=Trimestre, y=count, x=sex)) +
  geom_bar(position='dodge', stat='identity')

# plot number of people going to school per quarter by level of education
q12 %>% group_by(Trimestre, goes_to_school, edu_level) %>%
  summarise(count=n()) %>%
  filter(goes_to_school=="Sim") %>%
  ggplot(aes(fill=Trimestre, y=count, x=edu_level)) +
  geom_bar(position='dodge', stat='identity') + 
  theme(axis.text.x = element_text(angle = 45, hjust=1))

# plot number of people going to school per quarter by geographic area (urban/rural)
q12 %>% group_by(Trimestre, goes_to_school, urban) %>%
  summarise(count=n()) %>%
  filter(goes_to_school=="Sim") %>%
  ggplot(aes(fill=Trimestre, y=count, x=urban)) +
  geom_bar(position='dodge', stat='identity') 

# plot distribution of people per education level
q12 %>% group_by(edu_level)  %>% 
  summarise(count=n()) %>% 
  ggplot(aes(x = edu_level, y = count)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = count), vjust = -0.3) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

# same as above but for first and third quarters of 2020
q13 <- read_csv("pnad_q1q3_individual_bothpresent.csv")
q13$Trimestre <- as.character(q13$Trimestre)

# plot number of people going to school per quarter
q13 %>% group_by(Trimestre, goes_to_school) %>%
  summarise(count=n()) %>%
  ggplot(aes(x=goes_to_school, y=count, fill=Trimestre)) +
  geom_bar(position='dodge', stat='identity')

# plot number of people going to schoo per quarter by sex
q13 %>% group_by(Trimestre, goes_to_school, sex) %>%
  summarise(count=n()) %>%
  filter(goes_to_school=="Sim") %>%
  ggplot(aes(fill=Trimestre, y=count, x=sex)) +
  geom_bar(position='dodge', stat='identity')

# plot number of people going to school per quarter by level of education
q13 %>% group_by(Trimestre, goes_to_school, edu_level) %>%
  summarise(count=n()) %>%
  filter(goes_to_school=="Sim") %>%
  ggplot(aes(fill=Trimestre, y=count, x=edu_level)) +
  geom_bar(position='dodge', stat='identity') + 
  theme(axis.text.x = element_text(angle = 45, hjust=1))

# plot number of people going to school per quarter by geographic area (urban/rural)
q13 %>% group_by(Trimestre, goes_to_school, urban) %>%
  summarise(count=n()) %>%
  filter(goes_to_school=="Sim") %>%
  ggplot(aes(fill=Trimestre, y=count, x=urban)) +
  geom_bar(position='dodge', stat='identity') 

q12_2019 <- read_csv("pnad_q1q2_2019_individual_bothpresent.csv")
q12_2019$Trimestre <- as.character(q12_2019$Trimestre)

q12_2019 %>% group_by(Trimestre, goes_to_school) %>%
  summarise(count=n()) %>%
  ggplot(aes(x=goes_to_school, y=count, fill=Trimestre)) +
  geom_bar(position='dodge', stat='identity')

# plot number of people going to schoo per quarter by sex
q12_2019 %>% group_by(Trimestre, goes_to_school, sex) %>%
  summarise(count=n()) %>%
  filter(goes_to_school=="Sim") %>%
  ggplot(aes(fill=Trimestre, y=count, x=sex)) +
  geom_bar(position='dodge', stat='identity')

# plot number of people going to school per quarter by level of education
q12_2019 %>% group_by(Trimestre, goes_to_school, edu_level) %>%
  summarise(count=n()) %>%
  filter(goes_to_school=="Sim") %>%
  ggplot(aes(fill=Trimestre, y=count, x=edu_level)) +
  geom_bar(position='dodge', stat='identity') + 
  theme(axis.text.x = element_text(angle = 45, hjust=1))

# plot number of people going to school per quarter by geographic area (urban/rural)
q12_2019 %>% group_by(Trimestre, goes_to_school, urban) %>%
  summarise(count=n()) %>%
  filter(goes_to_school=="Sim") %>%
  ggplot(aes(fill=Trimestre, y=count, x=urban)) +
  geom_bar(position='dodge', stat='identity') 


# look at fertility:
fert <- read_csv("pnad_q1_2020_2021.csv")

fert$birth_date <- make_date(fert$birth_year_n, fert$birth_month_n, fert$birth_day_n)
interval_21 <- interval(ymd("2021-01-01"), ymd("2021-03-31"))
interval_20 <- interval(ymd("2020-01-01"), ymd("2020-03-31"))

fert$born_in_quarter <- ifelse(
  (fert$birth_date %within% interval_20 && fert$Ano == 2020) ||
    (fert$birth_date %within% interval_21 && fert$Ano == 2021), 1, 0
)

mean(fert$born_in_quarter)

fert %>% group_by(Ano) %>% summarise(mean = mean(born_in_quarter))