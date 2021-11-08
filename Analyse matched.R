library(tidyverse)
library(PNADcIBGE)

# ------------- Analyse matched first quarters of 2018-19, 2019-20, 2020-21 -----
df <- read_csv("data/pnad_q1_2018_2019_2020_matched.csv")
df$Gender[df$Gender == "Homem"] <- "Male"
df$Gender[df$Gender == "Mulher"] <- "Female"

df$Year <- as.character(df$Year)

df %>% filter(age %in% c(10,11,12)) %>%
  group_by(Year) %>% 
  summarise("Goes to school y1"=sum(goes_to_school_dum_y1)/n(),
            "Goes to school y2"=sum(goes_to_school_dum_y2)/n(),
            "Diff"=(sum(goes_to_school_dum_y1)-sum(goes_to_school_dum_y2))/n(),
            "Not goes to school y1"=sum(not_goes_to_school_y1)/n(),
            "Not goes to school y2"=sum(not_goes_to_school_y2)/n()) -> tp
tp %>% select("Year", "Goes to school y1", "Goes to school y2") %>%
  melt(id.vars="Year") %>%
  ggplot(aes(x=Year,y=value, fill=variable)) +
  geom_bar(position='dodge', stat='identity', width=0.5) +
  labs(y="Fraction of the sample", colour="Legend", title="Goes to school, kids aged 10-12")

df %>% filter(age %in% c(13,14)) %>%
  group_by(Year) %>% 
  summarise("Goes to school y1"=sum(goes_to_school_dum_y1)/n(),
            "Goes to school y2"=sum(goes_to_school_dum_y2)/n(),
            "Diff"=(sum(goes_to_school_dum_y1)-sum(goes_to_school_dum_y2))/n(),
            "Not goes to school y1"=sum(not_goes_to_school_y1)/n(),
            "Not goes to school y2"=sum(not_goes_to_school_y2)/n()) -> tp
tp %>% select("Year", "Goes to school y1", "Goes to school y2") %>%
  melt(id.vars="Year") %>%
  ggplot(aes(x=Year,y=value, fill=variable)) +
  geom_bar(position='dodge', stat='identity', width=0.5) +
  labs(y="Fraction of sample", colour="Legend", title="Goes to school, kids aged 13 and 14")

df %>% filter(age %in% c(15,16)) %>%
  group_by(Year) %>% 
  summarise("Goes to school y1"=sum(goes_to_school_dum_y1)/n(),
            "Goes to school y2"=sum(goes_to_school_dum_y2)/n(),
            "Diff"=(sum(goes_to_school_dum_y1)-sum(goes_to_school_dum_y2))/n(),
            "Not goes to school y1"=sum(not_goes_to_school_y1)/n(),
            "Not goes to school y2"=sum(not_goes_to_school_y2)/n()) -> tp
tp %>% select("Year", "Goes to school y1", "Goes to school y2") %>%
  melt(id.vars="Year") %>%
  ggplot(aes(x=Year,y=value, fill=variable)) +
  geom_bar(position='dodge', stat='identity', width=0.5) +
  labs(y="Fraction of sample", colour="Legend", title="Goes to school, kids aged 15 and 16")

df %>% filter(age %in% c(17,18)) %>%
  group_by(Year) %>% 
  summarise("Goes to school y1"=sum(goes_to_school_dum_y1)/n(),
            "Goes to school y2"=sum(goes_to_school_dum_y2)/n(),
            "Diff"=(sum(goes_to_school_dum_y1)-sum(goes_to_school_dum_y2))/n(),
            "Not goes to school y1"=sum(not_goes_to_school_y1)/n(),
            "Not goes to school y2"=sum(not_goes_to_school_y2)/n()) -> tp
tp %>% select("Year", "Goes to school y1", "Goes to school y2") %>%
  melt(id.vars="Year") %>%
  ggplot(aes(x=Year,y=value, fill=variable)) +
  geom_bar(position='dodge', stat='identity', width=0.5) +
  labs(y="Fraction of sample", colour="Legend", title="Goes to school, kids aged 17 and 18")

df %>% filter(age > 18) %>%
  group_by(Year) %>% 
  summarise("Goes to school y1"=sum(goes_to_school_dum_y1)/n(),
            "Goes to school y2"=sum(goes_to_school_dum_y2)/n(),
            "Diff"=(sum(goes_to_school_dum_y1)-sum(goes_to_school_dum_y2))/n(),
            "Not goes to school y1"=sum(not_goes_to_school_y1)/n(),
            "Not goes to school y2"=sum(not_goes_to_school_y2)/n()) -> tp
tp %>% select("Year", "Goes to school y1", "Goes to school y2") %>%
  melt(id.vars="Year") %>%
  ggplot(aes(Year,value, col=variable)) +
  geom_line() +
  labs(y="Fraction of sample", colour="Legend", title="Goes to school, people aged >18")

df %>% filter(age %in% c(10,11,12)) %>% group_by(Year, Gender) %>%
  summarise(Diff=(sum(goes_to_school_dum_y1)-sum(goes_to_school_dum_y2))/n()) %>%
  ggplot(aes(Year, Diff, col=Gender)) +
  geom_line() +
  labs(colour="Gender", title="Difference 1st and 5th interview, kids aged 10-12")

df %>% filter(age %in% c(13,14)) %>% group_by(Year, Gender) %>%
  summarise(Diff=(sum(goes_to_school_dum_y1)-sum(goes_to_school_dum_y2))/n()) %>%
  ggplot(aes(Year, Diff, col=Gender)) +
  geom_line() +
  labs(colour="Gender", title="Difference 1st and 5th interview, kids aged 13 and 14")

df %>% filter(age %in% c(15,16)) %>% group_by(Year, Gender) %>%
  summarise(Diff=(sum(goes_to_school_dum_y1)-sum(goes_to_school_dum_y2))/n()) %>%
  ggplot(aes(Year, Diff, col=Gender)) +
  geom_line() +
  labs(colour="Gender", title="Difference 1st and 5th interview, kids aged 15 and 16")

df %>% filter(age %in% c(17,18)) %>% group_by(Year, Gender) %>%
  summarise(Diff=(sum(goes_to_school_dum_y1)-sum(goes_to_school_dum_y2))/n()) %>%
  ggplot(aes(Year, Diff, col=Gender)) +
  geom_line() +
  labs(colour="Gender", title="Difference 1st and 5th interview, kids aged 17 and 18")

# ------------- Analyse matched all quarters of 2018-19, 2019-20, 2020-21 ---------
df <- read_csv("data/pnad_all_2018_2019_2020_matched.csv")
df$Gender[df$Gender == "Homem"] <- "Male"
df$Gender[df$Gender == "Mulher"] <- "Female"

df1 <- df %>% mutate(goes_to_school=goes_to_school_dum_p1, period=1) %>%
  select(Gender, goes_to_school, period, Year, edu_level, age,
                     hh_id, birth_month_n, birth_year_n, birth_day_n)
df2 <- df %>% mutate(goes_to_school=goes_to_school_dum_p2, period=2) %>%
  select(Gender, goes_to_school, period, Year, edu_level, age,
         hh_id, birth_month_n, birth_year_n, birth_day_n)
df3 <- df %>% mutate(goes_to_school=goes_to_school_dum_p3, period=3) %>%
  select(Gender, goes_to_school, period, Year, edu_level, age,
         hh_id, birth_month_n, birth_year_n, birth_day_n)
df4 <- df %>% mutate(goes_to_school=goes_to_school_dum_p4, period=4) %>%
  select(Gender, goes_to_school, period, Year, edu_level, age,
         hh_id, birth_month_n, birth_year_n, birth_day_n)
df5 <- df %>% mutate(goes_to_school=goes_to_school_dum_p5, period=5) %>%
  select(Gender, goes_to_school, period, Year, edu_level, age,
         hh_id, birth_month_n, birth_year_n, birth_day_n)

df <- bind_rows(df1, df2, df3, df4, df5)
rm(df1, df2, df3, df4, df5)

df$Year <- as.character(df$Year)

df %>% filter(age %in% c(10,11,12)) %>% group_by(Year, period, Gender) %>%
  summarise(goes_to_school_frac=sum(goes_to_school)/n()) -> tp
tp %>% filter(Gender=="Female") %>%
  ggplot(aes(period, goes_to_school_frac, col=Year)) +
  geom_line() +
  geom_line(data =tp[tp$Gender=="Male",], linetype="dashed") +
  labs(colour="Gender", title="Kids aged 10-12 (Dashed=Male, Continuous=Female)", y="Fraction of sample")


df %>% filter(age %in% c(13,14)) %>% group_by(Year, period, Gender) %>%
  summarise(goes_to_school_frac=sum(goes_to_school)/n()) -> tp
tp %>% filter(Gender=="Female") %>%
  ggplot(aes(period, goes_to_school_frac, col=Year)) +
  geom_line() +
  geom_line(data =tp[tp$Gender=="Male",], linetype="dashed") +
  labs(colour="Gender", title="Kids aged 13 and 14 (Dashed=Male, Continuous=Female)", y="Fraction of sample")


df %>% filter(age %in% c(15,16)) %>% group_by(Year, period, Gender) %>%
  summarise(goes_to_school_frac=sum(goes_to_school)/n()) -> tp
tp %>% filter(Gender=="Female") %>%
  ggplot(aes(period, goes_to_school_frac, col=Year)) +
  geom_line() +
  geom_line(data =tp[tp$Gender=="Male",], linetype="dashed")+
  labs(colour="Gender", title="Kids aged 15 and 16 (Dashed=Male, Continuous=Female)", y="Fraction of sample")


df %>% filter(age %in% c(17,18)) %>% group_by(Year, period, Gender) %>%
  summarise(goes_to_school_frac=sum(goes_to_school)/n()) -> tp
tp %>% filter(Gender=="Female") %>%
  ggplot(aes(period, goes_to_school_frac, col=Year)) +
  geom_line() +
  geom_line(data =tp[tp$Gender=="Male",], linetype="dashed") +
  labs(colour="Gender", title="Kids aged 17 and 18 (Dashed=Male, Continuous=Female)", y="Fraction of sample")

df %>% filter(age %in% c(13,14)) %>% group_by(Year, period) %>%
  summarise(goes_to_school_frac=sum(goes_to_school)/n()) %>%
  ggplot(aes(period, goes_to_school_frac, col=Year)) +
  geom_line()

df %>% filter(age %in% c(15,16)) %>% group_by(Year, period) %>%
  summarise(goes_to_school_frac=sum(goes_to_school)/n()) %>%
  ggplot(aes(period, goes_to_school_frac, col=Year)) +
  geom_line()

df %>% filter(age %in% c(17,18)) %>% group_by(Year, period) %>%
  summarise(goes_to_school_frac=sum(goes_to_school)/n()) %>%
  ggplot(aes(period, goes_to_school_frac, col=Year)) +
  geom_line()
