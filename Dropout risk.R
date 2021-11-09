library(tidyverse)
library("broom")

df <- read_csv("data/pnad_q1_2018_2019_2020_matched_v2.csv")

df <- df %>% select(hh_id, Gender, Year, Quarter, UF,
                    UPA, Estrato, sampling_group, urban,
                    geo_area, projected_pop, ppl_in_hh,
                    role_in_hh, age, race,
                    can_read_write, goes_to_school,
                    school, edu_level, what_grade,
                    completed_college, attended_school,
                    hi_lvl_attended, hi_grade_completed,
                    hi_lvl_attended_complete, Habitual,
                    Efetivo, goes_to_school_dum_y1,
                    not_goes_to_school_y1,
                    goes_to_school_dum_y2,
                    not_goes_to_school_y2,
                    hi_edu_level_hh_head,
                    tot_income) %>% 
  mutate(age2 = age*age, income_pc=tot_income / ppl_in_hh, 
         dropout=ifelse(goes_to_school_dum_y1==1,ifelse(not_goes_to_school_y2==1, 1,0),0))

# ----------------- Risk calculations 1 ------------------------
# here we estimate the risk of dropout

# first, select only the non-covid years and only poeople who initially go to school
df1 <- df %>% filter(Year %in% c(2019, 2018), goes_to_school_dum_y1==1)

# estimate a probit model on the non-covid years
probit1 <- glm(dropout ~ Gender+UF+urban+ppl_in_hh+age+age2+race+income_pc+hi_edu_level_hh_head,
      family = binomial(link = "probit"), data=df1)

# select only people who initially go to school, but across all years
newdf1 <- df %>% filter(goes_to_school_dum_y1==1)
newdf1 <- newdf1 %>% mutate(dropout_risk = predict(probit1, newdata = newdf1, se.fit = FALSE, type="response"))

newdf1 %>% filter(!is.na(dropout_risk))%>% group_by(Year) %>% summarise(avg_risk=mean(dropout_risk))

newdf1 %>% filter(!is.na(dropout_risk)) %>%
  mutate(year = as.character(Year)) %>%
  ggplot(aes(x=dropout_risk, color=year)) +
  geom_histogram(alpha=0.5, position="identity", binwidth = 0.05)

rm(newdf1, df1, probit1)
# ----------------- Risk calculations 2 ------------------------
# here we estimate the Probit only on the 2018 data to see if 
# differences arise

df2 <- df %>% filter(Year %in% c(2018), goes_to_school_dum_y1==1)

# estimate a probit model on the non-covid years
probit2 <- glm(dropout ~ Gender+UF+urban+ppl_in_hh+age+age2+race+income_pc+hi_edu_level_hh_head,
               family = binomial(link = "probit"), data=df2)

newdf2 <- df %>% filter(goes_to_school_dum_y1==1)
newdf2 <- newdf2 %>% mutate(dropout_risk = predict(probit2, newdata = newdf2, se.fit = FALSE, type="response"))

newdf2 %>% filter(!is.na(dropout_risk))%>% group_by(Year) %>% summarise(avg_risk=mean(dropout_risk))
newdf2 %>% filter(!is.na(dropout_risk)) %>%
  mutate(year = as.character(Year)) %>%
  ggplot(aes(x=dropout_risk, color=year)) +
  geom_histogram(alpha=0.5, position="identity", binwidth = 0.02)

rm(newdf2, df2, probit2)

# ----------------- Risk calculations 3 ------------------------
# Here we estimate the same model, but or sample is now every person aged 10 to 16

df3 <- df %>% filter(Year %in% c(2018), age >= 10, age <= 16)

# estimate a probit model on the non-covid years
probit3 <- glm(dropout ~ Gender+UF+urban+ppl_in_hh+age+age2+race+income_pc+hi_edu_level_hh_head,
               family = binomial(link = "probit"), data=df3)
probit31 <- glm(goes_to_school_dum_y1 ~ Gender+UF+urban+ppl_in_hh+age+age2+race+income_pc+hi_edu_level_hh_head,
               family = binomial(link = "probit"), data=df3)
probit32 <- glm(goes_to_school_dum_y2 ~ Gender+UF+urban+ppl_in_hh+age+age2+race+income_pc+hi_edu_level_hh_head,
                family = binomial(link = "probit"), data=df3)

newdf3 <- df %>% filter(age >= 10, age <= 16, race!="Ignorado")
newdf3 <- newdf3 %>% mutate(dropout_risk = predict(probit3, newdata = newdf3, se.fit = FALSE, type="response"),
                            goes_to_school_y1_prob = predict(probit31, newdata = newdf3, se.fit = FALSE, type="response"),
                            goes_to_school_y2_prob = predict(probit32, newdata = newdf3, se.fit = FALSE, type="response"))

newdf3 %>% filter(!is.na(dropout_risk))%>% group_by(Year) %>% 
  summarise(avg_risk=mean(dropout_risk),
            Prob_goes_to_school_y1=mean(goes_to_school_y1_prob),
            Prob_goes_to_school_y2=mean(goes_to_school_y2_prob))

newdf3 %>% filter(!is.na(dropout_risk)) %>%
  mutate(year = as.character(Year)) %>%
  ggplot(aes(x=dropout_risk, color=year)) +
  geom_histogram(alpha=0.5, position="identity", binwidth = 0.02)

newdf3 %>% filter(!is.na(goes_to_school_y1_prob)) %>%
  mutate(year = as.character(Year)) %>%
  ggplot(aes(x=goes_to_school_y1_prob, color=year)) +
  geom_histogram(alpha=0.5, position="identity", binwidth = 0.02)

newdf3 %>% filter(!is.na(goes_to_school_y2_prob)) %>%
  mutate(year = as.character(Year)) %>%
  ggplot(aes(x=goes_to_school_y2_prob, color=year)) +
  geom_histogram(alpha=0.5, position="identity", binwidth = 0.02)


rm(newdf3, df3, probit3, probit31, probit32)

