library(tidyverse)

df <- read_csv("data/pnad_q1_2018_2019_2020_matched_v2.csv")

df <- df %>% select(hh_id, gender, year, quarter, uf,
                    upa, estrato, sampling_group, urban,
                    geo_area, projected_pop, ppl_in_hh,
                    v2003, role_in_hh, age, race,
                    can_read_write, goes_to_school,
                    school, edu_level, what_grade,
                    completed_college, attended_school,
                    hi_lvl_attended, hi_grade_completed,
                    hi_lvl_attended_complete, habitual,
                    efetivo, goes_to_school_dum_y1,
                    not_goes_to_school_y1,
                    goes_to_school_dum_y2,
                    not_goes_to_school_y2,
                    hi_edu_level_hh_head,
                    tot_income) %>% 
  mutate(age2 = age*age, income_pc=tot_income / ppl_in_hh, 
         dropout=ifelse(goes_to_school_dum_y1==1,ifelse(not_goes_to_school_y2==1, 1,0),0))

probit1 <- df %>% filter(year %in% c(2019, 2018)) %>%
  glm(dropout ~ gender+uf+urban+ppl_in_hh+age+age2+race+income_pc+hi_edu_level_hh_head, family = binomial(link = "probit"))