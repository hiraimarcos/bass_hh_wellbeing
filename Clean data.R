library(tidyverse)
source("Utils.R")

# ---------- First and Second quarter of 2020 -----------------
# Get data for the first and second quarter of 2020
q1 <- get_data_for_quarter(2020, 1)
q2 <- get_data_for_quarter(2020, 2)
# save dataset with all of the observations
write.csv(rbind(q1,q2), "data/pnad_2020_q1q2_individual.csv")


# keep observation if household is present in both surveys
q1 <- q1[q1$hh_id %in% q2$hh_id,]
q2 <- q2[q2$hh_id %in% q1$hh_id,]
q12 <- rbind(q1,q2)
write.csv(q12, "data/pnad_2020_q1q2_individual_bothpresent.csv")


# get data aggregated by household:
summed <- q12 %>% get_dummies() %>% aggregate_sum()
firsts <- q12 %>% get_dummies() %>% aggregate_first()
q12_hh <- tibble(merge_hh(firsts, summed))
write.csv(q12_hh, "data/2020_q12_aggregated.csv")
rm(q12, q1, q2, q12_hh, firsts, summed)
gc()

# ---------- First and Third quarter of 2020 -----------------
# Get data to look for difference between first and third quarters of 2020.
# only keep observations that are in both quarters
q1 <- get_data_for_quarter(2020, 1)
q3 <- get_data_for_quarter(2020, 3)

write.csv(rbind(q1,q3), "data/pnad_2020_q1q3_individual.csv")

q1 <- q1[q1$hh_id %in% q3$hh_id,]
q3 <- q3[q3$hh_id %in% q1$hh_id,]
q13 <- rbind(q1,q3)

write.csv(q13, "data/pnad_2020_q1q3_individual_bothpresent.csv")

summed <- q13 %>% get_dummies() %>% aggregate_sum()
firsts <- q13 %>% get_dummies() %>% aggregate_first()
q13_hh <- tibble(merge_hh(firsts, summed))
write.csv(q13_hh, "data/2020_q13_aggregated.csv")
rm(q13, q1, q3, q13_hh, firsts, summed)
gc()

# ---------- First and Second quarter of 2019 -----------------
q1_19 <- get_data_for_quarter(2019, 1)
q2_19 <- get_data_for_quarter(2019, 2)
q1_19 <- q1_19[q1_19$hh_id %in% q2_19$hh_id,]
q2_19 <- q2_19[q2_19$hh_id %in% q1_19$hh_id,]
q12_19 <- rbind(q1_19,q2_19)
write.csv(q12_19, "data/pnad_2019_q1q2_individual_bothpresent.csv")


# get data aggregated by household:
summed <- q12_19 %>% get_dummies() %>% aggregate_sum()
firsts <- q12_19 %>% get_dummies() %>% aggregate_first()
q12_hh <- tibble(merge_hh(firsts, summed))
write.csv(q12_hh, "data/2019_q12_aggregated.csv")
rm(q12_19, q1_19, q2_19, q12_hh, firsts, summed)
gc()


# ---------- First and Third quarter of 2019 -----------------
q1_19 <- get_data_for_quarter(2019, 1)
q3_19 <- get_data_for_quarter(2019, 3)
q1_19 <- q1_19[q1_19$hh_id %in% q3_19$hh_id,]
q3_19 <- q3_19[q3_19$hh_id %in% q1_19$hh_id,]
q13_19 <- rbind(q1_19,q3_19)
write.csv(q13_19, "data/pnad_2019_q1q3_individual_bothpresent.csv")

summed <- q13_19 %>% get_dummies() %>% aggregate_sum()
firsts <- q13_19 %>% get_dummies() %>% aggregate_first()
q13_hh <- tibble(merge_hh(firsts, summed))
write.csv(q13_hh, "data/2019_q13_aggregated.csv")
rm(q13_19, q1_19, q3_19, q13_hh, firsts, summed)
gc()


# ---------- First, Second and Third quarter of 2019 and 2020 -----------------
# get data from the first three quarters of 2019 and 2020 and keep only
# households present in all three quarters of each year
q1_19 <- get_data_for_quarter(2019, 1)
q2_19 <- get_data_for_quarter(2019, 2)
q3_19 <- get_data_for_quarter(2019, 3)

q1_20 <- get_data_for_quarter(2020, 1)
q2_20 <- get_data_for_quarter(2020, 2)
q3_20 <- get_data_for_quarter(2020, 3)

#keep only hh that is in all three quarters
q1_20 <- q1_20 %>% filter(hh_id %in% q2_20$hh_id) %>%
  filter(hh_id %in% q3_20$hh_id)
q2_20 <- q2_20 %>% filter(hh_id %in% q1_20$hh_id) %>%
  filter(hh_id %in% q3_20$hh_id)
q3_20 <- q1_20 %>% filter(hh_id %in% q2_20$hh_id) %>%
  filter(hh_id %in% q1_20$hh_id)

#keep only hh that is in all three quarters
q1_19 <- q1_20 %>% filter(hh_id %in% q2_20$hh_id) %>%
  filter(hh_id %in% q3_20$hh_id)
q2_19 <- q2_20 %>% filter(hh_id %in% q1_20$hh_id) %>%
  filter(hh_id %in% q3_20$hh_id)
q3_19 <- q1_20 %>% filter(hh_id %in% q2_20$hh_id) %>%
  filter(hh_id %in% q1_20$hh_id)


df1920 <- rbind(q1_19, q2_19, q3_19, q1_20, q2_20, q3_20)
write.csv(df1920, "data/pnad_q1q2q3_2019_2020_individual.csv")


# Do same as above but aggregating at the hh level so we know how many people per
# household go to school, how many ppl go to college, how many men, how many women, etc.

s <- df1920 %>% get_dummies() %>% aggregate_sum()
f <- df1920 %>% get_dummies() %>% aggregate_first()
df1920_hh <- tibble(merge_hh(s,f))

write.csv(df1920_hh, "data/2019_2020_q123_hh.csv")

# below we do something similar to the one above, but each row will be a hh that
# is interviewed every quarter and the results for eaech quarter will be rows
s1_20 <- q1_20 %>% get_dummies() %>% aggregate_sum()
s2_20 <- q2_20 %>% get_dummies() %>% aggregate_sum()
s3_20 <- q3_20 %>% get_dummies() %>% aggregate_sum()
f1_20 <- q1_20 %>% aggregate_first()
f2_20 <- q2_20 %>% aggregate_first()
f3_20 <- q3_20 %>% aggregate_first()

s1_19 <- q1_19 %>% get_dummies() %>% aggregate_sum()
s2_19 <- q2_19 %>% get_dummies() %>% aggregate_sum()
s3_19 <- q3_19 %>% get_dummies() %>% aggregate_sum()
f1_19 <- q1_19 %>% aggregate_first()
f2_19 <- q2_19 %>% aggregate_first()
f3_19 <- q3_19 %>% aggregate_first()

m20 <- merge(merge(merge_hh(s1_20, f1_20), merge_hh(s2_20, f2_20)), merge_hh(s3_20, f3_20))
m19 <- merge(merge(merge_hh(s1_19, f1_19), merge_hh(s2_19, f2_19)), merge_hh(s3_19, f3_19))

write.csv(rbind(m19,m20), "data/2019_2020_q123_hh_horizontal.csv")


rm(q1_19, q2_19, q3_19, q1_20, q2_20, q3_20, s1_20, s2_20, s3_20,
   f1_20, f2_20, f3_20, s1_19, s2_19, s3_19, f1_19, f2_19, f3_19,
   m19, m20, s, f, df1920, df1920_hh)
gc()


# ---------- First quarters ----------------------
df <- get_first_quarters(2018,2019)
write.csv(df, "data/pnad_2019_2018_q1_individual_bothpresent.csv")
df <- get_first_quarters(2019,2020)
write.csv(df, "data/pnad_2020_2019_q1_individual_bothpresent.csv")
df <- get_first_quarters(2020,2021)
write.csv(df, "data/pnad_2021_2020_q1_individual_bothpresent.csv")
rm(df)


# ---------- All observations ----------------------
get_all_obs <- function(year){
  df1 <- get_data_for_quarter(year, 1)
  df2 <- get_data_for_quarter(year, 2)
  df3 <- get_data_for_quarter(year, 3)
  df4 <- get_data_for_quarter(year, 4)
  df5 <- get_data_for_quarter(year+1, 1)
  
  hh_ids <- intersect(intersect(df5$hh_id, df1$hh_id), intersect(df2$hh_id, df3$hh_id))
  hh_ids <- intersect(hh_ids, df4$hh_id)
  
  df1 <- df1 %>% filter(hh_id %in% hh_ids)
  df2 <- df2 %>% filter(hh_id %in% hh_ids)
  df3 <- df3 %>% filter(hh_id %in% hh_ids)
  df4 <- df4 %>% filter(hh_id %in% hh_ids)
  df5 <- df5 %>% filter(hh_id %in% hh_ids)
  
  return(bind_rows(df1, df2, df3, df4, df5))
}
df <- get_all_obs(2018)
write.csv(df, "data/pnad_all_2018_19_individual_bothpresent.csv")
df <- get_all_obs(2019)
write.csv(df, "data/pnad_all_2019_20_individual_bothpresent.csv")
df <- get_all_obs(2020)
write.csv(df, "data/pnad_all_2020_21_individual_bothpresent.csv")