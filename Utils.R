library(tidyverse)
library(PNADcIBGE)


# household identifier is UPA + V1008 + V1014. 

# In order for us to be able to repeat the cleaning for other quarters, I wrote a function to do the cleaning
get_data_for_quarter <-function(year, quarter){
  # names of variables we want
  var <- c("UF", "V1014", "V1016", "V1028", "V1027", "V1023", 
           "V1022", "V2001", "V2005", "V2007", "V2008", "V20081", 
           "V20082", "V403311", "V3002", "V1022", "V2009", "V3002", 
           "V3009A", "V2010", "V3001", "V3002", "V3002A", "V3003A",
           "V3006", "V3007", "V3008", "V3013", "V3014", "V4001",
           "V4009", "V4010", "V4012", "V4013", "V40132", "V4029",
           "V4033", "V40331", "V403311", "V40332", "V403321", "V40333", 
           "V403331", "V4039", "V4041", "V4034", "V40341", "V403411",
           "V40342", "V403421", "V4039C", "V4071", "V4073", "V4074A"
  )
  # get data from given year and quarter
  df1 <- get_pnadc(year=year, quarter=quarter, vars = var, design=FALSE)
  #rename variables to make our life easier
  df1 <- rename(df1,
                interview_num = V1016,
                projected_pop = V1029,
                hh_number = V1008,
                geo_area = V1023, # this is capital, metropolitan region, integrated region of economic development excluding capital, other part of state
                urban = V1022,
                sampling_group = V1014,
                ppl_in_hh = V2001,
                role_in_hh = V2005,
                sex = V2007,
                birth_day = V2008,
                birth_month = V20081,
                birth_year = V20082,
                age = V2009,
                race = V2010,
                can_read_write = V3001,
                goes_to_school = V3002,
                school = V3002A,
                edu_level = V3003A,
                what_grade = V3006,
                completed_college = V3007,
                attended_school = V3008,
                hi_lvl_attended = V3009A,
                hi_grade_completed = V3013,
                hi_lvl_attended_complete = V3014, # has the person completed the highest level of education that they attended?
                worked = V4001, # worked in the week of reference
                num_of_jobs = V4009,
                occupation_code = V4010,
                type_of_job = V4012,
                employer_activity = V4013,
                is_agriculture_extraction = V40132,
                formal_employment = V4029,
                earn_job = V4033, #whether question about earnings was answered
                earn_job_money_dummy = V40331, #whether person earned money
                earn_job_money = V403311, #range of value earned
                earn_job_products_dummy = V40332, #whether received in products/goods
                earn_job_products = V403321, #value earned in products
                earn_job_other_dummy = V40333, #whether received in products/goods
                earn_job_other = V403331,
                hours_job = V4039,
                code_job = V4041,
                
                earn_job_rm = V4034, # the variables above are usual earnings, the ones denoted "rm" are earnings in reference month
                earn_job_rm_money_dummy = V40341, 
                earn_job_rm_money = V403411,
                earn_job_rm_products_dummy = V40342,
                earn_job_rm_products = V403421,
                hours_job1 = V4039C,
                
                # we could do the same as above for secondary jobs, code would be almost the same
                
                looked_for_job = V4071,
                wanted_to_look_for_job = V4073,
                why_not_look_for_job = V4074A
  )
  
  # create single variable to use as household identifier
  df1$hh_id <- as.character(paste0(as.character(df1$UPA), as.character(df1$hh_number), as.character(df1$sampling_group)))
  
  # transform characters into numeric
  df1$Ano <- as.numeric(df1$Ano)
  df1$Trimestre <- as.numeric(df1$Trimestre)
  df1$birth_day <- as.numeric(df1$birth_day)
  df1$birth_month <- as.numeric(df1$birth_month)
  df1$birth_year <- as.numeric(df1$birth_year)
  
  # replace codes with NA
  df1 = df1 %>% mutate(birth_day_n = ifelse(birth_day==99, NA, birth_day))
  df1 = df1 %>% mutate(birth_month_n = ifelse(birth_month==99, NA, birth_month))
  df1 = df1 %>% mutate(birth_year_n = ifelse(birth_year==9999, NA, birth_year))
  
  return(df1)
}

# the functions below are going to be used to aggregate the data to the household level
get_dummies <- function(df){
  # create dummy variables for the given columns
  dummies_df <- df %>% mutate(
    male=ifelse(sex=="Homem", 1, 0),
    female=ifelse(sex=="Mulher", 1, 0),
    goes_to_school_y=ifelse(is.na(goes_to_school), 0, ifelse(goes_to_school=="Sim",1,0)),
    goes_to_school_n=ifelse(is.na(goes_to_school), 0, ifelse(goes_to_school=="Não",1,0)),
    private_school=ifelse(is.na(goes_to_school), 0, ifelse(school=="Rede privada",1,0)),
    public_school=ifelse(is.na(goes_to_school), 0, ifelse(school=="Rede pública",1,0)),
    white=ifelse(race=="Branca", 1, 0),
    black=ifelse(race=="Preta", 1, 0),
    brown=ifelse(race=="Parda", 1, 0),
    asian=ifelse(race=="Amarela", 1, 0),
    can_read_write=ifelse(is.na(can_read_write), 0, ifelse(can_read_write=="Sim",1,0)),
    not_read_write=ifelse(is.na(can_read_write), 0, ifelse(can_read_write=="Não",1,0)),
    in_college=ifelse(is.na(edu_level), 0, ifelse(edu_level=="Superior - graduação", 1, 0)),
    in_high_school=ifelse(is.na(edu_level), 0, ifelse(edu_level=="Regular do ensino médio", 1, 0)),
    in_middle_school=ifelse(is.na(edu_level), 0, ifelse(edu_level=="Regular do ensino fundamental", 1, 0)),
    in_pre_school=ifelse(is.na(edu_level), 0, ifelse(edu_level=="Pré-escola", 1, 0)),
    income_1=ifelse(is.na(earn_job_money), 0, ifelse(earn_job_money=="1 a [0,5SM]", 1, 0)), # first income bracket 0 to 0.5 minumum salaries
    income_2=ifelse(is.na(earn_job_money), 0, ifelse(earn_job_money=="[0,5SM]+1 a [1SM]", 1, 0)), # second income bracket 0.5 to 1 minimum salaries
    income_3=ifelse(is.na(earn_job_money), 0, ifelse(earn_job_money=="[1SM]+1 a [2SM]", 1, 0)),# third income bracket 1 to 2 minimum salaries
    income_4=ifelse(is.na(earn_job_money), 0, ifelse(earn_job_money=="[2SM]+1 a [3SM]", 1, 0)), # Fourth income group 2 to 3 minimum salaries
    income_5=ifelse(is.na(earn_job_money), 0, ifelse(earn_job_money=="[3SM]+1 a [5SM]", 1, 0)), #fifth income group 3 to 5 minimum salaries
    income_6=ifelse(is.na(earn_job_money), 0, ifelse(earn_job_money=="[5SM]+1 a [10SM]" || earn_job_money=="[10SM]+1 a [20SM]" || earn_job_money=="[20SM]+1 ou mais", 1, 0)) # sixth income bracket 5 or more minimum salaries
  )
  return(dummies_df)
}

aggregate_sum <- function(df){
  summed <- df %>% group_by(hh_id, Ano, Trimestre) %>% summarise(across(where(is.numeric), sum))
  to_sum = c(
    "male", "female", "goes_to_school_y", "goes_to_school_n",
    "private_school", "public_school", "white", "black",
    "asian", "can_read_write", "not_read_write", "in_college", "in_high_school",
    "in_middle_school", "in_pre_school", "income_1", "income_2", "income_3",
    "income_4", "income_5", "income_6"
  )
  return(summed)
}

aggregate_first <- function(df){
  to_keep = c(
    "UF", "UPA", "Estrato", "hh_number",
    "sampling_group", "interview_num", "urban", "geo_area", "V1027", "V1028",                  
    "projected_pop", "posest", "ppl_in_hh", "V2003"
  )
  firsts <- df %>% group_by(hh_id, Ano, Trimestre) %>% summarise(across(all_of(to_keep), first))
  return(firsts)
}

merge_hh <- function(df1, df2){
  return(merge(df1, df2, by=c("hh_id", "Ano", "Trimestre")))
}
