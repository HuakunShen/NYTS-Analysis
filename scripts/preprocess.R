library(janitor)
library(tidyverse)

dataFormat <- read.csv(here::here("inputs/data/smokeColNameMap.csv"))

data2014 <- readxl::read_xlsx(here::here("inputs/data/nyts2014.xlsx")) %>% clean_names()
data2015 <- readxl::read_xlsx(here::here("inputs/data/nyts2015.xlsx")) %>% clean_names()
data2016 <- readxl::read_xlsx(here::here("inputs/data/nyts2016.xlsx")) %>% clean_names()
data2017 <- readxl::read_xlsx(here::here("inputs/data/nyts2017.xlsx")) %>% clean_names()
data2018 <- readxl::read_xlsx(here::here("inputs/data/nyts2018.xlsx")) %>% clean_names()
data2019 <- readxl::read_xlsx(here::here("inputs/data/nyts2019.xlsx")) %>% clean_names()
data2020 <- readxl::read_xlsx(here::here("inputs/data/nyts2020.xlsx")) %>% clean_names()
data2021 <- readxl::read_xlsx(here::here("inputs/data/nyts2021.xlsx")) %>% clean_names()

data2014[c("qn1", "qn2", "qn3", "race_s", "school_id", "qn6", "qn19",
           "ccigt_r", "ccigar_r", "cslt_r", "cpipe_r", "cbidis_r", "chookah_r",
           "csnus_r", "cdissolv_r", "celcigt_r")]
selected_columns <- c("qn1", "qn2", "qn3", "race_s", "schooltype", "qn6", "qn19",
                      "ccigt", "ccigar", "cslt", "cpipe", "cbidis", "chookah",
                      "csnus", "cdissolv", "celcigt")
df2015 <- data2015[selected_columns]
df2015$q86j <- data2015$qn77j
df2016 <- data2016[selected_columns]
df2016$q86j <- data2016$q81j
df2017 <- data2017[selected_columns]
df2017$q86j <- data2017$qn86j
df2018 <- data2018[selected_columns]
df2018$q86j <- data2018$q86j
df2019 <- data2019[selected_columns]
df2019$q86j <- data2019$q102k
df2020 <- data2020[selected_columns]
df2020$q86j <- data2020$qn114k


df2015 <- df2015 %>% rename(
  Age = qn1,
  Sex = qn2,
  Grade = qn3,
  race = race_s,
  schooltype = schooltype,
  curious_smoke_cig = qn6,
  curious_trying_others = qn19,
  Smoked_cigarettes_on_1_or = ccigt,
  Smoked_cigars_cigarillos = ccigar,
  chewing_tobacco_snuff_or = cslt,
  Smoked_tobacco_pipe_not_h = cpipe,
  Smoked_bidis_on_1_or_more = cbidis,
  Smoked_tobacco_hookah_or = chookah,
  snus_such_as_Camel_or_Mar = csnus,
  dissolvable_tobacco_such = cdissolv,
  electronic_cigarettes_suc = celcigt,
  housemoate_no_smoke = q86j
)
df2015$Sex[df2015$Sex==1] = "M"
df2015$Sex[df2015$Sex==2] = "F"
df2015$race[df2015$race==1] = "White"
df2015$race[df2015$race==2] = "Black"
df2015$race[df2015$race==3] = "Hispanic"
df2015$race[df2015$race==4] = "Asian"
df2015$race[df2015$race==5] = "AI/AN"
df2015$race[df2015$race==6] = "NHOPI"
df2015$schooltype[df2015$schooltype==1] = "Middle"
df2015$schooltype[df2015$schooltype==2] = "High"
df2015$Smoked_cigarettes_on_1_or[df2015$Smoked_cigarettes_on_1_or == 2] = 0
df2015$Smoked_cigars_cigarillos[df2015$Smoked_cigars_cigarillos == 2] = 0
df2015$chewing_tobacco_snuff_or[df2015$chewing_tobacco_snuff_or == 2] = 0
df2015$Smoked_tobacco_pipe_not_h[df2015$Smoked_tobacco_pipe_not_h == 2] = 0
df2015$Smoked_bidis_on_1_or_more[df2015$Smoked_bidis_on_1_or_more == 2] = 0
df2015$Smoked_tobacco_hookah_or[df2015$Smoked_tobacco_hookah_or == 2] = 0
df2015$snus_such_as_Camel_or_Mar[df2015$snus_such_as_Camel_or_Mar == 2] = 0
df2015$dissolvable_tobacco_such[df2015$dissolvable_tobacco_such == 2] = 0
df2015$electronic_cigarettes_suc[df2015$electronic_cigarettes_suc == 2] = 0
df2015$smoked <- df2015$Smoked_cigarettes_on_1_or | 
df2015$Smoked_cigars_cigarillos | df2015$chewing_tobacco_snuff_or | 
df2015$Smoked_tobacco_pipe_not_h | df2015$Smoked_bidis_on_1_or_more | 
df2015$Smoked_tobacco_hookah_or | df2015$snus_such_as_Camel_or_Mar | 
df2015$dissolvable_tobacco_such | df2015$electronic_cigarettes_suc

df2015$housemoate_no_smoke


df2016 <- df2016 %>% rename(
  Age = qn1,
  Sex = qn2,
  Grade = qn3,
  race = race_s,
  schooltype = schooltype,
  curious_smoke_cig = qn6,
  curious_trying_others = qn19,
  Smoked_cigarettes_on_1_or = ccigt,
  Smoked_cigars_cigarillos = ccigar,
  chewing_tobacco_snuff_or = cslt,
  Smoked_tobacco_pipe_not_h = cpipe,
  Smoked_bidis_on_1_or_more = cbidis,
  Smoked_tobacco_hookah_or = chookah,
  snus_such_as_Camel_or_Mar = csnus,
  dissolvable_tobacco_such = cdissolv,
  electronic_cigarettes_suc = celcigt,
  housemoate_no_smoke = q86j
)
df2016$Sex[df2016$Sex==1] = "M"
df2016$Sex[df2016$Sex==2] = "F"
df2016$race[df2016$race==1] = "White"
df2016$race[df2016$race==2] = "Black"
df2016$race[df2016$race==3] = "Hispanic"
df2016$race[df2016$race==4] = "Asian"
df2016$race[df2016$race==5] = "AI/AN"
df2016$race[df2016$race==6] = "NHOPI"
df2016$schooltype[df2016$schooltype==1] = "Middle"
df2016$schooltype[df2016$schooltype==2] = "High"
df2016$Smoked_cigarettes_on_1_or[df2016$Smoked_cigarettes_on_1_or == 2] = 0
df2016$Smoked_cigars_cigarillos[df2016$Smoked_cigars_cigarillos == 2] = 0
df2016$chewing_tobacco_snuff_or[df2016$chewing_tobacco_snuff_or == 2] = 0
df2016$Smoked_tobacco_pipe_not_h[df2016$Smoked_tobacco_pipe_not_h == 2] = 0
df2016$Smoked_bidis_on_1_or_more[df2016$Smoked_bidis_on_1_or_more == 2] = 0
df2016$Smoked_tobacco_hookah_or[df2016$Smoked_tobacco_hookah_or == 2] = 0
df2016$snus_such_as_Camel_or_Mar[df2016$snus_such_as_Camel_or_Mar == 2] = 0
df2016$dissolvable_tobacco_such[df2016$dissolvable_tobacco_such == 2] = 0
df2016$electronic_cigarettes_suc[df2016$electronic_cigarettes_suc == 2] = 0
df2016$smoked <- df2016$Smoked_cigarettes_on_1_or | 
df2016$Smoked_cigars_cigarillos | df2016$chewing_tobacco_snuff_or | 
df2016$Smoked_tobacco_pipe_not_h | df2016$Smoked_bidis_on_1_or_more | 
df2016$Smoked_tobacco_hookah_or | df2016$snus_such_as_Camel_or_Mar | 
df2016$dissolvable_tobacco_such | df2016$electronic_cigarettes_suc

df2017 <- df2017 %>% rename(
  Age = qn1,
  Sex = qn2,
  Grade = qn3,
  race = race_s,
  schooltype = schooltype,
  curious_smoke_cig = qn6,
  curious_trying_others = qn19,
  Smoked_cigarettes_on_1_or = ccigt,
  Smoked_cigars_cigarillos = ccigar,
  chewing_tobacco_snuff_or = cslt,
  Smoked_tobacco_pipe_not_h = cpipe,
  Smoked_bidis_on_1_or_more = cbidis,
  Smoked_tobacco_hookah_or = chookah,
  snus_such_as_Camel_or_Mar = csnus,
  dissolvable_tobacco_such = cdissolv,
  electronic_cigarettes_suc = celcigt,
  housemoate_no_smoke = q86j
)
df2017$Sex[df2017$Sex==1] = "M"
df2017$Sex[df2017$Sex==2] = "F"
df2017$race[df2017$race==1] = "White"
df2017$race[df2017$race==2] = "Black"
df2017$race[df2017$race==3] = "Hispanic"
df2017$race[df2017$race==4] = "Asian"
df2017$race[df2017$race==5] = "AI/AN"
df2017$race[df2017$race==6] = "NHOPI"
df2017$schooltype[df2017$schooltype==1] = "Middle"
df2017$schooltype[df2017$schooltype==2] = "High"
df2017$Smoked_cigarettes_on_1_or[df2017$Smoked_cigarettes_on_1_or == 2] = 0
df2017$Smoked_cigars_cigarillos[df2017$Smoked_cigars_cigarillos == 2] = 0
df2017$chewing_tobacco_snuff_or[df2017$chewing_tobacco_snuff_or == 2] = 0
df2017$Smoked_tobacco_pipe_not_h[df2017$Smoked_tobacco_pipe_not_h == 2] = 0
df2017$Smoked_bidis_on_1_or_more[df2017$Smoked_bidis_on_1_or_more == 2] = 0
df2017$Smoked_tobacco_hookah_or[df2017$Smoked_tobacco_hookah_or == 2] = 0
df2017$snus_such_as_Camel_or_Mar[df2017$snus_such_as_Camel_or_Mar == 2] = 0
df2017$dissolvable_tobacco_such[df2017$dissolvable_tobacco_such == 2] = 0
df2017$electronic_cigarettes_suc[df2017$electronic_cigarettes_suc == 2] = 0
df2017$smoked <- df2017$Smoked_cigarettes_on_1_or | 
  df2017$Smoked_cigars_cigarillos | df2017$chewing_tobacco_snuff_or | 
  df2017$Smoked_tobacco_pipe_not_h | df2017$Smoked_bidis_on_1_or_more | 
  df2017$Smoked_tobacco_hookah_or | df2017$snus_such_as_Camel_or_Mar | 
  df2017$dissolvable_tobacco_such | df2017$electronic_cigarettes_suc



df2018 <- df2018 %>% rename(
  Age = qn1,
  Sex = qn2,
  Grade = qn3,
  race = race_s,
  schooltype = schooltype,
  curious_smoke_cig = qn6,
  curious_trying_others = qn19,
  Smoked_cigarettes_on_1_or = ccigt,
  Smoked_cigars_cigarillos = ccigar,
  chewing_tobacco_snuff_or = cslt,
  Smoked_tobacco_pipe_not_h = cpipe,
  Smoked_bidis_on_1_or_more = cbidis,
  Smoked_tobacco_hookah_or = chookah,
  snus_such_as_Camel_or_Mar = csnus,
  dissolvable_tobacco_such = cdissolv,
  electronic_cigarettes_suc = celcigt,
  housemoate_no_smoke = q86j
)
df2018$Sex[df2018$Sex==1] = "M"
df2018$Sex[df2018$Sex==2] = "F"
df2018$race[df2018$race==1] = "White"
df2018$race[df2018$race==2] = "Black"
df2018$race[df2018$race==3] = "Hispanic"
df2018$race[df2018$race==4] = "Asian"
df2018$race[df2018$race==5] = "AI/AN"
df2018$race[df2018$race==6] = "NHOPI"
df2018$schooltype[df2018$schooltype==1] = "Middle"
df2018$schooltype[df2018$schooltype==2] = "High"
df2018$Smoked_cigarettes_on_1_or[df2018$Smoked_cigarettes_on_1_or == 2] = 0
df2018$Smoked_cigars_cigarillos[df2018$Smoked_cigars_cigarillos == 2] = 0
df2018$chewing_tobacco_snuff_or[df2018$chewing_tobacco_snuff_or == 2] = 0
df2018$Smoked_tobacco_pipe_not_h[df2018$Smoked_tobacco_pipe_not_h == 2] = 0
df2018$Smoked_bidis_on_1_or_more[df2018$Smoked_bidis_on_1_or_more == 2] = 0
df2018$Smoked_tobacco_hookah_or[df2018$Smoked_tobacco_hookah_or == 2] = 0
df2018$snus_such_as_Camel_or_Mar[df2018$snus_such_as_Camel_or_Mar == 2] = 0
df2018$dissolvable_tobacco_such[df2018$dissolvable_tobacco_such == 2] = 0
df2018$electronic_cigarettes_suc[df2018$electronic_cigarettes_suc == 2] = 0
df2018$smoked <- df2018$Smoked_cigarettes_on_1_or | 
  df2018$Smoked_cigars_cigarillos | df2018$chewing_tobacco_snuff_or | 
  df2018$Smoked_tobacco_pipe_not_h | df2018$Smoked_bidis_on_1_or_more | 
  df2018$Smoked_tobacco_hookah_or | df2018$snus_such_as_Camel_or_Mar | 
  df2018$dissolvable_tobacco_such | df2018$electronic_cigarettes_suc

df2019 <- df2019 %>% rename(
  Age = qn1,
  Sex = qn2,
  Grade = qn3,
  race = race_s,
  schooltype = schooltype,
  curious_smoke_cig = qn6,
  curious_trying_others = qn19,
  Smoked_cigarettes_on_1_or = ccigt,
  Smoked_cigars_cigarillos = ccigar,
  chewing_tobacco_snuff_or = cslt,
  Smoked_tobacco_pipe_not_h = cpipe,
  Smoked_bidis_on_1_or_more = cbidis,
  Smoked_tobacco_hookah_or = chookah,
  snus_such_as_Camel_or_Mar = csnus,
  dissolvable_tobacco_such = cdissolv,
  electronic_cigarettes_suc = celcigt,
  housemoate_no_smoke = q86j
)
df2019$Age <- as.numeric(df2019$Age)
df2019$schooltype <- as.numeric(df2019$schooltype)
df2019$Sex[df2019$Sex==1] = "M"
df2019$Sex[df2019$Sex==2] = "F"
df2019$race[df2019$race==1] = "White"
df2019$race[df2019$race==2] = "Black"
df2019$race[df2019$race==3] = "Hispanic"
df2019$race[df2019$race==4] = "Asian"
df2019$race[df2019$race==5] = "AI/AN"
df2019$race[df2019$race==6] = "NHOPI"
df2019$schooltype[df2019$schooltype==1] = "Middle"
df2019$schooltype[df2019$schooltype==2] = "High"


df2019$Smoked_cigarettes_on_1_or <- as.numeric(df2019$Smoked_cigarettes_on_1_or)
df2019$Smoked_cigars_cigarillos <- as.numeric(df2019$Smoked_cigars_cigarillos)
df2019$chewing_tobacco_snuff_or <- as.numeric(df2019$chewing_tobacco_snuff_or)
df2019$Smoked_tobacco_pipe_not_h <- as.numeric(df2019$Smoked_tobacco_pipe_not_h)
df2019$Smoked_bidis_on_1_or_more <- as.numeric(df2019$Smoked_bidis_on_1_or_more)
df2019$Smoked_tobacco_hookah_or <- as.numeric(df2019$Smoked_tobacco_hookah_or)
df2019$snus_such_as_Camel_or_Mar <- as.numeric(df2019$snus_such_as_Camel_or_Mar)
df2019$dissolvable_tobacco_such <- as.numeric(df2019$dissolvable_tobacco_such)
df2019$electronic_cigarettes_suc <- as.numeric(df2019$electronic_cigarettes_suc)


df2019$Smoked_cigarettes_on_1_or[df2019$Smoked_cigarettes_on_1_or == 2] = 0
df2019$Smoked_cigars_cigarillos[df2019$Smoked_cigars_cigarillos == 2] = 0
df2019$chewing_tobacco_snuff_or[df2019$chewing_tobacco_snuff_or == 2] = 0
df2019$Smoked_tobacco_pipe_not_h[df2019$Smoked_tobacco_pipe_not_h == 2] = 0
df2019$Smoked_bidis_on_1_or_more[df2019$Smoked_bidis_on_1_or_more == 2] = 0
df2019$Smoked_tobacco_hookah_or[df2019$Smoked_tobacco_hookah_or == 2] = 0
df2019$snus_such_as_Camel_or_Mar[df2019$snus_such_as_Camel_or_Mar == 2] = 0
df2019$dissolvable_tobacco_such[df2019$dissolvable_tobacco_such == 2] = 0
df2019$electronic_cigarettes_suc[df2019$electronic_cigarettes_suc == 2] = 0
df2019$smoked <- df2019$Smoked_cigarettes_on_1_or | 
  df2019$Smoked_cigars_cigarillos | df2019$chewing_tobacco_snuff_or | 
  df2019$Smoked_tobacco_pipe_not_h | df2019$Smoked_bidis_on_1_or_more | 
  df2019$Smoked_tobacco_hookah_or | df2019$snus_such_as_Camel_or_Mar | 
  df2019$dissolvable_tobacco_such | df2019$electronic_cigarettes_suc

df2020 <- df2020 %>% rename(
  Age = qn1,
  Sex = qn2,
  Grade = qn3,
  race = race_s,
  schooltype = schooltype,
  curious_smoke_cig = qn6,
  curious_trying_others = qn19,
  Smoked_cigarettes_on_1_or = ccigt,
  Smoked_cigars_cigarillos = ccigar,
  chewing_tobacco_snuff_or = cslt,
  Smoked_tobacco_pipe_not_h = cpipe,
  Smoked_bidis_on_1_or_more = cbidis,
  Smoked_tobacco_hookah_or = chookah,
  snus_such_as_Camel_or_Mar = csnus,
  dissolvable_tobacco_such = cdissolv,
  electronic_cigarettes_suc = celcigt,
  housemoate_no_smoke = q86j
)
df2020$Sex[df2020$Sex==1] = "M"
df2020$Sex[df2020$Sex==2] = "F"
df2020$race[df2020$race==1] = "White"
df2020$race[df2020$race==2] = "Black"
df2020$race[df2020$race==3] = "Hispanic"
df2020$race[df2020$race==4] = "Asian"
df2020$race[df2020$race==5] = "AI/AN"
df2020$race[df2020$race==6] = "NHOPI"
df2020$schooltype[df2020$schooltype==1] = "Middle"
df2020$schooltype[df2020$schooltype==2] = "High"
df2020$Smoked_cigarettes_on_1_or[df2020$Smoked_cigarettes_on_1_or == 2] = 0
df2020$Smoked_cigars_cigarillos[df2020$Smoked_cigars_cigarillos == 2] = 0
df2020$chewing_tobacco_snuff_or[df2020$chewing_tobacco_snuff_or == 2] = 0
df2020$Smoked_tobacco_pipe_not_h[df2020$Smoked_tobacco_pipe_not_h == 2] = 0
df2020$Smoked_bidis_on_1_or_more[df2020$Smoked_bidis_on_1_or_more == 2] = 0
df2020$Smoked_tobacco_hookah_or[df2020$Smoked_tobacco_hookah_or == 2] = 0
df2020$snus_such_as_Camel_or_Mar[df2020$snus_such_as_Camel_or_Mar == 2] = 0
df2020$dissolvable_tobacco_such[df2020$dissolvable_tobacco_such == 2] = 0
df2020$electronic_cigarettes_suc[df2020$electronic_cigarettes_suc == 2] = 0
df2020$smoked <- df2020$Smoked_cigarettes_on_1_or | 
  df2020$Smoked_cigars_cigarillos | df2020$chewing_tobacco_snuff_or | 
  df2020$Smoked_tobacco_pipe_not_h | df2020$Smoked_bidis_on_1_or_more | 
  df2020$Smoked_tobacco_hookah_or | df2020$snus_such_as_Camel_or_Mar | 
  df2020$dissolvable_tobacco_such | df2020$electronic_cigarettes_suc


write.csv(df2015, here::here("inputs/data/df2015.csv"))
write.csv(df2016, here::here("inputs/data/df2016.csv"))
write.csv(df2017, here::here("inputs/data/df2017.csv"))
write.csv(df2018, here::here("inputs/data/df2018.csv"))
write.csv(df2019, here::here("inputs/data/df2019.csv"))
write.csv(df2020, here::here("inputs/data/df2020.csv"))




# predictor variables to keep:
# qn1
# qn2
# qn3
# race_s
# SchoolID
# qn6
# qn19

# response variables to keep
# ccigt_r
# ccigar_r
# cslt_r
# cpipe_r
# cbidis_r
# chookah_r
# csnus_r
# cdissolv_r
# celcigt_r
# qn1
# qn2


# race
# 1 White
# 2 Black
# 3 Hispanic
# 4 Asian
# 5 AI_AN
# 6 NHOPI