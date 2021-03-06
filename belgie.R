#
# Load in the data 
# https://epistat.wiv-isp.be/covid/

library("tidyverse")
library("zoo")

zipp <- read_csv("./data/zipcode-belgium.csv", col_names = c("zip","City" ,"Longitude", "Latitude" )) %>%
  mutate(CityFR = City)
dfFr  <- read_csv("data/COVID19BE_CASES_MUNI.csv") %>%
  select(DATE, `TX_DESCR_NL`, `TX_PROV_DESCR_NL`, `TX_ADM_DSTR_DESCR_NL`,`TX_DESCR_FR`,`TX_RGN_DESCR_NL`, `CASES`)  %>%
  rename( dateRep = DATE, 
          Province  =TX_PROV_DESCR_NL , 
          City = TX_DESCR_NL,
          CityFR = TX_DESCR_FR,
          Taal = TX_RGN_DESCR_NL, 
          Cases = CASES,
          Municipality = TX_ADM_DSTR_DESCR_NL ) %>%
  left_join(zipp, by = "CityFR" , suffix = c("",".x")) %>%
  drop_na()
dfNl  <- read_csv("data/COVID19BE_CASES_MUNI.csv") %>%
  select(DATE, `TX_DESCR_NL`, `TX_PROV_DESCR_NL`, `TX_ADM_DSTR_DESCR_NL`,`TX_DESCR_FR`,`TX_RGN_DESCR_NL`, `CASES`)  %>%
  rename( dateRep = DATE, 
          Province  =TX_PROV_DESCR_NL , 
          City = TX_DESCR_NL,
          CityFR = TX_DESCR_FR,
          Taal = TX_RGN_DESCR_NL, 
          Cases = CASES,
          Municipality = TX_ADM_DSTR_DESCR_NL ) %>%
  left_join(zipp, by = "City" , suffix = c("",".x")) %>%
  drop_na()
df <- full_join(dfFr, dfNl)
# Bailing out: appearently data *Changes* depending on appending the french or dutch coordinates. 

df$Cases[which(df$Cases == "<5")] = 0
df$Cases <- parse_integer(df$Cases)

# Replace NA's with most recent values
df$Cases <- as.integer(na.locf(df$Cases) )

belgie_long <- df
# set to match the world data set or NL Data Set
df <- df %>%
  pivot_wider(names_from = dateRep, values_from = Cases) #%>%
df <- na.locf(df)

# daily new infected -> sum of infected per day
for (i in 1:nrow(df)){
  temp <- as.numeric(df[i,11])
  for (j in 12:ncol(df)){
    temp <- as.numeric(temp) + as.numeric(df[i,j])
    df[i,j] <- temp
  }
}

belgie <- df
rm(df,zipp, dfNl, dfFr,temp)
