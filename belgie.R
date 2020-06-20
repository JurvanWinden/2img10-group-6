#
# Load in the data 
#

zipp <- read_csv("./data/zipcode-belgium.csv", col_names = c("zip","City" ,"Longitude", "Latitude" ))
df <- read_csv("data/COVID19BE_CASES_MUNI.csv") %>%
  select(DATE, `TX_DESCR_NL`, `TX_PROV_DESCR_NL`, `TX_ADM_DSTR_DESCR_NL`,`TX_RGN_DESCR_NL`, `CASES`)  %>%
  rename( dateRep = DATE, 
          Province  =TX_PROV_DESCR_NL , 
          City = TX_DESCR_NL, 
          Taal = TX_RGN_DESCR_NL, 
          Cases = CASES,
          Municipality = TX_ADM_DSTR_DESCR_NL ) %>%
  left_join(df, zipp)

problems(df)
# wrangling
df$Cases[which(df$Cases == "<5")] = 0
df$Cases <- parse_integer(df$Cases)

# set to match the world data set or NL Data Set
df <- df %>%
  pivot_wider(names_from = dateRep, values_from = Cases) #%>%
  
# Replace NA's with most recent values
df <- na.locf(df)  

belgie <- df
rm(df)
