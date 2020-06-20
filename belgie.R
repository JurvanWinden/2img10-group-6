#
# Load in the data 
#
df <- read_csv("data/COVID19BE_CASES_MUNI.csv") %>%
  select(DATE, `TX_DESCR_NL`, `TX_PROV_DESCR_NL`, `TX_ADM_DSTR_DESCR_NL`,`TX_RGN_DESCR_NL`, `CASES`)  
problems(df)
# wrangling
df$CASES[which(df$CASES == "<5")] = 0
df$CASES <- parse_integer(df$CASES)

# set to match the world data set or NL Data Set
df <- df %>%
  rename( dateRep = DATE, 
          Province  =TX_PROV_DESCR_NL , 
          City = TX_DESCR_NL, 
          Taal = TX_RGN_DESCR_NL, 
          Cases = CASES,
          Municipality = TX_ADM_DSTR_DESCR_NL )  %>%  
  pivot_wider(names_from = dateRep, values_from = Cases) #%>%
  

belgie <- df
rm(df)
