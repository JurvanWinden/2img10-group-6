install.packages('janitor')

#Read the corona world dataset from csv file
corona_world <- read.csv("~/GitHub/2img10-group-6/corona_world")
View(corona_world)
library(readxl)
library(janitor)


#read corona NL infection per municipality data over time
Corona_NL_Infections_municipality <- read_excel("COVIDNL.xlsx", sheet = "Infections", na = "0", n_max = 357, range = "A1:AI357")
#transform column names to actual dates
for (i in 3:35) {
  names(Corona_NL_Infections_municipality)[i]<-as.character(excel_numeric_to_date(as.numeric(names(Corona_NL_Infections_municipality)[i])))
}

#read corona NL infection per province data over time (accumulative)
Corona_NL_infections_provinces_total <- read_excel("COVIDNL.xlsx", sheet = "Infections", range = "B361:BE373", na = "0")
for (i in 2:56) {
  names(Corona_NL_infections_provinces_total)[i]<-as.character(excel_numeric_to_date(as.numeric(names(Corona_NL_infections_provinces_total)[i])))
}

#read corona NL infection per province data over time (daily increase)
Corona_NL_infections_provinces_daily <- read_excel("COVIDNL.xlsx", sheet = "Infections", range = "B399:BE411", na = "0")
for (i in 2:56) {
  names(Corona_NL_infections_provinces_daily)[i]<-as.character(excel_numeric_to_date(as.numeric(names(Corona_NL_infections_provinces_daily)[i])))
}

#read corona NL infectionc per day per province (average of 5 day window)
Corona_NL_infections_provinces_daily_average <- read_excel("COVIDNL.xlsx",  sheet = "Infections", range = "B436:BA448", na = "0")
for (i in 2:52) {
  names(Corona_NL_infections_provinces_daily_average)[i]<-as.character(excel_numeric_to_date(as.numeric(names(Corona_NL_infections_provinces_daily_average)[i])))
}


#read corona NL Hospitalizations per municipality data over time
Corona_NL_Hospitalizations_municipality <- read_excel("COVIDNL.xlsx", sheet = "Hospitalizations", na = "0", n_max = 357, range = "A1:X357")
#transform column names to actual dates
for (i in 3:24) {
  names(Corona_NL_Hospitalizations_municipality)[i]<-as.character(excel_numeric_to_date(as.numeric(names(Corona_NL_Hospitalizations_municipality)[i])))
}

#read corona NL Hospitalizations per province data over time (accumulative)
Corona_NL_Hospitalizations_provinces_total <- read_excel("COVIDNL.xlsx", sheet = "Hospitalizations", range = "B361:X373", na = "0")
for (i in 2:24) {
  names(Corona_NL_Hospitalizations_provinces_total)[i]<-as.character(excel_numeric_to_date(as.numeric(names(Corona_NL_Hospitalizations_provinces_total)[i])))
}

#read corona NL Hospitalizations per province data over time (daily increase)
Corona_NL_Hospitalizations_provinces_daily <- read_excel("COVIDNL.xlsx", sheet = "Hospitalizations", range = "B394:W406", na = "0")
for (i in 2:23) {
  names(Corona_NL_Hospitalizations_provinces_daily)[i]<-as.character(excel_numeric_to_date(as.numeric(names(Corona_NL_Hospitalizations_provinces_daily)[i])))
}

#read corona NL Hospitalizations per day per province (average of 5 day window)
Corona_NL_Hospitalizations_provinces_average <- read_excel("COVIDNL.xlsx",  sheet = "Hospitalizations", range = "B425:S437", na = "0")
for (i in 2:18) {
  names(Corona_NL_Hospitalizations_provinces_average)[i]<-as.character(excel_numeric_to_date(as.numeric(names(Corona_NL_Hospitalizations_provinces_average)[i])))
}

#read the coordinates from the Dutch municipalities
NL_municipality_coordinates <- read_excel("COVIDNL.xlsx", sheet = "Coords", range = "A1:D356")