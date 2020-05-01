### 
### Run this file first to load everything. 
### It should also install neccesary packages if needed. 
### 

# Read stuff & Install stuff --------------
requiredPackages <- c( 
  "tidyverse", 
  "readxl",
  "janitor",
  "TDA",
  "gridGraphics",
  "rgdal", 
  "sp"
  )

for( i in requiredPackages ){
  print(i)
  if (!require(i , character.only = T )) {
    install.packages(i)
  }
  library(i , character.only = T)
}
rm(i, requiredPackages )
# Set correct Work Directory ----------------
switch (Sys.info()[4]  
    ,"lenovo"     = setwd("~/Nextcloud/TUE/Current/2IMG10 - Topological Data Analysis/2img10-group-6/" )
    ,"bakbeestje" = setwd("~/Nextcloud/TUE/Current/2IMG10 - Topological Data Analysis/2img10-group-6/" )
    )
    
# Settings ----------------------
formals(txtProgressBar)$style <- 3 # Set default style to 3.
theme_set(theme_linedraw()) # ggplot theme
# Sys.setlocale(locale ="en_GB.UTF-8") #sets locale to brittish english

# run the load dataset file.  -------------
# source('corona_datasets.R')
