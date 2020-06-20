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
    "maps",
    "sp",
    "TDAmapper",   # mapper package
    "igraph",      # graph package for TDAmapper.
    "fastcluster"  # clustering, masks stat::hclust()
    "zoo" # for Duitsland.R
)

for( i in requiredPackages ){
   
    if (!require(i , character.only = T )) {
        install.packages(i)
    }
    if ( !( i %in% (.packages() ) ) ) {
        print("loading package:")
        print(i)
        library(i , character.only = T)
    } 
}
rm(i, requiredPackages )

# Settings ----------------------
formals(txtProgressBar)$style <- 3 # Set default style to 3.
theme_set(theme_linedraw()) # ggplot theme
# Sys.setlocale(locale ="en_GB.UTF-8") #sets locale to brittish english

# run the load dataset file.  -------------
clean_long_names <- T 
source('corona_datasets.R')
