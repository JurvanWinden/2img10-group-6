if (!require(package = "FNN")) {
  install.packages(pkgs = "FNN")
}
if (!require(package = "igraph")) {
  install.packages(pkgs = "igraph")
}
if (!require(package = "scales")) {
  install.packages(pkgs = "scales")
}
if (!require(package = "TDA")) {
  install.packages(pkgs = "TDA")
}
library(package = "TDA")
install.packages('gridGraphics')
library(gridGraphics)
install.packages('proj4')
library(proj4)
install.packages('rgdal')
library(rgdal)
install.packages('sp')
library(sp)
#install.packages('globe')
#library(package = "globe")

plot(NL_municipality_coordinates$Longitude,NL_municipality_coordinates$Latitude)

mun <- NL_municipality_coordinates$Municipality
lon <- NL_municipality_coordinates$Longitude
lat <- NL_municipality_coordinates$Latitude

LatLong <- data.frame(X = lat, Y = lon, Municipality=mun)
#add data to dataframe where muncipalities are equal
LatLong$Infections <- Corona_NL_Infections_municipality$`2020-03-30`[1:355] 
names(LatLong) <- c("X","Y","Municipality", "Infections")

# Convert it to a sp object
coordinates(LatLong) <- ~ Y + X # longitude first

# Add a coordinate reference system
proj4string(LatLong) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

# Project using spTransform
Utm <- spTransform(LatLong, CRS("+proj=utm +zone=11 ellps=WGS84"))

#plot it
plot(Utm, col=ifelse(is.na(Utm@data$Infections),'red','green'))
#turn it
grab_grob <- function(){
  grid.echo()
  grid.grab()
}
g <- grab_grob()
grid.newpage()
pushViewport(viewport(angle=230))
grid.draw(g)










