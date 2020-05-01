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
#install.packages('gridGraphics')
library(gridGraphics)
#install.packages('proj4')
library(proj4)
#install.packages('rgdal')
library(rgdal)
#install.packages('sp')
library(sp)
library(ggplot2)
library(mapproj)
library(sp)  

#read NL map and tidy it up
NLD <- readRDS("gadm36_NLD_1_sp.rds")
NLD_fortified <- fortify(NLD)

mun <- NL_municipality_coordinates$Municipality
lon <- NL_municipality_coordinates$Longitude
lat <- NL_municipality_coordinates$Latitude

#map_bounds <- c(left = 1, bottom = 47, right = 16, top = 56)
#coords.map <- get_stamenmap(map_bounds, zoom = 17, maptype = "toner-lite")
#plot(coords.map)

m <- mapproject(lon,lat)
K <- data.frame(m,Corona_NL_Infections_municipality[1:355,1:35])
names(K)<-c("X","Y",names(Corona_NL_Infections_municipality))
K2 <-K[c(1:355),c(1:2,33)]
K2[is.na(K2)]=0

f <- function(z){round(z/10)}
names(K2)<-c("X","Y","Z")

ggplot(K2, aes(x=X, y=Y))+
  geom_polygon(data = NLD_fortified, aes(x = long, y = lat, group = group), color = "grey", fill = "white")+
  geom_point(aes(x=X, y=Y,size=f(as.numeric(Z))), alpha=0.7)+
  scale_size(range = c(.1, 10), name="Infections")+
  coord_map()
