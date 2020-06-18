
plot(NL_municipality_coordinates$Longitude,NL_municipality_coordinates$Latitude)

mun <- NL_municipality_coordinates$Municipality
lon <- NL_municipality_coordinates$Longitude
lat <- NL_municipality_coordinates$Latitude

m <- mapproject(lon,lat)
K <- data.frame(m,Corona_NL_Infections_municipality[1:355,1:35])
names(K)<-c("X","Y",names(Corona_NL_Infections_municipality))
K2 <-K[c(1:355),c(1:2,22)]
K2[is.na(K2)]=0

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










