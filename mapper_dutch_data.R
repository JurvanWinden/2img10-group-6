#Call bootstrapper first. 
source("bootstrap.R")

# ggplot(hospital_city) +
#   geom_point(aes(x= Longitude , y = Latitude , size = value, color = 'red')) 
# 
# ggplot(hospital_city) +
#   geom_point(aes(x= Longitude , y = Latitude , size = value, color = 'red')) 
# 
# 
# ggplot(infection_city) +
#   geom_point(aes(x= Longitude , y = Latitude , size = value)) 

# infection_city_dist = dist(infection_city)
# infection_city_dist


#infection_city_mapper <- mapper(dist_object = infection_city_dist),

# delete all rows where an NA occurs
infection_city_comp_org <- na.omit(infection_city)

# take data of one day
infection_city_comp <- infection_city_comp_org %>% filter(date == '2020-04-20')

# plot Dutch data with dot size based on cases
# ggplot(infection_city_comp) +
#   geom_point(aes(x= Longitude , y = Latitude , size = value)) 

# determine the manhattan distance (Absolute distance between the two vectors) on the dutch data
infection_city_dist = dist(infection_city_comp[,5:6], method = "manhattan")


coord = infection_city_comp[,5:6]
# https://rdrr.io/cran/TDA/man/kde.html
# Kernel Density Estimator over a Grid of Points
KDE <- kde(coord, coord, 0.3)


# mapper based on manhattan distande and filter on KDE
infection_city_mapper <- mapper(dist_object = infection_city_dist,
                                filter_values = KDE,
                                num_intervals = 60,
                                percent_overlap = 90,
                                num_bins_when_clustering = 5)

# Create a graph based on the mapper
infection_city_graph <- graph.adjacency(infection_city_mapper$adjacency, mode="undirected")
# plot(infection_city_graph, layout = layout.auto(infection_city_graph) )


# determine the mean values of the number of infections per vertex of the graph
value.mean.vertex <- rep(0,infection_city_mapper$num_vertices)
for (i in 1:infection_city_mapper$num_vertices){
  points.in.vertex <- infection_city_mapper$points_in_vertex[[i]]
  value.mean.vertex[i] <-mean((infection_city_comp$value[points.in.vertex]))
}

# set vertex size based on how many cities are represented by this vertex
vertex.size <- rep(0,infection_city_mapper$num_vertices)
for (i in 1:infection_city_mapper$num_vertices){
  points.in.vertex <- infection_city_mapper$points_in_vertex[[i]]
  vertex.size[i] <- length((infection_city_mapper$points_in_vertex[[i]]))
}

# Mapper graph with the vertices colored in function of number of infections and vertex size proportional to the number of points inside
value.mean.vertex.grey <- grey(1-(value.mean.vertex - min(value.mean.vertex))/(max(value.mean.vertex) - min(value.mean.vertex) ))
V(infection_city_graph)$color <- value.mean.vertex.grey
V(infection_city_graph)$size <- vertex.size
infection_city_graph
# remark: If the plot is regenerated with the same data, the vertices are not drawn on the same place, but same vertex is represented by the same number.
# The number of holes and connected components are the same, independent of the day, the size of the vertex is also the same, only the color changes over time.
plot(infection_city_graph,main ="Mapper Graph")
legend(x=-2, y=-1, c("mean number small","mean number medium","mean number large"),pch=21,
       col="#777777", pt.bg=grey(c(1,0.5,0)), pt.cex=2, cex=.8, bty="n", ncol=1)



             
















# plot world map with dot sizes based on cases
world_map <- map_data("world")

# corona_world2 <- na.omit(corona_world)
# corona_world2 <- corona_world2%>% filter(day == '20', month == '4')
# 
# corona_world3 <- head(corona_world2, 20)
# corona_world3
# ggplot(corona_world3) +
#   geom_point(aes(x= Longitude , y = Latitude, colour="red")) +
#   geom_map(dat=world_map, map = world_map,
#            aes(map_id=region), fill="white", color="black") +
#   expand_limits(x = world_map$long, y = world_map$lat)


# delete all rows where an NA occurs
infection_corona_world <- na.omit(corona_world)

#only take data of Europe
infection_corona_world_Europe1 <- infection_corona_world %>% filter(continentExp == 'Europe')

#Get the number of deaths over complete time by country
sumDeaths <- aggregate(infection_corona_world_Europe1$cases, by=list(Category=infection_corona_world_Europe1$countriesAndTerritories), FUN=sum)
colnames(sumDeaths) <- c("countriesAndTerritories", "sumDeaths")

infection_corona_world_Europe1 <- merge(infection_corona_world_Europe1, sumDeaths,by="countriesAndTerritories")
#Filter on a day
infection_corona_world_Europe <- infection_corona_world_Europe1 %>% filter(day == '20', month == '4',)

# determine the manhattan distance (Absolute distance between the two vectors) on the Europe data
infection_corona_world_dist = dist(infection_corona_world_Europe[,12:13], method = "manhattan")

coord = infection_corona_world_Europe[,12:13]
# https://rdrr.io/cran/TDA/man/kde.html
# Kernel Density Estimator over a Grid of Points
KDE <- kde(coord, coord, 0.06)

# determine the ratio
infection_corona_world_Europe$ratio <- infection_corona_world_Europe$sumDeaths / infection_corona_world_Europe$popData2018

# mapper based on manhattan distande and filter on KDE
infection_corona_world_mapper <- mapper(dist_object = infection_corona_world_dist,
                                filter_values = infection_corona_world_Europe$ratio,
                                num_intervals = 15,
                                percent_overlap = 95,
                                num_bins_when_clustering = 5)

# show the mapper
infection_corona_world_graph <- graph.adjacency(infection_corona_world_mapper$adjacency, mode="undirected")
plot(infection_corona_world_graph, layout = layout.auto(infection_corona_world_graph) )


# determine the mean values of the number of infections
value.mean.vertex <- rep(0,infection_corona_world_mapper$num_vertices)
for (i in 1:infection_corona_world_mapper$num_vertices){
  points.in.vertex <- infection_corona_world_mapper$points_in_vertex[[i]]
  value.mean.vertex[i] <-mean((infection_corona_world_Europe$deaths[points.in.vertex]))
}
# value.mean.vertex

#set vertex size based on how many cities are represented by this vertex
vertex.size <- rep(0,infection_corona_world_mapper$num_vertices)
for (i in 1:infection_corona_world_mapper$num_vertices){
  points.in.vertex <- infection_corona_world_mapper$points_in_vertex[[i]]
  vertex.size[i] <- length((infection_corona_world_mapper$points_in_vertex[[i]]))
}
# vertex.size

# Mapper graph with the vertices colored in function of latitude data and vertex size proportional to the number of points inside
value.mean.vertex.grey <- grey(1-(value.mean.vertex - min(value.mean.vertex))/(max(value.mean.vertex) - min(value.mean.vertex) ))
V(infection_corona_world_graph)$color <- value.mean.vertex.grey
V(infection_corona_world_graph)$size <- vertex.size
plot(infection_corona_world_graph,main ="Mapper Graph")
legend(x=-2, y=-1, c("mean number small","mean number medium","mean number large"),pch=21,
       col="#777777", pt.bg=grey(c(1,0.5,0)), pt.cex=2, cex=.8, bty="n", ncol=1)


























