#Call bootstrapper first. 
source("bootstrap.R")

# plot world map with dot sizes based on cases
world_map <- map_data("world")

ggplot(corona_world) +
  geom_point(aes(x= Longitude , y = Latitude , size = cases, colour="red")) +
  geom_map(dat=world_map, map = world_map, 
           aes(map_id=region), fill="white", color="black") +
  expand_limits(x = world_map$long, y = world_map$lat)




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
infection_city_comp <- na.omit(infection_city)

#only take data of one day
infection_city_comp <- infection_city_comp %>% filter(date == '2020-04-03')
infection_city_comp

# plot Dutch data with dot size based on cases
ggplot(infection_city_comp) +
  geom_point(aes(x= Longitude , y = Latitude , size = value)) 

# determine distance on the dutch data
infection_city_dist = dist(infection_city_comp[,4:5])
#infection_city_dist <- na.omit(infection_city_dist)

#create mapper
infection_city_mapper <- mapper(dist_object = infection_city_dist,
                                filter_values = infection_city_comp$value,
                                num_intervals = 75,
                                percent_overlap = 75,
                                num_bins_when_clustering = 20)

# show the mapper
infection_city_graph <- graph.adjacency(infection_city_mapper$adjacency, mode="undirected")
plot(infection_city_graph, layout = layout.auto(infection_city_graph) )


# determine the mean values of the number of infections
value.mean.vertex <- rep(0,infection_city_mapper$num_vertices)
for (i in 1:infection_city_mapper$num_vertices){
  points.in.vertex <- infection_city_mapper$points_in_vertex[[i]]
  value.mean.vertex[i] <-mean((infection_city_comp$value[points.in.vertex]))
}
value.mean.vertex

#set vertex size based on how many cities are represented by this vertex
vertex.size <- rep(0,infection_city_mapper$num_vertices)
for (i in 1:infection_city_mapper$num_vertices){
  points.in.vertex <- infection_city_mapper$points_in_vertex[[i]]
  vertex.size[i] <- length((infection_city_mapper$points_in_vertex[[i]]))
}
vertex.size


# Mapper graph with the vertices colored in function of latitude data and vertex size proportional to the number of points inside
value.mean.vertex.grey <- grey(1-(value.mean.vertex - min(value.mean.vertex))/(max(value.mean.vertex) - min(value.mean.vertex) ))
V(infection_city_graph)$color <- value.mean.vertex.grey
V(infection_city_graph)$size <- vertex.size
plot(infection_city_graph,main ="Mapper Graph")
legend(x=-2, y=-1, c("mean number small","mean number medium","mean number large"),pch=21,
       col="#777777", pt.bg=grey(c(1,0.5,0)), pt.cex=2, cex=.8, bty="n", ncol=1)

# library(networkD3)
# MapperNodes <- mapperVertices(infection_city_mapper, 1:100 )
# MapperLinks <- mapperEdges(infection_city_mapper)
# forceNetwork(Nodes = MapperNodes, Links = MapperLinks, 
#              Source = "Linksource", Target = "Linktarget",
#              Value = "Linkvalue", NodeID = "Nodename",
#              Group = "Nodegroup", opacity = 1, 
#              linkDistance = 10, charge = -40)  



