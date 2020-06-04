#Call bootstrapper first. 
source("bootstrap.R")

m1 <- mapper1D(
  distance_matrix = dist(data.frame( x=2*cos(0.5*(1:100)), y=sin(1:100) )),
  filter_values = 2*cos(0.5*(1:100)),
  num_intervals = 15, #Higher = More accurate, but also more noise
  percent_overlap = 50,
  num_bins_when_clustering = 10)


g1 <- graph.adjacency(m1$adjacency, mode="undirected")
plot(g1, layout = layout.auto(g1) )

df = data.frame( x=2*cos(0.5*(1:100)), y=sin(1:100) )

ggplot(df) +
  geom_point(aes( x = x , y = y ))

dd <- hospital_city %>%
  select(x = Longitude, y = Latitude) %>%
  unique.data.frame()
ddd <- dist(dd)

n1 <- mapper1D(
  distance_matrix = dist(dd ),
  filter_values = dd$y,
)
plot(dd)

# sample points from two intertwined spirals
set.seed("1")
t <- runif(100, min=1, max=6.3) # theta
X <- data.frame( x = c( t*cos(t), -t*cos(t) ), y = c( t*sin(t), -t*sin(t) ) )
d <- dist(X)
plot(X[,1], X[,2])

filter <- X[,2] # height projection
num_intervals <- 10
percent_overlap <- 50
num_bins_when_clustering <- 10

m3 <- mapper1D(
  distance_matrix = d, 
  filter_values = filter,	
  # num_intervals = 10, # use default
  # percent_overlap = 50, # use default
  # num_bins_when_clustering = 10 # use default
)

g3 <- graph.adjacency(m3$adjacency, mode="undirected")
plot(g3, layout = layout.auto(g3) )


mmmm <- mapper2D()
gg <- graph.adjacency(mmmm$adjacency)
plot(gg)


g <- hospital_city %>% select(Latitude, Longitude, value)
f <- as.matrix(g)
f <- scale(g, center=F )
ksf <- ks::kde(
  f[,1:3], 
  H=diag(nrow=3), 
  eval.points = f[,1:3]
 )
ksf <- ks::kde(f)
mmm <- mapper2D(
  distance_matrix = dist(f),
  filter_values = f$value
  )
gg <- graph.adjacency(mmm$adjacency)
plot(gg)
plot(f)

g <- corona_world %>% select(day, cases, deaths)
f <- as.matrix(g)
f <- scale(g, center=F )
ksf <- ks::kde(
    f[,1:3], 
    H=diag(nrow=3), 
    eval.points = f[,1:3]
)
ksf <- ks::kde(f)
mmm <- mapper2D(
    distance_matrix = dist(f)
)
gg <- graph.adjacency(mmm$adjacency)
plot(gg)
plot(f)


##### Real dataset: the Miller-Reaven diabetes study
library("locfit")
library("ks")
data(chemdiab)
normdiab <- chemdiab
normdiab[,1:5] <- scale(normdiab[,1:5],center=FALSE)
normdiab.dist = dist(normdiab[,1:5])

filter.kde <- ks::kde(
  normdiab[,1:5],
  H=diag(1,nrow = 5),
  eval.points = normdiab[,1:5]
)$estimate

diab.mapper <- mapper1D(
  distance_matrix  = normdiab.dist,
  filter_values = filter.kde,
  num_intervals = 4,
  percent_overlap = 50,
  num_bins_when_clustering = 20)
diab.graph <- graph.adjacency(diab.mapper$adjacency, mode="undirected")
plot(diab.graph )


#filter ideeen
euclidean 
minowski
chebychev

# http://danifold.net/mapper/quickstart.html
# https://pypi.org/project/mapper/#history
# https://arxiv.org/abs/2004.03282
# 

world_map <- map_data("world")
ggplot(corona_world) +
    geom_point(aes(x= Longitude , y = Latitude , color = cases)) +
    geom_map(map = world_map)
