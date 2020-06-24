#### File which defines a bunch of functions. 
# So that you dont have to hunt them down in the Rmds.


create_tda_mapper <- function(filter_values, x_coord, y_coord, intervals=25, num_bins=10, overlap=30) {
  
  basic.coords = data.frame(x_coord, y_coord)
  # Compute distance matrix
  basic.dist = dist(basic.coords)
  length(filter_values)
  if(length(filter_values) == 2) {
    twodim_intervals = c(intervals, intervals)
    return (
      mapper2D(
        dist_object = basic.dist,
        filter_values = filter_values, # Expects a list of two vectors
        num_intervals = twodim_intervals,
        percent_overlap = overlap,
        num_bins_when_clustering = num_bins
      )
    )
  }
  
  return (
    mapper(
      dist_object = basic.dist,
      filter_values = filter_values, 
      num_intervals = intervals,
      percent_overlap = overlap,
      num_bins_when_clustering = num_bins
    )
  )
}

plot_mapper <- function (mapper) {
  # Plot graph 
  graph <- graph.adjacency(mapper$adjacency, mode="undirected")
  plot(graph, layout = layout.auto(graph) )
  return(graph)
}



plotForceNetwork <- function(mapper, labels) {
  MapperNodes <- mapperVertices(mapper, labels)
  MapperLinks <- mapperEdges(mapper)
  
  forceNetwork(
    Nodes = MapperNodes, Links = MapperLinks, 
    Source = "Linksource", Target = "Linktarget",
    Value = "Linkvalue", NodeID = "Nodename",
    Group = "Nodegroup", opacity = 1, 
    linkDistance = 25, charge = -10,
    Nodesize = "Nodesize", legend = T
  ) 
}
