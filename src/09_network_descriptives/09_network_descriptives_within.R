library(dplyr)
library(igraph)
library(lubridate)
library(sna)
library(intergraph)

data <- read.csv("data/od/jobsall.csv")

# focus on within county first
data <- data %>% 
  filter(grepl("^51059.+", w_geocode_tract) & grepl("^51059.+", h_geocode_tract))


# from residence 
# to workplace
# S000 total number of jobs

edgelist <- data %>% 
  select(h_geocode_tract, w_geocode_tract, S000) %>% 
  rename(from = h_geocode_tract, to = w_geocode_tract, weight = S000) %>% 
  group_by(from, to) %>% 
  summarize(weight = sum(weight)) %>% 
  arrange(-weight)

#Simple graphs are graphs which do not contain loop and multiple edges.
## the function below keeps the loops, which would indicate working and living 
## in the same tract, but removes multiple edges, because we will start wth a simple, undirected graph.

network <- simplify(graph.data.frame(edgelist, directed = FALSE), 
                         remove.loops = FALSE, 
                         edge.attr.comb = igraph_opt("edge.attr.comb"))
is_weighted(network)

#U: undirected
#N: Named
#W: Weighted
print(network)

network_stats <- data.frame(var="SOOO") 

network_stats$node_count <- gorder(network)  
network_stats$edge_count <- gsize(network)
network_stats$jobs_count <- sum(edgelist$weight)
network_stats$mean_deg <- mean(degree(network, mode = "all"))
network_stats$mean_btw <- mean(round(sna::betweenness(intergraph::asNetwork(network), cmode="undirected"), 4))

