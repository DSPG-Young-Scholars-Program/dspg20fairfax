vars <- c("S000", "SA01", "SA02",           
          "SA03", "SE01", "SE02", 
          "SE03", "SI01", "SI02", 
          "SI03" )

network_stats <- function(vars){
  
  data <- read.csv("/sfs/qumulo/qhome/sm9dv/dspg20fairfax/data/od/jobsall.csv")
  data <- data %>% 
    filter(grepl("^51059.+", w_geocode_tract) & grepl("^51059.+", h_geocode_tract))
  
  
  network_stats <- data.frame(var = vars)
  
  for(var in vars){
    
    # set up edge list
    edgelist <- data %>% 
      select(h_geocode_tract, w_geocode_tract, var) %>% 
      rename(from = h_geocode_tract, to = w_geocode_tract, weight = var) %>% 
      group_by(from, to) %>% 
      summarize(weight = sum(weight)) %>% 
      arrange(-weight)
    
    network <- simplify(graph.data.frame(edgelist, directed = TRUE), 
                        remove.loops = FALSE, 
                        edge.attr.comb = igraph_opt("edge.attr.comb"))
    
    if(is_weighted(network) != TRUE){
      stop("Network not weighted.")
    }
    
    if(is_directed(network)!= TRUE){
      stop("Network not directed.")
    }

    
    network_stats[network_stats$var == var, "node_count"] <- gorder(network)  
    network_stats[network_stats$var == var, "edge_count"] <- gsize(network)  
    network_stats[network_stats$var == var, "jobs_count"] <- sum(edgelist$weight)  
    
    network_stats[network_stats$var == var, "mean_deg_total"] <- mean(igraph::degree(network, mode = "total")) 
    network_stats[network_stats$var == var, "mean_deg_out"] <- mean(igraph::degree(network, mode = "out")) 
    network_stats[network_stats$var == var, "mean_deg_in"] <- mean(igraph::degree(network, mode = "in")) 
    
    #network_stats[network_stats$var == var, "mean_btw"] <- igraph::betweenness(network, direct= TRUE)
    
    network_stats[network_stats$var == var, "isolates"]  <- sum(igraph::degree(simplify(network))==0)
    
    
    network_stats[network_stats$var == var, "triads_003"] <- igraph::triad.census(network)[1] # empty graph, no connections
    network_stats[network_stats$var == var, "triads_012"] <- igraph::triad.census(network)[2]
    network_stats[network_stats$var == var, "triads_102"] <- igraph::triad.census(network)[3] #graph with a mutual connection between two vertices.
    network_stats[network_stats$var == var, "triads_021D"] <- igraph::triad.census(network)[4]
    network_stats[network_stats$var == var, "triads_021U"] <- igraph::triad.census(network)[5]
    network_stats[network_stats$var == var, "triads_021C"] <- igraph::triad.census(network)[6]
    network_stats[network_stats$var == var, "triads_111D"] <- igraph::triad.census(network)[7]
    network_stats[network_stats$var == var, "triads_111U"] <- igraph::triad.census(network)[8]
    network_stats[network_stats$var == var, "triads_030T"] <- igraph::triad.census(network)[9]
    network_stats[network_stats$var == var, "triads_030C"] <- igraph::triad.census(network)[10]
    network_stats[network_stats$var == var, "triads_201"] <- igraph::triad.census(network)[11] #graph with two mutual connection between two vertices.
    network_stats[network_stats$var == var, "triads_120D"] <- igraph::triad.census(network)[12]
    network_stats[network_stats$var == var, "triads_120U"] <- igraph::triad.census(network)[13]
    network_stats[network_stats$var == var, "triads_120C"] <- igraph::triad.census(network)[14]
    network_stats[network_stats$var == var, "triads_210"] <- igraph::triad.census(network)[15]
    network_stats[network_stats$var == var, "triads_300"] <- igraph::triad.census(network)[16] 
    
    
    network_stats[network_stats$var == var, "dyad_mut"] <- igraph::dyad_census(network)$mut
    network_stats[network_stats$var == var, "dyad_asym"] <- igraph::dyad_census(network)$asym
    network_stats[network_stats$var == var, "dyad_null"] <- igraph::dyad_census(network)$null
    
    network_stats[network_stats$var == var, "diameter"] <-  diameter(network,directed=TRUE, 
                                                                     unconnected=if (network_stats$isolates == 0) {FALSE} else {TRUE})
    network_stats[network_stats$var == var, "mean_distance"] <- mean_distance(network, directed = TRUE, 
                                                                              unconnected = if (network_stats$isolates == 0) {FALSE} else {TRUE})
    
    network_stats[network_stats$var == var, "density"] <- edge_density(network, loops=TRUE) 
    
    
    network_stats[network_stats$var == var, "transitivity_global"] <- transitivity(network, weights = TRUE, type = "global")
    network_stats[network_stats$var == var, "transitivity_local_avg"] <- mean(transitivity(network, weights = TRUE, type = "local"))
    
    louvain <- igraph::cluster_louvain(as.undirected(network))
    network_stats[network_stats$var == var, "louvain"] <- modularity(louvain)
    network_stats[network_stats$var == var, "louvain_scaled"] <- modularity(louvain) / gorder(network)
    network_stats[network_stats$var == var, "louvain_logged"] <- modularity(louvain) / log(gorder(network))
    
    fstgrdy <- cluster_fast_greedy(as.undirected(network))
    network_stats[network_stats$var == var, "fstgrdy"] <- modularity(fstgrdy)
    network_stats[network_stats$var == var, "fstgrdy_scaled"]  <- modularity(fstgrdy) / gorder(network)
    network_stats[network_stats$var == var, "fstgrdy_logged"]  <- modularity(fstgrdy) / log(gorder(network))
    
    
    
    
    
    
    
    
    assign(paste("network", var, sep = "_"), network, envir = .GlobalEnv)
    assign(paste("edgelist", var, sep = "_"), edgelist, envir = .GlobalEnv)  
    
  }
  
  network_stats <<- network_stats
  
}



network_stats(vars)







