vars <- c("S000", "SA01", "SA02",           
          "SA03", "SE01", "SE02", 
          "SE03", "SI01", "SI02", 
          "SI03" )



network_stats <- function(vars, years){
  library(dplyr)
  library(igraph)
  library(tnet)
  
  network_stats <- data.frame(var = rep(vars, each = length(years)), year = rep(years, length(vars)))
  
  for(year in years){
  
  data <- read.csv(paste("/sfs/qumulo/qhome/sm9dv/dspg20fairfax/data/od/jobs_all_", year, ".csv", sep = ""))
  data <- data %>% 
    filter(grepl("^51059.+", w_geocode_tract) & grepl("^51059.+", h_geocode_tract))
  

  
      for(var in vars){
        
        # set up edge list
        edgelist <- data %>% 
          select(h_geocode_tract, w_geocode_tract, var) %>% 
          rename(from = h_geocode_tract, to = w_geocode_tract, weight = var) %>% 
          group_by(from, to) %>% 
          summarize(weight = sum(weight)) %>% 
          arrange(-weight) %>%
          filter(weight != 0)
        

        network <- simplify(graph.data.frame(edgelist, directed = TRUE), 
                            remove.loops = FALSE, 
                            edge.attr.comb = igraph_opt("edge.attr.comb"))
      
        
        
        if(is_weighted(network) != TRUE){
          stop("Network not weighted.")
        }
        
        if(is_directed(network)!= TRUE){
          stop("Network not directed.")
        }
    
        
        network_stats[network_stats$var == var & network_stats$year == year, "node_count"] <- gorder(network)  
        network_stats[network_stats$var == var & network_stats$year == year, "edge_count"] <- gsize(network)  
        network_stats[network_stats$var == var & network_stats$year == year, "jobs_count"] <- sum(edgelist$weight)  
        
        network_stats[network_stats$var == var & network_stats$year == year, "mean_deg_total"] <- mean(igraph::degree(network, mode = "total")) 
        network_stats[network_stats$var == var & network_stats$year == year, "mean_deg_out"] <- mean(igraph::degree(network, mode = "out")) 
        network_stats[network_stats$var == var & network_stats$year == year, "mean_deg_in"] <- mean(igraph::degree(network, mode = "in")) 
        
        network_stats[network_stats$var == var & network_stats$year == year, "mean_strength_total"] <- mean(igraph::strength(network, mode = "total")) 
        network_stats[network_stats$var == var & network_stats$year == year, "mean_strength_out"] <- mean(igraph::strength(network, mode = "out")) 
        network_stats[network_stats$var == var & network_stats$year == year, "mean_strength_in"] <- mean(igraph::strength(network, mode = "in")) 
        
        
        #network_stats[network_stats$var == var & network_stats$year == year, "mean_btw"] <- igraph::betweenness(network, direct= TRUE)
        
        network_stats[network_stats$var == var & network_stats$year == year, "isolates"]  <- sum(igraph::degree(simplify(network))==0)
        
        
        network_stats[network_stats$var == var & network_stats$year == year, "triads_003"] <- igraph::triad.census(network)[1] # empty graph, no connections
        network_stats[network_stats$var == var & network_stats$year == year, "triads_012"] <- igraph::triad.census(network)[2]
        network_stats[network_stats$var == var & network_stats$year == year, "triads_102"] <- igraph::triad.census(network)[3] #graph with a mutual connection between two vertices.
        network_stats[network_stats$var == var & network_stats$year == year, "triads_021D"] <- igraph::triad.census(network)[4]
        network_stats[network_stats$var == var & network_stats$year == year, "triads_021U"] <- igraph::triad.census(network)[5]
        network_stats[network_stats$var == var & network_stats$year == year, "triads_021C"] <- igraph::triad.census(network)[6]
        network_stats[network_stats$var == var & network_stats$year == year, "triads_111D"] <- igraph::triad.census(network)[7]
        network_stats[network_stats$var == var & network_stats$year == year, "triads_111U"] <- igraph::triad.census(network)[8]
        network_stats[network_stats$var == var & network_stats$year == year, "triads_030T"] <- igraph::triad.census(network)[9]
        network_stats[network_stats$var == var & network_stats$year == year, "triads_030C"] <- igraph::triad.census(network)[10]
        network_stats[network_stats$var == var & network_stats$year == year, "triads_201"] <- igraph::triad.census(network)[11] #graph with two mutual connection between two vertices.
        network_stats[network_stats$var == var & network_stats$year == year, "triads_120D"] <- igraph::triad.census(network)[12]
        network_stats[network_stats$var == var & network_stats$year == year, "triads_120U"] <- igraph::triad.census(network)[13]
        network_stats[network_stats$var == var & network_stats$year == year, "triads_120C"] <- igraph::triad.census(network)[14]
        network_stats[network_stats$var == var & network_stats$year == year, "triads_210"] <- igraph::triad.census(network)[15]
        network_stats[network_stats$var == var & network_stats$year == year, "triads_300"] <- igraph::triad.census(network)[16] 
        
        
        network_stats[network_stats$var == var & network_stats$year == year, "dyad_mut"] <- igraph::dyad_census(network)$mut
        network_stats[network_stats$var == var & network_stats$year == year, "dyad_asym"] <- igraph::dyad_census(network)$asym
        network_stats[network_stats$var == var & network_stats$year == year, "dyad_null"] <- igraph::dyad_census(network)$null
        
        network_stats[network_stats$var == var & network_stats$year == year, "diameter"] <-  diameter(network,directed=TRUE, 
                                                                         unconnected=if (network_stats$isolates == 0) {FALSE} else {TRUE})
        network_stats[network_stats$var == var & network_stats$year == year, "mean_distance"] <- mean_distance(network, directed = TRUE, 
                                                                                  unconnected = if (network_stats$isolates == 0) {FALSE} else {TRUE})
        
        network_stats[network_stats$var == var & network_stats$year == year, "density"] <- edge_density(network, loops=TRUE) 
        
        
        network_stats[network_stats$var == var & network_stats$year == year, "transitivity_global"] <- transitivity(network, weights = TRUE, type = "global")
        network_stats[network_stats$var == var & network_stats$year == year, "transitivity_local_avg"] <- mean(transitivity(network, weights = TRUE, type = "local"))
        
        louvain <- igraph::cluster_louvain(as.undirected(network))
        network_stats[network_stats$var == var & network_stats$year == year, "louvain"] <- modularity(louvain)
        network_stats[network_stats$var == var & network_stats$year == year, "louvain_scaled"] <- modularity(louvain) / gorder(network)
        network_stats[network_stats$var == var & network_stats$year == year, "louvain_logged"] <- modularity(louvain) / log(gorder(network))
        
        fstgrdy <- cluster_fast_greedy(as.undirected(network))
        network_stats[network_stats$var == var & network_stats$year == year, "fstgrdy"] <- modularity(fstgrdy)
        network_stats[network_stats$var == var & network_stats$year == year, "fstgrdy_scaled"]  <- modularity(fstgrdy) / gorder(network)
        network_stats[network_stats$var == var & network_stats$year == year, "fstgrdy_logged"]  <- modularity(fstgrdy) / log(gorder(network))
        
        
        network_stats[network_stats$var == var & network_stats$year == year, "centr_deg_total"] <- round(centr_degree(network, mode = "all")$centralization, 3)
        network_stats[network_stats$var == var & network_stats$year == year, "centr_deg_out"] <- round(centr_degree(network, mode = "out")$centralization, 3)
        network_stats[network_stats$var == var & network_stats$year == year, "centr_deg_in"] <- round(centr_degree(network, mode = "in")$centralization, 3)
        
        network_stats[network_stats$var == var & network_stats$year == year, "centr_clo_total"] <- round(centr_clo(network, mode = "all")$centralization, 3)
        network_stats[network_stats$var == var & network_stats$year == year, "centr_clo_out"] <- round(centr_clo(network, mode = "out")$centralization, 3)
        network_stats[network_stats$var == var & network_stats$year == year, "centr_clo_in"] <- round(centr_clo(network, mode = "in")$centralization, 3)
        
        network_stats[network_stats$var == var & network_stats$year == year, "centr_betw_total"] <-  round(centr_betw(network, directed = TRUE)$centralization, 3)
       
        network_stats[network_stats$var == var & network_stats$year == year, "centr_eigen_total"] <- round(centr_eigen(network, directed = TRUE)$centralization, 3)
        
        
        # nodelist
        
        
        nodelist <- data.frame(id = c(1:(igraph::vcount(network))), tract = igraph::V(network)$name)
        
        nodelist$deg_cent_total <- igraph::degree(network, mode = "all")
        nodelist$deg_cent_out <- igraph::degree(network, mode = "out")
        nodelist$deg_cent_in <- igraph::degree(network, mode = "in")
        
        nodelist$wtd_deg_cent_total <- igraph::strength(network, mode = "all")
        nodelist$wtd_deg_cent_out <- igraph::strength(network, mode = "out")
        nodelist$wtd_deg_cent_in <- igraph::strength(network, mode = "in")
        
        nodelist$btw_cent <- round(sna::betweenness(intergraph::asNetwork(network), cmode="directed"), 4)
        
        nodelist$close_cent_total <- igraph::centr_clo(network, mode = "all")$res 
        nodelist$close_cent_out <- igraph::centr_clo(network, mode = "out")$res  
        nodelist$close_cent_in <- igraph::centr_clo(network, mode = "in")$res 
        
        nodelist$eigen_cent <- igraph::eigen_centrality(network, directed = TRUE)$vector
        
        components <- components(network)
        fstgrdy <- fastgreedy.community(as.undirected(network))
        louvain <- cluster_louvain(as.undirected(network))
        
        nodelist$component <- components$membership
        nodelist$louvain_comm <- louvain$membership
        nodelist$fstgrdy_comm <- fstgrdy$membership
        
        assign(paste("nodelist", var, year, sep = "_"), nodelist, envir = .GlobalEnv)
        assign(paste("network", var, year, sep = "_"), network, envir = .GlobalEnv)
        assign(paste("edgelist", var, year,  sep = "_"), edgelist, envir = .GlobalEnv)  
        
      }

  
  }
  
  
  network_stats <<- network_stats
  
}









