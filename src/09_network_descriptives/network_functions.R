vars <- c("S000", "SA01", "SA02",           
          "SA03", "SE01", "SE02", 
          "SE03", "SI01", "SI02", 
          "SI03" )

years <- 2010:2017

types <- c("all", "private","federal")

network_stats <- function(vars, types, years){
  library(dplyr)
  library(igraph)
  library(tnet)
  
  network_summary <- data.frame(year = rep(years, each = length(vars) * length(types)), 
                              type = rep(rep(types, each = length(vars)), length(years)),
                             var = rep(vars, length(years)*length(types)))
  
  network_summary <- network_summary[!(network_summary$year > 2015 & network_summary$type == "federal"),]
  
  for(year in years){
  
    for(type in types){
      

            
          if(!(type == "federal" & year > 2015)){
            
            
            data <- read.csv(paste("/sfs/qumulo/qhome/sm9dv/dspg20fairfax/data/od/jobs_", type, "_", year, ".csv", sep = ""))
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
              
              
              
              network_summary[network_summary$var == var & network_summary$year == year & network_summary$type == type, "node_count"] <- gorder(network)  
              network_summary[network_summary$var == var & network_summary$year == year & network_summary$type == type, "edge_count"] <- gsize(network)  
              network_summary[network_summary$var == var & network_summary$year == year & network_summary$type == type, "jobs_count"] <- sum(edgelist$weight)  
              
              network_summary[network_summary$var == var & network_summary$year == year & network_summary$type == type, "mean_deg_total"] <- mean(igraph::degree(network, mode = "total")) 
              network_summary[network_summary$var == var & network_summary$year == year & network_summary$type == type, "mean_deg_out"] <- mean(igraph::degree(network, mode = "out")) 
              network_summary[network_summary$var == var & network_summary$year == year & network_summary$type == type, "mean_deg_in"] <- mean(igraph::degree(network, mode = "in")) 
              
              network_summary[network_summary$var == var & network_summary$year == year & network_summary$type == type, "mean_strength_total"] <- mean(igraph::strength(network, mode = "total")) 
              network_summary[network_summary$var == var & network_summary$year == year & network_summary$type == type, "mean_strength_out"] <- mean(igraph::strength(network, mode = "out")) 
              network_summary[network_summary$var == var & network_summary$year == year & network_summary$type == type, "mean_strength_in"] <- mean(igraph::strength(network, mode = "in")) 
              
              
              #network_summary[network_summary$var == var & network_summary$year == year & network_summary$type == type, "mean_btw"] <- igraph::betweenness(network, direct= TRUE)
              
              network_summary[network_summary$var == var & network_summary$year == year & network_summary$type == type, "isolates"]  <- sum(igraph::degree(simplify(network))==0)
              
              
              network_summary[network_summary$var == var & network_summary$year == year & network_summary$type == type, "triads_003"] <- igraph::triad.census(network)[1] # empty graph, no connections
              network_summary[network_summary$var == var & network_summary$year == year & network_summary$type == type, "triads_012"] <- igraph::triad.census(network)[2]
              network_summary[network_summary$var == var & network_summary$year == year & network_summary$type == type, "triads_102"] <- igraph::triad.census(network)[3] #graph with a mutual connection between two vertices.
              network_summary[network_summary$var == var & network_summary$year == year & network_summary$type == type, "triads_021D"] <- igraph::triad.census(network)[4]
              network_summary[network_summary$var == var & network_summary$year == year & network_summary$type == type, "triads_021U"] <- igraph::triad.census(network)[5]
              network_summary[network_summary$var == var & network_summary$year == year & network_summary$type == type, "triads_021C"] <- igraph::triad.census(network)[6]
              network_summary[network_summary$var == var & network_summary$year == year & network_summary$type == type, "triads_111D"] <- igraph::triad.census(network)[7]
              network_summary[network_summary$var == var & network_summary$year == year & network_summary$type == type, "triads_111U"] <- igraph::triad.census(network)[8]
              network_summary[network_summary$var == var & network_summary$year == year & network_summary$type == type, "triads_030T"] <- igraph::triad.census(network)[9]
              network_summary[network_summary$var == var & network_summary$year == year & network_summary$type == type, "triads_030C"] <- igraph::triad.census(network)[10]
              network_summary[network_summary$var == var & network_summary$year == year & network_summary$type == type, "triads_201"] <- igraph::triad.census(network)[11] #graph with two mutual connection between two vertices.
              network_summary[network_summary$var == var & network_summary$year == year & network_summary$type == type, "triads_120D"] <- igraph::triad.census(network)[12]
              network_summary[network_summary$var == var & network_summary$year == year & network_summary$type == type, "triads_120U"] <- igraph::triad.census(network)[13]
              network_summary[network_summary$var == var & network_summary$year == year & network_summary$type == type, "triads_120C"] <- igraph::triad.census(network)[14]
              network_summary[network_summary$var == var & network_summary$year == year & network_summary$type == type, "triads_210"] <- igraph::triad.census(network)[15]
              network_summary[network_summary$var == var & network_summary$year == year & network_summary$type == type, "triads_300"] <- igraph::triad.census(network)[16] 
              
              
              network_summary[network_summary$var == var & network_summary$year == year & network_summary$type == type, "dyad_mut"] <- igraph::dyad_census(network)$mut
              network_summary[network_summary$var == var & network_summary$year == year & network_summary$type == type, "dyad_asym"] <- igraph::dyad_census(network)$asym
              network_summary[network_summary$var == var & network_summary$year == year & network_summary$type == type, "dyad_null"] <- igraph::dyad_census(network)$null
              
              network_summary[network_summary$var == var & network_summary$year == year & network_summary$type == type, "diameter"] <-  diameter(network,directed=TRUE, 
                                                                                                                                                 unconnected=if (network_summary$isolates == 0) {FALSE} else {TRUE})
              network_summary[network_summary$var == var & network_summary$year == year & network_summary$type == type, "mean_distance"] <- mean_distance(network, directed = TRUE, 
                                                                                                                                                          unconnected = if (network_summary$isolates == 0) {FALSE} else {TRUE})
              
              network_summary[network_summary$var == var & network_summary$year == year & network_summary$type == type, "density"] <- edge_density(network, loops=TRUE) 
              
              
              network_summary[network_summary$var == var & network_summary$year == year & network_summary$type == type, "transitivity_global"] <- transitivity(network, weights = TRUE, type = "global")
              network_summary[network_summary$var == var & network_summary$year == year & network_summary$type == type, "transitivity_local_avg"] <- mean(transitivity(network, weights = TRUE, type = "local"))
              
              louvain <- igraph::cluster_louvain(as.undirected(network))
              network_summary[network_summary$var == var & network_summary$year == year & network_summary$type == type, "louvain"] <- modularity(louvain)
              network_summary[network_summary$var == var & network_summary$year == year & network_summary$type == type, "louvain_scaled"] <- modularity(louvain) / gorder(network)
              network_summary[network_summary$var == var & network_summary$year == year & network_summary$type == type, "louvain_logged"] <- modularity(louvain) / log(gorder(network))
              
              fstgrdy <- cluster_fast_greedy(as.undirected(network))
              network_summary[network_summary$var == var & network_summary$year == year & network_summary$type == type, "fstgrdy"] <- modularity(fstgrdy)
              network_summary[network_summary$var == var & network_summary$year == year & network_summary$type == type, "fstgrdy_scaled"]  <- modularity(fstgrdy) / gorder(network)
              network_summary[network_summary$var == var & network_summary$year == year & network_summary$type == type, "fstgrdy_logged"]  <- modularity(fstgrdy) / log(gorder(network))
              
              
              network_summary[network_summary$var == var & network_summary$year == year & network_summary$type == type, "centr_deg_total"] <- round(centr_degree(network, mode = "all")$centralization, 3)
              network_summary[network_summary$var == var & network_summary$year == year & network_summary$type == type, "centr_deg_out"] <- round(centr_degree(network, mode = "out")$centralization, 3)
              network_summary[network_summary$var == var & network_summary$year == year & network_summary$type == type, "centr_deg_in"] <- round(centr_degree(network, mode = "in")$centralization, 3)
              
              network_summary[network_summary$var == var & network_summary$year == year & network_summary$type == type, "centr_clo_total"] <- round(centr_clo(network, mode = "all")$centralization, 3)
              network_summary[network_summary$var == var & network_summary$year == year & network_summary$type == type, "centr_clo_out"] <- round(centr_clo(network, mode = "out")$centralization, 3)
              network_summary[network_summary$var == var & network_summary$year == year & network_summary$type == type, "centr_clo_in"] <- round(centr_clo(network, mode = "in")$centralization, 3)
              
              network_summary[network_summary$var == var & network_summary$year == year & network_summary$type == type, "centr_betw_total"] <-  round(centr_betw(network, directed = TRUE)$centralization, 3)
              
              network_summary[network_summary$var == var & network_summary$year == year & network_summary$type == type, "centr_eigen_total"] <- round(centr_eigen(network, directed = TRUE)$centralization, 3)
              
              
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
              
              #nodelist$eigen_cent <- igraph::eigen_centrality(network, directed = TRUE)$vector
              
              components <- components(network)
              fstgrdy <- fastgreedy.community(as.undirected(network))
              louvain <- cluster_louvain(as.undirected(network))
              
              nodelist$component <- components$membership
              nodelist$louvain_comm <- louvain$membership
              nodelist$fstgrdy_comm <- fstgrdy$membership
              
              assign(paste("nodelist", year, var, type, sep = "_"), nodelist, envir = .GlobalEnv)
              assign(paste("network", year, var, type, sep = "_"), network, envir = .GlobalEnv)
              assign(paste("edgelist", year, var, type,  sep = "_"), edgelist, envir = .GlobalEnv)  
              
            }
            
          } else {
      
            print("Federal networks only available for 2010-2015")
        }
      }
    }
  network_summary <<- network_summary
}

network_stats(vars = "S000", types= "federal", years= 2014:2016)








