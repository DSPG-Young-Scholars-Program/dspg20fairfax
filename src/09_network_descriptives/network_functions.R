
network_stats <- function(vars, types, years, centiserve = FALSE, bottleneck = FALSE){
  library(dplyr)
  library(igraph)
  library(tnet)
  library(expm)
  
  network_summary <- data.frame(year = rep(years, each = length(vars) * length(types)), 
                              type = rep(rep(types, each = length(vars)), length(years)),
                             var = rep(vars, length(years)*length(types)))
  
  network_summary <- network_summary[!(network_summary$year > 2015 & network_summary$type == "federal"),]
  
  for(year in years){
  
    for(type in types){
      

            
          if(!(type == "federal" & year > 2015)){
            
            data <- read.csv(paste0(here::here(), "/data/od/jobs_", type, "_", year, ".csv", sep = ""))
            #data <- read.csv(paste("/sfs/qumulo/qhome/sm9dv/dspg20fairfax/data/od/jobs_", type, "_", year, ".csv", sep = ""))
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
              
              nodelist$eigen_cent <- igraph::eigen_centrality(network, directed = TRUE)$vector
              
              components <- components(network)
              fstgrdy <- fastgreedy.community(as.undirected(network))
              louvain <- cluster_louvain(as.undirected(network))
              
              nodelist$component <- components$membership
              nodelist$louvain_comm <- louvain$membership
              nodelist$fstgrdy_comm <- fstgrdy$membership
              
              
              nodelist$page_rank <- page_rank(network, directed=TRUE, weights=NULL)$vector
              nodelist$alpha_cent <- alpha_centrality(network, weights=NULL)
              #nodelist$power_cent <- power_centrality(network, rescale=TRUE)
              #breaks on 2015, federal, S000
              nodelist$subgraph_cent <- subgraph_centrality(network,diag=TRUE)
              nodelist$auth_score <- authority_score(network, scale = TRUE, weights = NULL)$vector
              nodelist$hub_score <- hub_score(network, scale = TRUE, weights = NULL)$vector
              nodelist$local_trans <- transitivity(network, type = "local", weights = NULL)
              nodelist$in_ecc_cent <- eccentricity(network, mode = "in")
              nodelist$out_ecc_cent <- eccentricity(network, mode = "out")
              nodelist$load_cent <- sna::loadcent(get.adjacency(network,sparse=F),
                                                  gmode="digraph",diag=TRUE,cmode="directed",rescale=TRUE)
              #nodelist$info_cent <- sna::infocent(get.adjacency(network,sparse=F),
              #                                    gmode="digraph",diag=TRUE,cmode="directed",rescale=TRUE)
              #breaks on 2015, federal, S000
              nodelist$stress_cent <- sna::stresscent(get.adjacency(network,sparse=F),
                                                      gmode="digraph",diag=TRUE,cmode="directed",rescale=TRUE)
              nodelist$gilschmidt <- sna::gilschmidt(get.adjacency(network,sparse=F),
                                                     gmode = "digraph", diag = TRUE, normalize=TRUE)
              nodelist$hyper_index <- netrankr::hyperbolic_index(network)
              
              nodelist$k_core_in <- coreness(network, mode="in")
              nodelist$k_core_out <- coreness(network, mode="out")
              
              if(centiserve == TRUE){

                  nodelist$comm_bet <- centiserve::communibet(network, normalized = TRUE)
                  nodelist$decay_in <- centiserve::decay(network, mode="in", weights=NULL)
                  nodelist$decay_out <- centiserve::decay(network, mode="out", weights=NULL)
                  nodelist$diffus_indeg <- centiserve::diffusion.degree(network,mode="in",loops=TRUE)
                  nodelist$diffus_outdeg <- centiserve::diffusion.degree(network,mode="out",loops=TRUE)     
                  nodelist$entropy_in <- 1/centiserve::entropy(network,mode="in",weights=NULL)
                  nodelist$entropy_out <- 1/centiserve::entropy(network,mode="out",weights=NULL)
                  nodelist$geokpath_in <- centiserve::geokpath(network,mode="in",weights=NULL)
                  nodelist$geokpath_out <- centiserve::geokpath(network,mode="out",weights=NULL)
                  nodelist$laplacian_in <- centiserve::laplacian(network,mode="in",loops=TRUE)
                  nodelist$laplacian_out <- centiserve::laplacian(network,mode="out",loops=TRUE)
                  nodelist$leverage_in <- centiserve::leverage(network,mode="in",loops=TRUE)  
                  nodelist$leverage_out <- centiserve::leverage(network,mode="out",loops=TRUE)  
                  nodelist$lin_cent_in <- centiserve::lincent(network,mode="in",weights=NULL)
                  nodelist$lin_cent_out <- centiserve::lincent(network,mode="out",weights=NULL)
                  nodelist$lobby_in <- centiserve::lobby(network,mode="in",loops=TRUE) 
                  nodelist$lobby_out <- centiserve::lobby(network,mode="out",loops=TRUE) 
                  nodelist$markov_cent <- centiserve::markovcent(network) 
                  nodelist$radiality_in <- centiserve::radiality(network, mode="in",weights=NULL)   
                  nodelist$radiality_out <- centiserve::radiality(network, mode="out",weights=NULL)
                  nodelist$mnc_in <- centiserve::mnc(network, mode="in")
                  nodelist$mnc_out <- centiserve::mnc(network, mode="out")
                  nodelist$dmnc_in <- centiserve::dmnc(network, mode="in")
                  nodelist$dmnc_out <- centiserve::dmnc(network, mode="out")
                  nodelist$epc <- centiserve::epc(network) 
                  nodelist$topocoefficient <- 1/centiserve::topocoefficient(as.undirected(network))
              }
              
              if(bottleneck == TRUE){
                  nodelist$bottleneck_in <- centiserve::bottleneck(network, mode="in")
                  nodelist$bottleneck_out <- centiserve::bottleneck(network, mode="out")
              }
              
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




vars <- c("S000", "SA01", "SA02",           
          "SA03", "SE01", "SE02", 
          "SE03", "SI01", "SI02", 
          "SI03" )

years <- 2010:2017

types <- c("all", "private","federal")

library(tidycensus)

data_fairfax <-   get_acs(geography = "tract",
                          variables = "B00001_001",
                          state = "VA",
                          county = 059, 
                          key = Sys.getenv("CENSUS_API_KEY"),
                          geometry = TRUE)

leaflet_creator <- function(types){
  
  library(leaflet)
  library(RColorBrewer)
  
  for(type in types){
    nodelist <- get(paste("nodelist_2015_S000_", type, sep = ""))
    
    fairfax_data <- merge(data_fairfax[, c("GEOID", "geometry")], 
                          nodelist[, c("tract", "wtd_deg_cent_out", "wtd_deg_cent_in", "btw_cent", "eigen_cent")], 
                          by.x= "GEOID", by.y = "tract", all = TRUE)
    fairfax_data <- st_transform(fairfax_data, 4326)
    
    
    pal_out <- colorQuantile(palette = c("#DEEBF7", "#232d4b"),5,  domain = fairfax_data$wtd_deg_cent_out)
    pal_in <- colorQuantile(palette = c("#E5F5F9", '#0e879c'),5, domain = fairfax_data$wtd_deg_cent_in)
    pal_btw <- colorQuantile(palette = c("#FAF6DB", "#E6CE3A"),5,  domain = fairfax_data$btw_cent)
    pal_eigen <- colorQuantile(palette = c("#FEE6CE", "#E6A01D"),5,  domain = fairfax_data$eigen_cent)
    
    
    l_out <- leaflet(data = fairfax_data,options = leafletOptions(minZoom = 10))%>%
      addTiles("https://{s}.basemaps.cartocdn.com/rastertiles/voyager_labels_under/{z}/{x}/{y}.png") %>%
      addPolygons(fillColor = ~pal_out(wtd_deg_cent_out), fillOpacity = .8, stroke = FALSE) %>%
      addLegend("bottomleft", 
                pal = pal_out, 
                values =  ~wtd_deg_cent_out,
                title = "Weighted Degree Centrality: Out<br> By Quantile Group", 
                opacity = 1, 
                labFormat = function(type = "quantile", cuts = 10, p = wtd_deg_cent_out) {
                  n = length(cuts)
                  paste0("[", round(cuts[-n], 3), " &ndash; ", round(cuts[-1], 3), ")")
                })
    
    l_in <- leaflet(data = fairfax_data,options = leafletOptions(minZoom = 10))%>%
      addTiles("https://{s}.basemaps.cartocdn.com/rastertiles/voyager_labels_under/{z}/{x}/{y}.png") %>%
      addPolygons(fillColor = ~pal_in(wtd_deg_cent_in), fillOpacity = .8, stroke = FALSE) %>%
      addLegend("bottomleft", 
                pal = pal_in, 
                values = ~wtd_deg_cent_in,
                title =  "Weighted Degree Centrality: In<br> By Quantile Group", 
                opacity = 1,
                labFormat = function(type = "quantile", cuts = 10, p = wtd_deg_cent_in) {
                  n = length(cuts)
                  paste0("[", round(cuts[-n], 3), " &ndash; ", round(cuts[-1], 3), ")")
                })
    
    l_btw <- leaflet(data = fairfax_data,options = leafletOptions(minZoom = 10))%>%
      addTiles("https://{s}.basemaps.cartocdn.com/rastertiles/voyager_labels_under/{z}/{x}/{y}.png") %>%
      addPolygons(fillColor = ~pal_btw(btw_cent), fillOpacity = .8, stroke = FALSE) %>%
      addLegend("bottomleft", 
                pal = pal_btw, 
                values = ~btw_cent,
                title = "Betweenness Centrality<br> By Quantile Group", 
                opacity = 1, 
                labFormat = function(type = "quantile", cuts = 10, p = btw_cent) {
                  n = length(cuts)
                  paste0("[", round(cuts[-n], 3), " &ndash; ", round(cuts[-1], 3), ")")
                })
    
    l_eigen <- leaflet(data = fairfax_data,options = leafletOptions(minZoom = 10))%>%
      addTiles("https://{s}.basemaps.cartocdn.com/rastertiles/voyager_labels_under/{z}/{x}/{y}.png") %>%
      addPolygons(fillColor = ~pal_eigen(eigen_cent), fillOpacity = .8, stroke = FALSE) %>%
      addLegend("bottomleft", 
                pal = pal_eigen, 
                values = ~eigen_cent,
                title = "Eigen Centrality<br> By Quantile Group", 
                opacity = 1, 
                labFormat = function(type = "quantile", cuts = 10, p = eigen_cent) {
                  n = length(cuts)
                  paste0("[", round(cuts[-n], 3), " &ndash; ", round(cuts[-1], 3), ")")
                })
    
    
    assign(paste("fairfax_2015_S000_", type, sep =  ""), fairfax_data, envir = .GlobalEnv)
    
    assign(paste("pal_in_2015_S000_", type, sep =  ""), pal_in,envir = .GlobalEnv)
    assign(paste("pal_out_2015_S000_", type, sep =  ""), pal_out,envir = .GlobalEnv)
    assign(paste("pal_btw_2015_S000_", type, sep =  ""), pal_btw,envir = .GlobalEnv)
    assign(paste("pal_eigen_2015_S000_", type, sep =  ""), pal_eigen,envir = .GlobalEnv)
    
    assign(paste("l_out_2015_S000_", type, sep =  ""), l_out,envir = .GlobalEnv)
    assign(paste("l_in_2015_S000_", type, sep =  ""), l_in,envir = .GlobalEnv)
    assign(paste("l_btw_2015_S000_", type, sep =  ""), l_btw,envir = .GlobalEnv)
    assign(paste("l_eigen_2015_S000_", type, sep =  ""), l_eigen, envir = .GlobalEnv)
    
  }
}




#network_stats(vars = "S000", types= types, years= 2015)


#https://stackoverflow.com/questions/38698118/calculating-alpha-power-centralities-by-igraph

