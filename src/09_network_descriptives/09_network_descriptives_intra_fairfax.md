Load packages

    library(dplyr)
    library(igraph)
    library(lubridate)
    library(sna)
    library(intergraph)

Load data frame and filter for residence AND workplace within Fairfax
County.

    data <- read.csv("/sfs/qumulo/qhome/sm9dv/dspg20fairfax/data/od/jobsall.csv")

    # focus on within county first
    data <- data %>% 
      filter(grepl("^51059.+", w_geocode_tract) & grepl("^51059.+", h_geocode_tract))

Create edge list, which defines the "from" and "to" relationships of the
Fairfax workforce. We define "from" as residence and "to" as workplace.
For this script, we will define the weight as S000, which is the total
number of jobs, in other words,the total number of jobs for which a
worker lives in the "from" Census tract and commutes to the workplace in
the "to" Census tract (check interpretation).

    edgelist <- data %>% 
      select(h_geocode_tract, w_geocode_tract, S000) %>% 
      rename(from = h_geocode_tract, to = w_geocode_tract, weight = S000) %>% 
      group_by(from, to) %>% 
      summarize(weight = sum(weight)) %>% 
      arrange(-weight)

Next, we create the **igraph** network object. Simple graphs are graphs
which do not contain loop and multiple edges. The function below keeps
the loops, which would indicate working and living in the same tract,
but removes multiple edges, because we will start wth a simple, directed
graph.

    network <- simplify(graph.data.frame(edgelist, directed = TRUE), 
                             remove.loops = FALSE, 
                             edge.attr.comb = igraph_opt("edge.attr.comb"))
    #D: directed
    #N: Named
    #W: Weighted

    print(network)

    ## IGRAPH 12ab6f5 DNW- 258 32620 -- 
    ## + attr: name (v/c), weight (e/n)
    ## + edges from 12ab6f5 (vertex names):
    ##  [1] 51059480202->51059480202 51059480202->51059482501 51059480202->51059460502
    ##  [4] 51059480202->51059461602 51059480202->51059490103 51059480202->51059471202
    ##  [7] 51059480202->51059482601 51059480202->51059490101 51059480202->51059471301
    ## [10] 51059480202->51059461601 51059480202->51059461100 51059480202->51059471100
    ## [13] 51059480202->51059492100 51059480202->51059491702 51059480202->51059492000
    ## [16] 51059480202->51059460400 51059480202->51059491201 51059480202->51059481106
    ## [19] 51059480202->51059482203 51059480202->51059482602 51059480202->51059440800
    ## [22] 51059480202->51059481202 51059480202->51059422102 51059480202->51059431001
    ## + ... omitted several edges

### Summary Statistics

Next, we will work through summary statistics of our network with
descriptions of each, in the context of the origin-destination data.

    network_stats <- data.frame(var="S000") 

##### Counts

The *order* of a graph indicates the number of nodes (or vertices). In
this case, the nodes are Census tracts.

    network_stats$node_count <- gorder(network)  
    print(network_stats$node_count )

    ## [1] 258

The *size* of graph indicates the number of edges (or links). Relevant
to OD data, the an edge represents a residence-workplace relationship
between Census tracts.

    network_stats$edge_count <- gsize(network)  
    print(network_stats$edge_count)

    ## [1] 32620

We can also add up the sum of weights for a count of jobs represented in
the graph.

    network_stats$jobs_count <- sum(edgelist$weight)
    print(network_stats$jobs_count)

    ## [1] 239741

The **degree** indicates the number of adjacent edges. An adjacent edge
shares a common vertex. In relation to the OD, the degree function
produces a count of links for each node, in this case, for each Census
tract. The average degree is printed.

    network_stats$mean_deg_total <- mean(igraph::degree(network))
    network_stats$mean_deg_out <- mean(igraph::degree(network, mode = "out"))
    network_stats$mean_deg_in <- mean(igraph::degree(network, mode = "in"))
    print(network_stats$mean_deg_total)

    ## [1] 252.8682

    hist(igraph::degree(network), breaks = seq(0,425, 25), main="Histogram of Node Degree")

![](09_network_descriptives_intra_fairfax_files/figure-markdown_strict/unnamed-chunk-9-1.png)

**Betweenness** is a measure of centrality, which counts the number of
shortest paths (geodesic) going through a node.

    network_stats$mean_btw <- mean(round(sna::betweenness(intergraph::asNetwork(network), cmode="directed"), 4))
    print(network_stats$mean_btw)

    ## [1] 131.6938

    hist(round(sna::betweenness(intergraph::asNetwork(network), cmode="directed"), 4), main = "Histogram of Betweenness", breaks = seq(0, 500, 25))

![](09_network_descriptives_intra_fairfax_files/figure-markdown_strict/unnamed-chunk-10-1.png)

##### Isolates, Dyads, and Triads

Isolates are non-connected nodes, which means either the workplace or
residence did not have a corresponding relationship.

    network_stats$isolates <- sum(igraph::degree(simplify(network))==0)

Triads Why do we care about triads?

    network_stats$triads_003 <- igraph::triad.census(network)[1] # empty graph, no connections
    network_stats$triads_012 <- igraph::triad.census(network)[2]
    network_stats$triads_102 <- igraph::triad.census(network)[3] #graph with a mutual connection between two vertices.
    network_stats$triads_021D <- igraph::triad.census(network)[4]
    network_stats$triads_021U <- igraph::triad.census(network)[5]
    network_stats$triads_021C <- igraph::triad.census(network)[6]
    network_stats$triads_111D <- igraph::triad.census(network)[7]
    network_stats$triads_111U <- igraph::triad.census(network)[8]
    network_stats$triads_030T <- igraph::triad.census(network)[9]
    network_stats$triads_030C <- igraph::triad.census(network)[10]
    network_stats$triads_201 <- igraph::triad.census(network)[11] #graph with two mutual connection between two vertices.
    network_stats$triads_120D <- igraph::triad.census(network)[12]
    network_stats$triads_120U <- igraph::triad.census(network)[13]
    network_stats$triads_120C <- igraph::triad.census(network)[14]
    network_stats$triads_210 <- igraph::triad.census(network)[15]
    network_stats$triads_300 <- igraph::triad.census(network)[16] # complete graph

Dyads (this will change with directionality)

    # The number of pairs with mutual connections.
    network_stats$dyad_mut <- igraph::dyad_census(network)$mut

    # The number of pairs with non-mutual connections.
    network_stats$dyad_asym <- igraph::dyad_census(network)$asym

    # The number of pairs with no connections.
    network_stats$dyad_null <- igraph::dyad_census(network)$null

##### Density and Transitivity

The **diameter** of a graph is the length of the longest shortest path
(geodesic) passing through a node.

    network_stats$diameter <- diameter(network,directed=TRUE, 
                                       unconnected=if (network_stats$isolates == 0) {FALSE} else {TRUE}, weights=NA)

    #UNCONNECTED Logical, what to do if the graph is unconnected. If FALSE, the function will
    #return a number that is one larger the largest possible diameter, which is always
    #the number of vertices. If TRUE, the diameters of the connected components
    #will be calculated and the largest one will be returned

    #WEIGHTS Optional positive weight vector for calculating weighted distances. If the graph
    #has a weight edge attribute, then this is used by default.

Mean Distance: The average length of all the shortest paths from or to
the nodes in the network.

    network_stats$mean_distance <- mean_distance(network, directed = TRUE, 
                                       unconnected = if (network_stats$isolates == 0) {FALSE} else {TRUE})

Edge Density: ratio of the number of edges and the number of possible
edges. The density is in the middle (1 is max, 0 is min density).

    network_stats$density <- edge_density(network, loops=TRUE) 

Transitivity:

    network_stats$transitivity <- transitivity(network, weights = TRUE, type = "undirected")

### Sources

<http://www.people.vcu.edu/~gasmerom/MAT131/graphs.html#>:~:text=Two%20vertices%20are%20said%20to,edge%20(arc)%20connecting%20them.&text=Adjacent%20edges%20are%20edges%20that%20share%20a%20common%20vertex.&text=The%20degree%20of%20a%20vertex,edges%20incident%20with%20that%20vertex.&text=A%20graph%20is%20said%20to,are%20joined%20by%20a%20path.

<https://github.com/uva-bi-sdad/oss-2020/blob/master/src/github-network-analysis/02_international-collaboration/03_country-networks/03_ctry-nets-cum-analysis.Rmd>

<https://www.jessesadler.com/post/network-analysis-with-r/>

<https://kateto.net/wp-content/uploads/2018/03/R%20for%20Networks%20Workshop%20-%20Ognyanova%20-%202018.pdf>

<https://kateto.net/network-visualization>

<https://ebookcentral-proquest-com.proxy01.its.virginia.edu/lib/uva/reader.action?docID=4199226&ppg=185>

<https://lehd.ces.census.gov/data/lodes/LODES7/LODESTechDoc7.4.pdf>

<https://igraph.org/r/doc/igraph.pdf>
