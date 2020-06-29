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
but removes multiple edges, because we will start wth a simple,
undirected graph.

(It might be good to create a direct network later, since we are
exploring flows FROM residence TO workplace.)

    network <- simplify(graph.data.frame(edgelist, directed = FALSE), 
                             remove.loops = FALSE, 
                             edge.attr.comb = igraph_opt("edge.attr.comb"))
    #U: undirected
    #N: Named
    #W: Weighted

    print(network)

    ## IGRAPH 29e6b37 UNW- 258 23930 -- 
    ## + attr: name (v/c), weight (e/n)
    ## + edges from 29e6b37 (vertex names):
    ##  [1] 51059480202--51059480202 51059480202--51059482501 51059480202--51059460502
    ##  [4] 51059480202--51059461602 51059480202--51059430400 51059480202--51059490103
    ##  [7] 51059480202--51059440501 51059480202--51059432202 51059480202--51059471202
    ## [10] 51059480202--51059482601 51059480202--51059430802 51059480202--51059490101
    ## [13] 51059480202--51059431500 51059480202--51059452400 51059480202--51059471301
    ## [16] 51059480202--51059492202 51059480202--51059491502 51059480202--51059432402
    ## [19] 51059480202--51059461601 51059480202--51059432500 51059480202--51059461100
    ## [22] 51059480202--51059491501 51059480202--51059471100 51059480202--51059491103
    ## + ... omitted several edges

### Summary Statistics

Next, we will work through summary statistics of our network with
descriptions of each, in the context of the origin-destination data.

    network_stats <- data.frame(var="S000") 

##### Counts

The *order* of a graph indicates the number of nodes (or vertices). In
this case, the nodes are Census tracts.

    network_stats$node_count <- gorder(network)  
    print(gorder(network))

    ## [1] 258

The *size* of graph indicates the number of edges (or links). Relevant
to OD data, the an edge represents a residence-workplace relationship
between Census tracts.

    network_stats$edge_count <- gsize(network)  
    print(gsize(network))

    ## [1] 23930

We can also add up the sum of weights for a count of jobs represented in
the graph.

    network_stats$jobs_count <- sum(edgelist$weight)
    print(sum(edgelist$weight))

    ## [1] 239741

The **degree** indicates the number of adjacent edges. An adjacent edge
shares a common vertex. In relation to the OD, the degree function
produces a count of links for each node, in this case, for each Census
tract. The average degree is printed.

    network_stats$mean_deg <- mean(igraph::degree(network))
    print(mean(igraph::degree(network)))

    ## [1] 185.5039

    hist(igraph::degree(network), breaks = seq(0,300, 25), main="Histogram of Node Degree")

![](09_network_descriptives_intra_fairfax_files/figure-markdown_strict/unnamed-chunk-9-1.png)

**Betweenness** is a measure of centrality, which counts the number of
shortest paths (geodesic) going through a node.

    network_stats$mean_btw <- mean(round(sna::betweenness(intergraph::asNetwork(network), cmode="undirected"), 4))
    print(mean(round(sna::betweenness(intergraph::asNetwork(network), cmode="undirected"), 4)))

    ## [1] 36.74031

    hist(round(sna::betweenness(intergraph::asNetwork(network), cmode="undirected"), 4), main = "Histogram of Betweenness", breaks = seq(0, 100, 10))

![](09_network_descriptives_intra_fairfax_files/figure-markdown_strict/unnamed-chunk-10-1.png)

##### Isolates, Dyads, and Triads

Isolates are non-connected nodes, which means either the workplace or
residence did not have a corresponding relationship.

    network_stats$isolates <- sum(igraph::degree(simplify(network))==0)

Triads (these will change if directionality is included.)

Why do we care about triads?

    network_stats$triads_003 <- igraph::triad.census(network)[1]

    ## Warning in igraph::triad.census(network): At motifs.c:1052 :Triad census called
    ## on an undirected graph

    # empty graph, no connections

    network_stats$triads_102 <- igraph::triad.census(network)[3]

    ## Warning in igraph::triad.census(network): At motifs.c:1052 :Triad census called
    ## on an undirected graph

    #  graph with a mutual connection between two vertices.

    network_stats$triads_201 <- igraph::triad.census(network)[11]

    ## Warning in igraph::triad.census(network): At motifs.c:1052 :Triad census called
    ## on an undirected graph

    #graph with two mutual connection between two vertices.

    network_stats$triads_300 <- igraph::triad.census(network)[16]

    ## Warning in igraph::triad.census(network): At motifs.c:1052 :Triad census called
    ## on an undirected graph

    # complete graph

Dyads (this will change with directionality)

    # The number of pairs with mutual connections.
    network_stats$dyad_mut <- igraph::dyad_census(network)$mut

    ## Warning in igraph::dyad_census(network): At motifs.c:858 :Dyad census called on
    ## undirected graph

    # The number of pairs with non-mutual connections.
    network_stats$dyad_asym <- igraph::dyad_census(network)$asym

    ## Warning in igraph::dyad_census(network): At motifs.c:858 :Dyad census called on
    ## undirected graph

    # The number of pairs with no connections.
    network_stats$dyad_null <- igraph::dyad_census(network)$null

    ## Warning in igraph::dyad_census(network): At motifs.c:858 :Dyad census called on
    ## undirected graph

##### Density and Transitivity

The **diameter** of a graph is the length of the longest shortest path
(geodesic) passing through a node.

### Sources

<http://www.people.vcu.edu/~gasmerom/MAT131/graphs.html#>:~:text=Two%20vertices%20are%20said%20to,edge%20(arc)%20connecting%20them.&text=Adjacent%20edges%20are%20edges%20that%20share%20a%20common%20vertex.&text=The%20degree%20of%20a%20vertex,edges%20incident%20with%20that%20vertex.&text=A%20graph%20is%20said%20to,are%20joined%20by%20a%20path.

<https://github.com/uva-bi-sdad/oss-2020/blob/master/src/github-network-analysis/02_international-collaboration/03_country-networks/03_ctry-nets-cum-analysis.Rmd>

<https://www.jessesadler.com/post/network-analysis-with-r/>

<https://kateto.net/wp-content/uploads/2018/03/R%20for%20Networks%20Workshop%20-%20Ognyanova%20-%202018.pdf>

<https://kateto.net/network-visualization>

<https://ebookcentral-proquest-com.proxy01.its.virginia.edu/lib/uva/reader.action?docID=4199226&ppg=185>

<https://lehd.ces.census.gov/data/lodes/LODES7/LODESTechDoc7.4.pdf>

<https://igraph.org/r/doc/igraph.pdf>
