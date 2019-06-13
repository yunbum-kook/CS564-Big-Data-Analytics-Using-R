### SN analysis part 2 ###
# Library
library(igraphdata); library(networkD3); library(statnet.common); library(jsonlite); library(visNetwork); library(igraph); library(threejs); library(dplyr); library(readr); library(visNetwork)

### 5. Vertex and Edge characteristics
# (1) Vertex Centrality
gs = graph.star(7, mode="undirected")
plot(gs, vertex.color=c("red", rep("cyan", 6)))

degree = degree(gs)
betweeness = betweeness(gs)
closeness = closeness(gs)
eigenvector = eigen_centrality(gs)$vect
data.frame(degree, betweeness, closeness, eigenvector)

# (2) Vertex Degree
# Sample data: karate club network
data(karate, package = 'igraphdata')
plot(karate, vertex.size=15)

V(karate)

# Distribution of Vertex Degree
hist(degree(karate), xlab='Vertex Degree',
     col='cyan', ylab='Frequency', xlim=c(0,50))
degree(karate)

# (3) Vertex Strength
# Distribution of Vertex Strength
hist(graph.strength(karate), col='magenta', xlim= c(0,50),
     xlab='Vertex Strength', ylab='Frequency', main="")
graph.strength(karate)

# (4) Vertex Strength
eb = edge.betweenness(karate)
E(karate)[order(eb, decreasing=T)[1:3]]
V(karate)
V(karate)$color = c("red", rep("cyan",18), "yellow", rep("cyan", 13), "blue")
plot(karate, edge.color="black")

### 6. Graph Partitioning
library(igraph)
data(karate,package="igraphdata")
kc <- cluster_fast_greedy(karate)
sizes(kc)

plot(kc,karate)
plot_dendrogram(kc)

### 8. Interactive Network Graphs with networkD3 and visNetwork
# (1) networkD3
web="https://cdn.rawgit.com/christophergandrud/networkD3/master/JSONdata/energy.json"
download.file(web,"energy.json")
library(jsonlite)
Energy <- fromJSON("energy.json")
Energy$nodes[1:5,]
Energy$links[1:5,]

sankeyNetwork(Links=Energy$links, Nodes=Energy$nodes, Source="source",
              Target="target", Value="value", NodeID="name",
              units="TWh", fontSize=12, nodeWidth=30)

# (2) visNetwork
# Minimal example
nodes = data.frame(id = 1:5)
edges = data.frame(from = 1:5, to = c(2,3,5,1,4))
visNetwork(nodes, edges, width="100%")
nodes$shadow = TRUE
nodes$color.border = "black"
nodes$color.background = c("red", "blue", "purple", "yellow")

#Directed Network with labels
nodes <- data.frame(id=1:5, group=c('A','A','B','B','B'),
                    label=LETTERS[1:5])
edges <- data.frame(from=c(2,5,3,3), to=c(1,2,4,2))
visNetwork(nodes, edges, width = "100%") %>%
  visNodes(shape = 'circle', shadow = TRUE ) %>%
  visEdges(arrows ='to') %>% 
  visGroups(groupname = 'A', color = 'gold') %>%
  visGroups(groupname = 'B', color = 'tomato')

### 9. Interactive 3D Scatter Plots, Networks, and Globes
library(igraph); library(threejs)
library(readr)
web <- "http://www.jaredlander.com/data/Flights_Jan_2.tsv"
flightJ2 <- read_tsv(web)
head(flightJ2,4)
airports <- flightJ2 %>% count(From_Lat,From_Long) %>% arrange(desc(n))
head(airports,4)

e1="http://mirrors.pglaf.org/nasa/bmng/world_8km/"
e2="world.topo.bathy.200412.3x5400x2700.jpg"
earth <- paste0(e1,e2)
library(igraph); library(threejs)
globejs(img=earth,lat=airports$From_Lat,
        long=airports$From_Long,
        value=airports$n*5, color="red",
        arcs=flightJ2 %>%
          select(From_Lat,From_Long,To_Lat,To_Long),
        arcsColor="#3e4ca2",arcsHeight=.4,arcsLwd=4,
        arcsOpacity=0.85, atmosphere=TRUE, fov=30)
