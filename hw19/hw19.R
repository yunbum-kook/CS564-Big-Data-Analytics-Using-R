##### HW19 #####
################
# Library
library(igraphdata); library(statnet.common);
library(visNetwork); library(igraph); library(threejs);
library(dplyr); library(readr); library(jsonlite);
library(networkD3);

# Problem 1
# (1)
mat=cor(t(mtcars[,c(1,3:6)])); mat[mat<0.995]=0
network=graph_from_adjacency_matrix( mat, weighted=TRUE, mode="directed", diag=FALSE)
plot(network, arrow.size=0.5, label.cex=0.2)

# (2)
kc = cluster_edge_betweenness(network, directed=TRUE, modularity = TRUE)
plot_dendrogram(kc)

df = scale(mtcars)
mat=cor(t(df[,c(1,3:6)])); mat[mat<0.995]=0
network=graph_from_adjacency_matrix(mat, weighted=TRUE, mode="directed", diag=FALSE)
kc = cluster_edge_betweenness(network, directed=TRUE, modularity = TRUE)
plot_dendrogram(kc)

####
hcst = hclust(dist(scale(mtcars)), "complete")
plot(hcst)
rect.hclust(hcst,4)

# (3)
hc = hclust(dist(mtcars), "ave")
radialNetwork(as.radialNetwork(hc))

# (4)
hc = hclust(dist(mtcars), "ave")
diagonalNetwork(as.radialNetwork(hc))

# (5)
hc = hclust(dist(mtcars), "ave")
dendroNetwork(hc, textColour=c("red", "magenta", "blue","green")[cutree(hc, 4)])
