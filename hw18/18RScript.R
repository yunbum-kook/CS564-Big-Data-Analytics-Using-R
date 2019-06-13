### SN analysis 1 ###
# Library
library(igraph); library(network); library(ggnetwork); library(geomnet); library(HSAUR3); library(qgraph); library(psych);

## 1. Manipulating Network with igraph
# (1) Vertex and Edge Attributes in igraph
library(igraph)
gt <- graph_from_literal(A-+B,A-+C,A-+D,B-+E,B-+F,C-+G)
igraph.options(vertex.size=35, edge.arrow.size=0.4, edge.color=1)
plot(gt, layout=layout.auto, vertex.color='cyan')

# (2) Creating Graph from Data Frame Containing Edge List
web = "http://www.dimiter.eu/Data_files/edgesdata3.txt"
dat <-read.table(web, header=TRUE)
head(dat)
gdf <- graph.data.frame(dat)
vcount(guf)
ecount(gdf)

plot(gdf, edge.arrow.size=0.2, vertex.label.cex=0.7,
     vertex.size=15, vertex.color='ivory',
     edge.arrow.size=0.5, edge.color='deepskyblue')

# (3) Creating Graph from Two Datasets with Edge and Vertex Lists
library(igraph)
actor <- read.table("actor.txt",header=TRUE)
relation <- read.table("relation.txt",header=TRUE)
net <- graph_from_data_frame(relation,directed=TRUE,vertices=actor)
V(net)[sex=='M']$color = 'cyan'
V(net)[sex=='F']$color = 'magenta'
V(net)$size = 0.8 * V(net)$age
V(net)$width = 0.8 * E(net)$friendship
kam = layout.kamada.kawai(net)
plot(net, layout=kam, vertex.label=V(net)$name, edge.color='blue')
legend('bottomright', 1,0,title="Types of Sex",
       legend=c("Female", "Male"), fill=c("magenta", "cyan"))

# (4) Message Channels of the Network
dev.new()
N = matrix(c(0,1,1,0,1,0, 0,0,1,0,0,1, 1,0,0,1,0,0,
             0,1,0,0,0,0, 1,0,0,1,0,0, 0,1,1,0,0,0),
           nrow=6, byrow=TRUE)
lab = LETTERS[1:6]
dimnames(N) = list(lab, lab)
gn = graph.adjacency(N)
plot(gn, vertex.color=2:7, vertex.size=15,
     edge.color="cornsilk4", edge.arrow.size=0.3)

### 2. Manipulating Network with ggnetwork
# (1) Sample Data 1 : flo
data(flo, package = 'network')
head(flo,2)
gnet = ggnetwork(flo)

ggplot(gnet, aes(x, y, xend=xend, yend=yend)) +
  geom_edges(alpha=0.5) +
  geom_nodes(size=12, color="aliceblue")
  geom_nodetext(aes(label=vertex.names), fontface="bold") +
  theme_blank()

ggplot(gnet, aes(x, y, xend=xend, yend=yend)) +
  geom_edges(color="bisque3") + theme_blank() +
  geom_nodelabel(aes(label=vertex.names))

# (2) Sample Data 2 : bikes
data(bikes, package = "geomnet")
library(geomnet)
head(bikes$trips,2)
head(bikes$station,2)

library(ggnetwork)
tripnet <- fortify(as.edgedf(bikes$trips), bikes$stations[,c(2,1,3:5)])
# create variable to identify Metro Stations
tripnet$Metro = FALSE
idx <- grep("Metro", tripnet$from_id)
tripnet$Metro[idx] <- TRUE
tripnet

ggplot(aes(from_id=from_id, to_id=to_id), data=tripnet) +
  geom_net(aes(linewidth=n/15,colour=Metro),labelon=TRUE,repel=TRUE) +
  theme_net() + xlim(c(-0.1, 1.1)) +
  scale_colour_manual("Metro Station", values=c("azure4","red")) +
  theme(legend.position="bottom")

### 3. Interactive Graph Drawing
library(igraph)
net <- graph.star(26,mode="undirected")
V(net)$name <- LETTERS[1:26]
V(net)$size <- ifelse(V(net)$name=="A",20,10)
tkplot(net, edge.color="blue", vertex.label.color="black",
       vertex.label=V(net)$name, vertex.color=c("red", rep("cyan", 25)))

### 4. Network Visualizations of Complex Relationships Among Variables
data(USairpollution, package='HSAUR3')
head(USairpollution,2)

cor_usa = cor_auto(USairpollution)
qgraph(cor_usa, layout='spring', color='cornsilk', posCol='red', negCol='blue')

data(bfi); bfin=bfi[,1:25]
dim(bfin); head(bfin,2)

library(qgraph) 
cor_bfi <- cor_auto(bfin)
cols <- c(rep("aquamarine",5),rep("magenta",5),
          rep("yellow",5),rep("ivory",5),rep("cyan",5))
qgraph(cor_bfi,layout="spring",posCol='red', negCol='blue',
       minimum=0.25,color=cols)

qgraph(cor_bfi,layout="spring",posCol='red', negCol='blue',
       minimum=0.3,color=cols)

