##### HW18 #####
################
# Library
library(igraph); library(network); library(ggnetwork);
library(geomnet); library(HSAUR3); library(qgraph); library(psych);

# Problem 1
N = matrix(c(0,1,1,1,0,
             1,0,0,1,1,
             0,1,0,1,0,
             0,1,1,0,0,
             0,1,1,0,0),
           nrow=5, byrow=TRUE)
lab = LETTERS[1:5]
dimnames(N) = list(lab, lab)
gn = graph.adjacency(N)
plot(gn, vertex.color=2:6, vertex.size=15,
     edge.color="cornsilk4", edge.arrow.size=0.3)

# Problem 2
dat <- read.table("edgesdata3.txt",header=TRUE)
gdf = graph.data.frame(dat)
names = V(gdf)$name
net = graph_from_data_frame(dat, directed=FALSE, vertices=names)
V(net)$color = ifelse(V(net)$name=="CA", "red", "cornsilk")
E(net)$color ='gray'
E(net)$color = ifelse(E(net)$spec=="X", "red",
                      ifelse(E(net)$spec == "Y", "blue", "gray"))

plot(net, edge.arrow.size=0.2, vertex.label.cex=0.7,
     vertex.size=15, edge.arrow.size=0.5)

# Problem 3
library(mapproj); library(ggmap);
library(geomnet); library(ggnetwork);
data(bikes, package = "geomnet")

tripnet <- fortify(as.edgedf(bikes$trips), bikes$stations[,c(2,1,3:5)])
# create variable to identify Metro Stations
tripnet$Metro = FALSE
idx <- grep("Metro", tripnet$from_id)
tripnet$Metro[idx] <- TRUE

metro_map = get_map(location=c(left=-77.22257, bottom=39.05721,
                               right=-77.11271, top=39.14247))

#tripnet = na.omit(tripnet)
ggmap(metro_map) +
  geom_net(data = tripnet, layout.alg = NULL, labelon = TRUE, repel=TRUE,
           aes(from_id = from_id, to_id = to_id, x = long, y = lat,
               linewidth = n/20, color = Metro)) +
  scale_colour_manual("Metro Station", values=c("azure4", "red")) +
  theme_net() %+replace% theme(aspect.ratio=NULL, legend.position="bottom") +
  coord_map()

