# Main Library
library(igraph)

# Load data
nodes <- read.csv("~/lol_champs/nodes.csv")
links <- read.csv("~/lol_champs/links.csv")

# Creat new column in data
links$newColumn <- ifelse(links$relationship == "Friendly", 1, ifelse(links$relationship == "Antagonistic", 2, ifelse(links$relationship == "Related", 3, ifelse(links$relationship=="Romantic", 4, 5))))
nodes$id_nospaces <- gsub(" ", "", nodes$id, fixed = TRUE)
# Use a function from the igraph library to convert data to a network
net <- graph_from_data_frame(d=links, vertices=nodes, directed=F)

# Instal a color package
library(RColorBrewer)

# Create a color Array
colrs <- brewer.pal(5, "Pastel2")

#Defie edge colors by relationship
E(net)$color <- colrs[E(net)$newColumn]

#Plot Area
par(bg="white")

# Plot Relationships
plot(net, vertex.label.cex=0.5, vertex.label.dist = 0.2, vertex.label.font= 3, vertex.label.color="black" ,vertex.size = degree(net)/2, vertex.color = "grey" ,layout=layout.fruchterman.reingold)
legend(x=1.5, y=0.1, c("Friendly","Antagonistic", "Related", "Romantic", "Other"), pch=21,col="#777777", pt.bg=colrs, pt.cex=1, cex=.8, bty="n", ncol=1)

# Community detection

# A number of algorithms aim to detect groups that consist of densely connected nodes with fewer connections across groups.

# Community detection based on edge betweenness (Newman-Girvan)
# High-betweenness edges are removed sequentially (recalculating at each step) and the best partitioning of the network is selected.

ceb <- cluster_edge_betweenness(net)

length(ceb) # Number of Communities
membership(ceb) # community membership for each node
modularity(ceb) # how modular the graph partitioning is (High modularity for a partitioning reflects dense connections within communities and sparse connections across communities.)
crossing(ceb, net)   # boolean vector: TRUE for edges across communities


dendPlot(ceb, mode="hclust", cex=0.5)

plot(ceb,net, vertex.label.cex=0.5, vertex.label.dist = 0.2, vertex.label.font= 3, vertex.label.color="black" ,vertex.size = degree(net)/2, vertex.color = "grey", layout=layout.fruchterman.reingold)

# Community detection based on based on propagating labels
#Assigns node labels, randomizes, than replaces each vertex’s label with the label that appears most frequently among neighbors. Those steps are repeated until each vertex has the most common label of its neighbors.

clp <- cluster_label_prop(net)
plot(clp,net, vertex.label.cex=0.5, vertex.label.dist = 0.2, vertex.label.font= 3, vertex.label.color="black" ,vertex.size = degree(net)/2, vertex.color = "grey", layout=layout.fruchterman.reingold)
groups <- membership(clp)
print(groups)

# Community detection based on greedy optimization of modularity

net_non_multiple <- simplify( net, remove.multiple = T, remove.loops = F)
cfg <- cluster_fast_greedy(net_non_multiple)
plot(cfg,net, vertex.label.cex=0.5, vertex.label.dist = 0.2, vertex.label.font= 3, vertex.label.color="black" ,vertex.size = degree(net)/2, vertex.color = "grey", layout=layout.fruchterman.reingold)

# Generate Javascript interactive map

library('visNetwork') 

# nodes$shape <- "dot"  
nodes$shadow <- FALSE # Nodes will drop shadow
nodes$title <- nodes$id # Node label
nodes$borderWidth <- .2 # Node border width
nodes$color.border <- "black"
# nodes$color.highlight.background <- "orange"
nodes$color.highlight.border <- "darkred"
path_to_images <- "https://dl.dropboxusercontent.com/u/3769494/lol_champ_icons/"
nodes$image <- paste0(path_to_images, nodes$id_nospaces, "Square.png")
nodes$shape <- "circularImage"
deg <- degree(net, mode="all")
# nodes$size <- deg*2

links$color <- colrs[links$newColumn]


visNetwork(nodes, links)  %>%
  visPhysics(stabilization = FALSE) %>%
  visEdges(smooth = FALSE, width=5) %>% 
  visInteraction(navigationButtons = TRUE) %>% 
  visNodes(shapeProperties = list(useBorderWithImage = TRUE)) %>%
  visLayout(randomSeed = 2)

# Working Project

# Simple Net
nodes$degree = degree(net)
bad.vs<-nodes[nodes$degree<3] #identify those vertices part of less than three edges
net.simple<-delete.vertices(net, bad.vs) #exclude them from the graph

# Simple Net
bad.vs<-V(net)[degree(net)<3] #identify those vertices part of less than three edges
net.simple<-delete.vertices(net, bad.vs) #exclude them from the graph

# Plot Relationships
plot(net.simple, vertex.label.cex=0.5, vertex.label.dist = 0.2, vertex.label.font= 3, vertex.label.color="black" ,vertex.size = degree(net)/2, vertex.color = "grey", layout=layout.fruchterman.reingold)
legend(x=1.5, y=0.1, c("Friendly","Antagonistic", "Related", "Romantic", "Other"), pch=21,col="#777777", pt.bg=colrs, pt.cex=1, cex=.8, bty="n", ncol=1)

#Find Communities
clp <- cluster_label_prop(net.simple)
plot(clp,net.simple, vertex.label.cex=0.5, vertex.label.dist = 0.2, vertex.label.font= 3, vertex.label.color="black" ,vertex.size = degree(net)/2, vertex.color = "grey", layout=layout.fruchterman.reingold)
groups <- membership(clp)
print(groups)