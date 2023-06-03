# LOAD ALL LIBRARIES #
library(igraph)
library(sna)
library(statnet)
library(xUCINET)
library(network)
library(intergraph)

# SET UP NETWORK #
# Descriptions
desc_network <-"The Working network represents the undirected interactions between employees at a sunglasses company. 
                The interactions were deemed work related and thus coded as a working network.The Frienship network represents the undirected interactions between employees at a sunglasses company. 
                The interactions were deemed non-work related or contain personal information and thus coded 
                as a friendship network. The advice network represents the directed interactions between supervisors and employees at a 
                sunglasses company. The interactions are ranked from 1 to 5 with 5 being more important
                conversations."

desc_attr <- c("Department 0=Executive 1=Marketing 2=Sales 3=Human Resources 4=Distribution 5=Manufacturing 6=Finance",
               "Years Worked (1-25)",
               "Seniority 0=Entry Level 1=Junior 2=Senior",
               "Mentorship 0=Mentee 1=Mentor",
               "Lenses 0=don't need 1=need",
               "Age (21-63)",
               "Gender 0=Male 1=Female",
               "Hobbies 0=Poker 1=Tennis 2=Cooking 3=Golf 5=Coffee Roasting 6=Video Games")

# filepaths
w_filepath <- "~/Data/working_adjacency.csv"
f_filepath <- "~/Data/friend_adjacency.csv"
a_filepath <- "~/Data/advice_adjacency.csv"
attr_filepath <- "~/Data/attr.csv"

# Main Network
Sunglass_Network<-xCreateProject(GeneralDescription=desc_network,
                                 NetworkName="Working",
                                 NETFILE=w_filepath,
                                 FileType = "csv",
                                 InFormatType="AdjMat",
                                 NetworkDescription="Working relationships between coworkers",
                                 Mode=c("People"),
                                 Directed=FALSE,
                                 Loops=FALSE,
                                 Values="Ordinal",
                                 Class="matrix",
                                 References="No References")

Sunglass_Network <- xAddToProject(Sunglass_Network, NetworkName = "Friendship",
                                  NETFILE1 = f_filepath, 
                                  FileType = 'csv',
                                  InFormatType = 'AdjMat', 
                                  NetworkDescription = 'Friend relationships between employees',
                                  Mode=c('People'),
                                  Directed = FALSE,
                                  Loops = FALSE,
                                  Values = 'Ordinal',
                                  Class='matrix')

Sunglass_Network <- xAddToProject(Sunglass_Network, NetworkName = "Advice",
                                  NETFILE1 = a_filepath, 
                                  FileType = 'csv',
                                  InFormatType = 'AdjMat', 
                                  NetworkDescription = 'Weighted advice between supervisor and employee',
                                  Mode=c('People'),
                                  Directed = TRUE,
                                  Loops = FALSE,
                                  Values = 'Ordinal',
                                  Class='matrix')

Sunglass_Network <- xAddAttributesToProject(ProjectName = Sunglass_Network,
                                            ATTFILE1 = attr_filepath,
                                            FileType = "csv",
                                            Mode = c("People"),
                                            AttributesDescription = desc_attr)


#### Analyze cutpoints ####
# WORKING #
g <- graph.adjacency(Sunglass_Network$Working, mode = "undirected")
edgelist <- get.edgelist(g)
# Detatch igraph package in order to properly run
detach(package:igraph)
library(sna)
# Convert igraph edgelist back to network for proper working
w_netE<- network(edgelist,matrix.type='edgelist', directed = FALSE,loops = TRUE, multiple=TRUE)
# Identify cutpoints in the Working network
cpnet <- cutpoints(w_netE,mode="digraph", connected = 'weak',
                   return.indicator=TRUE)
cpnet
# graph uncut network
gplot(w_netE,gmode="digraph",diag=FALSE, vertex.col=1:5,
      vertex.border=2:6,vertex.rot=(0:4)*72,
      displaylabels=TRUE,label.bg="gray90")

# Identify cutpoints based on cpnet output == TRUE and remove that point
w_netE2 <- w_netE
delete.vertices(w_netE2, 12)
components(w_netE2)

gplot(w_netE2,gmode="digraph",diag=FALSE, vertex.col=1:5,
      vertex.border=2:6,vertex.rot=(0:4)*72,
      displaylabels=TRUE,label.bg="gray90")

# The single cut point was removed and showed a very minor change in the structure 
# of the graph.
library(igraph)

# FRIENDSHIP #
g <- graph.adjacency(Sunglass_Network$Friendship, mode = "undirected")
edgelist <- get.edgelist(g)

detach(package:igraph)
library(sna)

f_netE<- network(edgelist,matrix.type='edgelist', directed = FALSE,loops = TRUE, multiple=TRUE)

cpnet <- cutpoints(f_netE,mode="digraph", connected = 'weak',
                   return.indicator=TRUE)
cpnet

gplot(f_netE,gmode="digraph",diag=FALSE, vertex.col=1:5,
      vertex.border=2:6,vertex.rot=(0:4)*72,
      displaylabels=TRUE,label.bg="gray90")

f_netE2 <- f_netE
delete.vertices(f_netE2, 3)
delete.vertices(f_netE2, 5)
delete.vertices(f_netE2, 7)
delete.vertices(f_netE2, 10)
components(f_netE2)

gplot(f_netE2,gmode="digraph",diag=FALSE, vertex.col=1:5,
      vertex.border=2:6,vertex.rot=(0:4)*72,
      displaylabels=TRUE,label.bg="gray90")

library(igraph)
# ADVICE #
g <- graph.adjacency(Sunglass_Network$Advice, mode = "directed")
edgelist <- get.edgelist(g)

detach(package:igraph)
library(sna)

a_netE<- network(edgelist,matrix.type='edgelist', directed = TRUE,loops = TRUE, multiple=TRUE)

cpnet <- cutpoints(a_netE,mode="digraph", connected = 'weak',
                   return.indicator=TRUE)
cpnet

gplot(a_netE,gmode="digraph",diag=FALSE, vertex.col=1:5,
      vertex.border=2:6,vertex.rot=(0:4)*72,
      displaylabels=TRUE,label.bg="gray90")

a_netE2 <- a_netE
delete.vertices(a_netE2, 16)
delete.vertices(a_netE2, 22)
delete.vertices(a_netE2, 29)
delete.vertices(a_netE2, 32)
delete.vertices(a_netE2, 47)
delete.vertices(a_netE2, 60)
components(a_netE2)

gplot(a_netE2,gmode="digraph",diag=FALSE, vertex.col=1:5,
      vertex.border=2:6,vertex.rot=(0:4)*72,
      displaylabels=TRUE,label.bg="gray90")



#### Analyzing Bridges ####
bridges <- function(dat, mode = "graph", connected = c("strong", "weak")) {
  e_cnt <- network.edgecount(dat) 
  if (mode == "graph") {
    cmp_cnt <- components(dat) 
    b_vec <- rep(FALSE, e_cnt)
    for (i in 1:e_cnt) {
      dat2 <- delete.edges(dat, i)  # Assign modified network to dat2
      b_vec[i] <- (components(dat2) != cmp_cnt) 
    }
  } else {
    cmp_cnt <- components(dat, connected = connected) 
    b_vec <- rep(FALSE, e_cnt)
    for (i in 1:e_cnt) {
      dat2 <- delete.edges(dat, i)  # Assign modified network to dat2
      b_vec[i] <- (components(dat2, connected = connected) != cmp_cnt) 
    }
  }
  return(b_vec) 
}

# WORKING #
brnet<- bridges(w_netE)
gplot(w_netE,gmode="digraph",vertex.col="#a73c4e",
      edge.col=brnet+2,
      jitter=FALSE,displaylabels=TRUE)

# FRIEND #
brnet<- bridges(f_netE)
gplot(f_netE,gmode="digraph",vertex.col="#a73c4e",
      edge.col=brnet+2,
      jitter=FALSE,displaylabels=TRUE)

# ADVICE #
brnet<- bridges(a_netE)
gplot(a_netE,gmode="digraph",vertex.col="#a73c4e",
      edge.col=brnet+2,
      jitter=FALSE,displaylabels=TRUE)

#### CLIQUE ANALYSIS ####
detach(package:statnet)
library(igraph)
# ADVICE #
g <- graph.adjacency(Sunglass_Network$Advice, mode = "directed")
# calculate the largest cliques
maximal.cliques(g)
# format the senority attribute for the advice network to see if cliques match senority
eAttr <- Sunglass_Network$Attributes[Sunglass_Network$Attributes$NodeName %in% V(g)$name,]
eAttr <- eAttr[match(V(g)$name, eAttr$NodeName),]
Sunglass_Network$Attributes$Senority..1.3. <- as.factor(Sunglass_Network$Attributes$Senority..1.3.)
V(g)$Senority <- eAttr$Senority..1.3.
colours <- c("#e00f0f", "#b5af36", "#9440dd")
V(g)$SenorityColor<- as.character(factor((eAttr$Senority..1.3.),levels = sort(unique(eAttr$Senority..1.3.)),labels = colours))
##graph the true class as the senority 
plot(simplify(g), vertex.label.color= 'white',vertex.color=V(g)$SenorityColor,
     vertex.label.cex= 1, edge.arrow.size=0.1, layout= layout.auto, label.bg="gray90",asp = 1.2)

# WORKING #
g <- graph.adjacency(Sunglass_Network$Working, mode = "undirected")
# calculate the largest cliques
maximal.cliques(g)
# format the department attribute for the working network to see if cliques match department
eAttr <- Sunglass_Network$Attributes[Sunglass_Network$Attributes$NodeName %in% V(g)$name,]
eAttr <- eAttr[match(V(g)$name, eAttr$NodeName),]
Sunglass_Network$Attributes$Department <- as.factor(Sunglass_Network$Attributes$Department)
V(g)$Department <- eAttr$Department
colours <- c("#e00f0f", "#b5af36", "#9440dd", '#2d2d48', '#923f4d', '#136d13', '#18a095')
V(g)$DepartmentColor<- as.character(factor((eAttr$Department),levels = sort(unique(eAttr$Department)),labels = colours))
##graph the true class as the department 
plot(simplify(g), vertex.label.color= 'white',vertex.color=V(g)$DepartmentColor,
     vertex.label.cex= 1, edge.arrow.size=0.1, layout= layout.auto, label.bg="gray90",asp = 1.2)

#### K-Core Analysis ####
# ADVICE #
g <- graph.adjacency(Sunglass_Network$Advice, mode = "directed")
graph.density(g)
coreness <- graph.coreness(g)
table(coreness)
maxCoreness <- max(coreness)
maxCoreness
par(mfrow= c(1,1))
Vname <- get.vertex.attribute(g, name="vertex.names", index=V(g))
V(g)$color <- coreness+1
op <- par(mar=rep(0,4))
plot(simplify(g),vertex.label.cex=0.8,edge.arrow.size=0.3)
par(mfrow= c(1,1))
colors <- rainbow(maxCoreness)
op <- par(mar=rep(0,4))
plot(simplify(g), vertex.label=coreness, vertex.color=colors[coreness],edge.arrow.size=0.3)

V(g)$name <- coreness
V(g)$color <- colors[match(coreness, unique(coreness))]

g1_9 <- g
g2_9 <- induced.subgraph(g, vids=which(coreness > 1))
g3_9 <- induced.subgraph(g, vids=which(coreness > 2))
g4_9 <- induced.subgraph(g, vids=which(coreness > 3))
g5_9 <- induced.subgraph(g, vids=which(coreness > 4))
g6_9 <- induced.subgraph(g, vids=which(coreness > 5))
g7_9 <- induced.subgraph(g, vids=which(coreness > 6))
g8_9 <- induced.subgraph(g, vids=which(coreness > 7))
g9_9 <- induced.subgraph(g, vids=which(coreness > 8))
op <- par(mfrow=c(3,2),mar=c(3,0,2,0))
lay=layout.fruchterman.reingold(g)
plot(simplify(g1_9), layout=lay, main="All k-cores",vertex.label.cex= 1, edge.arrow.size=0.1)
plot(simplify(g2_9), layout=lay[which(coreness >1),], main="k-cores 2-9",vertex.label.cex= 1, edge.arrow.size=0.1)
plot(simplify(g3_9), layout=lay[which(coreness >2),], main="k-cores 3-9",vertex.label.cex= 1, edge.arrow.size=0.1)
plot(simplify(g4_9), layout=lay[which(coreness >3),], main="k-cores 4-9",vertex.label.cex= 1, edge.arrow.size=0.1)
plot(simplify(g5_9), layout=lay[which(coreness >4),], main="k-cores 5-9",vertex.label.cex= 1, edge.arrow.size=0.1)
plot(simplify(g6_9), layout=lay[which(coreness >5),], main="k-cores 6-9",vertex.label.cex= 1, edge.arrow.size=0.1)
plot(simplify(g7_9), layout=lay[which(coreness >6),], main="k-cores 7-9",vertex.label.cex= 1, edge.arrow.size=0.1)
plot(simplify(g8_9), layout=lay[which(coreness >7),], main="k-cores 8-9",vertex.label.cex= 1, edge.arrow.size=0.1)
plot(simplify(g9_9), layout=lay[which(coreness >8),], main="k-cores 9-9",vertex.label.cex= 1, edge.arrow.size=0.1)

# WORKING #
g <- graph.adjacency(Sunglass_Network$Working, mode = "undirected")
graph.density(g)
coreness <- graph.coreness(g)
table(coreness)
maxCoreness <- max(coreness)
maxCoreness
par(mfrow= c(1,1))
Vname <- get.vertex.attribute(g, name="vertex.names", index=V(g))
V(g)$color <- coreness+1
op <- par(mar=rep(0,4))
plot(simplify(g),vertex.label.cex=0.8,edge.arrow.size=0.3)
par(mfrow= c(1,1))
colors <- rainbow(maxCoreness)
op <- par(mar=rep(0,4))
plot(simplify(g), vertex.label=coreness, vertex.color=colors[coreness],edge.arrow.size=0.3)
# analysis: Because these network are so isolated, the removal of coreness shows
# no new information as each cluster is well defined by its coreness value.

#### Modularity Analysis And Clustering Algorithm Graphing ####
# ADVICE # 
# Senority #
g <- graph.adjacency(Sunglass_Network$Advice, mode = "directed")
eAttr <- Sunglass_Network$Attributes[Sunglass_Network$Attributes$NodeName %in% V(g)$name,]
eAttr <- eAttr[match(V(g)$name, eAttr$NodeName),]
Sunglass_Network$Attributes$Senority..1.3. <- as.factor(Sunglass_Network$Attributes$Senority..1.3.)
V(g)$Senority <- eAttr$Senority..1.3.

table(V(g)$Senority)
modularity(g, V(g)$Senority) 

# Community detection algorithm for directed network
cw <- cluster_walktrap(simplify(g))
modularity(cw)
ceb <-cluster_edge_betweenness(simplify(g))
modularity(ceb)
clp <- cluster_label_prop(simplify(g))
modularity(clp)
co <- cluster_infomap(simplify(g))
modularity(co)

# compare each clustering method to the attribute data
compare(as.numeric(factor(V(g)$Senority)), cw, method = "adjusted.rand")
compare(as.numeric(factor(V(g)$Senority)), ceb, method = "adjusted.rand")
compare(as.numeric(factor(V(g)$Senority)), clp, method = "adjusted.rand")
compare(as.numeric(factor(V(g)$Senority)), co, method = "adjusted.rand")
# plot the different clustering algorithms
op<-par(mfrow=c(2,2), mar=c(2,0,2,0))
plot(cw, simplify(g), vertex.label=V(g)$role, main="Walktrap",vertex.label.cex= 1, edge.arrow.size=0.1)
plot(ceb, simplify(g), vertex.label=V(g)$role, main="Edge Betweeness",vertex.label.cex= 1, edge.arrow.size=0.1)
plot(clp, simplify(g), vertex.label=V(g)$role, main="Label Prop",vertex.label.cex= 1, edge.arrow.size=0.1)
plot(co, simplify(g), vertex.label=V(g)$role, main="infoMap",vertex.label.cex= 1, edge.arrow.size=0.1)

