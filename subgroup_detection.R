library(igraph)
library(sna)
library(statnet)
library(xUCINET)
library(network)
adviceE<- read.csv('EdgeList_ds/advice-Edgelist_updated.csv', header = TRUE)

#cutpoints
netE<- network(adviceE[,1:2],matrix.type='edgelist', directed = TRUE,loops = TRUE, multiple=TRUE)

cpnet <- cutpoints(netE,mode="digraph", connected = 'weak',
    return.indicator=TRUE)
cpnet
components(netE) #68
gplot(netE,gmode="digraph",diag=FALSE, vertex.col=1:5,
    vertex.border=2:6,vertex.rot=(0:4)*72,
    displaylabels=TRUE,label.bg="gray90")

net2 <- netE
delete.vertices(net2, 23)
delete.vertices(net2, 29)
delete.vertices(net2, 35)
delete.vertices(net2, 38)
delete.vertices(net2, 53)
cpnet <- cutpoints(net2,mode="digraph", connected = 'weak',
    return.indicator=TRUE)
components(net2) #63
gplot(net2,gmode="digraph",diag=FALSE, vertex.col=1:5,
    vertex.border=2:6,vertex.rot=(0:4)*72,
    displaylabels=TRUE,label.bg="gray90")

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
brnet<- bridges(netE)
brnet
gplot(netE,gmode="digraph",vertex.col="#a73c4e",
      edge.col=brnet+2,
      jitter=FALSE,displaylabels=TRUE)



## Prep for using iGraph with clique 
detach(package:statnet)
library(igraph)
iNet <- graph_from_edgelist(as.matrix(adviceE[,1:2]),directed=TRUE)
layout_save <- layout_with_fr(iNet2)
##Loading Attributes 
attributes <- read.csv('Adjacency_ds/attr.csv',header = T)
head(attributes)
attributes$Hobbies..0.6. <- as.factor(attributes$Hobbies..0.6.)
eAttr <- attributes[attributes$ID %in% V(iNet)$name,]
eAttr <- eAttr[match(V(iNet)$name, eAttr$ID),]
V(iNet)$Hobbies <- eAttr$Hobbies..0.6.
colours <- c("#e00f0f", "#b5af36", "#9440dd", '#2d2d48', '#923f4d', '#136d13', '#18a095')
V(iNet)$hobbyColor<- as.character(factor((eAttr$Hobbies..0.6.),levels = sort(unique(eAttr$Hobbies..0.6.)),labels = colours))
##graph the true class as the department 
plot(simplify(iNet), vertex.label.color= 'white',vertex.color=V(iNet)$hobbyColor,
    vertex.label.cex= 1, edge.arrow.size=0.4, layout= layout_save, label.bg="gray90")

summary(iNet)
clique.number(iNet) #4
cliques(iNet, min=3) #47
largest.cliques(iNet) #6
maximal.cliques(iNet) #91
V(iNet)[unlist(largest.cliques(iNet))] #24

iNet2 <- iNet
iNet2 <- iNet2 -c('a9','a1','a3','a5','a6')
plot(simplify(iNet2), vertex.color= V(iNet2)$hobbyColor,vertex.label.color='white',
vertex.label.cex= 1, edge.arrow.size=0.4, layout= layout_save)
summary(iNet)
iNet

##k-Cores
library(intergraph)
library(network)
iGnet <- asIgraph(adviceE)%>%
    set_vertex_attr(.,'names', value=V(.))
class(iGnet)
graph.density(iNet)
coreness <- graph.coreness(iNet)
table(coreness)
 
 maxCoreness <- max(coreness) #6
maxCoreness

V(iGnet)$color <- coreness + 1 
op <- par(mar = rep(0, 4)) 
plot(simplify(iGnet),vertex.label.cex=0.8) 
par(op)
colors <- rainbow(maxCoreness)
op <- par(mar=rep(0,4))
plot(simplify(iGnet),vertex.label=coreness, vertex.color=colors[coreness])

V(iGnet)$name <- coreness
V(iGnet)$color <- colors[coreness]
iGnet1_6 <- iGnet
iGnet2_6 <- induced.subgraph(iGnet, vids=which(coreness > 1))
iGnet3_6 <- induced.subgraph(iGnet, vids=which(coreness > 2))
iGnet4_6 <- induced.subgraph(iGnet, vids=which(coreness > 3))
iGnet5_6 <- induced.subgraph(iGnet, vids=which(coreness > 4))
iGnet6_6 <- induced.subgraph(iGnet, vids=which(coreness > 5))

lay <- layout.fruchterman.reingold(iNet)
op <- par(mfrow=c(3,2),mar=c(3,0,2,0))
plot(simplify(iGnet1_6), layout=lay, main="All k-cores", vertex.label.cex= 1, edge.arrow.size=0.4)
plot(simplify(iGnet2_6), layout=lay[which(coreness >1),], main="k-cores 2-6", vertex.label.cex= 1, edge.arrow.size=0.4)
plot(simplify(iGnet3_6), layout=lay[which(coreness >2),], main="k-cores 3-6", vertex.label.cex= 1, edge.arrow.size=0.4)
plot(simplify(iGnet4_6), layout=lay[which(coreness >3),], main="k-cores 4-6", vertex.label.cex= 1, edge.arrow.size=0.4)
plot(simplify(iGnet5_6), layout=lay[which(coreness >4),], main="k-cores 5-6", vertex.label.cex= 1, edge.arrow.size=0.4)
plot(simplify(iGnet5_6), layout=lay[which(coreness >5),], main="k-cores 6-6", vertex.label.cex= 1, edge.arrow.size=0.4)

#Modularity
table(V(iNet)$Hobbies)
modularity(iNet, V(iNet)$Hobbies)

# Community detection algorithm for directed network
ceb <-cluster_edge_betweenness(simplify(iNet))
modularity(ceb)
membership(ceb)

co <- cluster_infomap(simplify(iNet))
modularity(co)
membership(co)

compare(as.numeric(factor(V(iNet)$Hobbies)), co, method = "adjusted.rand")
compare(as.numeric(factor(V(iNet)$Hobbies)), ceb, method = "adjusted.rand")
compare(co,ceb, method="adjusted.rand")

op<-par(mfrow=c(2,2), mar=c(2,0,2,0))
plot(ceb, simplify(iNet), vertex.label=V(iNet)$role, main="Edge Betweeness")
plot(co, simplify(iNet), vertex.label=V(iNet)$role, main="infoMap")

