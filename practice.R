install.packages('igraph')
library(igraph)
el=matrix(c('a','b','c','d','a','d','a','b','c','d'),ncol=2,byrow=TRUE) #a sample edgelist
g=graph.data.frame(el)
get.adjacency(g,sparse=FALSE)

print(el)
g

library(igraph)
friendship <- read.csv('friendship_edgelist.csv')
friends <- graph.data.frame(as.matrix(friendship), directed = FALSE)
friends
get.adjacency(friends, sparse = FALSE)
