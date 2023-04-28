library(igraph)
friendship <- read.csv('friendship_edgelist.csv')
friends <- graph.data.frame(as.matrix(friendship), directed = FALSE)
friends
friend_adjacency <- get.adjacency(friends, sparse = FALSE)
class(friend_adjacency)
write.csv(friend_adjacency,"friend_adjacency.csv")
