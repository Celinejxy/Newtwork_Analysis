library(igraph)
friendship <- read.csv('friendship_edgelist.csv')
friendship

friends <- graph.data.frame(as.matrix(friendship), directed = FALSE)
friends2 <- simplify(friends, remove.multiple = FALSE, remove.loops = TRUE)
friend_adjacency <- get.adjacency(friends2, sparse = FALSE)
class(friend_adjacency)
friend_adjacency
write.csv(friend_adjacency,"friend_adjacency.csv")
