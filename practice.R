library(igraph)
friendship <- read.csv('EdgeList_ds/friendship_edgelist.csv')
friends <- graph.data.frame(as.matrix(friendship), directed = FALSE)
friends2 <- simplify(friends, remove.multiple = FALSE, remove.loops = TRUE)
friend_adjacency <- get.adjacency(friends2, sparse = FALSE)
class(friend_adjacency)
friend_adjacency
write.csv(friend_adjacency,"friend_adjacency.csv")

working <- read.csv('EdgeList_ds/working_edgeList.csv', header=FALSE)
work <- graph.data.frame(as.matrix(working), directed = FALSE)
work2 <- simplify(work, remove.multiple = TRUE, remove.loops = TRUE)
work_adjacency <- get.adjacency(work2, sparse = FALSE)

work_adjacency
write.csv(work_adjacency,"work_adjacency.csv")
