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

advices <- read.csv('EdgeList_ds/advice-Edgelist_updated.csv', header = TRUE)
advices <- graph_from_data_frame(advices, directed = TRUE)
advices2<- simplify(advices, remove.multiple = TRUE, remove.loops = TRUE)
advice_adjacency <- get.adjacency(advices2, sparse = FALSE, attr = 'weight' )
write.csv(advice_adjacency,"advice_adjacency.csv")
