library(igraph)
library(sna)

# Generate random edgelist
prob <- sapply(runif(70, 0, 1) * 0.1, rep, 70) # Edge probability matrix
random <- rgraph(70, tprob = prob, mode = 'digraph', return.as.edgelist = TRUE)

# Assign names and positions to the nodes
random[, 1] <- paste('a', random[, 1], sep = '') # Prefix 'a' to first column
random[, 2] <- paste('a', random[, 2], sep = '') # Prefix 'a' to second column

# Print the first two columns of the random edgelist
head(random[, 1:2])



# Create a graph from the edgelist
library(igraph)

graph <- graph_from_edgelist(as.matrix(random[, 1:2]), directed = TRUE)




##Loading Attributes 
attributes <- read.csv('Adjacency_ds/attr.csv',header = T)
head(attributes)
attributes$Department <- as.factor(attributes$Department)
eAttr <- attributes[attributes$ID %in% V(graph)$name,]
eAttr <- eAttr[match(V(graph)$name, eAttr$ID),]
eAttr


layout_save <- layout_with_fr(graph)
plot(graph,vertex.color = 'slateblue2', 
     vertex.label.color = "white", 
     layout = layout_save)

# Convert the graph to an adjacency matrix
adj_matrix <- as_adjacency_matrix(graph, sparse = FALSE)

# Print the adjacency matrix
print(adj_matrix)

##Vignette 1. Random Network- Brokerage roles&position
bNet <- as.network(random[,1:2], loops =TRUE, multiple=TRUE, directed=TRUE)
bNet %v% 'department' <- as.character(eAttr$Department)
adviceBro<-  brokerage(bNet, 'department')
head(adviceBro$raw.nli)


##Vignett 2 Random Network- Structural Equivalence 

##Vignette 2.Structural Equivalence
adj_mat <- as.matrix(as_adj(graph))
diag(adj_mat) <- 0
adj_mat
a_row <- adj_mat["a1", !colnames(adj_mat) %in% c("a1", "a2")]
b_row <- adj_mat["a2", !colnames(adj_mat) %in% c("a1", "a2")]
sum(abs(a_row-b_row))

# Not structurally equivalent, the absolute difference is 5

perfect_equivalence <- function(mat){
  matrix_vals <- mat
  matrix_vals[] <- 0
  # loop over the actors in the network, comparing pair-wise their values 
  for(i in 1:nrow(mat)){
    for(j in 1:nrow(mat)){
      a_row <- mat[i, c(-i, -j)]
      b_row <- mat[j, c(-i, -j)]
      abs_diff <- sum(abs(a_row-b_row)) # take sum of absolute differences 
      matrix_vals[i,j] <- abs_diff
    }
  }
  return(matrix_vals)
}

structurally_equivalent <- perfect_equivalence(adj_mat)

#convert into similarity matrix 
structurally_equivalent_sim <- 1-(structurally_equivalent/max(structurally_equivalent))

#KMeans Clustering 
group_ids_4 <- kmeans(structurally_equivalent_sim, centers = 4)
group_ids_4
plot(net, 
     vertex.color = group_ids_4$cluster, 
     layout = layout_save)

group_ids_5 <- kmeans(structurally_equivalent_sim, centers = 5)
group_ids_5
plot(net, 
     vertex.color = group_ids_5$cluster, 
     layout = layout_save)

#Vignette 3. Block Modeling with CONCOR
net_cor = cor(adj_mat)
net_cor[] <- ifelse(is.na(net_cor[]), 0, net_cor[])
net_concor <- net_cor
for(i in 1:200){
  net_concor <- cor(net_concor)
}
range(net_concor)
group <- net_concor[, 1] > 0
split_results <- list(adj_mat[, names(group[group])], adj_mat[, names(group[!group])])

cor_many_times <- function(x, times = 1000){
  for(i in 1:times){
    x <- cor(x)
    if(sd(x, na.rm = T) == 0){
      return(x)
    } else {
      x[] <- ifelse(is.na(x[]), 0, x[])
    }
  }
  return(x)
}

split_results_corred <- lapply(split_results, cor_many_times)
groups_2 <- lapply(split_results_corred, function(x) x[, 1] > 0)

split_results_again <- lapply(groups_2, 
    function(x) list(adj_mat[, names(x[x])], adj_mat[, names(x[!x])]))
split_results_again <- unlist(split_results_again, recursive = F)

final_blocks <- lapply(split_results_again, colnames)
final_blocks

clusters <- lapply(1:length(final_blocks), function(x) rep(x, length(final_blocks[[x]])))
clusters <- unlist(clusters)
names(clusters) = unlist(final_blocks)
clusters <- clusters[colnames(adj_mat)]

all_output = sna::blockmodel(adj_mat, 
                             clusters, 
                             glabels = "Feudal Network", 
                             plabels = colnames(adj_mat))
summary(all_output)
plot(simplify(graph), vertex.color = clusters)

# Vignette 3.Isomorphoc Local Graph
local_graphs <- make_ego_graph(netRandom, order = 4)
iso_mat <- matrix(FALSE, nrow = length(local_graphs), ncol = length(local_graphs))
for(i in 1:length(local_graphs)){
  for(j in 1:length(local_graphs)){
    iso_mat[i,j] <- isomorphic(local_graphs[[i]], local_graphs[[j]], method = 'vf2')
  }
}
iso_mat[] <- as.numeric(iso_mat[])
diag(iso_mat) = 0
clusters_iso <- cluster_louvain(graph.adjacency(iso_mat, mode = "undirected"))
plot(simplify(netRandom), 
     vertex.color = membership(clusters_iso), 
     layout = layout_save )

# Vignette 4. Stochastic Block Models
library(blockmodels)
sbm_out <- BM_bernoulli("SBM", 
                             as.matrix(adj_mat), 
                             verbosity = 3, 
                             plotting = "",
                             exploration_factor = 5) 
# run a bernoulli block model on the feudal matrix. 
# estimate the result
sbm_out$estimate()
best_fit_assignments <- sbm_out$memberships[[which.min(sbm_out$ICL)]]
 # extract the fit which is best according to ICL (Integrated Completed Likelihood), a measure for selecting the best model
head(best_fit_assignments$Z) # probabilities of belonging to each group
class_assignments <- apply(best_fit_assignments$Z, 1, which.max)
 # identify which column as the highest value for each node (row)

sbm_output = sna::blockmodel(adj_mat, 
                             class_assignments, 
                             glabels = "Feudal", 
                             plabels = colnames(adj_mat))

plot(sbm_output)


# Vignette 5. Feature-Based 
# this function limits the network to a given node's neighborhood 
#(defined n-steps away) and then runs the triad census on that neighborhood.
local_man <- function(graph, vertex_name, steps = 2){
  n_steps <- ego(net, order = steps, vertex_name, mode = "out")[[1]]
  subnet <- igraph::delete.vertices(net, !V(net) %in% n_steps)
  local_triad_count <- igraph::triad.census(subnet)
  return(local_triad_count)
}

onestep_triad_counts <- lapply(V(graph)$name, FUN = function(x) local_man(net, x, steps = 1))
twostep_triad_counts <- lapply(V(graph)$name, FUN = function(x) local_man(net, x, steps = 2))
threestep_triad_counts <- lapply(V(graph)$name, FUN = function(x) local_man(net, x, steps = 3))

onestep_triad_counts <- do.call("rbind", list(onestep_triad_counts))
twostep_triad_counts <- do.call("rbind", list(twostep_triad_counts))
threestep_triad_counts <- do.call("rbind", list(threestep_triad_counts))

# different centrality measures
indegrees <- igraph::degree(graph, mode = "in")
outdegrees <- igraph::degree(graph, mode = "out")

# put them all together
graph_features <- cbind(onestep_triad_counts, twostep_triad_counts, threestep_triad_counts, indegrees, outdegrees)

