
library(readr)
library(igraph)
library(statnet)
library(xUCINET)
library(network)


sunglass_network <-xCreateProject(GeneralDescription=Network_Description,
                                                                    NetworkName="Advice",
                                                                    NETFILE="Adjacency_ds/advice_adjacency.csv",
                                                                     FileType = "csv",
                                                                     InFormatType="AdjMat",
                                                                     NetworkDescription="Advice network and mentorship between employees",
                                                                     Mode=c("People"),
                                                                     Directed=TRUE,
                                                                     Loops=FALSE,
                                                                     Values="Ordinal",
                                                                     Class="matrix",
                                                                     References ="No References")
            
adviceN <- sunglass_network$Advice
adviceN

## using minimum and dichotomize using greater than 0
adviceDic <- xDichotomize(sunglass_network$Advice, Value = 0) 
adviceDic
save(adviceDic, file='adviceDic.RData')
write.csv(adviceDic, file='adviceDic.csv')

###save the dichotimized symmetrize matrix back into the project 
sunglass_network <- xAddToProject(sunglass_network, 
                                  NETFILE1 = adviceDic, 
                                  NetworkName = 'Dichotimized_advice',
                                  FileType = 'Robject')
adviceD <- sunglass_network$Dichotimized_advice
adviceD



# Create a graph from the adjacency matrix
net <- graph_from_adjacency_matrix(adviceD, mode = "directed")


layout_save <- layout_with_fr(net)
plot(net,vertex.color = 'slateblue2', 
     vertex.label.color = "white", 
     layout = layout_save)



##Loading Attributes 

#add attributes 
attributes <- xAddAttributesToProject(ProjectName = sunglass_network,
                                      ATTFILE1 = "Adjacency_ds/attr.csv",
                                      FileType = "csv",
                                      Mode = c("People"),
                                      AttributesDescription = c("Department 0=Executive 1=Marketing 2=Sales 3=Human Resources 4=Distribution 5=Manufacturing 6=Finance",
                                                                "Years Worked (1-25)",
                                                                "Seniority 0=Entry Level 1=Junior 2=Senior",
                                                                "Mentorship 0=Mentee 1=Mentor",
                                                                "Lenses 0=don't need 1=need",
                                                                "Age (21-63)",
                                                                "Gender 0=Male 1=Female",
                                                                "Hobbies 0=Poker 1=Tennis 2=Cooking 3=Golf 5=Coffee Roasting 6=Video Games"))



#Vignette 1. Brokerage roles&position
bNet <- as.network(adviceD, loops =TRUE, multiple=TRUE, directed=TRUE)
bNet %v% 'department' <- as.character(attributes$Department)
adviceBro<-  brokerage(bNet, 'department')
head(adviceBro$raw.nli)

##Vignette 2.Structural Equivalence
adj_mat <- as.matrix(as_adj(net))
diag(adj_mat) <- 0
adj_mat
a_row <- adj_mat["a1", !colnames(adj_mat) %in% c("a1", "a2")]
b_row <- adj_mat["a2", !colnames(adj_mat) %in% c("a1", "a2")]
sum(abs(a_row-b_row))
# Not equivalent, the absolute difference is 18

perfect_equivalence <- function(mat){
  matrix_vals <- mat
  matrix_vals[] <- 0
  # loop over the actors in the network, comparing pair-wise their values
  for(i in 1:nrow(mat)){
    for(j in 1:nrow(mat)){
      a_row <- mat[i, c(-i, -j)]
      b_row <- mat[j, c(-i, -j)]
      abs_diff <- sum(abs(a_row-b_row)) # take sum of absolute differences 
      matrix_vals[i,j] <- abs_diff}}
  return(matrix_vals)
}
structurally_equivalent <- perfect_equivalence(adj_mat)
structurally_equivalent

#convert into similarity matrix
structurally_equivalent_sim <- 1-(structurally_equivalent/max(structurally_equivalent))
structurally_equivalent_sim

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
plot(simplify(net), vertex.color = clusters)

# Vignette 3.Isomorphic Local Graph
local_graphs <- make_ego_graph(net, order = 4)
iso_mat <- matrix(FALSE, nrow = length(local_graphs), ncol = length(local_graphs))
for(i in 1:length(local_graphs)){
  for(j in 1:length(local_graphs)){
    iso_mat[i,j] <- isomorphic(local_graphs[[i]], local_graphs[[j]], method = 'vf2')
  }
}
iso_mat[] <- as.numeric(iso_mat[])
diag(iso_mat) = 0
clusters_iso <- cluster_louvain(graph.adjacency(iso_mat, mode = "undirected"))
plot(simplify(net), 
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
plot(simplify(net), vertex.color = class_assignments)

# Vignette 5. Feature-Based 
# this function limits the network to a given node's neighborhood 
#(defined n-steps away) and then runs the triad census on that neighborhood.
local_man <- function(net, vertex_name, steps = 2){
  n_steps <- ego(net, order = steps, vertex_name, mode = "out")[[1]]
  subnet <- igraph::delete.vertices(net, !V(net) %in% n_steps)
  local_triad_count <- igraph::triad.census(subnet)
  return(local_triad_count)
}


onestep_triad_counts <- lapply(V(net)$name, FUN = function(x) local_man(net, x, steps = 1))
twostep_triad_counts <- lapply(V(net)$name, FUN = function(x) local_man(net, x, steps = 2))
threestep_triad_counts <- lapply(V(net)$name, FUN = function(x) local_man(net, x, steps = 3))

onestep_triad_counts <- do.call("rbind", list(onestep_triad_counts))
twostep_triad_counts <- do.call("rbind", list(twostep_triad_counts))
threestep_triad_counts <- do.call("rbind", list(threestep_triad_counts))

# different centrality measures
indegrees <- igraph::degree(net, mode = "in")
outdegrees <- igraph::degree(net, mode = "out")

# put them all together
graph_features <- cbind(onestep_triad_counts, twostep_triad_counts, threestep_triad_counts, indegrees, outdegrees)


