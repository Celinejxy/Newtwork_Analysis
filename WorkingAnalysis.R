library("xUCINET")
library(sna)
library(readr)
library(igraph)

#### Load and Clean Data ####
data2 <- read_csv("Adjacency_ds/working_adjacency.csv") 
dataframe2 <- as.data.frame(data2)
# correct row IDs becoming col 1
row.names(dataframe2) <- dataframe2[,1]
dataframe2 <- dataframe2[, -1]
dataframe2
Network_Description <- "This network represents the undirected interactions between employees at a sunglasses company. The interactions were deemed work related and thus coded as a working network."

# create a xUcinet obj
sunglass_network <-xCreateProject(GeneralDescription=Network_Description,
                                  NetworkName="friendship",
                                  NETFILE= "Adjacency_ds/friend_adjacency.csv", 
                                  FileType = "csv",
                                  InFormatType="AdjMat",
                                  NetworkDescription="friendships between coworkers",
                                  Mode=c("People"),
                                  Directed=FALSE,
                                  Loops=FALSE,
                                  Values="Ordinal",
                                  Class="matrix",
                                  References="No References")
sunglass_network$friendship



#Working Network
sunglass_network <- xAddToProject(sunglass_network, NetworkName = "working",
                                  NETFILE1 = 'Adjacency_ds/working_adjacency.csv', 
                                  FileType = 'csv',
                                  InFormatType = 'AdjMat', 
                                  NetworkDescription = 'Work relationship between employees',
                                  Mode=c('People'),
                                  Directed = TRUE,
                                  Loops = FALSE,
                                  Values = 'Ordinal',
                                  Class='matrix')

sunglass_network$working
#### Network Paths ####
# calculate the number of 4-link paths people in the network can take 
W_matrix <- as.matrix(dataframe2)
WN_walk <- W_matrix %*% W_matrix %*% W_matrix %*% W_matrix
# subset the matrix to only include diagonal values and corresponding column names
diag_df <- data.frame(diag_values = diag(WN_walk), column_names = colnames(WN_walk))
# sort the diagonal values in decreasing order and select the top 10 values
top_diag_df <- head(sort(diag_df, decreasing = TRUE), 10)
# create a bar plot of the top 25 diagonal values
barplot(top_diag_df$diag_values, names.arg = top_diag_df$column_names, xlab = "Column Names", ylab = "Diagonal Values")
# ANALYSIS #
# As seen by the graph of the first 10 sorted values, They all have about 2600 4-vertex paths to get back to themselves. 
# This means that they all have similar levels of popularity in work setting within the sunglasses company. They all have a high amount of connections in a
# working network.


#### Basic Descriptors Of The Network ####
# create a network and analyze 
W_network <- as.network.matrix(W_matrix)
# breakdown of the network
summary(W_network)
# Number of nodes in the network
network.size(W_network)
# structure of the network (Density): 2*Number of links / Nodes * Nodes -1
network.density(W_network)
# Transitivity of the network: Dyads / Triads (percentage of shared information)
gtrans(W_network) 
# number of total dyads
network.dyadcount(W_network)
# Which members are isolates and have no connections
isolates(W_network)
length(isolates(W_network))

# Not a directed graph, therefore, no need to analyze using this method
#dyad.census(W_network) 
#triad.census(W_network, mode="graph")
#triad.census(W_network, mode="digraph")
#grecip(W_network, measure="dyadic.nonnull")

# ANALYSIS #
# In this network there are 70 nodes. This network is relatively dense as the density 
# is  0.1602. This means that the network is more dense than sparse
# it is not very spread out with many links 
# and people who are colleagues happens between a lot of individuals.
# There are closely connected networks of work.
# The Transitivity of this network is equal to 0.96 meaning that This suggests that the network has a high level of transitivity. 
# This can be interpreted as individuals are more likely to have connections to other individuals who are also connected to each other, 
# forming clusters or communities within the network. This can have important implications for the diffusion of information
# There are no isolates, it may be concluded that either every individual is connected in a working relationship with at least one person.
# this shows that all individual have a connection or more.



#### Graph The Network ####
# Network Graph
gplot(W_network, gmode="graph", displaylabels=TRUE) #We can observe the the clusters within the network.
# Histogram of the lengths of the network
workDist <- geodist(W_network, count.paths = TRUE, inf.replace = 0)
hist(workDist$gdist)
hist(workDist$counts)
# ANALYSIS #
# The numeric outputs from calculating transitivity and density can be more clearly seen with 
# the graph output. This is a left-skewed distribution or a negative skew.
# This suggests that there are many pairs of nodes with relatively small geodesic distances, 
# but a few pairs of nodes with very large geodesic distances.
# It suggests that these nodes are closely connected and that information or influence can flow easily between them. 



