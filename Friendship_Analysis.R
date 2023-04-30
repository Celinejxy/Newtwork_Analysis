library("xUCINET")
library(sna)
library(readr)
library(igraph)

#### Load and Clean Data ####
Network_Description <- "This network represents the undirected interactions between employees at a sunglasses company. The interactions were deemed non-work related,
or included personal information and interactions and thus coded as a friendship network."

# create a xUcinet obj
sunglass_network <-xCreateProject(GeneralDescription=Network_Description,
                                  NetworkName="friendship",
                                  NETFILE="Adjacency_ds/friend_adjacency.csv",
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



###Advice Network####
sunglass_network <- xAddToProject(sunglass_network, NetworkName = "advices",
                                  NETFILE1 = 'Adjacency_ds/advice_adjacency.csv', 
                                  FileType = 'csv',
                                  InFormatType = 'AdjMat', 
                                  NetworkDescription = 'Advices and mentorship between employees',
                                  Mode=c('People'),
                                  Directed = TRUE,
                                  Loops = FALSE,
                                  Values = 'Ordinal',
                                  Class='matrix')

sunglass_network$advices

f_dataframe <- as.data.frame(sunglass_network$friendship)
# correct row IDs becoming col 1
row.names(f_dataframe) <- f_dataframe[,1]
f_dataframe <- f_dataframe[, -1]
f_dataframe

#### Network Paths ####
# calculate the number of 4-link paths people in the network can take 
F_matrix <- as.matrix(f_dataframe)
FN_walk <- F_matrix %*% F_matrix %*% F_matrix %*% F_matrix
# subset the matrix to only include diagonal values and corresponding column names
diag_df <- data.frame(diag_values = diag(FN_walk), column_names = colnames(FN_walk))
# sort the diagonal values in decreasing order and select the top 10 values
top_diag_df <- head(sort(diag_df, decreasing = TRUE), 10)
# create a bar plot of the top 25 diagonal values
barplot(top_diag_df$diag_values, names.arg = top_diag_df$column_names, xlab = "Column Names", ylab = "Diagonal Values")
# ANALYSIS #
# As seen by the graph of the first 10 sorted values, a2 has 11 different 4-vertex paths to get back to themselves,
# a13 has 9, a5 and a10 have 7, a15 and a11 have 4, and several others have 3, 2, and 1. The conclusion can be drawn
# that a2 and a11 are the most popular in the sunglasses company. They have the highest amount of connections in a
# friendship network and have been observed the most performing social interactions. a5 and a10 are close behind them
# with slightly fewer.


#### Basic Descriptors Of The Network ####
# create a network and analyze 
F_network <- as.network.matrix(F_matrix)
# breakdown of the network
summary(F_network)
# Number of nodes in the network
network.size(F_network)
# structure of the network (Density): 2*Number of links / Nodes * Nodes -1
network.density(F_network)
# Transitivity of the network: Dyads / Triads (percentage of shared information)
gtrans(F_network) 
# number of total dyads
network.dyadcount(F_network)
# Which members are isolates and have no connections
isolates(F_network)
length(isolates(F_network))

# Not a directed graph, therefore, no need to analyze using this method
#dyad.census(F_network) 
#triad.census(F_network, mode="graph")
#triad.census(F_network, mode="digraph")
#grecip(F_network, measure="dyadic.nonnull")

# ANALYSIS #
# In this network there are 70 nodes. This network is very sparse as the density 
# is only 0.0083. This means that the network is very spread out with many more nodes
# than links and people who are friends only happen between a handful of individuals. 
# There are no wide-spread networks of friendships.
# The Transitivity of this network is equal to 0.3 meaning that there are more triads than 
# dyads. This can be interpreted as the people who have friendships are connected to
# another employee and are not simply one on one.
# With almost half of the network containing isolates, it may be concluded that either
# the interviewers did not collect data on the actual friendships present in the company,
# or, this is a business culture where friendships are not openly displayed / not 
# present.


#### Graph The Network ####
# Network Graph
gplot(F_network, gmode="graph", displaylabels=TRUE)
# Histogram of the lengths of the network
friendsDist <- geodist(F_network, count.paths = TRUE, inf.replace = 0)
hist(friendsDist$gdist)
hist(friendsDist$counts)
# ANALYSIS #
# The numeric outputs from calculating transitivity and density can be more clearly seen with 
# the graph output where the sparsity is easy to see and the lack of complex networks
# shows why the density is so low.
# This is again shown through the histogram of geodesics where the lengths of all the connections 
# are zero, meaning that the distance between employees cannot be calculated due to the
# lack of linkages between nodes.


#### Centrality ####
# create a graph object in order to calculate centrality
g <- graph_from_adjacency_matrix(F_matrix, mode = "undirected")
# organize metrics into a data frame
df.prom2 <- data.frame(
  degree = degree(g), 
  between = betweenness(g),
  ev =evcent(g)$vector
)
# add the row names to the data
row.names(df.prom2)<- F_network %v% "vertex.names"
# sort the data by descending centrality values
df.prompt <- df.prom2[order(-df.prom2$degree),]
# Add all cumulative values for centralization to the dataframe
cd <- centralization(F_matrix, degree)
cb <- centralization(g, betweenness)
cv <- centralization(g, evcent)
df.prompt <- rbind(df.prompt, c(cd,cb,cv))
print(df.prompt)
# calculate the correlation of each metric to one another
cor(df.prom2)

# ANALYSIS #
# As seen by the ordered outputs of the centralized data, the outcome from the matrix multiplication is similar 
# with the degree mapping to the order of the 4-connection output from above.
# The between data starts to diminish quickly after the first couple entries and the EV values follow somewhat of a similar
# pattern as the degree values.
# Our data does not show a strong correlation between any of the metrics where a change in degree explains only 65% of the
# variation in between and only 52% of variation in ev. The correlation between ev and between is even lower with only 
# 28% explained.
