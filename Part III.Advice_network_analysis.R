
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
adviceN <- sunglass_network$advices
adviceN

## while the weights indicates the importance and quality of the advices and mentorship had given by 
## the source vertices, with 1 being very little and 5 being a great deal, the raw data is asymmetric

## using minimum and dichotomize using greater than 0
adviceDic <- xDichotomize(sunglass_network$advices, Value = 0) 
adviceDic
save(adviceDic, file='adviceDic.RData')
write.csv(adviceDic, file='adviceDic.csv')

###save the dichotimized symmetrize matrix back into the project 
sunglass_network <- xAddToProject(sunglass_network, 
                                  NETFILE1 = adviceDic, 
                                  NetworkName = 'Dichotimized_advice',
                                  FileType = 'Robject')
adviceD<- sunglass_network$Dichotimized_advice
adviceD

adviceWalk <- adviceD %*% adviceD%*% adviceD %*% adviceD

## Vignette #2 getting basic statistics for a network.
adviceDN <- as.network(adviceD)
summary(adviceDN)
network.size(adviceDN) #70 
network.density(adviceDN) #0.024
## adding network that represents the diff in seniority level based on the attribute 
seniority<- c(2,1,1,1,1,0,1,0,0,1,1,0,1,0,0,0,0,0,0,0,1,0,2,0,0,0,0,0,2,0,0,1,0,0,2,2,0,2,2,0,0,0,0,0,2,0,1,1,2,1,0,1,2,2,0,1,0,1,0,1,0,0,2,0,2,1,1,2,2,2)
seniorityN <- xAttributeToNetwork(seniority, Type= 'Diffij')
seniorityN
length(seniority)
nameS <- paste('a',seq(1,70),sep='')
rownames(seniorityN) <- nameS
colnames(seniorityN)<- nameS
save(seniorityN, file = 'seniority.RData')
write.csv(seniorityN, file= 'seniority_adjacency.csv')
### save the seniority network back into the project 
sunglass_network <- xAddToProject(sunglass_network, 
                                  NETFILE1 = seniorityN, 
                                  NetworkName = 'Seniority_AdjacencyN',
                                  FileType = 'Robject')
sunglass_network$Seniority_AdjacencyN

###visualization 
gden(adviceDN)
gtrans(adviceDN)
network.dyadcount(adviceDN)

library(igraph)
##convert into graph object 
adviceGraph <- graph_from_adjacency_matrix(sunglass_network$Dichotimized_advice,
                                            mode = 'directed',
                                            weighted = TRUE,
                                            diag = FALSE)
component_distribution(adviceGraph, cumulative = TRUE)
components(adviceGraph, mode = c('weak'))
lgc<- component.largest(adviceDN, connected="weak", 
                        result = 'graph', 
                        return.as.edgelist = FALSE)
gplot(adviceDN,vertex.col=2+lgc)  #Plot with component membership
                           #Plot largest component itself 
isolates(adviceDN)
closeness(adviceGraph, mode='all')%>% sort(decreasing = TRUE)%>%.[1:4]

gplot(adviceDN,vertex.col=2+lgc)  #Plot with component membership
                           #Plot largest component itself 
gplot(component.largest(adviceDN,connected="weak",result="graph"))


E(adviceGraph)[which(E(adviceGraph)$weight <= 2)]$color <- '#bfbff7'
E(adviceGraph)[which(E(adviceGraph)$weight >2)]$color <- '#583d68'
adviceGraph <- delete.vertices(adviceGraph, degree(adviceGraph)==0)
V(adviceGraph)$name <- V(adviceGraph)$name
V(adviceGraph)$shape <- 'circle'
V(adviceGraph)$color <- '#b2b7b9'
V(adviceGraph)$vertex.frame.color <-'grey'

set.seed(961)
plot(
  adviceGraph,
  layout= layout_with_fr,
  edge.curved=FALSE,
  vertex.label.color="#3a0b57",
  asp=FALSE,
  vertex.label.cex=1.8,
  arrow.mode=2,
  edge.width=E(adviceGraph)$weight*4,
  main="Advice Network Graph")

## Vignette #4 Dyads and Triags
dyad.census(adviceGraph)
triad.census(adviceGraph)

florentineDist <- geodist(adviceDN, count.paths = TRUE, inf.replace = 0)
hist(florentineDist$gdist)
hist(florentineDist$counts)
