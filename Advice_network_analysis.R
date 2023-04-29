library(xUCINET)
library(sna)

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

## symmetrize using minimum and dichotomize using greater than 0
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
network.size(adviceDN)
network.density(adviceDN)
gden(adviceDN)
gtrans(adviceDN)
network.dyadcount(adviceDN)


## Vignette #3 Dealing with isolates
components(adviceDN)
lgc <- component.largest(adviceDN, result="graph")
gd <-geodist(lgc)
max(gd$gdist)
isolates(adviceDN)
length(isolates(adviceDN))

## Vignette #4 Dyads and Triags
dyad.census(adviceDN)
triad.census(adviceDN, mode="graph")
triad.census(adviceDN, mode="digraph")
grecip(adviceDN, measure="dyadic.nonnull")

#Vignette #5 Understanding degrees
gplot(adviceDN, gmode="graph", displaylabels=TRUE)
degree(adviceDN, gmode="graph")

##Vignette 6 Remember how to add vertex information
list.vertex.attributes(adviceDN)
get.vertex.attribute(adviceDN, "vertex.names")
set.vertex.attribute(adviceDN, "Seniority_level", c(2,1,1,1,1,0,1,0,0,1,1,0,1,0,0,0,0,0,0,0,1,0,2,0,0,0,0,0,2,0,0,1,0,0,2,2,0,2,2,0,0,0,0,0,2,0,1,1,2,1,0,1,2,2,0,1,0,1,0,0,1,0,0,2,0,2,1,1,2,2,2))
list.vertex.attributes(adviceDN)
get.vertex.attribute(adviceDN,"Seniority_level")

##Vignette #8
florentineDist <- geodist(adviceDN, count.paths = TRUE, inf.replace = 0)
hist(florentineDist$gdist)
hist(florentineDist$counts)
