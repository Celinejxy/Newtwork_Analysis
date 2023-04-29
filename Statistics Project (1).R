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
lgc <- component.largest(PadgettN, result="graph")
gd <-geodist(lgc)
max(gd$gdist)
isolates(PadgettN)
length(isolates(PadgettN))

## Vignette #4 Dyads and Triags
dyad.census(PadgettN)
triad.census(PadgettN, mode="graph")
triad.census(PadgettN, mode="digraph")
grecip(PadgettN, measure="dyadic.nonnull")
laz = Lazega_Lawyers$Advice
grecip(laz, measure="dyadic.nonnull")
dyad.census(laz)
triad.census(laz, mode="graph")
triad.census(laz, mode="digraph")

#Vignette #5 Understanding degrees
gplot(PadgettN, gmode="graph", displaylabels=TRUE)
degree(PadgettM, gmode="graph")
laz = Lazega_Lawyers$Advice
degree(laz)
degree(laz, cmode="indegree")
degree(laz, cmode="outdegree")
gplot(laz, gmode="graph", displaylabels=TRUE)

##Vignette 6 Remember how to add vertex information
list.vertex.attributes(PadgettN)
get.vertex.attribute(PadgettN, "vertex.names")
set.vertex.attribute(PadgettN, "Age", c("30","20","10","40","100","25","4","8", "60", "22","18", "11", "77", "10","90", "33"))
list.vertex.attributes(PadgettN)
get.vertex.attribute(PadgettN,"Age")
## The other way was variable <- PadgettN %v% attribute values

## Vignette #7 Centralization this use
# dPadgett.prom <- data.frame(degree = degree(PadgettN))
# row.names(dPadgett.central) <- PadgettN %v% "vertex.names"
# dPadgett.centralsort <- dPadgett.central[row.names(order(-dPadgett.central$degree)),]
# dPadgett.centralsort2 <- rbind(dPadgett.centralsort, c(cd))

# data(PadgettN)
df.prom2 <- data.frame(
  degree = degree(PadgettN),
  between = betweenness(PadgettN),
  ev =evcent(PadgettN)
)

row.names(df.prom2)<- PadgettN %v% "vertex.names"
df.prompt <- df.prom2[order(-df.prom2$degree),]
cd <- centralization(PadgettN, degree)
cb <- centralization(PadgettN, betweenness)
cv <- centralization(PadgettN, evcent)
df.prompt <- rbind(df.prompt, c(cd,cb,cv))
print(df.prompt)

cor(df.prom2)

##Vignette #8
florentineDist <- geodist(PadgettN, count.paths = TRUE, inf.replace = 0)
hist(florentineDist$gdist)
hist(florentineDist$counts)
