library(xUCINET)
library(sna)

## Vignette 1 Getting Padgett data
Padgett <- Padgett_FlorentineFamilies
Padgett
PadgettM <- Padgett$Marriage
PadgettM
class(PadgettM)
as.network.matrix(PadgettM)
PadgettWalk <- PadgettM %*% PadgettM %*% PadgettM %*% PadgettM

## Vignette #2 getting basic statistics for a network.
PadgettN <- as.network(PadgettM)
summary(PadgettN)
network.size(PadgettN)
network.density(PadgettN)
gden(PadgettN)
gtrans(PadgettN)
network.dyadcount(PadgettN)

## Vignette #3 Dealing with isolates
components(PadgettN)
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
