load("../Yelp_Data/Model.RData")
source("packageskit.R")
source("functions.R")

## BBN Graphs
### Fast Food
#### Normal BBN plot
names(yelp_FastFood) <- c("ID", "L", "S", "H", "R", "N", paste0("A",1:37))
table(yelp_FastFood$L)
#yelp_FastFood.gs <- gs(yelp_FastFood[,c(3,2,4:43)])
yelp_FastFood.hc <- hc(yelp_FastFood[,c(3,2,4:43)])
#compare(yelp_FastFood.gs, yelp_FastFood.hc)
#plot(yelp_FastFood.gs,  main  =  "Constraint-based  algorithms",  highlight  =  c("S",  "L"))
plot(yelp_FastFood.hc,  main  =  "Hill-Climbing",  highlight  =  c("S",  "L"))

#### BBN Graphviz
#gs.opt <- list(nodes = c("S", "L"), arcs = c("S", "L"), col = "red", fill = "grey")
#graphviz.plot(yelp_FastFood.gs, highlight = gs.opt)
hc.opt <- list(nodes = c("S", "L"), arcs = c("S", "L"), col = "red", fill = "grey")
graphviz.plot(yelp_FastFood.hc, highlight = hc.opt)

##### In the case of fast food, both "Constraint-based  algorithms" and "Hill-Climbing" plot the Location as the prior event of stars.
##### Constraint-based algorithms pick the direct events to 'stars': 'review count', 'Outdoor.Seating'and 'dessert'. 'Loc' is the prior event of 'review count'

##### "Loc" is the top node that directly influence "Accepts.Credit.Cards", "Good.For.Groups", "Price.Range", and "Good.for.Kids".

### Chinese
names(yelp_Chinese) <- c("ID", "L", "S", "H", "R", "N", paste0("A",1:37))
table(yelp_Chinese$L)
#yelp_Chinese.gs <- gs(yelp_Chinese[,c(3,2,4:43)])
yelp_Chinese.hc <- hc(yelp_Chinese[,c(3,2,4:43)])
#plot(yelp_Chinese.gs,  main  =  "Chinese resturants: Constraint-based  algorithms",  highlight  =  c("stars",  "Loc"))
#plot(yelp_Chinese.hc,  main  =  "Chinese resturants: Hill-Climbing",  highlight  =  c("stars",  "Loc"))
#gs.opt <- list(nodes = c("stars", "Loc"), arcs = c("stars", "Loc"), col = "red", fill = "grey")
#graphviz.plot(yelp_Chinese.gs, highlight = gs.opt)
hc.opt <- list(nodes = c("S", "L"), arcs = c("S", "L"), col = "red", fill = "grey")
graphviz.plot(yelp_Chinese.hc, highlight = hc.opt)

#####  "Loc" decide the hours of open of Chinese resturants. Hours of open directly influence "stars". "Loc" and "stars" decide the other attributes of Chinese resturants.


# Check how many businesses in each city
with(data=yelp_FastFood, table(Loc))
with(data=yelp_Chinese, table(Loc))

dcity <- c("Phoenix", "Charlotte") # Take the cities for analysis

## Split the data sets
#id <- names(yelp_FastFood)[1]
#info <- names(yelp_FastFood)[2:6]
#attributes <- names(yelp_FastFood)[7:43]

ids <- c("ID", "L")
FastFoodf <- names(yelp_FastFood)[3:43]

Phoenix_FastFood = subset(yelp_FastFood, L == dcity[1])[,3:43]
#row.names(Phoenix_FastFood) = 1:dim(Phoenix_FastFood)[1]
Charlotte_FastFood = subset(yelp_FastFood, L == dcity[2])[,3:43]
Phoenix_Chinese = subset(yelp_Chinese, L == dcity[1])[,3:43]
Charlotte_Chinese = subset(yelp_Chinese, L == dcity[2])[,3:43]

## Codes of fit.the.model and xval.the.model from http://www.bnlearn.com/research/genetics14/


## Analyze Phoenix FastFood
# cl = makeCluster(10)
# invisible(clusterEvalQ(cl, library(bnlearn)))
# invisible(clusterEvalQ(cl, library(lme4))) # we don't need lmer in this project
# clusterExport(cl = cl, c("ids","FastFoodf", "fit.the.model"))
FastFood001 = vector(10, mode = "list")

for (i in 1:10)
  FastFood001[[i]] = xval.the.model(Phoenix_FastFood, alpha = 0, ridge = FALSE)
# stopCluster(cl)

pred.FastFood001.summary = sapply(FastFood001, `[[`, "predcor")
print(rowMeans(pred.FastFood001.summary))

post.FastFood001.summary = sapply(FastFood001, `[[`, "postcor")
print(rowMeans(post.FastFood001.summary))

FastFood001.gr <- gather.arc(FastFood001, names(Phoenix_FastFood), "dot", 0.01, enhanced = FALSE)

# In Phoenix, "star" is a common inter-variable among other variables. The public reputation and service dicide how successful of a resturant.

## Analyze Charlotte FastFood
FastFood002 = vector(10, mode = "list")

for (i in 1:10)
  FastFood002[[i]] = xval.the.model(Charlotte_FastFood, alpha = 0, ridge = FALSE)

pred.FastFood002.summary = sapply(FastFood002, `[[`, "predcor")
print(rowMeans(pred.FastFood002.summary))

post.FastFood002.summary = sapply(FastFood002, `[[`, "postcor")
print(rowMeans(post.FastFood002.summary))

FastFood002.gr <- gather.arc(FastFood002, names(Charlotte_FastFood), "dot", 0.01, enhanced = FALSE)
# In Charlotte, "stars", "attrie", and "open at late night" decide how favorite a fast food resturant

## Analyze Phoenix Chinese 
Chinese001 = vector(10, mode = "list")

for (i in 1:10)
  Chinese001[[i]] = xval.the.model(Phoenix_Chinese, alpha = 0, ridge = FALSE)

pred.Chinese001.summary = sapply(Chinese001, `[[`, "predcor")
print(rowMeans(pred.Chinese001.summary))

post.Chinese001.summary = sapply(Chinese001, `[[`, "postcor")
print(rowMeans(post.Chinese001.summary))

Chinese001.gr <- gather.arc(Chinese001, names(Phoenix_Chinese), 'dot', 0.01, enhanced = FALSE)

## Attraction to the tourists is the key to have a successful Chinese resturant in Phoenix


## Analyze Charlotte Chinese 
Chinese002 = vector(10, mode = "list")

for (i in 1:10)
  Chinese002[[i]] = xval.the.model(Charlotte_Chinese, alpha = 0, ridge = FALSE)

pred.Chinese002.summary = sapply(Chinese002, `[[`, "predcor")
print(rowMeans(pred.Chinese002.summary))

post.Chinese002.summary = sapply(Chinese002, `[[`, "postcor")
print(rowMeans(post.Chinese002.summary))

Chinese002.gr <- gather.arc(Chinese002, names(Charlotte_Chinese), 'dot', 0.01, enhanced = FALSE)
## There are many attributes the guests consider a good Chinese resturant in Charlotte. They are 