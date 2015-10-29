load("../Yelp_Data/Raw.RData")
source("packageskit.R")
source("functions.R")

# build the business_id index cross data set
idi <- yelp_business$Loc
names(idi) <- yelp_business$business_id



table(idi[yelp_review$business_id]) # How many reviews in each city
table(idi[yelp_tip$business_id]) # How many tips in each city
table(yelp_business$Loc) ## How many businesses in every city



## Average reviews for a bussiness in every city
table( idi[yelp_review$business_id] )/table(yelp_business$Loc)

## Average users give review for a bussiness in every city
idu <- with( data = yelp_review,   tapply( user_id , Loc, unique ) )

table( idi[yelp_review$business_id] )/sapply(idu, length)

#cbind(table( idi[yelp_review$business_id] )/table(yelp_business$Loc), table( idi[yelp_review$business_id] )/sapply(idu, length))

## Average tips for a bussiness in every city
table( idi[yelp_tip$business_id] )/table(yelp_business$Loc)

## Average users give tip for a bussiness in every city
idt <- with( data = yelp_tip,   tapply( user_id , Loc, unique ) )

table( idi[yelp_tip$business_id] )/sapply(idt, length)


## My question
## Yelp has developed the software to filter the reliable reviews for the registered businesses in each city. 
## For the Chinese restaurants in Montreal and in Phoenix, did yelp users recommend them based on one set of dimensions? 
## If the data could tell us the dimensions how users evaluate businesses, Will we figure a global standard across cities or many local standards in each city?


## My solutions
## We have to focus on the specific categories of business across cities(Need a pick up procedure)
## The attributes to build the exploratory model include the data in "business" and "check-in"
## Use SOM to find the organizations for these businesses
## Check the contribution of location to the map
## Build the map of the second, third, .... Nth category 
## Find the dimensions across the maps

## Isolate the categories for analysis
FoodCategories <- names( sort( table(unlist(yelp_business$categories)) , decreasing = TRUE) )[c(13,14,15,18,19,21,22,23,24,25)]

## Count how many business of each category in every city
i <- 1
while(i <= length(FoodCategories)){
  print(FoodCategories[i])
  print(table( yelp_business$Loc[grep(gsub("[()]","",FoodCategories )[i], gsub("[()]","",yelp_business$categories) )] )   )
  i = i + 1
}

## possible variables in the cluster analysis for one category
head( yelp_business$hours ) # 14 columns, open time and close time from Mon to Sun
yelp_business$review_count # How many active reviews are counted for this business
unlist( lapply(yelp_business$neighborhood, length) ) # how many neighborhood zones around the business
yelp_business$stars  # average star scores
head(yelp_business$attributes)  # Take the attributes by the category
head(yelp_checkin$checkin_info) # Take the checkin info by the category

yelp_FastFood <- yelp_business[grep(gsub("[()]","",FoodCategories )[1], gsub("[()]","",yelp_business$categories) ),c("business_id","Loc","stars","hours", "review_count", "neighborhoods", "attributes")]

yelp_FastFood <- Variables_Transfer(yelp_FastFood)

##  Distribuions of stars among cities
table(yelp_FastFood$Loc)
chisq.test( table(yelp_FastFood$stars, yelp_FastFood$Loc) )
chisq.test( table(yelp_FastFood$stars, yelp_FastFood$Loc)[,c(4,7)] )
chisq.test( table(yelp_FastFood$stars, yelp_FastFood$Loc)[,c(1,2)] )
chisq.test( table(yelp_FastFood$stars, yelp_FastFood$Loc)[,-c(1,2,4,7)] )


apply(yelp_FastFood$attributes, 2, unique) 

## Cluster Fast Food by self-organising map
## codes are from "Self-Organising Maps for Customer Segmentation using R" 
## http://www.shanelynn.ie/self-organising-maps-for-customer-segmentation-using-r/
library(kohonen)
tmp <- yelp_FastFood$attributes + 1
tmp <- replace(tmp, is.na(tmp), 0)
data_train <- data.frame(yelp_FastFood[, c(4, 5, 6)], tmp)
rm(tmp)
data_train_matrix <- as.matrix(scale(data_train))  # remove the NA values in the data
som_grid <- somgrid(xdim = 30, ydim=30, topo="hexagonal")
som_model <- som(data_train_matrix, 
                 grid=som_grid, 
                 rlen=100, 
                 alpha=c(0.05,0.01), 
                 keep.data = TRUE,
                 n.hood='circular' )

plot(som_model, type="changes")
plot(som_model, type="count")
plot(som_model, type="codes")

mydata <- som_model$codes 
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var)) 
for (i in 2:15) {
  wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
}
plot(wss)

som_cluster <- cutree(hclust(dist(som_model$codes)), 6)

pretty_palette <- c("black","red","green3","blue","cyan","magenta","yellow","gray")
plot(som_model, type="mapping", bgcol = pretty_palette[som_cluster], main = "Clusters") 
add.cluster.boundaries(som_model, som_cluster)

plot(som_model, type="codes", bgcol = pretty_palette[som_cluster], main = "Clusters")
add.cluster.boundaries(som_model, som_cluster)

## Add clusters back to the raw observations
yelp_FastFood <- data.frame(yelp_FastFood, cluster=som_cluster[som_model$unit.classif])
table(yelp_FastFood$Loc, yelp_FastFood$cluster)
chisq.test(table(yelp_FastFood$Loc, yelp_FastFood$cluster))

table(yelp_FastFood$stars, yelp_FastFood$cluster)
### Cluster 6 have only 6 shopes that located in Las Vegas and Phoenix

## SOM on Las Vegas and Phoenix
yelp_FastFood_LV_P = subset(yelp_FastFood, (Loc == "Las Vegas"| Loc == "Phoenix" ) )
tmp <- yelp_FastFood_LV_P$attributes + 1
tmp <- replace(tmp, is.na(tmp), 0)
data_train <- data.frame(yelp_FastFood_LV_P[, c(4, 5, 6)], tmp)
rm(tmp)
data_train_matrix <- as.matrix(scale(data_train))  # remove the NA values in the data
som_grid <- somgrid(xdim = 30, ydim=30, topo="hexagonal")
som_model <- som(data_train_matrix, 
                 grid=som_grid, 
                 rlen=100, 
                 alpha=c(0.05,0.01), 
                 keep.data = TRUE,
                 n.hood='circular' )
plot(som_model, type="changes")
mydata <- som_model$codes 
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var)) 
for (i in 2:15) {
  wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
}
plot(wss)
som_cluster <- cutree(hclust(dist(som_model$codes)), 6)

pretty_palette <- c("black","red","green3","blue","cyan","magenta","yellow","gray")
plot(som_model, type="mapping", bgcol = pretty_palette[som_cluster], main = "Clusters") 
add.cluster.boundaries(som_model, som_cluster)

plot(som_model, type="codes", bgcol = pretty_palette[som_cluster], main = "Clusters")
add.cluster.boundaries(som_model, som_cluster)

yelp_FastFood_LV_P <- data.frame(yelp_FastFood_LV_P, cluster=som_cluster[som_model$unit.classif])

table(yelp_FastFood_LV_P$stars, yelp_FastFood_LV_P$cluster)
table(yelp_FastFood_LV_P$Loc, yelp_FastFood_LV_P$cluster)
## If the location has the singificant contribution... text mining on reviews and tips
## If the location has the insingificant contribution... conclude the model from the attributes.