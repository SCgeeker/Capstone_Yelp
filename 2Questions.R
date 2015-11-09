load("../Yelp_Data/Raw.RData")
source("packageskit.R")
source("functions.R")

# build the business_id index cross data set
yelp_business <- data.frame(yelp_business, Loc = City_Tag(yelp_business$latitude, yelp_business$longitude))
idi <- yelp_business$Loc
names(idi) <- yelp_business$business_id
yelp_review <- data.frame(yelp_review, Loc = idi[yelp_review$business_id])
yelp_tip <- data.frame(yelp_tip, Loc = idi[yelp_tip$business_id])



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

## Build stars model for one category
head( yelp_business$hours ) # 14 columns, open time and close time from Mon to Sun
yelp_business$review_count # How many active reviews are counted for this business
unlist( lapply(yelp_business$neighborhood, length) ) # how many neighborhood zones around the business
yelp_business$stars  # average star scores
head(yelp_business$attributes)  # Take the attributes by the category
head(yelp_checkin$checkin_info) # Take the checkin info by the category

yelp_FastFood <- yelp_business[grep(gsub("[()]","",FoodCategories )[1], gsub("[()]","",yelp_business$categories) ),c("business_id","Loc","stars","hours", "review_count", "neighborhoods", "attributes")]
yelp_FastFood <- Variables_Transfer(yelp_FastFood)

yelp_Chinese <- yelp_business[grep(gsub("[()]","",FoodCategories )[8], gsub("[()]","",yelp_business$categories) ),c("business_id","Loc","stars","hours", "review_count", "neighborhoods", "attributes")]
yelp_Chinese <- Variables_Transfer(yelp_Chinese)

##  Distribuions of stars among cities
table(yelp_Chinese$Loc)
with(data = yelp_Chinese, table(stars, Loc))
chisq.test( table(yelp_Chinese$stars, yelp_Chinese$Loc) )

Variables_Tags <- names(yelp_FastFood)

save.image("../Yelp_Data/Model.RData")

## If the location has the singificant contribution... text mining on reviews and tips
## If the location has the insingificant contribution... conclude the model from the attributes.