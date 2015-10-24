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
## For Chinese restaurants in Montreal and in Phoenix, did the yelp users evaluate them in the same set of dimensions? 
## Will we figure a global evaluation system across cities or many local evaluation systems in each city?


## My solutions
## We have to focus on the specific categories of business across cities(Need a pick up procedure)
## The attributes to build the exploratory model include the data in "business" and "check-in"
## Use SOM to find the organizations for these businesses
## Check the contribution of location to the map
## Build the map of the second, third, .... Nth category 
## Find the dimensions across the maps

## If the location has the singificant contribution... text mining on reviews and tips
## If the location has the insingificant contribution... conclude the model from the attributes.