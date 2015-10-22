load("../Yelp_Data/Corpus.RData")
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

## Average tips for a bussiness in every city
table( idi[yelp_tip$business_id] )/table(yelp_business$Loc)



Review_dtm <- list()
for( i in 1:length(Review_docs)){
  Review_dtm <- list(Review_dtm, DocumentTermMatrix( unlist(Review_docs[i]) ))
}


dtm <- DocumentTermMatrix( Review_docs[[1]] )
