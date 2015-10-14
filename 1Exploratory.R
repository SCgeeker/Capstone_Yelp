load("../Yelp_Data/Raw.RData")
source("packageskit.R")
source("functions.R")

# Features in each data set
str( yelp_business, max.level = 1)
str( yelp_checkin, max.level = 1)
str( yelp_review, max.level = 1)
str( yelp_tip, max.level = 1)
str( yelp_user, max.level = 1)

# Mark id columns
# "business_id":  yelp_business, yelp_checkin, yelp_review, yelp_tip
# "user_id"    :  yelp_review, yelp_tip, yelp_user
# "review_id"  :  yelp_review
## "business_id" and "user_id" are cross at yelp_review and yelp_tip

# Explore features in each data set

## business
sort( table(yelp_business$city) ) # Count the registed cities
table(unlist(lapply(yelp_business$state, unique)))  # Count the registed states 
## city and state store many unavailable location names
str(yelp_business$neighborhoods)  # List neighborhoods
unlist(lapply(yelp_business$neighborhoods, length))   # number of neighborhood around the budiness
sum( is.na(yelp_business$longitude) ) # Check missing geographical data
sum( is.na(yelp_business$latitude) ) # Check missing geographical data

## Preview business geographic locations
plot( SpatialPoints( yelp_business[,c("longitude","latitude")], proj4string = CRS("+init=epsg:4326")), pch = 1)

table( round(yelp_business[,c("latitude","longitude")]) )[11:1,]

## Cities(From west to east): Las Vegas, Phoenix, Madison, Urbana-Champaign, Charlotte, Waterloo, Pittsburgh, Montreal, Edinburgh, Karlsruhe
## (Lat, Lon): (36, -115), (33,-112), (43,-89), (40, -88), (35,-81), (43,-81), (40,-80), (45, -74), (56,-3), (49, 8)
## substraction within +/- 1 equals to that city!

## update business data: Pricise locations
yelp_business <- data.frame(yelp_business, Loc = City_Tag(yelp_business$latitude, yelp_business$longitude))
table(yelp_business$Loc) ## Count how many businesses in each city

City_Neighbor <- list()
for(i in 1:length(unique(yelp_business$Loc))){
  City_Neighbor[[i]] <- list(
      city = as.character( unique(yelp_business$Loc)[i] ),
      Neighborhood = table(unlist(lapply(yelp_business$neighborhoods[yelp_business$Loc == unique(yelp_business$Loc)[i]], unique)) ),
      Neighborhood_Size = length( table(unlist(lapply(yelp_business$neighborhoods[yelp_business$Loc == unique(yelp_business$Loc)[i]], unique)) ) )
  )
}

City_Neighbor[[1]][["city"]]
City_Neighbor[[1]][["Neighborhood_Size"]]


str(yelp_business$hours)          # Data frame
names(yelp_business$hours)
table(yelp_business$open)         # Business open status
str(yelp_business$categories)     # List
unlist(lapply(yelp_business$categories, length))   # number of categories assigned to the budiness
table( unlist(lapply(yelp_business$categories, length)) ) # How many categories were labeled to a business
sort( table(unlist(lapply(yelp_business$categories, unique))), decreasing = TRUE)[1:30] # How many categories the business was labeled
table(yelp_business$stars)        # frequency table of stars
table(unlist(lapply(yelp_business$categories, length)), yelp_business$stars)[11:1,]  # Table of stars and number of categories
cor.test(unlist(lapply(yelp_business$categories, length)), yelp_business$stars)
## There is significant but weak positive coreelation (r = 0.03) between number of categories and stars

cor.test(yelp_business$review_count, yelp_business$stars)
# There is significant but weak positive coreelation (r = 0.023) between review counts and stars

class(yelp_business$attributes)   # Data frame
str(yelp_business$attributes, max.level = 1)   # Check attributes of businesses
names(yelp_business$attributes)
table( sapply(yelp_business$attributes,  class) )  # Summarize calsses of 1st level attributes
names( which( unlist( sapply(yelp_business$attributes,  is.data.frame)  ) == TRUE) ) # Get the attributes which are data frames
str(yelp_business$attributes[, which( unlist( sapply(yelp_business$attributes,  is.data.frame)  ) == TRUE)] ) # Explore the 1st level attributes which are data frames
sapply(yelp_business$attributes[, which( unlist( sapply(yelp_business$attributes,  is.data.frame)  ) == TRUE)], names ) # Show the names of 2nd level attributes
sapply(yelp_business$attributes[, which( unlist( sapply(yelp_business$attributes,  is.data.frame)  ) == TRUE)], dim )[2,] # Show the numbers of 2nd level attributes
summary( unlist( lapply( yelp_business$attributes$`Accepts Credit Cards`, unique) ) ) # Explore the 1st level attribute which is list
sum(yelp_business$review_count)  # sum up review counts of all businesses; more than yelp_review
table(yelp_business$review_count)  # Frequency table of review counts of all businesses
yelp_business$name[yelp_business$review_count >= 1000]

cor.test(yelp_business$review_count, yelp_business$stars)
## A significant but small positive correlation (r = .023) between review counts and business stars

## review
length(unique(yelp_review$review_id))    # every text is unique
length(unique(yelp_review$user_id))      # equal to yelp_user
length(unique(yelp_review$business_id))  # less than yelp_business
summary(yelp_review$stars)
table(paste(year(yelp_review$date), month(yelp_review$date) ) )
str(yelp_review$votes, max.level = 1)           # Data frame
sum(yelp_review$votes)
100 * ( table(yelp_review$stars)/sum(table(yelp_review$stars)) )   # Frequency table of stars

dim(yelp_review[(yelp_business$business_id[yelp_business$open == FALSE] %in% yelp),])

## tip
length( unique(yelp_tip$user_id) )
length( unique(yelp_tip$business_id))
summary(yelp_tip$likes)
unique(yelp_tip$likes)
table( paste(year(yelp_tip$date),month(yelp_tip$date)) )  # count of tip from each month
## tips are coded since 2009, April

## checkin
length( unique(yelp_checkin$business_id))
summary(yelp_checkin$checkin_info)
## Not all businesses had the check-in records

## user
table( substr(yelp_user$yelping_since, 1,4) ) # Number of initial users for every year
str(yelp_user$votes)             # Data frame of votes
summary(yelp_user$votes)
sum(yelp_user$review_count)      # sum up review counts of all users
table(yelp_user$review_count)      # Frequency table: review counts of all users
str(yelp_user$friends)           # List
unlist(lapply(yelp_user$friends, length))   # number of friends the user owned
table(unlist(lapply(yelp_user$friends, length)))  # frequency table of friends
str(yelp_user$compliments)       # Data frame of compliments
summary(yelp_user$compliments)
str(yelp_user$elite)             # List of active years
unlist(lapply(yelp_user$elite, length))  # how many years the user is active on yelp
table(unlist(lapply(yelp_user$elite, unique)))  # frequency table of active years

## Votes are not identical to compliments
sum( yelp_user$votes$funny == yelp_user$compliments$funny )
sum( yelp_user$votes$cool == yelp_user$compliments$cool )

cor.test(unlist(lapply(yelp_user$elite, length)), yelp_user$review_count) # correlation: length of available years vs. review counts
### A significant poistive correlation (r = .72) between length of available years and review counts

cor.test(yelp_user$fans, yelp_user$review_count) # correlation: number of fans vs. review counts
lm(yelp_user$fans ~ yelp_user$review_count) # regression:  number of fans ~ review counts
lm(yelp_user$review_count ~ yelp_user$fans )  # regression:  review counts ~ number of fans
plot(log10( yelp_user$fans ), log10(yelp_user$review_count) )
### A significant poistive correlation (r = .60) between number of fans and review counts

cor.test(unlist(lapply(yelp_user$friends, length)), yelp_user$review_count) # correlation: number of friends vs. review counts
lm(unlist(lapply(yelp_user$friends, length)) ~ yelp_user$review_count) # regression:  number of friends ~ review counts
lm(yelp_user$review_count ~ unlist(lapply(yelp_user$friends, length)) )  # regression:  review counts ~ number of friends
plot(log10( unlist(lapply(yelp_user$friends, length)) ), log10(yelp_user$review_count) )
### A significant poistive correlation (r = .48) between number of friends and review counts

cor.test(yelp_user$fans, unlist(lapply(yelp_user$friends, length))) # correlation: number of fans vs. review counts
### A significant poistive correlation (r = .68) between number of fans and number of friends

cor.test(unlist(lapply(yelp_user$elite, length)), unlist(lapply(yelp_user$friends, length))) # correlation: length of available years vs. number of friends
### A significant poistive correlation (r = .40) between length of available years and number of friends

cor.test(unlist(lapply(yelp_user$elite, length)), yelp_user$fans) # correlation: length of available years vs. number of fans
### A significant poistive correlation (r = .46) between length of available years and number of fans




## Cross business, user, review
# Review count vs business star
with( data = yelp_business[yelp_business$open == FALSE,],
      plot(review_count,
           stars, xlab = "Review Counts", ylab = "Stars", main = "Closed Business")
)

with( data = yelp_business[yelp_business$open == TRUE,],
      plot(review_count,
           stars, xlab = "Review Counts", ylab = "Stars", main = "Business on line")
)

dim( subset(yelp_business, open == FALSE)) # How many closed businesses
sum( subset(yelp_business, open == FALSE)$review_count ) # How many reviews for closed businesses, summary from business
dim( yelp_review[yelp_review$business_id %in% subset(yelp_business, open == FALSE)$business_id,])# How many reviews for closed businesses, summary from review

closed_review_users <- table( yelp_review[yelp_review$business_id %in% subset(yelp_business, open == FALSE)$business_id,]$user_id )
summary(as.numeric( closed_review_users) )
sum( closed_review_users[closed_review_users >= 10] )/sum(closed_review_users) # proportion of the users who contribute as least 10 reviews/users who contribute reviews to closed businesses
dim( yelp_user[yelp_user$user_id %in% names( closed_review_users[closed_review_users >= 10] ),]  )  # access the users who contribute as least 10 reviews
sum( yelp_user[yelp_user$user_id %in% names( closed_review_users[closed_review_users >= 10] ),]$review_count )/sum( yelp_user$review_count )  # proportion of the users who contribute reviews to closed businesses

table( yelp_review[yelp_user$user_id %in% names( closed_review_users[closed_review_users >= 10] ),]$date ) # the dates when the reviews for the closed businesses submitted

table(
  paste(  year(yelp_review[yelp_user$user_id %in% names( closed_review_users[closed_review_users >= 10] ),]$date),  month(yelp_review[yelp_user$user_id %in% names( closed_review_users[closed_review_users >= 10] ),]$date) )  
) # numbers of the reviews for closed businesses, distributed by months (2005/11 ~ 2015/1)

### The number of reviews for closed businesses are increasing.

## Closed businesses stars vs Review stars
summary( subset(yelp_business, open == FALSE)$stars )
length( subset(yelp_business, open == FALSE)$stars )

closed_businesses_reviews <- subset(yelp_business, open == FALSE)
closed_businesses_reviews_stars <- data.frame(  review_stars = with(data = closed_businesses_reviews, tapply(stars, business_id, mean)) ) 
closed_businesses_reviews_stars <- data.frame(business_id = row.names(closed_businesses_reviews_stars),closed_businesses_reviews_stars)
closed_businesses_reviews_stars <- data.frame(closed_businesses_reviews_stars ,business_stars = yelp_business$stars[yelp_business$business_id %in% closed_businesses_reviews_stars$business_id] )
cor.test(closed_businesses_reviews_stars[,2],closed_businesses_reviews_stars[,3])
### No correlation between closed business stars and review stars


## Closed reviews' stars vs User stars
closed_businesses_reviews_stars <- closed_businesses_reviews$stars
names(closed_businesses_reviews_stars) <- closed_businesses_reviews$business_id
user_avgstars <- yelp_user$average_stars
names(user_avgstars) <- yelp_user$user_id
cor.test(closed_businesses_reviews_stars, user_avgstars[names(closed_businesses_reviews_stars)])
### An insignificant positive correlation (r = .12) between closed reviews stars and users' average stars

## Opening reviews' stars vs User stars
dim(subset(yelp_business, open == TRUE))

opening_businesses_reviews <- subset(yelp_business, open == TRUE)
opening_businesses_reviews_stars <- data.frame(review_stars = with(data = opening_businesses_reviews, tapply(stars, business_id, mean)) ) 
opening_businesses_reviews_stars <- data.frame(business_id = row.names(opening_businesses_reviews_stars),opening_businesses_reviews_stars)
opening_businesses_reviews_stars <- data.frame(opening_businesses_reviews_stars ,business_stars = yelp_business$stars[yelp_business$business_id %in% opening_businesses_reviews_stars$business_id] )
cor.test(opening_businesses_reviews_stars[,2],opening_businesses_reviews_stars[,3])
### No correlation between openning business stars and review stars

## Opening reviews' stars vs User stars
opening_businesses_reviews_stars <- opening_businesses_reviews$stars
names(opening_businesses_reviews_stars) <- opening_businesses_reviews$business_id
cor.test(opening_businesses_reviews_stars, user_avgstars[names(opening_businesses_reviews_stars)])
### An insignificant negative correlation (r = -.06) between opening reviews stars and users' average stars

length(yelp_business$Loc)
length(yelp_business$business_id)
length(yelp_review$business_id)

tmp <- yelp_business$Loc
names(tmp) <- yelp_business$business_id

yelp_review <- data.frame(yelp_review, Loc = tmp[yelp_review$business_id])
yelp_tip <- data.frame(yelp_tip, Loc = tmp[yelp_tip$business_id])

table(yelp_business$Loc) ## Count how many businesses in each city
table( tmp[yelp_review$business_id] ) # How many reviews in each city
cor.test(table(yelp_business$Loc), table( tmp[yelp_review$business_id] ))  # More businesses in a city, more reviews in a city

user_review_loc <- table(yelp_review$user_id, tmp[yelp_review$business_id]) # Record user's review shown for every city
user_review_loc_df <- subset( data.frame( userid = rep(rownames(user_review_loc), dim(user_review_loc)[2]), Review_Count = as.numeric(user_review_loc), Loc = rep(colnames(user_review_loc),each = dim(user_review_loc)[1])), Review_Count != 0)
table(user_review_loc_df$userid)[table(user_review_loc_df$userid) > 2]  # How many users posted reviews across more than two cities

table( tmp[yelp_tip$business_id] ) # How many tips in each city
cor.test(table(yelp_business$Loc), table( tmp[yelp_tip$business_id] ))  # More businesses in a city, more tips in a city

user_tip_loc <- table(yelp_tip$user_id, tmp[yelp_tip$business_id]) # Record user's review shown for every city
user_tip_loc_df <- subset( data.frame( userid = rep(rownames(user_tip_loc), dim(user_tip_loc)[2]), Tip_Count = as.numeric(user_tip_loc), Loc = rep(colnames(user_tip_loc),each = dim(user_tip_loc)[1])), Tip_Count != 0)
table(user_tip_loc_df$userid)[table(user_tip_loc_df$userid) > 2]  # How many users posted tips across more than two cities

yelp_business <- data.frame(yelp_business, True_review_count = table(yelp_review$business_id)[yelp_business$business_id], True_tip_count = table(yelp_tip$business_id)[yelp_business$business_id] )

cor.test(yelp_business$stars, yelp_business$True_review_count)
cor.test(yelp_business$stars, yelp_business$True_tip_count)
# business star has weak positie correlation with (true) review counts and tip counts

business_review_stars_stat <- tapply(yelp_review$stars, yelp_review$business_id, mean)
business_tip_likes_stat <- tapply(yelp_tip$likes, yelp_tip$business_id, sum)
cor.test( business_review_stars_stat[names(business_tip_likes_stat[business_tip_likes_stat > 0])], business_tip_likes_stat[business_tip_likes_stat > 0] )
# A weak positive correlation beteen averaged reveiew stars and averaged tip likes (r = .013)
# A higher but weak positive correlation beteen averaged reveiew stars and summed tip likes (r = .038)
# The corrleation was up after excluded the likes = 0 (r = .057)

table( round(business_review_stars_stat[names(business_tip_likes_stat)]) )  # check the distribution of review stars for the businesses having tips
table(round(business_tip_likes_stat) )

table( round(business_review_stars_stat[names(business_tip_likes_stat)]), round(business_tip_likes_stat) )  # Frequency table of review stars and tip likes

## Split texts by locations
### Add location lables to reviews and tips
yelp_review <- data.frame(yelp_review, Loc = tmp[yelp_review$business_id])
yelp_tip <- data.frame(yelp_tip, Loc = tmp[yelp_tip$business_id])

### filter the reviews and tips by location 
length(yelp_review$text[yelp_review$Loc == unique(yelp_review$Loc)[9]])
length(yelp_tip$text[yelp_tip$Loc == unique(yelp_tip$Loc)[10]])

## Text mining reviews and tips
Review_docs <- 
dim( subset(yelp_tip, Loc == unique(yelp_tip$Loc)[2]) )
# 
save.image("../Yelp_Data/Corpus.RData")
