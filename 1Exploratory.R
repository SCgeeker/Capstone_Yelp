load("../Yelp_Data/Raw.RData")

# Features in each data set
str( yelp_business, max.level = 1)
str( yelp_checkin, max.level = 1)
str( yelp_review, max.level = 1)
str( yelp_tip, max.level = 1)
str( yelp_user, max.level = 1)

# Mark id columns
Bid <- "business_id"  # registed business in the data sets
Uid <- "user_id"      # registed user in the data sets
Rid <- "review_id"    # available review in the data sets

# Explore features in each data set

## business
str(yelp_business$hours)          # Data frame
names(yelp_business$hours)
str(yelp_business$categories)     # List
unlist(lapply(yelp_business$categories, length))   # number of categories assigned to the budiness
table(unlist(lapply(yelp_business$categories, unique)))
sort( table(yelp_business$city) ) # Count the registed cities
str(yelp_business$neighborhoods)  # List neighborhoods
unlist(lapply(yelp_business$neighborhoods, length))   # number of neighborhood around the budiness
table(unlist(lapply(yelp_business$state, unique)))  # Count the registed states 
table(yelp_business$stars)        # frequency table of stars
sum( is.na(yelp_business$longitude) ) # Check missing geographical data
sum( is.na(yelp_business$latitude) ) # Check missing geographical data
class(yelp_business$attributes)   # Data frame
str(yelp_business$attributes, max.level = 1)  
names(yelp_business$attributes)
table( sapply(yelp_business$attributes,  class) )  # Summarize calsses of 1st level attributes
names( which( unlist( sapply(yelp_business$attributes,  is.data.frame)  ) == TRUE) ) # Get the attributes which are data frames
str(yelp_business$attributes[, which( unlist( sapply(yelp_business$attributes,  is.data.frame)  ) == TRUE)] ) # Explore the 1st level attributes which are data frames
sapply(yelp_business$attributes[, which( unlist( sapply(yelp_business$attributes,  is.data.frame)  ) == TRUE)], names ) # Show the names of 2nd level attributes
sapply(yelp_business$attributes[, which( unlist( sapply(yelp_business$attributes,  is.data.frame)  ) == TRUE)], dim )[2,] # Show the numbers of 2nd level attributes
summary( unlist( lapply( yelp_business$attributes$`Accepts Credit Cards`, unique) ) ) # Explore the 1st level attribute which is list
sum(yelp_business$review_count)  # sum up review counts of all businesses

## checkin
str(yelp_checkin$checkin_info)
names(yelp_checkin$checkin_info)
colMeans(yelp_checkin$checkin_info, na.rm = TRUE)  # Mean check-in times for a business
summary(yelp_checkin$checkin_info[1], na.rm = TRUE)

## review
str(yelp_review$votes)           # Data frame
sum(yelp_review$votes)
100 * ( table(yelp_review$stars)/sum(table(yelp_review$stars)) )   # Frequency table of stars

## user
str(yelp_user$votes)             # Data frame
sum(yelp_user$votes)
sum(yelp_user$review_count)      # sum up review counts of all users
str(yelp_user$friends)           # List
unlist(lapply(yelp_user$friends, length))   # number of friends the user owned
###table(unlist(lapply(yelp_user$friends, unique)))
str(yelp_user$compliments)       # Data frame
str(yelp_user$elite)             # List
unlist(lapply(yelp_user$elite, length))  # how many years the user is alive on yelp
table(unlist(lapply(yelp_user$elite, unique)))  # frequency table of alive years

## Preview business geographic locations
plot( SpatialPoints( yelp_business[,c("longitude","latitude")], proj4string = CRS("+init=epsg:4326")), pch = 1)

table( round(yelp_business[,c("longitude")]) )
table( round(yelp_business[,c("latitude")]) )
table( round(yelp_business[,c("longitude","latitude")]) )

#dist(yelp_business[,c("longitude","latitude")], method = "manhattan")

# Business open status
table(yelp_business$open)

# Review count: businesses vs. users
plot(
  yelp_business$review_count[yelp_review$business_id %in% yelp_business$business_id]/1000,
  yelp_user$review_count[yelp_review$user_id %in% yelp_user$user_id]/1000, xlab = "businesses(1,000)", ylab = "users(1,000)")

# Review count vs business star
plot(
  yelp_business$review_count,
  yelp_business$stars, xlab = "Review Counts", ylab = "Stars")

cor.test(yelp_business$review_count, yelp_business$stars)

# Review count vs user fans
plot(
  yelp_user$review_count,
  yelp_user$fans, xlab = "Review Counts", ylab = "Fans")

cor.test(yelp_user$review_count,  yelp_user$fans)
