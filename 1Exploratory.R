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

grep(Bid, names(yelp_business))
grep(Bid, names(yelp_checkin))
grep(Bid, names(yelp_review))
grep(Bid, names(yelp_tip))
grep(Bid, names(yelp_user))

grep(Uid, names(yelp_business))
grep(Uid, names(yelp_checkin))
grep(Uid, names(yelp_review))
grep(Uid, names(yelp_tip))
grep(Uid, names(yelp_user))

grep(Rid, names(yelp_business))
grep(Rid, names(yelp_checkin))
grep(Rid, names(yelp_review))
grep(Rid, names(yelp_tip))
grep(Rid, names(yelp_user))

# Locations of businesses
sort( table(yelp_business$city) )


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
