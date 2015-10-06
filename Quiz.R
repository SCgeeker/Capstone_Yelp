fisher.test(
table( yelp_user$fans > 1, yelp_user$votes$funny > 1, useNA = "ifany" )
)