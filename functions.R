# From http://stackoverflow.com/questions/8751497/latitude-longitude-coordinates-to-state-code-in-r
# The single argument to this function, pointsDF, is a data.frame in which:
#   - column 1 contains the longitude in degrees (negative in the US)
#   - column 2 contains the latitude in degrees

# latlong2state <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
#  states <- map('worldHires', fill=TRUE, col="transparent", plot=FALSE)
#  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
#  states_sp <- map2SpatialPolygons(states, IDs=IDs,
#                                   proj4string=CRS("+init=epsg:4326"))
  
  # Convert pointsDF to a SpatialPoints object 
#  pointsSP <- SpatialPoints(pointsDF, 
#                            proj4string=CRS("+init=epsg:4326"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
#  indices <- over(pointsSP, states_sp)
  
  # Return the state names of the Polygons object containing each point
#  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
#  stateNames[indices]
# }

## Cities(From west to east): Las Vegas, Phoenix, Madison, Urbana-Champaign, Charlotte, Waterloo, Pittsburgh, Montreal, Edinburgh, Karlsruhe
## (Lat, Lon): (36, -115), (33,-112), (43,-89), (40, -88), (35,-81), (43,-81), (40,-80), (45, -74), (56,-3), (49, 8)
## substraction within +/- 2 equals to that city!

D_Cities <- function(lat, lon){
  C01 <- c(36, -115)  # Las Vegas, 
  C02 <- c(33,-112)   # Phoenix, 
  C03 <- c(43,-89)    # Madison, 
  C04 <- c(40, -88)   # Urbana-Champaign, 
  C05 <- c(35,-81)    # Charlotte, 
  C06 <- c(43,-81)    # Waterloo, 
  C07 <- c(40,-80)    # Pittsburgh, 
  C08 <- c(45, -74)   # Montreal, 
  C09 <- c(56,-3)     # Edinburgh, 
  C10 <- c(49, 8)     # Karlsruhe
  
  df <- data.frame(
    D_C01 = data.frame(
      lat = round( abs(lat - C01[1]) ),
      lon = round( abs(lon - C01[2]) )
    ),
    D_C02 = data.frame(
      lat = round( abs(lat - C02[1]) ),
      lon = round( abs(lon - C02[2]) )
    ),
    D_C03 = data.frame(
      lat = round( abs(lat - C03[1]) ),
      lon = round( abs(lon - C03[2]) )
    ),
    D_C04 = data.frame(
      lat = round( abs(lat - C04[1]) ),
      lon = round( abs(lon - C04[2]) )
    ),
    D_C05 = data.frame(
      lat = round( abs(lat - C05[1]) ),
      lon = round( abs(lon - C05[2]) )
    ),
    D_C06 = data.frame(
      lat = round( abs(lat - C06[1]) ),
      lon = round( abs(lon - C06[2]) )
    ),
    D_C07 = data.frame(
      lat = round( abs(lat - C07[1]) ),
      lon = round( abs(lon - C07[2]) )
    ),
    D_C08 = data.frame(
      lat = round( abs(lat - C08[1]) ),
      lon = round( abs(lon - C08[2]) )
    ),
    D_C09 = data.frame(
      lat = round( abs(lat - C09[1]) ),
      lon = round( abs(lon - C09[2]) )
    ),
    D_C10 = data.frame(
      lat = round( abs(lat - C10[1]) ),
      lon = round( abs(lon - C10[2]) )
    )
  )
  
  C.DF <- data.frame(
    C01 = ifelse( (df[1] < 2  & df[2] < 2), "Las Vegas", NA),
    C02 = ifelse( (df[3] < 2  & df[4] < 2), "Phoenix", NA),
    C03 = ifelse( (df[5] < 2  & df[6] < 2), "Madison", NA),
    C04 = ifelse( (df[7] < 2  & df[8] < 2), "Urbana-Champaign", NA),
    C05 = ifelse( (df[9] < 2  & df[10] < 2), "Charlotte", NA),
    C06 = ifelse( (df[11] < 2 & df[12] < 2), "Waterloo", NA),
    C07 = ifelse( (df[13] < 2  & df[14] < 2), "Pittsburgh", NA),
    C08 = ifelse( (df[15] < 2  & df[16] < 2), "Montreal", NA),
    C09 = ifelse( (df[17] < 2  & df[18] < 2), "Edinburgh", NA),
    C10 = ifelse( (df[19] < 2  & df[20] < 2), "Karlsruhe", NA)
  )
  
  IND <- list(
    which(!is.na(C.DF[,1])),
    which(!is.na(C.DF[,2])),
    which(!is.na(C.DF[,3])),
    which(!is.na(C.DF[,4])),
    which(!is.na(C.DF[,5])),
    which(!is.na(C.DF[,6])), 
    which(!is.na(C.DF[,7])),
    which(!is.na(C.DF[,8])),  
    which(!is.na(C.DF[,9])),
    which(!is.na(C.DF[,10]))  
  )
  
  return(IND)
}

City_Tag <- function(lat, lon){
  
  LIST <- D_Cities(lat, lon)
  
  city <- c("Las Vegas", "Phoenix", "Madison", "Urbana-Champaign", "Charlotte", "Waterloo", "Pittsburgh", "Montreal", "Edinburgh", "Karlsruhe")
  
  Tag <- as.character()
  for(i in 1:10)
    Tag[LIST[[i]]] <- city[i]
  
    return(Tag)
}


Variables_Transfer <- function(DATA){
  ### Label open/close information in numeric variable
  tmp <- matrix(unlist( sapply( DATA$hours, as.vector) ), ncol =  7*2 )
  hour = as.factor(apply(tmp,1,toString) )
  ## 0 == no information; 1 == 24 hours open
  levels(hour) <- c(1:(nlevels(hour) - 1), 0)
  DATA$hours = as.numeric(as.character(hour) )
  
  ### Count neighborhoods
  DATA$neighborhoods <- unlist( lapply(DATA$neighborhood, length) )
  
  
  ### Take and transfer attributes with information
  ## codes in testing: ## str(DATA$attributes, max.level = 1)## ATTR <- DATA$attributes
  
  ## lift the variables under level 1 variables
  ## chage list(Accept Credit Card) to logic values(TRUE, FALSE, NA)
  for(i in rev(1:dim(DATA$attributes)[2]) ){
    if(is.data.frame(DATA$attributes[,i]))
    {DATA$attributes <- data.frame(DATA$attributes, DATA$attributes[,i]);DATA$attributes[,i] <- NULL}
    if(is.list(DATA$attributes[,i]))
    {DATA$attributes[,i] = unlist( lapply( DATA$attributes[,i] , function(x)ifelse(is.null(x), NA, x)) )}  ### From http://r.789695.n4.nabble.com/List-elements-of-NULL-to-value-td3064384.html
  }
  
  ## Remove the variables with NA > 90%
  for(i in rev(1:dim(DATA$attributes)[2]) ){
    if(sum( !is.na(DATA$attributes[,i]) )/length(DATA$attributes[,i]) <= .10)
      DATA$attributes[,i] <- NULL
  }
  
  ## Transfer variable values to numeric
  for(i in 1:dim(DATA$attributes)[2]){
    if( is.logical(DATA$attributes[,i])  )  
    {
      ### For variables are logic
      DATA$attributes[,i] <- replace(DATA$attributes[,i], c(TRUE, FALSE), c(1,0))
    }
    if( is.character( DATA$attributes[,i] ) )  
    {
      ### For variables are character
      TMP = factor(DATA$attributes[,i], exclude = NULL)
      levels(TMP)[1:(nlevels(TMP)-1)] <- 1:(nlevels(TMP) - 1)
      DATA$attributes[,i] <- as.numeric(TMP)
    }
  }
  
  return(DATA)
}


Build_Cleaned_Docs <- function(text) {
  ### Build Corpus
  docs <- Corpus(VectorSource(text))
  # summary(docs)
 
  ### Preprocessing docs
  docs <- tm_map(docs, removePunctuation)                  # Removing punctuation
  
  docs <- tm_map(docs, removeNumbers)                      # Removing numbers
  
  docs <- tm_map(docs, tolower)                            # Converting to lowercase
  docs <- tm_map(docs, removeWords, stopwords("english"))  # Removing common words
  # Removing particular words: if necessary
  # docs <- tm_map(docs, stemDocument)  # Removing common word endings
  
  docs <- tm_map(docs, stripWhitespace)                    # Removing white spaces
  for(j in seq(docs))   
  {   
    docs[[j]] <- gsub("/", "", docs[[j]])   
    docs[[j]] <- gsub("@", "", docs[[j]])   
    docs[[j]] <- gsub("\\|", "", docs[[j]])   
    docs[[j]] <- gsub("^ ", "", docs[[j]])   
  }   
  
  
  docs <- tm_map(docs, PlainTextDocument)                  # tells R to treat your preprocessed documents as text documents.
  
  return(docs)
}

# Make_Som_Model <- function(df, city, x, y) {
#   if( (city %in% c("Charlotte","Edinburgh","Karlsruhe","Las Vegas","Madison",
#       "Montreal","Phoenix","Pittsburgh","Urbana-Champaign","Waterloo") ) == TRUE ){
#     require(kohonen)
#     tmp <- df[df$Loc == city,]$attributes + 1
#     tmp <- replace(tmp, is.na(tmp), 0)
#     data_train <- data.frame(df[df$Loc == city, c(4, 5, 6)], tmp)
#     rm(tmp)
#     data_train_matrix <- as.matrix(scale(data_train))  # remove the NA values in the data
#     som_grid <- somgrid(xdim = x, ydim= y, topo="hexagonal")
#     som_model <- som(data_train_matrix, 
#                      grid=som_grid, 
#                      rlen=100, 
#                      alpha=c(0.05,0.01), 
#                      keep.data = TRUE,
#                      n.hood='circular' )
#   
#   }
#   return(som_model)
# }

