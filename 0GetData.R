## download the dataset to the external directory
yelpdata <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/yelp_dataset_challenge_academic_dataset.zip"
if(dir.exists("../Yelp_Data") == FALSE) dir.create("../Yelp_Data")
download.file(yelpdata, "../Yelp_Data/yelp.zip")

## Explore the dataset
### store the file names
filenames <- unzip("../Yelp_Data/yelp.zip", list = TRUE)
### Extract the pdf files to the project directory
#unzip(zipfile = "../Yelp_Data/yelp.zip",filenames[c(2,8),1], junkpaths = TRUE)
### Extract the json files to the dataset diretory
unzip(zipfile = "../Yelp_Data/yelp.zip",filenames[c(3:7),1], exdir = "../Yelp_Data", junkpaths = TRUE)
Jsons.info <- file.info( dir("../Yelp_Data", pattern = "*.json", full.names = TRUE))


## Load json data to R objects
yelp_business <- fromJSON(sprintf("[%s]", paste(readLines(dir("../Yelp_Data", full.names = TRUE, pattern = "*.json")[1]), collapse=",")))
names(yelp_business)
head(yelp_business)

yelp_checkin <- fromJSON(sprintf("[%s]", paste(readLines(dir("../Yelp_Data", full.names = TRUE, pattern = "*.json")[2]), collapse=",")))

yelp_review <- fromJSON(sprintf("[%s]", paste(readLines(dir("../Yelp_Data", full.names = TRUE, pattern = "*.json")[3]), collapse=",")))

yelp_tip <- fromJSON(sprintf("[%s]", paste(readLines(dir("../Yelp_Data", full.names = TRUE, pattern = "*.json")[4]), collapse=",")))

yelp_user <- fromJSON(sprintf("[%s]", paste(readLines(dir("../Yelp_Data", full.names = TRUE, pattern = "*.json")[5]), collapse=",")))

save.image("../Yelp_Data/Raw.RData")
