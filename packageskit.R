# install and load the packages for this project
if(!require(devtools)){install.packages('devtools')}

if(!require(tm)){install.packages('tm')}

if(!require(SnowballC)){install.packages('SnowballC')}

# if(!require(rJava)){install.packages('rJava')}
# library(rJava)

# if(!require(Rwordseg)){install.packages('Rwordseg')}
# library(Rwordseg)

if(!require(ggplot2)){install.packages('ggplot2')}

if(!require(jsonlite)){install.packages('jsonlite')}

# if(!require(RDataCanvas)){install_github('DataCanvasIO/RDataCanvas')}
# library("RDataCanvas")

if(!require(lubridate)){install.packages('lubridate')}

if(!require(dplyr)){install.packages('dplyr')}

# if(!require(sp)){install.packages('sp')}
# library(sp)

# if(!require(maps)){install.packages('maps')}
# library(maps)

# if(!require(maptools)){install.packages('maptools')}
# library(maptools)

# if(!require(mapdata)){install.packages('mapdata')}
# library(mapdata)

# if(!require(ggmap)){install.packages('ggmap')}
# library(ggmap)


library("devtools")
library("tm")
library("ggplot2")
library("jsonlite")
library("lubridate")
library("dplyr")
library("SnowballC")
