#Mapping Police Response Locations
#READYING THE ENVIRONMENT AND UPLOADING THE MAIN FILE

#Add tools to ease removing duplicates and tools for data cleaning, 
#manipulation, and visualization in R.
install.packages("tidyverse") 
install.packages('ggmap') #Added Gerospatial functionality
#setwd("~/R/InnovatoR")

###READYING THE ENVIRONMENT AND UPLOADING THE MAIN FILE###

#Add libraries that enable removing duplicates and manipulating texts, and other formatting tricks
library(tidyverse)
library(stringr) #Additional verb-like functions for manipulating data
library(dplyr) #Simplify manipulation tasks
#load ggmap
library(ggmap) #add map visuals to R plots using online sources like Google Maps

getwd() #Confirm working directory 

#Store current datetime as 'now' to use in file name
now <- format(Sys.time(), format = "%m.%d.%Y.%Hh %Mm %Ss")

## Tell R to read the key and the main report export file from the directory.
la_llave <- as.factor(read.csv("ggmap key.csv"))
la_llave
register_google(key = la_llave)

original_main_df <- read.csv("sapd_service_requests_Jan2019.csv", na.strings=c(""," ","NA"), stringsAsFactors=FALSE )
# Can do code for historical file as time allows

#### Re-Run Code from HERE ####

#Check pre-deduplication dimensions
main_df <- original_main_df
dim(main_df)
#Remove duplicates rows (if any) and check dimensions again
main_df <- unique(main_df[,])
dim(main_df)

names(main_df)
summary(main_df)

#Create a new field that combines 'address' (street number, street name) with city, state, zip, and country.
main_df$friendly_address <- paste(main_df$ADDRESS, "SAN ANTONIO, TX", main_df$ZIP, "USA", sep = ", " )
#### Remove Special Text that messes with formatting ####
main_df$friendly_address <- gsub("[*]", "", main_df$friendly_address)
main_df$friendly_address <- gsub("[#]", "", main_df$friendly_address)
main_df$friendly_address <- gsub("@", "", main_df$friendly_address)
main_df$friendly_address <- gsub("&amp;", "", main_df$friendly_address)
main_df$friendly_address <- gsub("\n", "", main_df$friendly_address)
main_df$friendly_address <- gsub("[\"]", "", main_df$friendly_address)
main_df$friendly_address <- gsub("[']", "", main_df$friendly_address)
main_df$friendly_address <- gsub("&quot;", "", main_df$friendly_address)
main_df$friendly_address <- gsub("\"", "", main_df$friendly_address)


#preview results
head(main_df$friendly_address)
tail(main_df$friendly_address)

#### Geocoding a csv column of "addresses" in R ####


# Initialize the data frame
geocoded <- data.frame(stringsAsFactors = FALSE)

# Loop through the addresses to get the latitude and longitude of each address and add it to the
# origAddress data frame in new columns lat and lon
for(i in 1:nrow(main_df))
{
  # Print("Working...")
  result <- geocode(main_df$friendly_address[i], output = "latlona", source = "google")
  main_df$lon[i] <- as.numeric(result[1])
  main_df$lat[i] <- as.numeric(result[2])
  main_df$geoAddress[i] <- as.character(result[3])
}

main_df$lat_lon <- paste(main_df$lat, main_df$lon, sep = ", ")
## Write a CSV file containing original Address to the working directory
#write.csv(main_df, "geocoded.csv", row.names=FALSE)
file_name <- paste0("geocded911calls", ".csv" )
write.table(main_df, sep=",", file_name, row.names = FALSE, )
dim(main_df)