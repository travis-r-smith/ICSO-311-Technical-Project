##Integrated Community Safety Office Interview Project ##
#READYING THE ENVIRONMENT AND UPLOADING THE MAIN FILE

#Add tools to ease removing duplicates and tools for data cleaning, 
#manipulation, and visualization in R.
install.packages("tidyverse")
#Add libraries that enable removing duplicates and manipulating texts, and other formatting tricks
library(tidyverse)
library(dplyr) #Simplify manipulation tasks

#Confirm working directory
getwd()
#Modify working directory if necessary
setwd()

#Store current timestamp as 'now' to use in file name
now <- format(Sys.time(), format = "%m.%d.%Y.%Hh %Mm %Ss")
#print(now)

### Connect to data sources ###
#Grab primary data source: 311 data.
#Considered connecting to API but each day results could change and make 
#my analysis look inaccurate
original_main_df <- read.csv("cosa_311_Service_Requests_March2024_mung.csv", na.strings=c(""," ","NA"), stringsAsFactors=FALSE)

#Grab police response data that I manually scraped and then attempted to add geocode information on
police_jan2019_df <- read.csv("geocded911calls.csv", na.strings=c(""," ","NA"), stringsAsFactors=FALSE)

#Grab 78205 Weather dataset that I paid $10 for
weather_df <- read.csv("openweathermap_78205_Jan1980_Feb2024.csv", na.strings=c(""," ","NA"), stringsAsFactors=FALSE)

#### Re-Run Code from HERE ####
main_df <- original_main_df
dim(main_df)
dim(police_jan2019_df)
#Remove duplicates rows. These are rows that have exact same values in all fields
main_df <- unique(main_df[,])
police_jan2019_df <- unique(police_jan2019_df[,])

#Deduplicate on a specifc column if necessary
#main_df <- main_df[!duplicated(main_df$INCIDENT_NUMBER), ]
dim(main_df)
dim(police_jan2019_df)

names(main_df)
summary(main_df)

#view all column names in each dataset
names(main_df)
names(police_jan2019_df)
names(weather_df)

#View structure and class of datasets
str(main_df)
class(main_df)

str(police_jan2019_df)
class(police_jan2019_df)

str(weather_df)
class(weather_df)

#Convert all of the dates that came over as 'factors' to 'dates'
main_df$OPENEDDATETIME <- as.Date(main_df$OPENEDDATETIME, "%d/%m/%Y")
main_df$CLOSEDDATETIME <- as.Date(main_df$CLOSEDDATETIME, "%d/%m/%Y")
main_df$SLA_Date <- as.Date(main_df$SLA_Date, "%d/%m/%Y")
main_df$CASEID <- as.integer(main_df$CASEID) #Store CaseID as integer
main_df$Council.District <- as.factor(main_df$Council.District) #Treat council district as factor and not a number
main_df$Council.DistrictNum <- as.numeric(main_df$Council.District) #Sepearate field to treat district as number when needed

#Rename ugly names
colnames(main_df)[colnames(main_df) == "Late..Yes.No."] <- "Late"


#confirm conversion
class(main_df$OPENEDDATETIME)
class(main_df$CLOSEDDATETIME)
class(main_df$CASEID)
class(main_df$Council.District)
class(main_df$Council.DistrictNum)

#Create DAYSTOCLOSE
main_df$DAYSTOCLOSE <- as.numeric((main_df$CLOSEDDATETIME - main_df$OPENEDDATETIME))

#Create SLA Length
main_df$SLA_Length <- as.numeric((main_df$SLA_Date - main_df$OPENEDDATETIME))
#Create PCTofSLAtime aka percentage of allotted SLA time that it took to close the case
main_df$PCTofSLAtime <- as.numeric((main_df$DAYSTOCLOSE / main_df$SLA_Length ))

#Create a function to format field as % instead of decimal
percentage <- function(x, digits = 4) {
  paste0(formatC(100 * x, format = "f", digits = digits), "%")
}

main_df$PCT_SLAtime <- percentage(main_df$PCTofSLAtime)

#Take a peak at the first/last several values for each field
head(main_df, n = 30)

tail(main_df)

#Sort data frame in descending order by the number of DAYSTOCLOSE
main_df <- main_df %>% arrange(desc(DAYSTOCLOSE))


#Remove observations where DAYSTOCLOSE is negative. Accounts for 6,951 rows
main_df <- main_df %>% filter(is.na(DAYSTOCLOSE) | DAYSTOCLOSE >= 0)

#Remove observations where DAYSTOCLOSE is negative. Accounts for 11,689 rows. 
#Total of about 3.3% of the original dataset removed due to errors
main_df <- main_df %>% filter(is.na(SLA_Length) | SLA_Length >= 0)

#Calculate various numerical summaries related to distribution and center for each field in the data set.
summary(main_df)

# Merge dataframes on dt_iso_date and OPENED_DATE (assuming they are in similar format)
# Group by dt_iso_date and filter for the row with highest temp_max
weather_df$dt_iso_date <- as.Date(weather_df$dt_iso_date, "%d/%m/%Y")

weather_df <- weather_df %>%
  group_by(dt_iso_date) %>%
  #filter(temp_max == max(temp_max) & dt_iso_time == max(dt_iso_time) ) %>%
  arrange(desc(temp_max)) %>%  # Arrange by temp_max (highest first)
  top_n(1)  # Keep only the top row (highest temp_max)
  
  
weather_df <- weather_df %>%
  group_by(dt_iso_date) %>%
  #filter(temp_max == max(temp_max) & dt_iso_time == max(dt_iso_time) ) %>%
  arrange(desc(dt_iso_UTC)) %>%  # Arrange by temp_max (highest first)
  top_n(1)  # Keep only the top row (highest temp_max)
  
deduplicated_df <- weather_df %>%
  distinct(dt_iso_date)

# Optionally, add remaining columns (if any)
if (ncol(weather_df) > 1) {
  # Group by dt_iso_date and keep the first row (similar to method 1)
  deduplicated_df <- weather_df %>%
    group_by(dt_iso_date) %>%
    slice_head(n=1)
}

deduplicated_df$dt_iso_date <- as.Date(deduplicated_df$dt_iso_date, "%d/%m/%Y")
merged_df <- merge(main_df, deduplicated_df, by.x = "OPENEDDATETIME", by.y = "dt_iso_date", all.x = TRUE)
main_df <- merged_df


#Check for correlation in various numeric fields
cor(main_df$Council.DistrictNum, main_df$DAYSTOCLOSE, use = "complete.obs")

cor(main_df$SLA_Length, main_df$DAYSTOCLOSE, use = "complete.obs")

cor(main_df$CASEID, main_df$Council.DistrictNum, use = "complete.obs")

cor(main_df$Council.DistrictNum, main_df$temp_max, use = "complete.obs")
cor(main_df$DAYSTOCLOSE, main_df$temp_max, use = "complete.obs")
cor(main_df$SLA_Length, main_df$temp_max, use = "complete.obs")


#Average and distributions by district
fivenum(main_df$DAYSTOCLOSE[main_df$Council.District == "1"])

fivenum(main_df$DAYSTOCLOSE[main_df$Council.District == "2"])

fivenum(main_df$DAYSTOCLOSE[main_df$Council.District == "3"])

fivenum(main_df$DAYSTOCLOSE[main_df$Council.District == "4"])


fivenum(main_df$DAYSTOCLOSE[main_df$Council.District == "5"])

fivenum(main_df$DAYSTOCLOSE[main_df$Council.District == "6"])

fivenum(main_df$DAYSTOCLOSE[main_df$Council.District == "7"])

fivenum(main_df$DAYSTOCLOSE[main_df$Council.District == "8"])

fivenum(main_df$DAYSTOCLOSE[main_df$Council.District == "9"])

fivenum(main_df$DAYSTOCLOSE[main_df$Council.District == "10"])

fivenum(main_df$DAYSTOCLOSE[main_df$Council.District == "0"])
mean(main_df$DAYSTOCLOSE, na.rm = TRUE)

#D10 AND D4 HAVE HIGHER 3RD QUARTILES
#D6 and D1 have lower
#
# Test if there is a statistically significant difference between case closing times by council districts
#t.test(DAYSTOCLOSE ~ Late, data = main_df)
main_df %>%
  select(DAYSTOCLOSE, Council.District) %>%
  filter(Council.District %in% c("1","2")) %>%
  drop_na(DAYSTOCLOSE) %>%
  t.test(DAYSTOCLOSE ~ Council.District, data = .)



main_df %>%
  select(DAYSTOCLOSE, Council.District) %>%
  filter(Council.District %in% c("1","10")) %>%
  drop_na(DAYSTOCLOSE) %>%
  t.test(DAYSTOCLOSE ~ Council.District, data = .)


main_df %>%
  select(DAYSTOCLOSE, Council.District) %>%
  filter(Council.District %in% c("5","10")) %>%
  drop_na(DAYSTOCLOSE) %>%
  t.test(DAYSTOCLOSE ~ Council.District, data = .)

main_df %>%
  select(DAYSTOCLOSE, Council.District) %>%
  filter(Council.District %in% c("2","10")) %>%
  drop_na(DAYSTOCLOSE) %>%
  t.test(DAYSTOCLOSE ~ Council.District, data = .)
#Difference between D2 and D10 resolutions were the LEAST statistically significant result

main_df %>%
  select(DAYSTOCLOSE, Council.District) %>%
  filter(Council.District %in% c("2","5")) %>%
  drop_na(DAYSTOCLOSE) %>%
  t.test(DAYSTOCLOSE ~ Council.District, data = .)

#Difference between D2 and D5 resolutions were the most statistically significant result

main_df %>%
  select(DAYSTOCLOSE, Council.District) %>%
  filter(Council.District %in% c("2","9")) %>%
  drop_na(DAYSTOCLOSE) %>%
  t.test(DAYSTOCLOSE ~ Council.District, data = .)


#Hypothesis test on Close Time vs type of issue

main_df %>%
  select(DAYSTOCLOSE, Category) %>%
  filter(Category %in% c("Traffic Signals and Signs","Solid Waste Services")) %>%
  drop_na(DAYSTOCLOSE) %>%
  t.test(DAYSTOCLOSE ~ Category, data = .)

main_df %>%
  select(DAYSTOCLOSE, Category) %>%
  filter(Category %in% c("Animals","Property Maintenance")) %>%
  drop_na(DAYSTOCLOSE) %>%
  t.test(DAYSTOCLOSE ~ Category, data = .)


#Check for correlation on ALL numeric field combinations
main_df_num <- data.frame(main_df$CASEID, !is.na(main_df$SLA_Length), 
                          !is.na(main_df$PCTofSLA), !is.na(main_df$DAYSTOCLOSE), 
                          main_df$Council.DistrictNum, !is.na(main_df$XCOORD), 
                          !is.na(main_df$YCOORD), !is.na(main_df$temp_max),
                          !is.na(main_df$wind_speed), !is.na(main_df$feels_like),
                          !is.na(main_df$visibility), !is.na(main_df$humidity),
                          !is.na(main_df$wind_deg))
str(main_df_num)
numnames <- c('CaseID', 'SLAlength', 'PCTofSLA', 'DaystoClose', 
              'CouncilDistrict', 'X-Coor', 'Y-Coor', 'temp_max',
              'wind_speed','feels_like','visibility','humidity',
              'wind_deg')
correlation_matrix <- matrix(cor(main_df_num), 13, 13, dimnames = list(numnames, numnames))

#There do not appear to be any meaningful correlations

#Make a dot chart that shows the number of days it took to close cases by district.
plot(main_df$Council.District, main_df$DAYSTOCLOSE)

#Calculate metrics for center and distribution values for each of the numerical fields.
summary(main_df_num)


#Create a bar chart of total cases per district.
councicasevol <- plot(main_df$Council.District)
title(main = "Council District 311 Case Volume", xlab = "Council District", ylab = "311 Case Volume",
      font = 2, col = "blue")
#names(main_df)

#summary(main_df$Council.DistrictNum)

#summary(main_df$Council.District)

#Store district case totals in a vector
district_volume <- as.numeric(c(summary(main_df$Council.District)))

#Analyze the list of district case totals
summary(district_volume)

#Save list of 11 district names and verify structure.
Council.DistrictNumNames <- as.numeric(c(0:10))

str(Council.DistrictNumNames)

#Verifey district_volume structure
str(district_volume)

district_volume

#Calculate the correlation between district number and quantity of cases
cor(Council.DistrictNumNames, district_volume)



#Store results as files Create files 
correlation_file_name <- paste0("matrix_311_correlation", ".csv" )
write.table(correlation_matrix, sep=",", correlation_file_name, row.names = FALSE, )

main_file_name <- paste0("311_calls", ".csv")
write.table(main_df, sep=",", main_file_name, row.names = FALSE, )

police_file_name <- paste0("SAPD_jan_2019", ".csv" )
write.table(police_jan2019_df, sep=",", police_file_name, row.names = FALSE, )
dim(police_jan2019_df)

