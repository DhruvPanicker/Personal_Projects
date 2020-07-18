#Fifa Women's World Cup Project

#Load the packages
library(readr)
library(dplyr)
# Read in the data from the datasets folder
wwc_raw <- read_csv("C:/Users/Dhruv/Desktop/Personal/R course/Datacamp/Projects/Importing and Cleaning Data/datasets/2019_WWCFIFA_summary.csv")

#check the dimensions and structure of the data
class(wwc_raw)
glimpse(wwc_raw)
summary(wwc_raw)
str(wwc_raw)

# Read in the data specifying column types (assigning data types to the Round, Date, and Venue columns)

wwc_raw <- read_csv("C:/Users/Dhruv/Desktop/Personal/R course/Datacamp/Projects/Importing and Cleaning Data/datasets/2019_WWCFIFA_summary.csv",
            col_types = cols(
                        Round = col_factor(),
                        Date = col_date("%m/%d/%y"),
                        Venue = col_factor()
                            )        
                    )
# Look at the summary and structure of the data
glimpse(wwc_raw)
summary(wwc_raw)

# Print the dataset
print(wwc_raw)

#Removing rows of NA
#We have 55 rows of 13 variables. But there are 52 games. hence extra rows are present
#we need to fix NA rows, missing data values ,NA in score and pks, column headers are a mix of lower and upper case

#loading the tidyr package
library(tidyr)
# Remove rows of NA
wwc_1 <- wwc_raw %>%
  rename_all(tolower) %>% 
  filter(!is.na(round))

# Get the dimensions and inspect the first 10 and last 10 rows
head(wwc_1)
tail(wwc_1)

#now we have 52 observations.Each row corresponds to a match in the tournament
#checking how many NA in each column below

colSums(is.na(wwc_1))

#NA still exists in date & venue & pks
#cleaning date & venue below

wwc_2  <- wwc_1

# Find, view, and replace NA in column date
index_date <- which(is.na(wwc_2$date))
wwc_2[index_date,]
wwc_2$date[index_date] <-"2019-06-09"

# Find, view, and replace NA in column venue
index_venue <- which(is.na(wwc_2$venue))
wwc_2[index_venue,]
wwc_2$venue[index_venue] <- "Groupama Stadium"

#it is a good idea to get the two data points in score and two data points in pks into their own columns for future data sleuthing.
# Separate columns and replace NA
wwc_3 <- wwc_2 %>% 
  separate(score,c("home_score","away_score"), sep = "-", convert = TRUE) %>% 
  separate(pks,c("home_pks", "away_pks"), sep ="-", convert = TRUE) %>% 
  mutate (home_pks = replace_na(home_pks,0),
          away_pks = replace_na(away_pks,0))
#We corrected the NA in the date and venue columns, and separated the score and pks columns to have one score per column.
#Now we can take a look at attendance and find the information the boss wants. Let's plot the data to see if there are any outliers
# Housekeeping for plot size
options(repr.plot.width=6, repr.plot.height=4)

# Load the package
library(ggplot2)

# Make a boxplot of attendance by venue and add the point data
ggplot(wwc_3, aes(venue, attendance)) +
  geom_boxplot() +
  geom_jitter(color = "red", size = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#error in Groupama stadium of extra zero. Lets fix the outlier
# Summarize the number of games, and min and max attendance for each venue
attendance_outlier  <- wwc_3  %>% 
  group_by(venue)  %>% 
  summarize(nb_of_games = n(), 
            min_attendance = min(attendance), 
            max_attendance = max(attendance))

# Correct the outlier
wwc_4  <- wwc_3  %>% 
  mutate(attendance = replace(attendance, which(attendance == 579000), 57900))

# Print an updated summary table 
wwc_venue_summary <- wwc_4  %>% 
  group_by(venue)  %>% 
  summarize(nb_of_games = n(), 
            min_attendance = min(attendance), 
            max_attendance = max(attendance))

#Making boxplot again
ggplot(wwc_4, aes(venue, attendance)) +
  geom_boxplot() +
  geom_jitter(color = "red", size = 0.5) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
