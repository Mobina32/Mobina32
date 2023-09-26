## Scientific Data Management Course
## Data Cleaning and Standards Assignment
## Using BWG dataset provided by the SDM course
## Mobina Gholamhosseini
## 2023-09-15
## Since my PhD project is related to spatial and temporal changes, 
## I choose the Dates and Coordinates data cleaning task

## NOTE ##
## In order to import the data with this script, your working directory needs to
## be set to the correct folder!
## 1. Create a new folder (or use the zip folder you downloaded)
## 2. Make sure this script into the folder you are using.
## 3. Also make sure the BWG_database folder is in the same folder.
## 4. Close and reopen R by opening the this script with RStudio (right-click and
##    select "Open with" -> RStudio. Alternatively, you can also navigate to the 
##    folder in the "Files" tab then clicking "More" (with the gear icon) and 
##    selecting "Set As Working Directory"
## Your working directory should now be set to the correct folder.


# initial set up ----------------------------------------------------------

### Working Directory ###

## check if working directory is set to current folder
getwd()

## if it is not the correct folder, use setwd() or here::here() to set your 
## working directory to the correct folder.
## Alternatively, in the panel with the "Files" tab, navigate to the correct folder,
## then click "More" (with the gear next to it), and select "Set As Working Directory"

### import (or install) required packages (I wrote their function next to them)
install.packages("tidyverse") #data tidying
library(tidyverse) 
install.packages("dplyr") #to select, rename, and arrange or ascend the order
library(dplyr)
install.packages("tidyr") #to separate a column into new ones
library(tidyr) 
install.packages("lubridate") #extracting data related to time and date
library(lubridate) 
install.packages("sf") #extracting spatial data
library(sf)
install.packages("assertr") #quality control 
library(assertr) 

#you can only run the library if you already had all the packages installed on 
#your RStudio 

#another option to install packages:
#pkgs <- c("tidyr", "assertr", "lubridate", "stringdist",
          #"sf",  "tidyverse")
#lapply(pkgs, library, character.only = TRUE)
#rm(pkgs)


### Import Files ###

## what files are in the BWG database
myfiles <- list.files(pattern = "*.csv", full.names = TRUE)
myfiles
#so 7 datasets exist in our working directory

#import all tables as separate data frames, remove file path 
#and file extensions (.csv)

list2env(
  lapply(
    setNames(myfiles, 
             make.names(
               gsub(".*1_", "", 
                    tools::file_path_sans_ext(myfiles)))), 
    read_csv), 
  envir = .GlobalEnv)


#let's take a look at the structure of all data frames
#so that we can choose the ones with date and coordinate information
glimpse(abundance) # or str(abundance)
glimpse(ownership)
glimpse(datasets) 
glimpse(visits) 
glimpse(bromeliads) 
glimpse(owners) 
glimpse(traits) 

#so only datasets and vistits data frame have date and coordinate variables

## DATE ---------------------------------------------------------------
#first let's work on dataset data frame
View(datasets)

## to select columns by name:
datasets %>%
  select(dataset_id , bwg_release, public_release) 

#or we can assign a name to our dataset simultaneously 

datasets_selected <- datasets %>%
  select(dataset_id , bwg_release, public_release) 
View(datasets_selected)

##NOTE about pipe: %>% ###
# the pipe %>% allows for "chaining", which means that you can invoke multiple 
# method calls without needing variables to store the intermediate results.
## keyboard shortcut: Ctrl+shift+m / cmd+shift+m

##we can write #%>% View at the end of each chunk,
##to make sure that each line/chunk is running correctly.


####### Task1 = Dates and Coordinates #######
###Part1: convert all dates to ISO standards (yyyy-mm-dd, ISO 8601).

#note that the date format in this data frame can create ambiguity. 
#for example, 06/01/2016 can be interpreted as Jan 6th or Jun 1st!
## the LUBRIDATE package covers all your date-handling needs!

## Let's look at the visits table##
head(visits)
# check out the date column
visits$date
visits$date <- lubridate::as_date(visits$date) 

# or visits$date <- ymd(visits$date) %>%View

## Let's look at the datasets table##

datasets$public_release
datasets$public_release <- dmy(datasets$public_release) %>%View

#or 
datasets$bwg_release <- dmy(datasets$bwg_release) 
View(datasets$bwg_release)

### NOTE: I discuss the problem regarding the dataset data frame during the office 
#hour, but it wasn't resolved### 
#I hope the problem with datasets date format will not impact my points for 
#this assignment as I tried my best to troubleshoot that but wasn't solved!


##Part2: for the 'visits' dataframe, change the name 'date' to 'visit_date',

## DPLYR::RENAME ###
glimpse(visits)
visits_date <- visits %>%
  dplyr::rename(visit_date = date) 

View(visits_date)

rm(visits_date) #let's remove this from the datafarme since we need to refer
#to the name "date" in the next 2 question

##Part3: add columns for day, month, and year of each visit 
##place these columns immediately after the date column

#Using tidyr::separate we split the string on "-"

visits <- visits %>% 
  separate(date, into = c("day", "month", "year"), 
           sep = "-",remove = FALSE)
View(visits)
#or

#visits <- visits %>%
  #separate(col = date,
  #into = c("day", "month", "year"), 
  #sep = "-",remove = FALSE)

#the new columns are placed after the date column


##Part4: Lastly, add the 'date' column from 'visits' to the 'bromeliads' dataframe,
##immediately after the column 'collection_date'. 

bromeliad_visits <- visits %>% 
  select(date, visit_id) %>% 
  right_join(., bromeliads, by = "visit_id") %>% 
  relocate(date, .after = collection_date)
View(bromeliad_visits)

#Does the visit_date match with the 'collection_date' in the bromeliads dataframe?
#NO. 
#Based on what has been said in the course, collection date and visit date
#should not necessarily match 


## coordinate --------------------------------------------------------------- 
##Part5:	Assign a coordinate reference system to all coordinates, 
##and then project geographic coordinates

#Note that the correct system is 6709 standards 
#and using decimal system rather than degree system

#Let's look at all data frames again to see which one has coordinate information
#so that we can work on them
data <- list(datasets, visits, abundance, 
             bromeliads, owners, ownership, traits)

glimpse(data)

#so there is coordinate data in datasets and visits data frame

#let's do the task first on the datasets data frame
head(datasets)
str(datasets)

# note that we have columns for longitude and latitude
xy <- datasets[c("lat", "lng")]
xy

## first, convert coordinates to "SpatialPoints"
xy <- st_as_sf(xy, 
               coords = c("lat", "lng"),
               crs = "+proj=longlat +datum=WGS84")
xy

# transform to UTM coordinate system
xy_utm <- st_transform(xy, crs = "+proj=utm +zone=16 +datum=WGS84")
xy_utm


#let's do the same thing for the visits data frame
head(visits)
str(visits)

# note that we have columns for longitude and latitude
visit_coords <- visits[c("latitude", "longitude")]
visit_coords

## first, convert coordinates to "SpatialPoints"
visit_coords <- st_as_sf(visit_coords, 
               coords = c("latitude", "longitude"),
               crs = "+proj=longlat +datum=WGS84")
visit_coords

# transform to UTM coordinate system
visit_coords_utm <- st_transform(visit_coords, crs = "+proj=utm +zone=16 +datum=WGS84")
visit_coords_utm


####### Quality Control #######
##Part1: perform quality control checks on three measured variables in the
##bromeliads dataset (e.g., max_water, num_leaf, height)

#NOTE that your checks might include, for example, looking for the proportion of
#observations that include data on each variable, or identifying outliers or 
#improbably values. Remember that some bromeliad variables should be correlated 
#with each other. 

# package: ASSERTR#

## one way to find the errors in a data frame is:
## to calculate something, with some assertions upstream
## if the assertions fail, the calculation will also fail

## let's calculate the mean max_water among each species in bromeliads dataset
bromeliads %>% 
  group_by(species) %>% 
  summarise(mean_max_water = mean(max_water, na.rm = TRUE))  %>% View

#Note: the following code will cover the part1 of the task: 
#Look for the proportion of observations that include data on each variable

# (1) priori constraint: should include 100 observations ------------------------------------
bromeliads %>% 
  verify(nrow(.) == 100) %>% 
  group_by(species) %>% 
  summarise(mean_max_water = mean(max_water, na.rm = TRUE))

##stopped execution because there aren't 100 rows
##but how many are there?? 
##there are 76 rows in the max_water column 
#if we run the code again with nrow(.) == 76 , the assertaion will not fail
bromeliads %>% 
  verify(nrow(.) == 76) %>% 
  group_by(species) %>% 
  summarise(mean_max_water = mean(max_water, na.rm = TRUE)) 


# (2) post hoc constraint: max_water too much or too low (outlier) ------------------

bromeliads %>% 
  insist(within_n_mads(10), max_water) %>% 
  group_by(species) %>% 
  summarise(mean_max_water = mean(max_water, na.rm = TRUE))


## stopped execution because one value (6116) is more than 10 SD
## away from the mean max_water across all species
## so this value is an outlier in the max_water column

#Note: we can also use within_n_sds() but the problem with that is the mean 
## and standard deviation are so heavily influenced by outliers, their 
## very presence will compromise attempts to identify them using these 
## statistics. In contrast with within_n_sds(), within_n_mads() uses 
## the robust statistics, median and median absolute deviation, to 
## identify potentially erroneous data points."

#we can also see other outliers in the max_water data through:
#(1) histogram  
hist(bromeliads$max_water, nclass = 30)

## we would need to look at just the Vriesea_sanguinolenta species to see 
## the other subtle outlier

hist(bromeliads$max_water[bromeliads$species == "Vriesea_sanguinolenta"], 
     nclass = 30)


#or (2) scatter plot to see the correlation between max_water, num_leaf, and height

ggplot(data = bromeliads, 
       mapping = aes(x = max_water, y = num_leaf, 
                     colour = species)) + 
  geom_point(size = 2, position = position_jitter(w = 0.2, h = 0.2)) + 
  facet_wrap(~ species, nrow = 2, scales = "free") + 
  theme(legend.position = "none")

## we can see that max_water = 1530 and 650 can be outlier because they are too
## much given the number of leaf of the individual plant in each species(as an example!
##since I am not very familiar with this dataset and in fact bromeliad!)


## these bivariate outliers can be identified with the Mahalanobis distance 
#for each row
#bromeliads %>% 
  #insist_rows(maha_dist, within_n_mads(10), everything())
#The code leads to an error "Error in svd(X) : infinite or missing values in 'x'"
#although I followed the same code as the lesson-3 script! In the penguins dataset
#we also have NA values, so this I guess might not be the reason.




