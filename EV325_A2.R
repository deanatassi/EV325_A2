
# In-class activity
################################################################################
#Loading packages and libraries
install.packages(c("lubridate", "dplyr"))
library(lubridate)
library(dplyr)

stream_gauge <- read.csv("/cloud/project/activtiy02/stream_gauge.csv")

site_info <- read.csv("/cloud/project/activtiy02/site_info.csv")

# Parsing Date

stream_gauge$dateF <- ymd_hm(stream_gauge$datetime,
                          tz = 'America/New_York')


# Join site info to stream gauge height

floods <- full_join(stream_gauge,site_info, by = "siteID")

peace <- floods %>% filter(siteID == 2295637)
peace

example <- floods %>% filter(gheight.ft >= 10) 

plot(peace$dateF, peace$gheight.ft, type = 'l')

max_height <- 
  floods %>% 
  group_by(names) %>% 
  summarise(max_height_ft = max(gheight.ft, na.rm = TRUE),
            mean_ft = mean(gheight.ft, na.rm = TRUE),
            min_height = min(gheight.ft, na.rm = TRUE))


# Earliest date that each river reached the flood stage 

flood_stage <- 
  floods %>% 
  group_by(names) %>% 
  filter(gheight.ft >= flood.ft) %>% 
  summarise(Date = min(dateF))
flood_stage

#Homework 1
################################################################################
library(knitr)

# Using Arrange (allows you to arrange the rows of a table according to the values of a column)
arrange_example <- example(arrange, package = "dplyr") # In this case we are arranging by Sepal Length in descending order
tibble_arrange <- as_tibble(arrange_example)
tibble_arrange

## Coding it ourselves for floods data, arranging by gauge height in descending order

floods_arrange <- arrange(floods, desc(gheight.ft))
tibble_floods_arrange <- as_tibble(floods_arrange)


# Using Slice (allows you to index rows by their integer locations to select, remove, and duplicate rows)
slice_example <- example(slice, package = 'dplyr')

mtcars
mtcars %>% slice(5:n())

slice(mtcars,1)
slice(mtcars,4)
slice(mtcars,-(1:4))

## Using Slice on floods data to eliminate the rows from 10-20

slice(floods, -(10:20))


# Using Slice Max (used to display the rows with the maximum values of a variable)

# an example using the iris data set - we can display the 10 rows with the largest sepal length 

iris %>% slice_max(Sepal.Length, n = 10)

## Using slice_max to isolate the data with the highest gauge height in the floods data
## Displaying the 10 largest

floods %>% slice_max(gheight.ft, n = 10)
#################################################################################
#Homework 2

# 1 - Displaying Stream Stage Data for each River (Stream stage = height of the water's surface, so relevant variable should be gheight.ft)


# River 1 (	FISHEATING CREEK AT PALMDALE - siteID = 2256500)

fisheating_plot <- floods %>% filter(siteID == 2256500) 
plot(fisheating_plot$dateF, fisheating_plot$gheight.ft, xlab = "Date", ylab = "Stage Height (ft)", main = "Stage Height for Fisheating Creek")

# River 2 (PEACE RIVER AT US 17 AT ZOLFO SPRINGS - siteID = 2295637)

peace <- floods %>% filter(siteID == 2295637) 
plot(peace$dateF, peace$gheight.ft, xlab = "Date", ylab = "Stage Height (ft)", main = "Stage Height for Peace River")

# River 3 (WITHLACOOCHEE RIVER AT US 301 AT TRILBY - siteID = 2312000)

withlacoochie <- floods %>% filter(siteID == 2312000)
plot(withlacoochie$dateF, withlacoochie$gheight.ft, xlab = "Date", ylab = "Stage Height (ft)", main = "Stage Height for Withlacoochee River")

# River 4 (SANTA FE RIVER NEAR FORT WHITE - siteID = 2322500)

santa_fe <- floods %>% filter(siteID == 2322500)
plot(santa_fe$dateF, santa_fe$gheight.ft, xlab = "Date", ylab = "Stage Height (ft)", main = "Stage Height for Santa Fe River")

# Question 2

# What was the earliest date of occurrence for each flood category in each river
# (from class code)
flood_stage <- 
  floods %>% 
  group_by(names) %>% 
  filter(gheight.ft >= flood.ft) %>% 
  summarise(Flood_Date = min(dateF))
flood_stage

# How quickly did changes in flood category occur for each river
action_stage <- floods %>% 
  group_by(names) %>% 
  filter(gheight.ft >= action.ft) %>% 
  summarise(Action_Date = min(dateF))
action_stage

moderate_stage <- floods %>% 
  group_by(names) %>% 
  filter(gheight.ft >= moderate.ft) %>% 
  summarise(Moderate_Date = min(dateF))
moderate_stage

major_stage <- floods %>% 
  group_by(names) %>% 
  filter(gheight.ft >= major.ft) %>% 
  summarise(Major_Date = min(dateF))
major_stage
# Time Differences for withlaoochee
withlacoochie_action_flood_diff <- flood_stage$Flood_Date[1] - action_stage$Action_Date[1] # Time difference between Flood Stage and Action Stage
withlacoochie_moderate_flood_diff <- moderate_stage$Moderate_Date[1] - flood_stage$Flood_Date[1] # Time difference between Moderate Stage and Flood Stage
withlacoochie_major_moderate_diff <- major_stage$Major_Date[1] - moderate_stage$Moderate_Date[1] # Time difference between Major Stage and Moderate Stage



# Time Differences for Fisheating Creek

fisheating_action_flood_diff <-  flood_stage$Flood_Date[2] - action_stage$Action_Date[2]
fisheating_moderate_flood_diff <- moderate_stage$Moderate_Date[2] - flood_stage$Flood_Date[2]
fisheating_major_moderate_diff <- major_stage$Major_Date[2] - moderate_stage$Moderate_Date[2]

# Time Differences for Peace River
peace_action_flood_diff <-  flood_stage$Flood_Date[3] - action_stage$Action_Date[3]
peace_moderate_flood_diff <- moderate_stage$Moderate_Date[3] - flood_stage$Flood_Date[3]
peace_major_moderate_diff <- major_stage$Major_Date[3] - moderate_stage$Moderate_Date[3]

# Question 3 - Which river had the highest stream stage above its listed height in the major flood category?

above <- floods %>% 
  group_by(names) %>% 
  mutate(difference = gheight.ft - major.ft) %>% 
  summarise(Max_Difference = max(difference))
above
# Given the above table, we can conclude that peace river had the highest stream stage above its listed major flood category

