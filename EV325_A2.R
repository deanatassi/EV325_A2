
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


################################################################################











