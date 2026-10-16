#PSYC 259 Homework 3 - Data Transformation
#For full credit, provide answers for at least 7/11

#List names of students collaborating with: 

### SETUP: RUN THIS BEFORE STARTING ----------

#Load packages
library(tidyverse)
ds <- read_csv("data_raw/rolling_stone_500.csv")

### Question 1 ---------- 

#Use a function to check the class of all of the variables in 'ds' 
#Convert "Year" to a numeric, saving it back to 'ds'
#Use a function to check that your conversion succeeded

#ANSWER
glimpse(ds)
ds$Year <- as.numeric(ds$Year)
glimpse(ds)

### Question 2 ---------- 

# Using a dplyr function,
# change ds so that all of the variables are lowercase

#ANSWER
ds <- ds %>% rename_with(tolower)
glimpse(ds)

### Question 3 ----------

# Use mutate to create a new variable in ds that has the decade of the year as a number
# For example, 1971 would become 1970, 2001 would become 2000
# Hint: read the documentation for ?floor and make sure year is numeric (from Question 1)

#ANSWER
?floor

ds <- ds %>% mutate(d_year = floor(year/10)) %>%
  mutate(d_year = d_year*10)
print(ds)
glimpse(ds)

### Question 4 ----------

# Sort the dataset by rank so that 1 is at the top

#ANSWER
ds <- ds %>% arrange(ds, rank)
ds

### Question 5 ----------

# Use filter and select to create a new tibble called 'top10'
# That just has the artists and songs for the top 10 songs

#ANSWER

top10 <- filter(ds, rank >0, rank <11)
glimpse(top10)

### Question 6 ----------

# Use summarize to find the earliest, most recent, and average release year
# and save the results to a new tibble called "ds_sum"
# Hint, the function drop_na() might help. Read ?drop_na

#ANSWER

ds_sum <- drop_na(ds, year)
ds_sum <- ds %>% summarize(min_year = min(year, na.rm = T),
                 max_year = max(year, na.rm = T),
                 m_year = mean(year, na.rm = T))
ds_sum$m_year <- round(ds_sum$m_year)
                 

### Question 7 ----------

# Use filter on the original dataset 'ds' to find all of the artists/song titles from 
# the min/max/mean years that you identified in 'ds_sum'
# Use one filter command only, and sort the responses by year
# Bonus: find a way to do it without hard coding the years (i.e. pull them from ds_sum)
# (this might take some searching to find functions that will help)

#ANSWER
ds %>% filter(year %in% c(ds_sum$min_year,
                          ds_sum$max_year,
                          ds_sum$m_year)) %>% 
  arrange(year)


### Question 8 ---------- 

# There's an error here. The oldest song "Brass in Pocket"
# is from 1979! Use mutate and ifelse to fix the error in 'ds'
# then recalculate decade, and then
# recalculate the responses from Questions 6-7 to
# find the correct oldest, averag-ist, and most recent songs

#ANSWER
#correcting the song
ds <- ds %>% 
  mutate(year = ifelse(song == "Brass in Pocket", 1979, year))
ds %>% filter(song == "Brass in Pocket") #this is just me confirming it worked
#readding decade
ds <- ds %>% 
  mutate(d_year = floor(year/10)) %>%
  mutate(d_year = d_year*10)
glimpse(ds)
ds %>% filter(song == "Brass in Pocket") #again checking it's right
#recalculating min max and mean
ds_sum <- drop_na(ds, year)
ds_sum <- ds %>% summarize(min_year = min(year, na.rm = T),
                           max_year = max(year, na.rm = T),
                           m_year = mean(year, na.rm = T))
ds_sum$m_year <- round(ds_sum$m_year)
glimpse(ds_sum)

#refiltering
ds %>% filter(year %in% c(ds_sum$min_year,
                          ds_sum$max_year,
                          ds_sum$m_year)) %>% 
  arrange(year)

### Question 9 ---------

# Use group_by and summarize to find the average rank and 
# number of songs on the list by decade. To make things easier
# filter out the NA values from decade before summarizing
# You don't need to save the results anywhere
# Use the pipe %>% to string the commands together

#ANSWER
ds %>% 
  drop_na(d_year) %>% 
  group_by(d_year) %>% 
  summarize(m_rank = mean(rank, na.rm = T),
            n_songs = n())

### Question 10 --------

# Look up the dplyr "count" function
# Use it to count up the number of songs by decade
# Then use slice_max() to pull the row with the most songs
# Use the pipe %>% to string the commands together
# Check against your answers from Question 9 to make sure it matches

#ANSWER
?count
ds %>% 
  drop_na(d_year) %>% 
  count(d_year) %>%
  slice_max(n)
### Question 11 ------
# Use RStudio's styler to style all the code in your homework
install.packages("styler")
library(styler)
