

rm(list = ls())
library(tidyverse)

# Read ttidyverse# Read the data directly from the URL
df_lower <- read_table2( "https://www.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt")


head(df_lower)
tail(df_lower)

View(df_lower)

# subsetting on a data frame on rows

#df_lower[rows, columns]

df_lower[1:550,] %>% View()

df_lower$Year

# comments starts on $Year equal to "Year", write code that is dynamic 
which(df_lower$Year %in% "Year")

# dynamic sub-setting 
df_lower <- df_lower[1:which(df_lower$Year %in% "Year")-1,] 

head(df_lower)
tail(df_lower)

# work with the first 5 columns 

names(df_lower) # the names of variables in the dataframe 

df_lower %>% select(Year, Mo,Globe,Land,Ocean) %>% head()
df_lower %>% select(Year, Mo,Globe,Land,Ocean) %>% tail()


# create a Date variable, lubridate 
library(lubridate)

df_lower %>% 
  select(Year,Mo,Globe,Land,Ocean) %>% 
  mutate(Date = ymd(paste(Year,Mo,1, sep="-")))









