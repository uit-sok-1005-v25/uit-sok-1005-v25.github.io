
# Global Temperature

rm(list=ls())

library(tidyverse)

df_lower <- read_table2("https://www.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt")
# We would like remove the comments in the bottom

head(df_lower)
tail(df_lower, 20)

# Subsetting on a data frame on rows 
# df_lower[rows, columns]

View(df_lower)
df_lower[1:550,] 
# subset data on rows, correct but not dynamic!

# comments starts on $Year equal to "Year", write code that is dynamic
df_lower$Year
which(df_lower$Year %in% "Year")

# dynamic select
df_lower <- df_lower[1:which(df_lower$Year %in% "Year")-1, ]

# This works!
tail(df_lower)

# Work with the first 5 columns, 
# Selecting on columns using tidyverse, pipe to console
df_lower %>% select(Year, Mo, Globe, Land, Ocean) %>% head()
df_lower %>% select(Year, Mo, Globe, Land, Ocean) %>% tail()

# Create a Date variable, lubridate
library(lubridate)

df_lower %>% 
  select(Year, Mo, Globe, Land, Ocean) %>% 
  mutate(Date = ymd(paste(Year, Mo, 1, sep="-")))

# Selecting on columns and save 
df_tidy <- df_lower %>% 
  mutate(Date = ymd(paste(Year, Mo, 1, sep="-"))) %>% 
  select(Date, Globe, Land, Ocean) 

df_tidy

df_tidy %>% 
  mutate(Year = year(Date), # extract year from the date variable 
         Month = month(Date), # extract numeric month from date variable 
         Month2 = month(Date, label = TRUE, abbr = FALSE))# extract text month from date

# update
df_tidy <- df_tidy %>% mutate(Year = year(Date),
                              Month = month(Date),
                              Month2 = month(Date, label = TRUE, abbr = FALSE))

df_tidy <- df_tidy %>%
  select(Year, Month, Date, Globe, Land, Ocean)

df_tidy  

# looking at the structure of the data frame/ directly from the tibble
str(df_tidy)

# chr? mean characters or letters

# turning a character into a numeric,  many variables at the same time
df_tidy %>%
  mutate_at(vars(Globe, Land, Ocean), ~as.numeric(.))

# Alternatively 
df_tidy %>%
  mutate_at(vars(Globe, Land, Ocean), as.numeric)

# Even more general, using mutate_if() 
df_tidy %>%
  mutate_if(is.character, as.numeric)# note generic is.character means all character in data

# update 
df_tidy <- df_tidy %>%
  mutate_if(is.character, as.numeric)

str(df_tidy)


# All data wrangling done above in one sweep
df_lower %>%
  select(Year, Mo, Globe, Land, Ocean) %>% 
  mutate(Date = ymd(paste(Year, Mo, 1, sep="-"))) %>% 
  mutate(Year = year(Date), 
         Month = month(Date)) %>%
  select(Year, Month, Date, Globe, Land, Ocean) %>%  
  mutate_if(is.character, as.numeric) 


# Task: Calculate the average  globe temperature per year 

# data manipulation, select on rows dplyr::filter()
df_tidy %>% filter(Year > 1978)
df_tidy %>% filter(Year >= 1979)
df_tidy %>% filter(Year != 1978)

# summarise using group_by
df_tidy %>% filter(Year != 1978) %>% 
  group_by(Year) %>% 
  summarise(Average.Temp=mean(Globe))


# use mutate fun instead of summarise 
df_tidy %>% filter(Year != 1978) %>%
  group_by(Year) %>% 
  mutate(Average.Temp=mean(Globe)) %>%
  ggplot(aes(x=Date, y=Globe)) + 
  geom_line() +
  geom_line(aes(x=Date, y=Average.Temp), col="blue")



# Task: calculating the 13-month (centered) moving average and plot 

# help
?zoo::rollmean

df_tidy %>% filter(Year != 1978) %>%
  group_by(Year) %>% 
  mutate(Average.Temp=mean(Globe)) %>% 
  ggplot(aes(x=Date, y=Globe)) + 
  geom_line() +
  geom_line(aes(y=zoo::rollmean(Globe, 13, fill=NA)), col="red", size=1.3) 

library(zoo)
df_tidy %>% 
  filter(Year != 1978) %>%
  group_by(Year) %>% 
  mutate(Average.Temp=mean(Globe)) %>% 
  ggplot(aes(x=Date, y=Globe)) + 
  geom_line() +
  geom_line(aes(y=rollmean(Globe, 13, fill=NA)), col="red", size=1.3) 


# change color of line, add data points
df_tidy %>% 
  filter(Year != 1978) %>%
  group_by(Year) %>%  
  ggplot(aes(x=Date, y=Globe)) + 
  geom_line(col="lightblue") + 
  geom_point(col="blue") +
  geom_line(aes(y=zoo::rollmean(Globe, 12, fill=NA)), col="red", size=1.3) 

# add labels, title
df_tidy %>% filter(Year != 1978) %>% 
  group_by(Year) %>% 
  ggplot(aes(x=Date, y=Globe)) + 
  geom_line(col="lightblue") +
  geom_point(col="lightblue") +
  geom_line(aes(y=zoo::rollmean(Globe, 13, fill=NA)), col="red", size=1.3) +
  ggtitle("Plot of temperature over time") +
  ylab("Global Temperature") + 
  xlab("Time") 

# make a plot into an object
p1 <- df_tidy %>% 
  filter(Year != 1978) %>% 
  group_by(Year) %>% 
  ggplot(aes(x=Date, y=Globe)) + 
  geom_line(col="lightblue") +
  geom_point(col="lightblue") +
  geom_line(aes(y=zoo::rollmean(Globe, 13, fill=NA)), col="red", size=1.3)

p1

p1 + ggtitle("Plot of temperature over time") +
  ylab("Global Temperature") +
  xlab("Time") 

# One plot per year
df_tidy %>%
  filter(Year != 1978) %>% 
  group_by(Year) %>% 
  ggplot(aes(x=Month, y=Globe)) +
  scale_x_continuous(breaks=seq(1,12,1)) + 
  geom_line(col="lightblue") + 
  geom_point(col="lightblue") +
  ggtitle("Plot of temperature over time") +
  ylab("Global Temperature") + xlab("Time") + 
  facet_wrap(~Year)



# Keep the number of objects/data frames and variables under control!

# Monthly, one line per year, note that Year a numeric,
#has to be a factor, in order to make one line per year
df_tidy %>% 
  filter(Year != 1978) %>% 
  ggplot(aes(x=Month, y=Globe)) +
  scale_x_continuous(breaks=seq(1,12,1)) + 
  geom_line(aes(colour=as.factor(Year))) + #note here as.
  ggtitle("Plot of temperature over time") +
  ylab("Global Temperature") + xlab("Month") 

df_tidy %>% 
  filter(Year != 1978) %>% 
  ggplot(aes(x=Month, y=Globe)) +
  scale_x_continuous(breaks=seq(1,12,1)) + 
  geom_line(aes(colour=Year)) +
  ggtitle("Plot of temperature over time") +
  ylab("Global Temperature") + xlab("Month")

# Task: How do we remove as.factor(Year) into Year?
df_tidy %>%
  filter(Year != 1978) %>% 
  mutate(Year=as.factor(Year)) %>%  # this solves the problem 
  ggplot(aes(x=Month, y=Globe)) +
  scale_x_continuous(breaks=seq(1,12,1)) + 
  geom_line(aes(colour=Year)) +
  ggtitle("Plot of temperature over time") +
  ylab("Global Temperature") + xlab("Month") 


# Task: Create a plot showing temperatures for the globe, land, 
# and ocean combined in a single visualization. 

# Note no "labels"
df_tidy %>%
  filter(Year != 1978) %>%
  ggplot(aes(x=Date, y=Globe)) + 
  geom_line(col="blue") +
  geom_line(aes(x=Date, y=Land), col="red") +
  geom_line(aes(x=Date, y=Ocean), col="orange")


# Wide data to long data
df_tidy # wide
df_tidy %>% 
  filter(Year != 1978) %>% 
  select(Date, Globe, Land, Ocean) %>% 
  pivot_longer(-Date, names_to = "Location", values_to = "Temperature") # long

# same as above, but with labels on location
df_tidy %>% filter(Year != 1978) %>% 
  select(Date, Globe, Land, Ocean) %>% 
  pivot_longer(-Date, names_to = "Location", values_to = "Temperature") %>% 
  ggplot(aes(x=Date, y=Temperature, group=Location)) + 
  geom_line(aes(color=Location)) 


# Task: Make one "long" plot of the averages per year, add a trend line (regression)
df_tidy %>% 
  filter(Year != 1978) %>%
  group_by(Year) %>% 
  summarise(Globe=mean(Globe)) %>% 
  ggplot(aes(x=Year, y=Globe)) +
  geom_line() +
  ggtitle("Plot of temperature over time") +
  ylab("Global Temperature") + xlab("Year") +
  geom_smooth(method = lm, se = FALSE) # note the geom_smooth()


# Some data analysis 
library(broom)

df_tidy %>%
  filter(Year != 1978) %>%
  group_by(Year) %>% 
  summarise(Globe=mean(Globe)) %>%
  ungroup() %>% 
  do(model = lm(Globe ~ Year, data = .)) -> fit

fit <- tidy(fit$model[[1]])

fit

0.2/fit$estimate[2] # years for average global temperature to increase 0.2 degree C


# Data Science?
el.nino.years <- c(1982,1983,1986,1987,1991,1992,1993,1994,1997,1998,2002,2003,2006,2007,2015,2016)

df_tidy %>% 
  filter(Year != 1978) %>%
  group_by(Year) %>% 
  summarise(Globe=mean(Globe)) %>%
  ungroup() %>% 
  ggplot(aes(x=Year, y=Globe)) + 
  geom_line(col="blue") +
  geom_vline(xintercept = as.numeric(el.nino.years), linetype=4) +
  ggtitle("Yearly global temperature and el nino years")

# Replicate the analysis above with the yearly sea temperature?

df_tidy %>% filter(Year != 1978) %>%
  group_by(Year) %>% 
  summarise(Ocean=mean(Ocean)) %>%
  ungroup() %>% 
  ggplot(aes(x=Year, y=Ocean)) +
  geom_line(col="red") +
  geom_vline(xintercept = as.numeric(el.nino.years), linetype=4) +
  ggtitle("Yearly ocean temperature and el nino years")
