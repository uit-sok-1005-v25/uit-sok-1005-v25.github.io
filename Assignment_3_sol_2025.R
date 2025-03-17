

# TAsk 1

rm(list=ls())
library(tidyverse)

# Download the excel data from the link below and save in your GitHUb repository and load to R studio 
# https://www.ifw-kiel.de/publications/ukraine-support-tracker-data-20758/
  

Ukrain <- read_excel("C:/Users/dki007/OneDrive - UiT Office 365/V2025_folder/V2025_Vår_semister/SOK-1005_2025/Ukrain.xlsx")
#View(Ukrain)

Ukrain


df_long <- Ukrain %>%
  pivot_longer(cols = c(Financial, Humanitarian, Military), 
               names_to = "Category", 
               values_to = "Contribution")
head(df_long)


# Create the stacked bar plot
df_long %>%
  mutate(Country = fct_reorder(Country, Contribution, .fun = sum)) %>%
  ggplot(aes(x = Country, y = Contribution, fill = Category)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip coordinates to have countries on the y-axis
  theme_minimal() +
  labs(title = "Contributions by Country", x = "Country", y = "Contribution") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#https://www.ifw-kiel.de/topics/war-against-ukraine/ukraine-support-tracker/
#https://www.ifw-kiel.de/publications/the-ukraine-support-tracker-which-countries-help-ukraine-and-how-20852/



# TASK 2

# Try the following exercise, we will work them together in class tomorrow.
# The exercise is based on superstore sales data in different province of Canada.
#' 
#' From reading list:
#' Chiu, David
#' R for data science cookbook:
#' over 100 hands-on recipes to effectively solve real-world data problems using
#' the most popular R packages and techniques


rm(list=ls())

library(tidyverse)
library(lubridate)
library(janitor)


superstore <- read_csv("https://github.com/ywchiu/rcookbook/raw/master/chapter7/superstore_sales.csv", 
                       col_types = cols(`Order Date` = col_date(format = "%Y/%m/%d")))



#' Task 0: Do Exploratory Data Analysis first to understand the data frame 

str(superstore)
names(superstore)

superstore$`Order ID`

#' fix names
superstore <- superstore %>% clean_names()

names(superstore)

#
#https://rpubs.com/Mahmoud_hafez/sales_analysis

#Q. Find the best month for sales, and visualize using  bar plot  
superstore %>%
  select(sales, province, order_date) %>% 
  mutate(month = month(order_date,label = TRUE)) %>%
  group_by(month) %>%  
  summarise(total_sales = sum(sales)) %>% 
  ggplot(aes(x=month,y=total_sales,fill = month))+
  geom_bar(stat = "identity") +
  labs(title = "Best Month for Sales",
       x = "Month",
       y = "Total sales")+
  scale_y_continuous(labels = scales::comma)  # Use comma separator for large numbers


#Q2: Find the province with the highest number of sales, and visualize using bar chart 

superstore %>%
  select(sales, province, order_date) %>% 
  group_by(province) %>%  
  summarise(total_sales = sum(sales)) %>% 
  
  
  ggplot(aes(x = reorder(province, total_sales), y = total_sales, fill = province)) +
  geom_bar(stat = "identity") +
  labs(title = "Province with Highest Sales",
       x = "province",
       y = "Total Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(labels = scales::comma)  # Use comma separator for large numbers


# Q. In each province, What product category sold the most? Visualize most sold product category
#  using bar chart 
superstore %>%
  group_by(product_category,province) %>%
  summarize(TotalQuantity = sum(order_quantity)) %>%
  arrange(desc(TotalQuantity)) %>% 
  ggplot(aes(x=product_category,y=TotalQuantity, fill = province))+
  geom_bar(stat = "identity")+
  facet_wrap(~province)







# TASK 3 

rm(list=ls())

library(tidyverse)
library(gapminder)

# Use the gapminder data-set from the gapminder package
data(gapminder)


#------------------------------------
# How many variables are factors?
str(gapminder)
glimpse(gapminder)

# And how many levels has each factor?
gapminder %>% select(country) %>% unique()
gapminder %>% select(continent) %>% unique()

levels(gapminder$country)
fct_count(gapminder$country)

# or 
gapminder %>% count(country)
gapminder %>% count(continent)

# or 
gapminder %>% group_by(continent) %>% tally()

# or as a bar plot of counts
gapminder %>% ggplot(aes(continent)) + geom_bar()

# number of observations and countries by continent
gapminder %>% 
  group_by(continent) %>% 
  summarize(nobs = n(),
            no_countries = n_distinct(country))

#-------------------------------------


# Q0
# select 1952 and 2007 then
# group by the 5 continents and the 2 years,
# calculate mean and median for lifeExp and gdpPercap

gapminder %>% 
  filter(year %in% c(1952,2007)) %>% 
  group_by(continent) %>% 
  summarise_at(vars(lifeExp, gdpPercap), list(mean, median)) %>% 
  rename(lifeExp_Mean=lifeExp_fn1,
         gdpPercap_Mean=gdpPercap_fn1,
         lifeExp_Median=lifeExp_fn2,
         gdpPercap_Median=gdpPercap_fn2)

# a).

# in Asia, for each year, what is the min and max lifeExp?

gapminder %>%
  filter(continent=="Asia") %>% 
  group_by(year) %>% 
  summarise(min_lifeExp=min(lifeExp),
            max_lifeExp=max(lifeExp))

# b


# measure percentage changes as log differentials between years,
# what has grown most, the min or max lifeExp in Asia?

gapminder %>%
  filter(continent=="Asia") %>% 
  group_by(year) %>% 
  summarise(min_lifeExp=min(lifeExp), 
            max_lifeExp=max(lifeExp)) %>%
  
  mutate(min_log_diff = c(NA, diff(log(min_lifeExp))),
         max_log_diff = c(NA, diff(log(max_lifeExp))),
         
         comp_min = c(1, 1+cumsum(na.omit(min_log_diff))),
         comp_max = c(1, 1+cumsum(na.omit(max_log_diff)))) %>% 
  select(year, comp_min, comp_max)

# c


# for each country, find the average lifeExp and gdp
# make a point plot of each variable (x) against country (y)
# also order the point plot above from small to large (x), against country (y)

gapminder %>% 
  group_by(country) %>% 
  summarise(lifeExp=mean(lifeExp, na.rm = TRUE),
            gdp=mean(gdpPercap, na.rm = TRUE)) %>% 
  ggplot(aes(x=lifeExp, y=country)) + geom_point()

gapminder %>% 
  group_by(country) %>% 
  summarise(lifeExp=mean(lifeExp, na.rm = TRUE),
            gdp=mean(gdpPercap, na.rm = TRUE)) %>% 
  
  mutate(country=fct_reorder(country, lifeExp)) %>% 
  ggplot(aes(x=lifeExp, y=country)) + geom_point()


gapminder %>% 
  group_by(country) %>% 
  summarise(lifeExp=mean(lifeExp, na.rm = TRUE),
            gdp=mean(gdpPercap, na.rm = TRUE)) %>% 
  mutate(country=fct_reorder(country, desc(lifeExp))) %>% 
  ggplot(aes(x=lifeExp, y=country)) + geom_point()


gapminder %>% 
  group_by(country) %>% 
  summarise(lifeExp=mean(lifeExp, na.rm = TRUE),
            gdp=mean(gdpPercap, na.rm = TRUE)) %>% 
  ggplot(aes(x=lifeExp, y=fct_reorder(country, lifeExp))) + geom_point()


#

gapminder %>% 
  group_by(country) %>% 
  summarise(lifeExp=mean(lifeExp, na.rm = TRUE),
            gdp=mean(gdpPercap, na.rm = TRUE)) %>% 
  
  ggplot(aes(x=gdp, y=country)) + geom_point()

gapminder %>% 
  group_by(country) %>% 
  summarise(lifeExp=mean(lifeExp, na.rm = TRUE),
            gdp=mean(gdpPercap, na.rm = TRUE)) %>% 
  
  mutate(country=fct_reorder(country, gdp)) %>% 
  ggplot(aes(x=gdp, y=country)) + geom_point()


gapminder %>% 
  group_by(country) %>% 
  summarise(lifeExp=mean(lifeExp, na.rm = TRUE),
            gdp=mean(gdpPercap, na.rm = TRUE)) %>% 
  mutate(country=fct_reorder(country, desc(gdp))) %>% 
  ggplot(aes(x=gdp, y=country)) + geom_point()


gapminder %>% 
  group_by(country) %>% 
  summarise(lifeExp=mean(lifeExp, na.rm = TRUE),
            gdp=mean(gdpPercap, na.rm = TRUE)) %>% 
  ggplot(aes(x=gdp, y=fct_reorder(country, gdp))) + geom_point()


# d.


# for each country, find the average lifeExp and gdp
# make a point plot of the average lifeExp (y) against gdp (x)

gapminder %>% 
  group_by(country) %>% 
  summarise(lifeExp=mean(lifeExp, na.rm = TRUE),
            gdp=mean(gdpPercap, na.rm = TRUE)) %>% 
  ggplot(aes(x=gdp, y=lifeExp)) + geom_point()

library(ggrepel)

gapminder %>% 
  group_by(country) %>% 
  summarise(lifeExp=mean(lifeExp, na.rm = TRUE),
            gdp=mean(gdpPercap, na.rm = TRUE)) %>% 
  ggplot(aes(x=gdp, y=lifeExp, label=country)) + geom_point(col="red") +
  geom_text_repel()


gapminder %>% 
  group_by(country) %>% 
  summarise(lifeExp=mean(lifeExp, na.rm = TRUE),
            gdp=mean(gdpPercap, na.rm = TRUE),
            continent=unique(continent)) %>% 
  ggplot(aes(x=gdp, y=lifeExp, color=continent)) + geom_point()


# change the scale from levels to logs
gapminder %>% 
  group_by(country) %>% 
  summarise(lifeExp=mean(lifeExp, na.rm = TRUE),
            gdp=mean(gdpPercap, na.rm = TRUE),
            continent=unique(continent)) %>% 
  ggplot(aes(x=log(gdp), y=log(lifeExp), color=continent)) + geom_point()


library(broom)

# log-log regression
gapminder %>% 
  group_by(country) %>% 
  summarise(lifeExp=mean(lifeExp, na.rm = TRUE),
            gdp=mean(gdpPercap, na.rm = TRUE),
            continent=unique(continent)) %>% 
  do(tidy(lm(log(lifeExp)~log(gdp), data = .)))

# when gdp increases by 10%, lifeExp goes up by 1.39%


# e)

# for each country, calculate the life expectancy gain in years,
# i.e., each (5) year - first year [1952]

gapminder %>% 
  group_by(country) %>% 
  select(country, year, lifeExp) %>% 
  mutate(lifeExp_gain = lifeExp - first(lifeExp))


# within a continent, what country experienced the sharpest 5-year drop in life expectancy?
# within a country, take (lifeExp in year t) - (lifeExp in year t-1)
# positive means lifeExp went up, negative means it went down

gapminder %>% 
  select(country, continent, year, lifeExp) %>% 
  group_by(country, continent) %>% 
  mutate(le_delta = lifeExp - lag(lifeExp)) %>%
  
  # within a country, keep the row with the smallest lifeExp change (smallest = most negative)
  summarise(worst_le_delta= min(le_delta, na.rm = TRUE)) %>% 
  # within a continent keep the row with the lowest value
  top_n(-1, wt=worst_le_delta) %>% 
  arrange(worst_le_delta)


gapminder %>% 
  select(country, year, lifeExp) %>% 
  filter(country=="Rwanda")


