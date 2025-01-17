


##############################
#'  Forelesning 1 
##############################
#' `Objective`: review basic commands from `dplyr`
#' 
#' R makes use of the # or #' signs to add comments, 
#' so that you and others can understand what the R code 
#' is about.

##########################################################
##########################################################
######
#' `dplyr- Basic data manipulation`
#' browseURL("https://r4ds.had.co.nz/transform.html")
#' `filter()` - Pick observations (rows) by their values   
#' `select()` -  Pick variables (columns) by their names  
#' `mutate()` - Create new variables (columns) with functions of existing variables  
#' `arrange()`-  Reorder rows using  
#' `summarise()` - Collapse many values down to a single summary 
#' `rename()`  - Rename columns (variables):
#' `group_by()` - Group rows by columns (variables)
#' 
##########################################################
#############################################################
########


rm (list = ls())
library(tidyverse)

#' In this exercise, we utilize the gapminder dataset 
#' provided by the gapminder R package.

#install.packages("gapminder")
library(gapminder) 

data("gapminder")

head(gapminder)
tail(gapminder)

View(gapminder)

?gapminder

str(gapminder)

# Some Questions:




#' Task 1. From the gapminder dataset, select country, year & pop 

gapminder %>% select(country,year,pop)

# Advanced selection 
gapminder %>% 
  select(starts_with("c")) # select variables that starts with letter c

gapminder %>% 
  select(ends_with(c("try","Exp"))) # select variables that ends with the expression try and Exp


#' Task 2. Filter the data from Norway, Sweden, or Denmark 
#' before and including 1970. And select the variables
#' country, year, gdpPercap. And rename gdpPercap by gdpc

gapminder %>% 
  filter(year >=1970) %>% 
  filter(country %in% c("Norway","Sweden","Denmark")) %>% 
  select(country,year,gdpPercap) %>% 
  rename(gdpc= gdpPercap)


#' Task 3. Following Q2. Arrange rows according to 
#' ascending and descending order of "gdpc" using the function 
#' arrange()  

# Ascending order 
gapminder %>% 
  filter(year >=1970) %>% 
  filter(country %in% c("Norway","Sweden","Denmark")) %>% 
  select(country,year,gdpPercap) %>% 
  rename(gdpc= gdpPercap) %>% 
  arrange(gdpc)

# descending order 
gapminder %>% 
  filter(year >=1970) %>% 
  filter(country %in% c("Norway","Sweden","Denmark")) %>% 
  select(country,year,gdpPercap) %>% 
  rename(gdpc= gdpPercap) %>% 
  arrange(desc(gdpc))

#' Task 4.  Arrange rows according to ascending order 
#' of "gdpc", within each year. 

gapminder %>% 
  filter(year >=1970) %>% 
  filter(country %in% c("Norway","Sweden","Denmark")) %>% 
  select(country,year,gdpPercap) %>% 
  rename(gdpc= gdpPercap) %>% 
  arrange(year,gdpc) 


#' Task 5. Create a new variable that is gdp from gdpPercap & pop and obtain data only from Norway 

gapminder %>% 
  filter(country =="Norway") %>% 
  mutate(gdp = gdpPercap*pop)


#' Task 6. Data from Norway. 
#' Create a new variable called "gdpNOK" that is GDP per per billion NOK (1 000 000 000 NOK) 
#' (1 USD=9 NOK)

gapminder %>% 
  filter(country =="Norway") %>% 
  mutate(gdp = gdpPercap*pop) %>% 
  mutate(gdpNOK = gdp*9/1e9)

#' Task 7. Use mutate and ifelse to Categorise "gdpNOK" into 3 categories,
#'  (i.e., less than or equal to 999, between 1000 and 1999, and greater than 2000).

gapminder %>% 
  filter(country =="Norway") %>% 
  mutate(gdp = gdpPercap*pop) %>% 
  mutate(gdpNOK = gdp*9/1e9) %>% 
  mutate(gdpcat = ifelse(gdpNOK<=999,"less than a billion",
                         ifelse(gdpNOK >=1000 & gdpNOK <=1999,"between a billion and two billions",
                                ifelse(gdpNOK >2000,"larger than two bilions","NA"))))

# or 
gapminder %>% 
  filter(country =="Norway") %>% 
  mutate(gdp = gdpPercap*pop) %>% 
  mutate(gdpNOK = gdp*9/1e9) %>% 
  mutate(gdpcat = ifelse(gdpNOK<=999,"less than a billion",
                         ifelse(between(gdpNOK,1000, 1999),"between a billion and two billions",
                                ifelse(gdpNOK >2000,"larger than two bilions","NA"))))


#' Task 8. Calculate the average lifExp of all three Nordic countries
#' (i.e., Norway, Sweden, Denmark).
#' And assign a name to the calculated mean lifeEXP, call it avlfexp

gapminder %>% 
  filter(year >=1970) %>% 
  filter(country %in% c("Norway","Sweden","Denmark")) %>% 
  summarize(avlfexp = mean(lifeExp, na.rm= TRUE))



#' Task 9 Calculate the average lifExp of the three countries,per country. 
#' (i.e., average per country, name variable )

gapminder %>% 
  filter(year >=1970) %>% 
  filter(country %in% c("Norway","Sweden","Denmark")) %>% 
  group_by(country) %>% 
  summarize(avlfexp = mean(lifeExp, na.rm= TRUE))


# Task: how many unique countries does the gapminder data set contain, by continent?
gapminder %>%
  group_by(continent) %>%
  summarize(n_obs = n(), 
            n_countries = n_distinct(country))



#' Task 10. Calculate mean life expectancy per country 
#' (i.e., for every country in the gapminder data set)

gapminder %>% 
  group_by(country) %>% 
  summarise(avlfexp = mean(lifeExp, na.rm = TRUE))

#' Task 11. Calculate mean life expectancy per continent.
gapminder %>% 
  group_by(continent) %>% 
  summarise(avlfexp = mean(lifeExp, na.rm = TRUE))

#' Task 12. calculate mean life expectancy by continent & country
gapminder %>% 
  group_by(continent,country) %>% 
  summarise(avlfexp = mean(lifeExp, na.rm = TRUE))

#' Task 13. Calculate mean life expectancy by continent & add min and max lifeExp

gapminder %>% 
  group_by(continent,country) %>% 
  summarise(avlfexp = mean(lifeExp, na.rm = TRUE),
            min.lfexp = min(lifeExp),
            max.lfexp = max(lifeExp))


#' Visualization

# Task 14. Scatter plot of gdpPercap vs lifeExp  

gapminder %>% 
  ggplot(aes(x=gdpPercap,y= lifeExp))+
  geom_point(alpha=0.5)+
  ggtitle("Life Expectancy and GDP per capita")

# Task 15. Scatter plot of gdpPercap vs lifeExp by Continent
gapminder %>% 
  ggplot(aes(x=gdpPercap,y= lifeExp, colour = continent, shape = continent))+
  geom_point(alpha=0.5)+
  ggtitle("Life Expectancy and GDP per capita")


#' Task 16. scatter plot per continent, separate graphs for each continent 
gapminder %>% 
  ggplot(aes(x=gdpPercap,y= lifeExp, colour = continent, shape = continent))+
  geom_point(alpha=0.5)+
  facet_wrap(~continent,ncol=3)
ggtitle("Life Expectancy and GDP per capita")



#' Task 17.  histogram plot of lifeExp per continent 

gapminder %>% 
  ggplot(aes(x= lifeExp))+
  geom_histogram(binwidth = 2)+
  facet_wrap(~continent,ncol=3)+
  ggtitle("Life Expectancy by continent")


#' Task 18. Scatter plot of gdpPercap vs lifeExp for the 3 nordic countries

gapminder %>% 
  filter(year >=1970) %>% 
  filter(country %in% c("Norway","Sweden","Denmark")) %>% 
  ggplot(aes(x= gdpPercap, y=lifeExp, colour = country))+
  geom_point(alpha=0.5)

gapminder %>% 
  filter(year >=1970) %>% 
  filter(country %in% c("Norway","Sweden","Denmark")) %>% 
  ggplot(aes(x= gdpPercap, y=lifeExp, colour = country))+
  geom_point(alpha=0.5)+
  geom_text(aes(label = as.character(year),hjust = 0, vjust = 1))+
  geom_line()




#' Data structure

#' the same data can be represented/arranged in multiple ways. 
#' But not all are equally easy to use.
#' It is easy to work with tidy dataset


#' Task 19. Filter the data from Norway, Sweden, or Denmark 
#' after and including 1970. And select the variables
#' country, year, gdpPercap. And rename gdpPercap by gdpc

# long-format 
gapminder %>% 
  filter(country %in% c("Norway","Sweden","Denmark"),
         year >=1970) %>% 
  select(country,year,gdpPercap) %>% 
  rename(gdpc = gdpPercap)
  


#' Task 20. Change the above data to wider format
gapminder %>% 
  filter(country %in% c("Norway","Sweden","Denmark"),
         year >=1970) %>% 
  select(country,year,gdpPercap) %>% 
  rename(gdpc = gdpPercap) %>% 
  pivot_wider(names_from = year, # year is the key variable 
              values_from = gdpc)

gapminder %>% 
  filter(country %in% c("Norway","Sweden","Denmark"),
         year >=1970) %>% 
  select(country,year,gdpPercap) %>% 
  rename(gdpc = gdpPercap) %>% 
  pivot_wider(names_from = country, # country is the key variable 
              values_from = gdpc)
               

#' Task 21: change the data back to long format

gapminder %>% 
  filter(country %in% c("Norway","Sweden","Denmark"),
         year >=1970) %>% 
  select(country,year,gdpPercap) %>% 
  rename(gdpc = gdpPercap) %>% 
  pivot_wider(names_from = country, 
              values_from = gdpc) %>% 
  pivot_longer(-year,names_to = "country",
               values_to = "gdpc")

#' Merging/joining dataframes  

df1 <- gapminder %>% 
  filter(country %in% c("Norway","Sweden","Denmark"),
         year >=1970) %>% 
  select(country,year,gdpPercap) %>% 
  rename(gdpc = gdpPercap)


df2 <- gapminder %>% 
  filter(country %in% c("Norway","Sweden","Denmark"),
         year >=1970) %>% 
  select(country,year,pop) 

df1; df2


#left_join()

left_join(df1,df2) # this works

# however, this is a good practice 
left_join(df1,df2, by = c("country","year"))

#  third data frame 
df3 <- gapminder %>% 
  filter(country %in% c("Norway","Sweden","Denmark"),
         year >=1970) %>% 
  select(country,year,lifeExp) 

df3

# adding the third data frame 
left_join(df1,df2, by = c("country","year")) %>% 
  left_join(df3, by = c("country","year"))


#' Task 22. Find the correlation between lifeExp and gdpPercap for all countries
#' and sort in ascending  order. And also in descending order 

# Ascending order 
gapminder %>%
  group_by(country) %>%
  summarize(correlation = cor(lifeExp, gdpPercap)) %>% 
  arrange(correlation)

# descending order 
gapminder %>%
  group_by(country) %>%
  summarize(correlation = cor(lifeExp, gdpPercap)) %>% 
  arrange(desc(correlation))

#' Task 23:  From the previous task, take the top two and bottom two countries.
#' And plot lifeExp vs gdpPercap per country 

gapminder %>% 
  filter(country %in% c("France", "Austria", "Kuwait","Madagascar")) %>% 
  ggplot(aes(x = lifeExp, y = gdpPercap, colour = country))+
  geom_point(alpha = 0.9)+
  xlab("Life Expectancy")+
  ylab("GDP per capita")+
  ggtitle("Plot of Life Expectancy vs GDP per capita")

# scale matters 
gapminder %>% 
  filter(country %in% c("Madagascar")) %>% 
  ggplot(aes(x = lifeExp, y = gdpPercap, colour = country))+
  geom_point(alpha = 0.9)+
  xlab("Life Expectancy")+
  ylab("GDP per capita")+
  ggtitle("Plot of Life Expectancy vs GDP per capita")


#' Task 24: calculate the percentage and logaritmic growth of gdp for Norway 

gapminder %>% 
  filter(country %in% c("Norway")) %>%
  rename(gdp = gdpPercap) %>% 
  mutate(perc.diff = 100*(gdp-lag(gdp))/lag(gdp),
         log.diff = 100*c(NA, diff(log(gdp))))

# using summarize function in stead of mutate ()
gapminder %>% 
  filter(country %in% c("Norway")) %>%
  rename(gdp = gdpPercap) %>% 
  summarise(perc.diff = 100*(gdp-lag(gdp))/lag(gdp),
         log.diff = 100*c(NA, diff(log(gdp))))
  
  

#' Task 25: calculate the logaritmic growth in gdp for all countries

gapminder %>% 
  select(country, year, gdpPercap) %>% 
  rename(gdp = gdpPercap) %>% 
  group_by(country) %>% 
  mutate(perc.diff = 100*(gdp-lag(gdp))/lag(gdp),
         log.diff = 100*c(NA, diff(log(gdp))))
  
#' Task 26: what country has the highest and lowest average logaritmic growth in gdp?  

#' lowest
gapminder %>% 
  select(country, year, gdpPercap) %>% 
  rename(gdp = gdpPercap) %>% 
  group_by(country) %>% 
  mutate(perc.diff = 100*(gdp-lag(gdp))/lag(gdp),
         log.diff = 100*c(NA, diff(log(gdp)))) %>% 
  
  group_by(country) %>% 
  summarise(mean.gdp = mean(log.diff, na.rm = TRUE)) %>% 
  arrange(mean.gdp)
  


#' highest

gapminder %>% 
  select(country, year, gdpPercap) %>% 
  rename(gdp = gdpPercap) %>% 
  group_by(country) %>% 
  mutate(perc.diff = 100*(gdp-lag(gdp))/lag(gdp),
         log.diff = 100*c(NA, diff(log(gdp)))) %>% 
  
  group_by(country) %>% 
  summarise(mean.gdp = mean(log.diff, na.rm = TRUE)) %>% 
  arrange(desc(mean.gdp))


# desnity plot 

gapminder %>% 
  select(continent, country,year,gdpPercap) %>% 
  rename(gdp = gdpPercap) %>% 
  group_by(country) %>% 
  mutate(log.diff = 100*c(NA, diff(log(gdp)))) %>% 
  ggplot()+
  geom_density(aes(x=log.diff, group = continent, fill = continent, alpha = 0.3))+
  ggtitle("Average GDP growth (log) by continent")+
  xlim(c(-25,50))+
  xlab("% average growth in GDP")
  









