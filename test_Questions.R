
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



# Some Questions:

#' Task 1. From the gapminder dataset, select country, year & pop 





#' Task 2. Filter the data from Norway, Sweden, or Denmark 
#' before and including 1970. And select the variables
#' country, year, gdpPercap. And rename gdpPercap by gdpc



#' Task 3. Following Q2. Arrange rows according to 
#' ascending and descending order of "gdpc" using the function 
#' arrange()  



#' Task 4.  Arrange rows according to ascending order 
#' of "gdpc", within each year. 




#' Task 5. Create a new variable that is gdp from gdpPercap & pop and obtain data only from Norway 


#' Task 6. Data from Norway. 
#' Create a new variable called "gdpNOK" that is GDP per per billion NOK (1 000 000 000 NOK) 
#' (1 USD=9 NOK)



#' Task 7. Use mutate and ifelse to Categorise "gdpNOK" into 3 categories,
#'  (i.e., less than or equal to 999, between 1000 and 1999, and greater than 2000).




#' Task 8. Calculate the average lifExp of all three Nordic countries
#' (i.e., Norway, Sweden, Denmark).
#' And assign a name to the calculated mean lifeEXP, call it avlfexp




#' Task 9 Calculate the average lifExp of the three countries,per country. 
#' (i.e., average per country, name variable )


#' Task 10. Calculate mean life expectancy per country 
#' (i.e., for every country in the gapminder data set)


#' Task 11. Calculate mean life expectancy per continent.


#' Task 12. calculate mean life expectancy by continent & country


#' Task 13. Calculate mean life expectancy by continent & add min and max lifeExp



#' Visualization

# Task 14. Scatter plot of gdpPercap vs lifeExp  



# Task 15. Scatter plot of gdpPercap vs lifeExp by Continent


#' Task 16. scatter plot per continent, separate graphs for each continent 



#' Task 17.  histogram plot of lifeExp per continent 


#' Task 18. Scatter plot of gdpPercap vs lifeExp for the 3 nordic countries





#' Data structure

#' the same data can be represented/arranged in multiple ways. 
#' But not all are equally easy to use.
#' It is easy to work with tidy dataset


#' Task 19. Filter the data from Norway, Sweden, or Denmark 
#' before and including 1970. And select the variables
#' country, year, gdpPercap. And rename gdpPercap by gdpc


#' Task 20. Change the above data to wider format


#' Task 21: chnage the data back to long format


#' Merging/joining dataframes  






#' Task 22. Find the correlation between lifeExp and gdpPercap for all countries
#' and sort in ascending  order. And also in descending order 



#' Task 23:  From the previous task, take the top two and bottom two countries.
#' And plot lifeExp vs gdpPercap per country 



#' Task 24: calculate the percentage and logaritmic growth of gdp for Norway 


#' Task 25: calculate the logaritmic growth in gdp for all countries


#' Task 26: what country has the highest and lowest average logaritmic growth in gdp?  

#' lowest


#' highest
