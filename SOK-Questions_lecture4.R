
# Try the following exercises, we will work them together in the class tomorrow.
# The exercise is based on superstore sales data in different provinces of Canada.
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


# Task 1. For each province, calculate the total monthly sales per year 




#' Task 2: Following task 1, change the data structure from long to wide



#' Task 3: Make a line plot of the monthly total sales in Alberta and Youkon in 2010 and 2011




# Task 4: Change the line type to "dashed" line




# Task 5: In the plot above, increase the number of grids on the x axis (date)?
 # compact date format
#browseURL("https://www.r-bloggers.com/2013/08/date-formats-in-r/")



# Task 6. Following the last task above. Rotate the labels on x axis?



#' Task 7: Instead of the line plot above, create stacked bar chart, why?



# Task 8: Create a scatter plot instead  




# Task 9: facet_wrap() the scatter plots above 





# Task 10: Put/arrange the scatter plot and the line plot in the same window
# side by side. 
library(grid)
grid.newpage()






#' Task 11: Find weekday of date from the superstore data frame 




#' Task 12: Find average sales by weekday, but exclude Saturdays and Sundays.





# Task 13: Find the average sales on Saturdays and Sundays





#' Task 14: Find average sales by weekday and Customer Segment. Make one column per Customer Segment.




#' Task 15: Your boss loves excel (sigh), and would like a spreadsheet with the weekdays and segments report in it.
#' use a relevant folder on your computer to save as excel file.
library(openxlsx)





#' Task 16: Use the segments data frame and calculate the market share per customer segment and day




#' Task 17: Find average profit per customer segment and product category in 2011, for all provinces except
#' Nunavut, Newfoundland and Manitoba.
#' What segment produced the highest profit?





#' A bit more advanced


#' Task 18: What customer segment and product category has the highest correlation between
#' unit price and order_quantity?


