

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

str(superstore)
head(superstore)

#' Exploratory Data Analysis
#install.packages("DataExplorer") 
library(DataExplorer)

plot_str(superstore)
plot_missing(superstore)
plot_histogram(superstore)
plot_density(superstore)
plot_correlation(superstore, type = 'continuous')

# For character/factors
plot_bar(superstore)  

length(unique(superstore$order_date))
#Finds all unique order dates (removes duplicates).Counts the number of unique order dates.
min(superstore$order_date)
max(superstore$order_date)

length(unique(floor_date(superstore$order_date, unit = "year")))
#The function floor_date() in R (from the lubridate package) 
#rounds down a date or datetime to the nearest specified time unit (e.g., day, month, year).
length(unique(floor_date(superstore$order_date, unit = "month")))
4*12
length(unique(superstore$province))
13*48 # max

# Task1. For each province, calculate the total monthly sales per year 
#' All days in a month set to 1 
superstore %>%
  select(sales, province, order_date) %>% 
  mutate(year_month = floor_date(order_date, unit = "month")) %>%
  group_by(year_month, province) %>%  
  summarise(total_sales = sum(sales)) 

#' Task 2: Following task 1, change the data structure from long to wide
superstore %>% 
  select(sales, province, order_date) %>%
  mutate(year_month = floor_date(order_date, unit = "month")) %>%
  group_by(year_month, province) %>%  
  summarise(total_sales = sum(sales)) %>% 
  pivot_wider(names_from = province, values_from = total_sales) %>% 
  clean_names()


#' Task 3:  Make a line plot of the monthly total sales in Alberta and Youkon in 2010 and 2011
superstore %>%
  select(sales, province, order_date) %>% 
  mutate(year_month = floor_date(order_date, unit = "month")) %>%
  group_by(year_month, province) %>% 
  summarise(total_sales = sum(sales)) %>%
  filter(province %in% c("Alberta","Yukon")) %>% 
  mutate(year = year(year_month)) %>%
  filter(year %in% c(2010, 2011)) %>%
  ggplot(., aes(x=year_month, y=total_sales, color=province)) +
  geom_line() +
  xlab("year") + ylab("Total Sales Amount") + 
  ggtitle("Monthly Total Sale Amount By province") 


# Task 4: Change the line type to "dashed" line
p <- 
  superstore %>%
  select(sales, province, order_date) %>% 
  mutate(year_month = floor_date(order_date, unit = "month")) %>%
  group_by(year_month, province) %>% 
  summarise(total_sales = sum(sales)) %>%
  filter(province %in% c("Alberta","Yukon")) %>% 
  mutate(year = year(year_month)) %>%
  filter(year %in% c(2010, 2011)) %>%
  ggplot(., aes(x=year_month, y=total_sales, color=province))

p  # blank canvas

p + geom_line(linetype="dashed", size=2) +
  xlab("year") + ylab("Sales Amount") + 
  ggtitle('Change Linetype and Size')


# Task 5: In the plot above, increase the number of grids on the x axis (date)?
p + geom_line(linetype="dashed", size=2) + 
  xlab("year") + ylab("Sales Amount") + 
  ggtitle('Change Linetype and Size') + 
  scale_x_date(date_breaks = "1 month", date_labels = "%m/%y")  # compact date format
#browseURL("https://www.r-bloggers.com/2013/08/date-formats-in-r/")



# Task 6. Following the last task above. Rotate the labels on x axis?
p + geom_line(linetype="dashed", size=2) +
  xlab("year") + ylab("Sales Amount") + 
  ggtitle('Change Linetype and Size') + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#' Task 7: Instead of the line plot above, create stacked bar chart, why?
p + geom_bar(stat = "identity", aes(fill=province) , position = "stack") + 
  xlab("year Month")  + ylab("Sale Amount") + 
  ggtitle('Stack Position') # one bar on top of the other

#' Fill position, why?
p + geom_bar(stat = "identity", aes(fill=province), position = "fill") +
  xlab("year Month") + ylab("Sale Amount") + ggtitle('Fill Position')

#' Dodge position, why?
p + geom_bar(stat = "identity", aes(fill=province), position = "dodge")  + 
  xlab("year Month")  + ylab("Sale Amount") + ggtitle('Dodge Position') +
  scale_fill_brewer(palette=2) + 
  theme_bw() 

# Task 8: Create a scatter plot instead  
p + geom_point(size=3) + xlab("year Month") + ylab("Sale Amount") + ggtitle('Adding Smoother')
p + geom_point(size=3) + xlab("year Month") + ylab("Sale Amount") + geom_smooth() + ggtitle('Adding Smoother')
p + geom_point(size=3) + xlab("year Month") + ylab("Sale Amount") + geom_smooth(se=FALSE) + ggtitle('Adding Smoother')
p + geom_point(size=3) + xlab("year Month") + ylab("Sale Amount") + geom_smooth(method=lm,se=FALSE) + ggtitle('Adding Linear Regression')
p + geom_point(size=3) + xlab("year Month") + ylab("Sale Amount") + geom_point(stat = "summary", fun.y = "mean", colour = "blue", size = 4) + ggtitle('Adding Mean Points')

p + geom_point(aes(size=total_sales)) + scale_size_continuous(range=c(1,10)) +xlab("year Month") + ylab("Sale Amount") + ggtitle('Resize The Point')
p + geom_point(aes(colour=total_sales), size=5) + scale_color_gradient() +xlab("year Month") + ylab("Sale Amount")  +ggtitle('Repaint The Point in Gradient Color')


# Task 9: facet_wrap() the scatter plots above 
p + geom_point(size = 5) + facet_wrap(~province) +xlab("year Month") + ylab("Sale Amount") + ggtitle('Create Multiple Subplots by province')
p + geom_point(size = 5) + facet_wrap(~province, ncol=1) +xlab("year Month") + ylab("Sale Amount") + ggtitle('Multiple Subplots in Vertical Direction')

p + geom_point(size=5) + theme_bw()+xlab("year Month") + ylab("Sale Amount")  +ggtitle('theme_bw Example')
p + geom_point(size=5) + theme_dark()+ xlab("year Month") + ylab("Sale Amount") +ggtitle('theme_dark Example')

p + geom_point(size=5) + scale_color_manual(values=c("#E69F00", "chartreuse")) +
  theme(
    axis.text = element_text(size = 12),
    legend.background = element_rect(fill = "white"),
    panel.grid.major = element_line(colour = "yellow"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "blue")
  ) + xlab("year Month") + ylab("Sale Amount") +ggtitle('Customized Theme')


# Task 10: Put/arrange the scatter plot and the line plot in the same window
# side by side. 
library(grid)
grid.newpage()

plot1 <- p + geom_point(size=5) +xlab("year Month") + ylab("Sale Amount") + ggtitle('Scatter Plot')
plot2 <- p + geom_line(size=3) + xlab("year Month") + ylab("Sale Amount") + ggtitle('Line Chart')

pushViewport(viewport(layout = grid.layout(1, 2)))
print(plot1, vp =viewport(layout.pos.row = 1, layout.pos.col = 1))
print(plot2, vp =viewport(layout.pos.row = 1, layout.pos.col = 2))

library(gridExtra)
grid.arrange(plot1,plot2, ncol=2)

#' Task 11: Find weekday of date from the superstore data frame
#' Min and Max date
summarise(superstore, min(order_date))
summarise(superstore, max(order_date))

superstore %>% 
  select(order_date) %>% 
  mutate(weekday = wday(order_date, label=TRUE),
         weekdayno = wday(order_date))

#' Task 12: Find average sales by weekday, but exclude Saturdays and Sundays.
superstore %>% 
  select(sales, order_date) %>% 
  mutate(weekday = wday(order_date)) %>% 
  filter(!weekday %in% c(1, 7)) %>% 
  group_by(weekday) %>%
  summarise(average_sales=mean(sales))

#' or
superstore %>% 
  select(sales, order_date) %>% 
  mutate(weekday = wday(order_date, label=TRUE)) %>% 
  filter(!weekday %in% c("Sun","Sat")) %>% 
  group_by(weekday) %>%
  summarise(average_sales=mean(sales))


# Task 13: Find the average sales on Saturdays and Sundays
superstore %>% 
  select(sales, order_date) %>% 
  mutate(weekday = wday(order_date, label=TRUE)) %>% 
  filter(weekday %in% c("Sun","Sat")) %>% 
  group_by(weekday) %>%
  summarise(average_sales=mean(sales))

#' Task 14: Find average sales by weekday and Customer Segment. 
#' Make one column per Customer Segment.
superstore %>%
  select(sales, order_date, customer_segment) %>%
  mutate(weekday = wday(order_date, label=TRUE)) %>% 
  group_by(weekday, customer_segment) %>%
  summarise(average_sales=mean(sales)) %>% 
  pivot_wider(names_from=customer_segment,
              values_from=average_sales)


#' Task 15: Your boss loves excel (sigh), and would like a spreadsheet with the weekdays and segments report in it.
#' use a relevant folder on your computer to save as excel file.
#browseURL("https://cran.r-project.org/web/packages/openxlsx/index.html")

library(openxlsx)

weekdays <- 
  superstore %>% 
  select(sales, order_date) %>% 
  mutate(weekday = wday(order_date, label=TRUE)) %>% 
  group_by(weekday) %>%
  summarise(average_sales=mean(sales))

segments <- 
  superstore %>% 
  select(sales, order_date, customer_segment) %>% 
  mutate(weekday = wday(order_date, label=TRUE)) %>% 
  group_by(weekday, customer_segment) %>%
  summarise(average_sales=mean(sales)) %>% 
  pivot_wider(names_from = customer_segment, values_from=average_sales) 

# put them in one file and save them as list 
sheets <- list(weekdays = weekdays, segments = segments)

#' Make sure you use a relevant folder on your computer
# getwd()
# dir()
openxlsx::write.xlsx(sheets, file = "C:/Users/dki007/OneDrive - UiT Office 365/V2025_folder/V2025_Vår_semister/SOK-1005_2025/My_excel_dataset.xlsx")


#' Task 16: Use the segments data frame and calculate the market share per customer segment and day
segments %>%
  pivot_longer(-weekday, names_to = "province", values_to = "average_sale") %>%
  group_by(weekday) %>%
  mutate(share = 100*average_sale/sum(average_sale)) %>%
  select(-average_sale) %>% 
  pivot_wider(names_from = province, values_from = share)



#' Task 17: Find average profit per customer segment and product category in 2011, for all provinces except
#' Nunavut, Newfoundland and Manitoba.
#' What segment produced the highest profit?


#' Task 18:  For the last 3 months of 2012, calculate the total Sales by month, 
#' for Alberta and Yukon in the Customer_Segment, Corporate, and Consumer. 


#' Task 19: Make a plot of the monthly total Sales in British Columbia and 
#'          Ontario in 2009, 2010, and 2011.


# Task 20: Following the last question, identify the months where the total Sales in Ontario
#'         is greater than the total Sales in "British Columbia".



#' A bit more advanced


#' Task 18: What customer segment and product category has the highest correlation between
#' unit price and order_quantity?


