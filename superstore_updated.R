

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
View(superstore)

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



#' Task 17: Find the average profit per customer segment and product category
#' in 2011, for all provinces except: Nunavut, Newfoundland and Manitoba.
#' What segment produced the highest profit?

#Counts the number of rows in each group 
#(i.e., the number of occurrences of each province).
superstore %>% group_by(province) %>% tally() 

unique(superstore$province)

superstore %>%
  select(order_date,customer_segment, product_category, province,profit) %>% 
  mutate(year = year(order_date),
         year2 = floor_date(order_date, unit = "year")) %>% 
  filter(year==2011) %>% 
  filter(!province %in% c("Nunavut", "Newfoundland", "Manitoba")) %>% 
  group_by(customer_segment, product_category) %>% 
  summarise(average_profit = mean(profit)) %>%
  arrange(-average_profit) 

#Alternatively 
superstore %>%
  select(order_date,customer_segment, product_category, province,profit) %>% 
  mutate(year = year(order_date),
         year2 = floor_date(order_date, unit = "year")) %>% 
  filter(year==2011) %>% 
  filter(!province %in% c("Nunavut", "Newfoundland", "Manitoba")) %>% 
  group_by(customer_segment, product_category) %>% 
  summarise(average_profit = mean(profit)) %>%
  arrange(desc(average_profit))   # note desc()

#' Task 18: Make a plot of the monthly total Sales in British Columbia and 
#'          Ontario in 2009, 2010, and 2011. 

superstore %>%
  mutate(Year = year(order_date),
         Month = month(order_date,label = TRUE),
         Day = day(order_date)) %>%
  arrange(Year,Month,Day) %>% 
  filter(Year >=2009 &  Year <=2011, # or filter(Year %in% c("2009", "2010", "2011"),
         province %in% c("British Columbia","Ontario"),
         customer_segment %in% c("Corporate", "Consumer")) %>% 
  group_by(Year,Month,province) %>% 
  summarise(Tot.sales = sum(sales, na.rm = TRUE)) %>%
  mutate(year_month = make_date(Year,Month,1)) %>% 
  ggplot(aes(x=year_month,y = Tot.sales, color = province))+
  geom_line()

# Task 19:In the last problem, instead of the line plots, 
# use bar plot to show the total monthly sales.
superstore %>%
  mutate(Year = year(order_date),
         Month = month(order_date,label = TRUE),
         Day = day(order_date)) %>%
  arrange(Year,Month,Day) %>% 
  filter(Year >=2009 &  Year <=2011, # or filter(Year %in% c("2009", "2010", "2011"),
         province %in% c("British Columbia","Ontario"),
         customer_segment %in% c("Corporate", "Consumer")) %>% 
  group_by(Year,Month,province) %>% 
  summarise(Tot.sales = sum(sales, na.rm = TRUE)) %>%
  ggplot(aes(x=Month, y=Tot.sales))+
  geom_col(aes(fill=province), position="dodge")+
  facet_wrap(~Year, nrow=3)+
  theme_bw()+
  labs(x="",
       y="Total sales", 
       title="Total monthly sales in British Columbia, and Ontario", color="province") +
  scale_y_continuous(labels = scales::dollar) +
  scale_fill_manual(values = c("British Columbia" = "#8fc3f8", "Ontario" = "#567595"),
                    name = "Province")+
  theme(legend.position="right") 


# Task 20: Following the previous question, identify the months where the total Sales in Ontario
#'         is greater than the total Sales in "British Columbia".

superstore %>%
  mutate(Year = year(order_date),
         Month = month(order_date,label = TRUE),
         Day = day(order_date)) %>%
  arrange(Year,Month,Day) %>% 
  filter(Year==2011, 
         province %in% c("British Columbia","Ontario"),
         customer_segment %in% c("Corporate", "Consumer")) %>% 
  group_by(Year,Month,province) %>% 
  summarise(Tot.sales = sum(sales, na.rm = TRUE)) %>%
  pivot_wider(names_from = province, values_from = Tot.sales) %>% 
  filter(Ontario > `British Columbia`)




# Task 21: Calculate the share of profits per product category and 
 # display the results using Pie chart

  superstore %>% 
   group_by(product_category) %>% 
   summarise(mean_profit = mean(profit)) %>%
   ungroup() %>% 
   mutate(total.profit = sum(mean_profit)) %>% #summarise() is not helpful here
   #share profit 
   group_by(product_category) %>% 
   mutate(share = 100*mean_profit/total.profit) %>% 
   ggplot(aes(x = "", y = share, fill = product_category)) +
   geom_bar(stat = "identity", width = 0.1) + 
   coord_polar(theta = "y") + 
   labs(title = "Mean Profit by Product Category") + 
   theme_void() + #removes all background elements, axes, gridlines, 
    #and text from the plot, leaving only the data visualization (useful for pie charts). 
   scale_fill_manual(values = c("Furniture" = "blue", #Color customization
                                "Office Supplies" = "red", 
                                "Technology" = "green"))
 
  # Normally, geom_bar() defaults to stat = "count", which counts observations.
  # Here, stat = "identity" means the exact y values (share) are used instead of counts.
  # width = 1 ensures that the bars take up the full width, making them appear as segments rather than narrow bars.
 
  # coord_polar(theta = "y"): Transforms the bar chart into a pie chart 
  # by mapping the y aesthetic to the radial axis (theta = "y").
  # Without coord_polar(), the chart would be a regular bar chart.

  # Adding text 
 #By default, the values are not displayed inside each slice. 
 #You can add them with geom_text. Note that position_stack(vjust = 0.5) will place 
 #the labels in the correct position.
 superstore %>% 
   group_by(product_category) %>% 
   summarise(mean_profit = mean(profit)) %>%
   ungroup() %>% 
   mutate(total.profit = sum(mean_profit)) %>% 
   #share profit 
   group_by(product_category) %>% 
   mutate(share = 100*mean_profit/total.profit) %>% 
   ggplot(aes(x = "", y = share, fill = product_category)) +
   geom_bar(stat = "identity", width = 1) + 
   coord_polar(theta = "y") + 
   labs(title = "Mean Profit by Product Category") + 
   theme_void() + 
   scale_fill_manual(values = c("Furniture" = "blue", 
                                "Office Supplies" = "red", 
                                "Technology" = "green"))+
   # Add the numbers inside the pie 
   geom_text(aes(label = share),
             position = position_stack(vjust = 0.5))
   
 
 
 # Round the numbers 
 superstore %>% 
   group_by(product_category) %>% 
   summarise(mean_profit = mean(profit)) %>%
   ungroup() %>% 
   mutate(total.profit = sum(mean_profit)) %>% 
   #share profit 
   group_by(product_category) %>% 
   mutate(share = round(100*mean_profit/total.profit,2)) %>% #rounding 
   ggplot(aes(x = "", y = share, fill = product_category)) +
   geom_bar(stat = "identity", width = 1) + 
   coord_polar(theta = "y") + 
   labs(title = "Mean Profit by Product Category") + 
   theme_void() + 
   scale_fill_manual(values = c("Furniture" = "blue", 
                                "Office Supplies" = "red", 
                                "Technology" = "green"))+
   # Add the numbers inside the pie 
   geom_text(aes(label = share),
             position = position_stack(vjust = 0.5))
   
 
 
# Customize the legend 
 superstore %>% 
   group_by(product_category) %>% 
   summarise(mean_profit = mean(profit)) %>%
   ungroup() %>% 
   mutate(total.profit = sum(mean_profit)) %>% 
   #share profit 
   group_by(product_category) %>% 
   mutate(share = round(100*mean_profit/total.profit,2)) %>% #rounding 
   ggplot(aes(x = "", y = share, fill = product_category)) +
   geom_bar(stat = "identity", width = 1) + 
   coord_polar(theta = "y") + 
   labs(title = "Figure: Mean Profit by Product Category") + 
   theme_void() + 
   scale_fill_manual(values = c("Furniture" = "blue", 
                                "Office Supplies" = "red", 
                                "Technology" = "green"))+
   # Add the numbers inside the pie 
   geom_text(aes(label = share),
             position = position_stack(vjust = 0.5))+ 
   guides(fill = guide_legend(title = "This is the legend Title"))
 
 
 
 
 
### ----------------------------------------------------------------------
#' A bit more advanced
### ----------------------------------------------------------------------

#' Task: What customer segment and product category has the highest correlation between
#' unit price and order_quantity?

 # considering the customer segments only
 superstore %>%
   select(customer_segment, order_quantity,unit_price) 
 
superstore %>%
  select(customer_segment, order_quantity,unit_price) %>%
  group_by(customer_segment) %>%
  summarize(cor(order_quantity,unit_price))

browseURL("https://cran.r-project.org/web/packages/broom/vignettes/broom_and_dplyr.html")

superstore %>% 
  select(customer_segment,order_quantity,unit_price, product_category) %>% 
  group_by(customer_segment, product_category) %>%
  nest() -> nested 

nested
#nest() → Creates a nested data frame where each group (combination 
#of customer_segment and product_category) is stored as a list-column named data.

# Instead of spreading the grouped data across multiple rows, it bundles
#all the remaining columns (order_quantity and unit_price) into a single
#list-column for each unique (customer_segment, product_category) pair.

# if you want to see what is inside the nested data
nested$data

#Now we can calculate the correlation between the variables in each of the list
library(broom)
nested %>% 
  mutate(test = map(data, ~ cor.test(.x$unit_price, .x$order_quantity))) %>%  # S3 list-col
  mutate(tidied = map(test, tidy)) %>% #the tidy() fun converts model outputs (like regression 
  #results) into a clean, structured tibble format, making it easier to interpret and work with.
  unnest(tidied)

browseURL("https://purrr.tidyverse.org/")
# elementary example on how map()
library(purrr)
# Two numeric vectors
x <- c(1, 2, 3)
# Square each element
map(x, ~ .x^2) # List: 1, 4, 9, 16, 25

# Another similar fun map2()
y <- c(10, 20, 30)

# Add corresponding elements
map2(x, y, ~ .x + .y)
#Here, map2() takes one element from x and one from y at the same index, adds them, and returns a list.
map2_dbl(x, y, ~ .x + .y)

#: Another example Creating Sentences
names <- c("Alice", "Bob", "Charlie")
ages <- c(25, 30, 22)
# Combine names and ages into sentences
map2(names, ages, ~ glue::glue("{.x} is {.y} years old."))
map2_chr(names, ages, ~ glue::glue("{.x} is {.y} years old."))


#' Task: Make a scatter plot of order_quantity vs unit_price (for all combination)
nested$group <- 1:length(nested$customer_segment)

library(glue)

many_plots <- 
  nested %>% 
  mutate(plot = map2(data, 
                     group,
                     ~ ggplot(data = .x, aes(x = order_quantity, y = unit_price)) +
                       ggtitle(glue("Group {.y}")) + 
                       geom_point()))
many_plots
print(many_plots$plot) 



#install.packages("cowplot")
library(cowplot)
cowplot::plot_grid(plotlist = many_plots$plot)


#Task 23: Replicate the code for three provinces, i.e., Nunavu, British Columbia, Ontirruwa,  
superstore$Province
