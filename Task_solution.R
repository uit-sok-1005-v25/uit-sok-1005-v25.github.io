#' Use the getSymbols() function from the quantmod package 
#' to download the daily stock prices for Exxon Mobil Corporation (XOM),traded at NYSE 
#' from the Yahoo! Finance site  
browseURL("https://finance.yahoo.com/quote/XOM/history")

#' Use the Adjusted closing price from January 4th 2010 as the starting date. 
#' And calculate the monthly average using trading volume as a weight, 
#' and save this variable as  “exxon”. 
#' Then,plot exxon against the date variable.   

################################################
#ChatGPT: I want to load the Exxon Mobil Corporation (XOM) data using R code. 
#how can I do that? briefly
#########################################
library(quantmod)

xom <- getSymbols("XOM", src = "yahoo", auto.assign = FALSE) %>%
  data.frame() %>% 
  clean_names() %>% 
  rownames_to_column(var="date") %>% 
  select(date, xom_adjusted, xom_volume) %>%
  rename(adj_close = xom_adjusted, 
         volume = xom_volume)  %>% 
  mutate(Date = as.Date(date,format = "%Y-%m-%d"),
         # or using lubridate function ymd() 
         Date2 = ymd(date)) %>% 
  select(-date,-Date2) %>% 
  rename(date = Date) %>% 
  filter(date >= "2010-01-04" & date <= "2024-12-31") %>% 
  #convert the data from daily to monthly data
  mutate(year = year(date), 
         month = month(date), 
         day = day(date)) %>% 
  group_by(year, month) %>% 
  summarise(exxon = weighted.mean(adj_close, volume)) %>% 
  mutate(date = make_date(year,month,1)) %>% 
  ungroup() %>% 
  select(date, exxon)

xom %>% 
  ggplot(aes(x= date, y= exxon))+
  geom_line()+
  theme_bw()+
  labs(x="",
       y="Total sales", 
       title="Monthly average ",
       caption = "source: Author plot using data from XXX database" ) 




#Import the Brent Crude oil price from FRED

###########################
#ChatGPT: how can I download using R code, the daily Brent Crude Oil Price from FRED
##############################

loadSymbols("DCOILBRENTEU", src="FRED")
#getSymbols("DCOILBRENTEU", src = "FRED",from ="2010-01-04", to = "2022-01-13")

oil_data <- DCOILBRENTEU
class(oil_data) # this is xts or zoo

# convert it to a data frame
oil_df <- as.data.frame(oil_data)
class(oil_df)
head(oil_df)

# calculate the monthly arithmetic average of oil price data.
oil <- oil_df %>%
  # create a date variable from the rownames of the dataframe 
  mutate(Date = ymd(rownames(oil_df))) %>% 
  # filter the data only between 2010-01-04 and 2022-12-30
  # since our package imports all data from 1987-05-20
  filter(Date>="2010-01-04",Date<"2024-12-31") %>% 
  
  # convert the daily data to monthly data
  mutate(year = year(Date),
         month = month(Date),
         day = day(Date)) %>%  
  group_by(year,month) %>% 
  # calculate the monthly arithmetic average
  summarise(oil_monthly= mean(DCOILBRENTEU, na.rm = TRUE)) %>% 
  mutate(date = make_date(year,month,1)) %>% # make the date variable, setting all day's as first day 
  as_tibble() %>%  
  select(date,oil_monthly) %>% rename(brent =oil_monthly)

#output
oil

# Combine/merge the xom and oil data as one data frame, and rename variables. 
dframe <- xom %>% 
  left_join(oil) # combine the two data frame as one 
head(dframe)

# Plot 
dframe %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = brent, colour = "Brent")) +  
  geom_line(aes(y = exxon, colour = "Exxon")) +  
  scale_colour_manual(values = c("Brent" = "red", "Exxon" = "blue"))


# Alternatively, taking exxon as x variable and brent as y variable
dframe %>% 
  ggplot(aes(x = brent,y = exxon)) +
  geom_point()+
  geom_smooth(method = "lm",se=FALSE)


# Linear regression model, lm() function 
linear_regression <- lm(exxon~brent,data = dframe)
summary(linear_regression)
# Interprate the estimated parameters/coefficients:
# Intercept: 49.32: when the price of brent is equal to zero,the price of exxon is 49.32
# slope: 0.159:when the price of brent increase by 1 unit,the price of exxon increase by 0.159 unit.
