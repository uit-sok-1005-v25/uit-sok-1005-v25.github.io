
rm(list=ls())

library(quantmod)
library(Quandl)
library(tidyverse)

# Import the Daily crude oil prices: Brent - Europe (DCOILBRENTEU) from FRED
browseURL("https://fred.stlouisfed.org/series/DCOILBRENTEU")

loadSymbols("DCOILBRENTEU", src="FRED")

oil <- DCOILBRENTEU %>% 
  data.frame() %>% 
  rownames_to_column(var ="date") %>% 
  rename(Value =DCOILBRENTEU) 

head(oil) 

# MIssing values 
colSums(is.na(oil))

# copy oil data
dframe <- oil

# Try replacing NA's with previous recent value 
dframe <- dframe %>% fill(Value,.direction = "down")

# as tibble
dframe <- as_tibble(dframe) %>% 
  mutate(Date = as.Date(date)) %>% 
  select(Date,Value)
dframe


# Plots 
plot(dframe, type="l")

dframe %>% 
  ggplot(aes(x=Date, y=Value)) + 
  geom_line() + theme_bw()

# diffifferent color 
dframe %>% ggplot(aes(x=Date, y=Value)) +
  geom_line(color = "#20A0E0") + 
  ggtitle("Daily Crude Oil Prices") + theme_bw()

# Note the ordering of dates!
dframe 
tail(dframe)
# first row must be first time observation
dframe <- dframe %>% arrange(Date)

# How to measure returns
browseURL("https://en.wikipedia.org/wiki/Rate_of_return")

# Arithmetic returns, looses first observation
dframe %>% 
  mutate(arit_ret = (Value - lag(Value))/lag(Value)) %>% 
  head()

# Returns over time, geometric mean
# Note that we set the first observation (of this index) equal to 1
dframe %>%
  mutate(arit_ret = (Value - lag(Value))/lag(Value),
          comp_arit_ret = c(1, cumprod(1+na.omit(arit_ret)))) %>% head()                  

# Logarithmic (or log-returns), commonly used in economic models
dframe %>% mutate(log_ret = log(Value) - log(lag(Value))) %>% head()
# or
dframe %>% mutate(log_ret = log(Value/lag(Value))) %>% head()
# or
dframe %>% mutate(log_ret = c(NA,diff(log(Value)))) %>% head()

# Compounding log returns
dframe %>% 
  mutate(log_ret = log(Value/lag(Value)),
         comp_log_ret = c(1, 1+cumsum(na.omit(log_ret)))) %>% head() # see https://rpubs.com/esteban926/978486


# Update dataframe with log returns
dframe <- dframe %>%
  mutate(log_ret = log(Value/lag(Value)),
        comp_log_ret = c(1, 1+cumsum(na.omit(log_ret))))

# plot log returns
dframe %>%
  ggplot(aes(x=Date, y=log_ret)) + geom_line()

mosaic::favstats(~log_ret, data = dframe)

dframe %>% ggplot(aes(log_ret)) + geom_density()

# the daily nominal oil price since 2003
dframe %>% ggplot(aes(x=Date, y=comp_log_ret)) + geom_line()

# index comp_log_ret with the value of January 4th 2010 = 100
dframe[dframe$Date == "2010-01-04",]
dframe[dframe$Date == "2010-01-04","comp_log_ret"]$comp_log_ret

dframe %>% 
  ggplot(aes(x=Date, y=100*comp_log_ret/dframe[dframe$Date == "2010-01-04","comp_log_ret"]$comp_log_ret)) + geom_line()

# or mutate new variable first
dframe <- dframe %>% 
  mutate(new_comp_log_ret = 100*comp_log_ret/comp_log_ret[Date == "2010-01-04"]) 

dframe %>% 
  ggplot(aes(x=Date, y=new_comp_log_ret)) + geom_line()





# ----------------------------

# Rename dframe back to oil 
oil <- dframe

# calculate monthly averages....from the daily values 

library(tsibble)
# Coerce to a tsibble with as_tsibble() 
# To coerce a data frame to tsibble, we need to declare key and index.
# key: identifying variables that define series
# index: a variable that represents time

# As you coerce a dataframe to tsibble object using tsibble,
# An interval is automatically obtained based on the corresponding
# time representation. That is, a tsibble of monthly intervals expects 
# the yearmonth/yearmon class in the index column.

# Interval	Class
# --------------------
# Annual	integer/double
# Quarterly	yearquarter
# Monthly	yearmonth
# Weekly	yearweek
# Daily	Date/difftime
# Subdaily	POSIXt/difftime/hms


# You cab read more about Tsibble On: 
browseURL("https://cran.rstudio.com/web/packages/tsibble/vignettes/intro-tsibble.html")
browseURL("https://github.com/tidyverts/tsibble")
browseURL("https://tsibble.tidyverts.org/")

# Also tibbletime
browseURL("https://business-science.github.io/tibbletime/reference/rollify.html")
browseURL("https://blog.earo.me/2018/02/06/tsibble-or-tibbletime/")

#browseURL("https://tsibble.tidyverts.org/articles/intro-tsibble.html")
#browseURL("https://otexts.com/fpp3/tsibbles.html")


head(oil)
str(oil)


oil_month <- as_tsibble(oil, index = Date) %>% 
  index_by(year_month=yearmonth(Date)) %>% # index_by() is the counterpart of group_by() in temporal context
  summarise(oil_avg = mean(Value, na.rm = TRUE)) %>% 
  mutate(Date = as.Date(year_month, format = "%Y %b")) 

head(oil_month)
str(oil_month)

#################################################
# Gold price 
# You can try loading Gold price from Nasdaq, but first you need to have your own api
# Read Quandl data
# Gold prices, US$ per troy ounce
#browseURL("https://www.quandl.com/tools/r")
# Quandl.api_key("your api key")
# gold <- Quandl("LBMA/GOLD")
# head(gold)
###############################################

# Get the Gold future price from 
library(readr)
browseURL("https://www.investing.com/commodities/gold")
Gold_price <- read_csv("https://raw.githubusercontent.com/uit-sok-1005-v25/uit-sok-1005-v25.github.io/refs/heads/main/Gold_price.csv")

head(Gold_price)

Gold_price <- Gold_price %>% 
  mutate(Date = mdy(Date)) %>% 
  arrange(Date) %>% select(Date, Price) %>% 
  rename(gold_avg=Price) # renaming Price as average monthly gold price

head(Gold_price)


# Merge oil and gold price by date
dframe <- left_join(oil_month, Gold_price, by="Date")

head(dframe)
tail(dframe)

dframe <- dframe %>% drop_na()


# change data from wide to long 
dframe.long <- dframe %>% select(-Date) %>% 
     pivot_longer(-year_month,
                  names_to = "commodity",
                  values_to = "price")

head(dframe.long)
str(dframe.long)

dframe.long %>%
  ggplot(aes(x=year_month, y=price, col=commodity)) +
  geom_line()


# Rolling 12 month correlation
browseURL("https://business-science.github.io/tibbletime/reference/rollify.html")

library(tibbletime)

# With 2 args, use the purrr syntax of ~ and .x, .y
# Rolling correlation 12 periods
cor_roll <- rollify(~cor(.x, .y), window = 12)

# use function
dframe <- dframe %>% 
     mutate(cor_12 = cor_roll(oil_avg, gold_avg))

mean.cor <- cor(dframe$oil_avg, dframe$gold_avg)
mean.12.cor <- mean(dframe$cor_12, na.rm = TRUE)

dframe %>% 
  ggplot(aes(x = year_month, y = cor_12)) + geom_line() + 
  geom_hline(yintercept=mean.cor, linetype="dashed", color = "red", size=2) +
  geom_hline(yintercept=mean.12.cor, linetype="dashed", color = "blue", size=2)

# 12 month moving average
# Turn the normal mean function into a rolling mean with a 12 row window
mean_roll_12 <- rollify(mean, window = 12)

dframe.long <- dframe.long %>% 
       group_by(commodity) %>% 
      mutate(MA_12 = mean_roll_12(price))

dframe.long

dframe.long %>% 
  ggplot(aes(x = year_month, y = MA_12, color = commodity)) + geom_line() 

# The broom equivalent for time series, sweep
browseURL("https://business-science.github.io/sweep/index.html")
browseURL("https://slides.earo.me/rstudioconf19/#1")
#Dynamic harmonic regression: 
browseURL("https://otexts.com/fpp3/dhr.html")


