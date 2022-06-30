# data source: 
# https://ourworldindata.org/explorers/coronavirus-data-explorer?tab=map&zoomToSelection=true&facet=none&pickerSort=asc&pickerMetric=location&Interval=7-day+rolling+average&Relative+to+Population=true&Color+by+test+positivity=false&country=USA~CHN~VNM&Metric=Confirmed+cases

# install package
install.packages(tidyverse) # for data manipulation
install.packages(lubridate) # for dates
install.packages("ggplot2")

# activate packages
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library("ggplot2")

# sets the working directory
setwd("/Users/alisondang/Documents/Academic/2020 - 2021 school year/FALL 2020/STAT 040/Project")

# load the dataset & choose columns to analyze
covid = read.csv("owid-covid-data.csv", header = TRUE, sep = ",", na.strings = c(NA, ""," ")) %>% 
  select(location, date, total_cases, total_deaths, population)
show(covid)

# get china data
covid_china = covid %>% 
  filter(location == "China")
show(covid_china)
tail(covid_china)

# get USA data
covid_usa = covid %>% 
  filter(location == "United States")
show(covid_usa)
tail(covid_usa)

# get Vietnam data
covid_vietnam = covid %>% 
  filter(location == "Vietnam")
show(covid_vietnam)
tail(covid_vietnam)

# calculate the avg case of Quarter 1 2020 in China, USA, & Vietnam
start_Q1_2020 = as.Date('2020-01-22')
end_Q1_2020 = as.Date('2020-03-31')

Q1_2020_covid_china = covid_china %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q1_2020 & date <= end_Q1_2020) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q1_2020 = round(mean(total_cases), digits = 2)) # calculate the avg of that month
Q1_2020_covid_china

Q1_2020_covid_usa = covid_usa %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q1_2020 & date <= end_Q1_2020) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q1_2020 = round(mean(total_cases), digits = 2)) # calculate the avg of that month
Q1_2020_covid_usa

Q1_2020_covid_vietnam = covid_vietnam %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q1_2020 & date <= end_Q1_2020) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q1_2020 = round(mean(total_cases), digits = 2)) # calculate the avg of that month
Q1_2020_covid_vietnam

# calculate the avg case of Quarter 2 2020 in China, USA, & Vietnam
start_Q2_2020 = as.Date('2020-04-01')
end_Q2_2020 = as.Date('2020-06-30')

Q2_2020_covid_china = covid_china %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q2_2020 & date <= end_Q2_2020) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q2_2020 = round(mean(total_cases), digits = 2)) # calculate the avg of that month
Q2_2020_covid_china

Q2_2020_covid_usa = covid_usa %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q2_2020 & date <= end_Q2_2020) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q2_2020 = round(mean(total_cases), digits = 2))  # calculate the avg of that month
Q2_2020_covid_usa

Q2_2020_covid_vietnam = covid_vietnam %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q2_2020 & date <= end_Q2_2020) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q2_2020 = round(mean(total_cases), digits = 2)) # calculate the avg of that month
Q2_2020_covid_vietnam

# calculate the avg case of Q3 2020 in China, USA, & Vietnam
start_Q3_2020 = as.Date('2020-07-01')
end_Q3_2020 = as.Date('2020-09-30')

Q3_2020_covid_china = covid_china %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q3_2020 & date <= end_Q3_2020) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q3_2020 = round(mean(total_cases), digits = 2)) # calculate the avg of that month
Q3_2020_covid_china

Q3_2020_covid_usa = covid_usa %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q3_2020 & date <= end_Q3_2020) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q3_2020 = round(mean(total_cases), digits = 2)) # calculate the avg of that month
Q3_2020_covid_usa

Q3_2020_covid_vietnam = covid_vietnam %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q3_2020 & date <= end_Q3_2020) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q3_2020 = round(mean(total_cases), digits = 2)) # calculate the avg of that month
Q3_2020_covid_vietnam

# calculate the avg case of Q4 2020 in China, USA, & Vietnam
start_Q4_2020 = as.Date('2020-10-01')
end_Q4_2020 = as.Date('2020-12-31')

Q4_2020_covid_china = covid_china %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q4_2020 & date <= end_Q4_2020) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q4_2020 = round(mean(total_cases), digits = 2)) # calculate the avg of that month
Q4_2020_covid_china

Q4_2020_covid_usa = covid_usa %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q4_2020 & date <= end_Q4_2020) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q4_2020 = round(mean(total_cases), digits = 2)) # calculate the avg of that month
Q4_2020_covid_usa

Q4_2020_covid_vietnam = covid_vietnam %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q4_2020 & date <= end_Q4_2020) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q4_2020 = round(mean(total_cases), digits = 2)) # calculate the avg of that month
Q4_2020_covid_vietnam

# calculate the avg case of Q1 2021 in China, USA, & Vietnam
start_Q1_2021 = as.Date('2021-01-01')
end_Q1_2021 = as.Date('2021-03-31')

Q1_2021_covid_china = covid_china %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q1_2021 & date <= end_Q1_2021) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q1_2021 = round(mean(total_cases), digits = 2)) # calculate the avg of that month
Q1_2021_covid_china

Q1_2021_covid_usa = covid_usa %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q1_2021 & date <= end_Q1_2021) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q1_2021 = round(mean(total_cases), digits = 2)) # calculate the avg of that month
Q1_2021_covid_usa

Q1_2021_covid_vietnam = covid_vietnam %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q1_2021 & date <= end_Q1_2021) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q1_2021 = round(mean(total_cases), digits = 2)) # calculate the avg of that month
Q1_2021_covid_vietnam

# calculate the avg case of Q2 2021 in China, USA, & Vietnam
start_Q2_2021 = as.Date('2021-04-01')
end_Q2_2021 = as.Date('2021-06-30')

Q2_2021_covid_china = covid_china %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q2_2021 & date <= end_Q2_2021) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q2_2021 = round(mean(total_cases), digits = 2)) # calculate the avg of that month
Q2_2021_covid_china

Q2_2021_covid_usa = covid_usa %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q2_2021 & date <= end_Q2_2021) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q2_2021 = round(mean(total_cases), digits = 2)) # calculate the avg of that month
Q2_2021_covid_usa

Q2_2021_covid_vietnam = covid_vietnam %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q2_2021 & date <= end_Q2_2021) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q2_2021 = round(mean(total_cases), digits = 2)) # calculate the avg of that month
Q2_2021_covid_vietnam

# calculate the avg case of Q3 2021 in China, USA, & Vietnam
start_Q3_2021 = as.Date('2021-07-01')
end_Q3_2021 = as.Date('2021-09-30')

Q3_2021_covid_china = covid_china %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q3_2021 & date <= end_Q3_2021) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q3_2021 = round(mean(total_cases), digits = 2)) # calculate the avg of that month
Q3_2021_covid_china

Q3_2021_covid_usa = covid_usa %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q3_2021 & date <= end_Q3_2021) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q3_2021 = round(mean(total_cases), digits = 2)) # calculate the avg of that month
Q3_2021_covid_usa

Q3_2021_covid_vietnam = covid_vietnam %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q3_2021 & date <= end_Q3_2021) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q3_2021 = round(mean(total_cases), digits = 2)) # calculate the avg of that month
Q3_2021_covid_vietnam

# calculate the avg case of Q4 2021 in China, USA, & Vietnam
start_Q4_2021 = as.Date('2021-10-01')
end_Q4_2021 = as.Date('2021-12-31')

Q4_2021_covid_china = covid_china %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q4_2021 & date <= end_Q4_2021) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q4_2021 = round(mean(total_cases), digits = 2)) # calculate the avg of that month
Q4_2021_covid_china

Q4_2021_covid_usa = covid_usa %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q4_2021 & date <= end_Q4_2021) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q4_2021 = round(mean(total_cases), digits = 2)) # calculate the avg of that month
Q4_2021_covid_usa

Q4_2021_covid_vietnam = covid_vietnam %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q4_2021 & date <= end_Q4_2021) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q4_2021 = round(mean(total_cases), digits = 2)) # calculate the avg of that month
Q4_2021_covid_vietnam

# calculate the avg case of Q1 2022 in China, USA, & Vietnam
start_Q1_2022 = as.Date('2022-01-01')
end_Q1_2022 = as.Date('2022-03-31')

Q1_2022_covid_china = covid_china %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q1_2022 & date <= end_Q1_2022) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q1_2022 = round(mean(total_cases), digits = 2)) # calculate the avg of that month
Q1_2022_covid_china

Q1_2022_covid_usa = covid_usa %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q1_2022 & date <= end_Q1_2022) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q1_2022 = round(mean(total_cases), digits = 2)) # calculate the avg of that month
Q1_2022_covid_usa

Q1_2022_covid_vietnam = covid_vietnam %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q1_2022 & date <= end_Q1_2022) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q1_2022 = round(mean(total_cases), digits = 2)) # calculate the avg of that month
Q1_2022_covid_vietnam

# calculate the avg case of Q2 2022 in China, USA, & Vietnam
start_Q2_2022 = as.Date('2022-04-01')
end_Q2_2022 = as.Date('2022-06-30')

Q2_2022_covid_china = covid_china %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q2_2022 & date <= end_Q2_2022) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q2_2022 = round(mean(total_cases), digits = 2)) # calculate the avg of that month
Q2_2022_covid_china

Q2_2022_covid_usa = covid_usa %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q2_2022 & date <= end_Q2_2022) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q2_2022 = round(mean(total_cases), digits = 2)) # calculate the avg of that month
Q2_2022_covid_usa

Q2_2022_covid_vietnam = covid_vietnam %>% # take the data frame
  mutate(timeRangeOfInterest = date >= start_Q2_2022 & date <= end_Q2_2022) %>%
  filter(timeRangeOfInterest) %>% # filter out only the time range of interest
  summarise(Q2_2022 = round(mean(total_cases), digits = 2)) # calculate the avg of that month
Q2_2022_covid_vietnam

# create a new table for China
china <- c(Q1_2020_covid_china, Q2_2020_covid_china, Q3_2020_covid_china,
                      Q4_2020_covid_china, Q1_2021_covid_china, Q2_2021_covid_china,
                      Q3_2021_covid_china, Q4_2021_covid_china, Q1_2022_covid_china, 
                      Q2_2022_covid_china)
china

# create a new table for USA
usa <- c(Q1_2020_covid_usa, Q2_2020_covid_usa, Q3_2020_covid_usa,
                      Q4_2020_covid_usa, Q1_2021_covid_usa, Q2_2021_covid_usa,
                      Q3_2021_covid_usa, Q4_2021_covid_usa, Q1_2022_covid_usa, 
                      Q2_2022_covid_usa)
usa

# create a new table for china
vietnam <- c(Q1_2020_covid_vietnam, Q2_2020_covid_vietnam, Q3_2020_covid_vietnam, Q4_2020_covid_vietnam, 
             Q1_2021_covid_vietnam, Q2_2021_covid_vietnam, Q3_2021_covid_vietnam, Q4_2021_covid_vietnam, 
             Q1_2022_covid_vietnam, Q2_2022_covid_vietnam)
vietnam

# merge the mean data of China, USA, Vietnam into one table
table1 = rbind(china, usa)
table1
table2 = rbind(table1, vietnam)
table2

# convert columns of an table2 data frame into rows
table3 <- as.data.frame(t(table2))
table3

# assigning the unnamed first column to a new name of a data frame
table4 = setNames(cbind(rownames(table3), table3, row.names = NULL), 
         c("timeperiod", "China", "USA", "Vietnam"))
table4

# this is a draft for line graph in 3 different countries
# create a line graph
plot(as.numeric(table4$timeperiod), table4$China, type = "l",
     ylab = "Timeline", xlab = "Number of total cases",
     xaxt = "n")

# Second variable
lines(as.numeric(table4$timeperiod), table4$USA, col = 2)

# Group names
axis(Totalcases, labels = as.character(table4$timeperiod), at = as.numeric(table4$timeperiod))


# create an Y column
TimeCovid <- c("Q1-2020", "Q2-2020", "Q3-2020", "Q4-2020", "Q1-2021", "Q2-2021",
               "Q3-2021", "Q4-2021", "Q1-2022", "Q2-2022")
TimeCovid

# create a new table
table5 <- data.frame(TimeCovid, table4)
table5

# remove a duplicate column
table5$timeperiod <- NULL
table5

# create a line graph
line <- ggplot(table5, aes(TimeCovid, table4)) +
  geom_line()
line
