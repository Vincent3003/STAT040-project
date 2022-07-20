# activate packages
library(dplyr)
library(tidyr)

# sets the working directory
setwd("/Users/alisondang/Documents/FALL 2020/STAT 040/Project")

# load the datas
Covid_data = read.csv("Covid data.csv", header = TRUE, sep = ",", na.strings = c(NA, ""," "))

# dropping unnecessary variables in COVID data 
Covid_data2 <- subset(Covid_data, select = -c(iso_code, continent, new_cases, new_cases_smoothed, new_deaths:stringency_index, population_density:human_development_index))
Covid_data2 <- Covid_data2 [-c(1:11329, 11331: 54238, 54240:55801, 55803:57395),]

# rename the columns
Covid_data2 <- rename(Covid_data2, Country = location)
Covid_data2 <- rename(Covid_data2, TotalCases = total_cases)
Covid_data2 <- rename(Covid_data2, TotalDeaths = total_deaths)
Covid_data2 <- rename(Covid_data2, Date = date)
Covid_data2 <- rename(Covid_data2, Population = population)

# Create the total cases chart
barplot(TotalCases ~ Country,
       data = Covid_data2,
       main = "The total Coronavirus cases in 3 countries",
       xlab = "Country",
       ylab = "Numbers of cases",
       col = c("light blue", "gray", "light pink"))

# Create the total deaths chart
barplot(TotalDeaths ~ Country,
        data = Covid_data2,
        main = "The total deaths by Coronavirus in 3 countries",
        xlab = "Country",
        ylab = "Numbers of deaths",
        col = c("light green", "light pink", "brown"))

# calculate the percentage of the Coronaviruses cases in China & America
Covid_data2 = mutate(Covid_data2,
               Cases_Percentage = TotalCases/Population,
               Deaths_Percentage = TotalDeaths/Population)

# Create the deaths percentage chart
barplot(Deaths_Percentage ~ Country,
        data = Covid_data2,
        main = "The percentage of deaths by Coronavirus in 3 countries",
        xlab = "Country",
        ylab = "Percentage of deaths",
        col = c("sea green", "light green", "pale violetred"))

# Create the cases percentage chart
barplot(Cases_Percentage ~ Country,
        data = Covid_data2,
        main = "The percentage of cases by Coronavirus in 3 countries",
        xlab = "Country",
        ylab = "Percentage of cases",
        col = c("dark sea green", "coral", "deep sky blue"))

