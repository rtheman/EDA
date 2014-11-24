# Project 02
# <URL: https://class.coursera.org/exdata-008/human_grading/view/courses/972597/assessments/4/submissions
# Last modified on Sun, 23Nov2014 by Rich Leung

library(datasets)
library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)


# Initializing working directory
WD = "/Users/richleung/Dropbox/Projects/Coursera/EDA_local/Project02/Input/exdata-data-NEI_data"
# WD = "C:\Users\rich.leung.000\Dropbox\Projects\Coursera\EDA_local\Project02\Input\exdata-data-NEI_data"
setwd(WD)

# Import raw data file
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")





# ==-- Question 1 --==
# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.
# Ans: The total emissions from PM2.5 has generally decreased from 7.3 tons to 3.5 tons in U.S. from year 1999-2008.

# Observe the emission trends by taking sum and mean of emissions.
Tot_PM25_byYear <- NEI %>% 
  group_by(year) %>% 
  summarise(Emissions_sum = sum(Emissions, na.rm = TRUE)) %>% 
  arrange(year)

# Plot pollutant emission (sum) over 10 years period ('99, '02, '05, '08)
par(mar = c(4.2, 4.7, 3.2, 1))
plot(Tot_PM25_byYear, type="o", col="blue", lty=1, ylim=c(0, 8e6), 
  xlab="Year", ylab=expression("Total" ~ PM[2.5] ~ "Emissions (tons)"),
  main=expression("Total US" ~ PM[2.5] ~ "Emissions by Year")
  )
# export graph as png file
dev.copy(png, file = "plot1.png")
dev.off()