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





# ==-- Question 2 --==
# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? Use the base plotting system to make a plot answering this question.
# Ans: While the total emissions of PM2.5 in Baltimore City, MD has decreased in years 1999-2008, a rebound of emission was noticed between year 2002-2005 based on the 2005 reading.

Tot_PM25_atBmore_byYear <- NEI %>% 
  filter(fips == "24510") %>% 
  group_by(year) %>% 
  summarise(Emissions_sum = sum(Emissions, na.rm = TRUE)) %>% 
  arrange(year)

# Plot pollutant emission (sum) at Baltimore, MD over 10 years period ('99, '02, '05, '08)
par(mar = c(4.2, 4.7, 3.2, 1))
plot(Tot_PM25_atBmore_byYear, type="o", col="blue", lty=1, ylim=c(0, 3500), 
  xlab="Year", ylab=expression("Total" ~ PM[2.5] ~ "Emissions (tons)"),
  main=expression("Total Baltimore" ~ PM[2.5] ~ "Emissions by Year")
  )
# export graph as png file
dev.copy(png, file = "plot2.png")
dev.off()