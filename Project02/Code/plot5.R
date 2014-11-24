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





# ==--Question 5 --==
# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
# Ans: A logarithmic decrease of emissions from motor vehicle sources in Baltimore from 1999-2008.

Tot_PM25_atBmore_OnRoad <- NEI %>%
  filter((type == "ON-ROAD") & (fips == "24510")) %>% 
  group_by(year) %>% 
  summarise(Emissions_sum = sum(Emissions, na.rm = TRUE)) %>% 
  arrange(year)
# Plot emmissions from motor vehicle sources changed from 1999–2008 in Baltimore City
g <- 
  ggplot(data = Tot_PM25_atBmore_OnRoad, aes(x = year, y = Emissions_sum)) + 
  geom_line() + 
  labs(x = "Year", y = expression("Total" ~ PM[2.5] ~ "Emissions (tons)"), title = expression("Total Vehicles related" ~ PM[2.5] ~ "Emissions in Baltimore City, MD")) + 
  ggsave(filename = "plot5.png")
print (g)