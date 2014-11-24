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





# ==-- Question 3 --==
# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.
# Ans: Non-Road, Non-Point, and On-Road sources show decrease in emissions from 1999-2008.  However, Point source shows an increase in emissions during the same period.

# --- summed observations 
Tot_PM25_atBmore_byYear_byPol <- NEI %>% 
  filter(fips == "24510") %>% 
  group_by(type, year) %>% 
  summarise(Emissions_sum = sum(Emissions, na.rm = TRUE)) %>% 
  arrange(type, year)

# Plot total pollutant emissions at Baltimre, MD by source types over 10 years period ('99, '02, '05, '08)
g <- ggplot(Tot_PM25_atBmore_byYear_byPol, aes(x=year, y=Emissions_sum))
g + geom_point(color = "steelblue") + geom_smooth(method = "lm") + facet_grid(. ~ type) + labs(x = "Year", y = expression("Total" ~ PM[2.5] ~ "Emissions (tons)"), title = expression("Baltimore" ~ PM[2.5] ~ "Total Emissions by Source Type & Year")) + ggsave(filename = "plot3.png")