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






# ==--Question 6 --==
# Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?
# Ans: Baltimore City saw the greatest change over time in motor vehicle emmissions at -34% compared to Los Angeles County with +10%.

Tot_PM25_Bmore_LA <- NEI %>% 
  filter ((type == "ON-ROAD") & ((fips == "24510") | (fips == "06037"))) %>% 
  group_by (fips, year) %>% 
  summarise(Emissions_sum = sum(Emissions, na.rm = TRUE)) %>% 
  arrange(fips, year)
# Plot emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, CA
g1 <- 
  ggplot(data = Tot_PM25_Bmore_LA, aes(x = year, y = Emissions_sum, group = fips, colour = fips)) + 
  geom_line() + 
  labs(x = "Year", y = expression("Total" ~ PM[2.5] ~ "Emissions (tons)"), title = expression("Total" ~ PM[2.5] ~ "Emissions from vehicles sources between Baltimore & LA"), labs = "City") + 
  ggsave(filename = "plot6_orig.png")
print (g1)

# Standardised to year 1999's observation
Emiss_1999_Bmore <- Tot_PM25_Bmore_LA %>% 
  filter((year == 1999) & (fips == "24510")) %>% 
  select(Emissions_sum)
Emiss_1999_LA <- Tot_PM25_Bmore_LA %>% 
  filter((year == 1999) & (fips == "06037")) %>% 
  select(Emissions_sum)
# dplyr doesn't seems to allow assignment of observation from global variable (outside dataset)
Tot_PM25_Bmore_LA <- Tot_PM25_Bmore_LA %>%
  select(everything()) %>% 
  mutate(Emiss_1999 = ifelse(fips == "06037", 3931.12, 346.82))
# Calculate emission measure standardised to year 1999 value for each fips (city)
Tot_PM25_Bmore_LA <- Tot_PM25_Bmore_LA %>%
  select(everything()) %>% 
  mutate(Emissions_std = Emissions_sum / Emiss_1999)
# Plot
g2 <-
  ggplot(data = Tot_PM25_Bmore_LA, aes(x = year, y = Emissions_std, group = fips, colour = fips)) + 
  geom_line() + 
  labs(x = "Year", y = expression("Total" ~ PM[2.5] ~ "Emissions (tons)"), title = expression("Total" ~ PM[2.5] ~ "Emissions from vehicles sources between Baltimore & LA"), labs = "City") + 
  ggsave(filename = "plot6_std.png")
print (g2)

# Combining both plots (emissions sum per yr and emissions sum per yr per source) into single canvas
grid.arrange(g1, g2)
g3 <- arrangeGrob(g1, g2)
ggsave(filename = "plot6.png", g3)
# Using multiplot function
# g3 <- multiplot(g1, g2, cols = 2)
# print (g3)