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



# =====================================================================
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
# =====================================================================





# ==-- Quick EDA --==
# a quick glimpse of the Emission data set, NEI
glimpse(NEI)
# a quick glimpse of the Source Classification Code table, SCC
glimpse(SCC)
# Data are captured from 4 distinct year [1999, 2002, 2005, and 2008]
distinct(select(NEI, year))
# 4 different types of sources used to capture pollutant [Point, Non-Point, On-Road, and Non-Road]
distinct(select(NEI, type))
# 3263 different counties (fips)
group_by(distinct(select(NEI, fips))) %>%
  tally(sort = TRUE)





# ==-- Question 1 --==
# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.
# Ans: The total emissions from PM2.5 has generally decreased from 7.3 tons to 3.5 tons in U.S. from year 1999-2008.

# --- Explore data ---
# # Det. the num. of emission recorded per year by type
# NEI %>%
#   group_by(type, year) %>%
#   tally(sort = TRUE) %>% 
#   arrange(type, year)

# # Observe the initial few Pollutant readings
# NEI %>%
#   select(year, type, fips, NEI, Pollutant, Emissions) %>% 
#   arrange(type, year) %>% 
#   head(20)

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



# #--- actual observations
# Tot_PM25_atBmore_byYear_byPol <- NEI %>% 
#   filter(fips == "24510") %>% 
#   group_by(fips, type, year) %>% 
#   arrange(type, year)

# # Plot pollutant emissions at Baltimre, MD by source types over 10 years period ('99, '02, '05, '08)
# g <- ggplot(Tot_PM25_atBmore_byYear_byPol, aes(x=year, y=Emissions))
# g + geom_point(color = "steelblue") + geom_smooth(method = "lm") + facet_grid(. ~ type) + labs(x = "Year", y = expression("Total" ~ PM[2.5] ~ "Emissions (tons)"), title = expression("Baltimore" ~ PM[2.5] ~ "Emissions by Source Type & Year")) + theme_grey() + ggsave(filename = "plot3.png", scale = 2





# ==--Question 4 --==
# Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
# Ans: In general, emissions from coal combustion-related sources has decreased in years 1999-2008.  However, emissions from Point source has seen a decrease in years 1999-2008, while Non-Point source maintained relatively the same emission level.

# Using EI.Sector vector to filter for observations with coal combusion-related sources
tmp <- SCC %>% 
  filter(grepl('[Cc]oal', EI.Sector)) %>% 
  arrange(SCC, Data.Category)

# convert the source type variable [SCC] from Factor to String type
tmp2 <- tmp
tmp2[, "SCC"] <- sapply(tmp2[, "SCC"], as.character)

# Merge tmp2 with NEI dataset by [SCC] variable
tmp2_merge <- semi_join(NEI, tmp2, by = "SCC")
tmp2_merge <- tmp2_merge %>% 
  group_by(fips, type, year) %>% 
  arrange(type, year)


# Calculate emissions sum per year
Tot_PM25_Coal_byYear <- tmp2_merge %>% 
  # filter(grepl('[0123456789]', fips)) %>% 
  group_by(year) %>% 
  summarise(Emissions_sum = sum(Emissions, na.rm = TRUE)) %>% 
  arrange(year)
# Plot total pollutant emissions from coal combustion-related sources over years period ('99, '02, '05, '08)
g1 <- 
  ggplot(Tot_PM25_Coal_byYear, aes(x=year, y=Emissions_sum)) + 
# g1 + geom_line()
  geom_point(color = "steelblue") + 
  geom_smooth(method = "lm") + 
  labs(x = "Year", y = expression("Total" ~ PM[2.5] ~ "Emissions (tons)"), title = expression("Total Coal combustion-related" ~ PM[2.5] ~ "Emissions")) + 
  ggsave(filename = "plot4_Total.png")
print (g1)

# Calculate emissions sum per year by source type
Tot_PM25_Coal_byYear_bySrc <- tmp2_merge %>% 
  filter(grepl('[0123456789]', fips)) %>% 
  group_by(type, year) %>% 
  summarise(Emissions_sum = sum(Emissions, na.rm = TRUE)) %>% 
  arrange(type, year)
# Plot total pollutant emissions from coal combustion-related sources by source types over years period ('99, '02, '05, '08)
g2 <- 
  ggplot(data = Tot_PM25_Coal_byYear_bySrc, aes(x = year, y = Emissions_sum, group = type, colour = type)) + 
# geom_line() 
  geom_point(color = "steelblue") + 
  geom_smooth(method = "lm") + 
  facet_grid(. ~ type) + 
  labs(x = "Year", y = expression("Total" ~ PM[2.5] ~ "Emissions (tons)"), title = expression("Total Coal combustion-related" ~ PM[2.5] ~ "Emissions By Sources")) + 
  ggsave(filename = "plot4_ByType.png")
print(g2)

# Combining both plots (emissions sum per yr and emissions sum per yr per source) into single canvas
grid.arrange(g1, g2)
g3 <- arrangeGrob(g1, g2)
ggsave(filename = "plot4.png", g3)
# Using multiplot function
# g3 <- multiplot(g1, g2, cols = 2)
# print (g3)





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





# ==--Question 6 --==
# Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?
# Ans: Baltimore City saw the greatest change over time in motor vehicle emissions at -34% compared to Los Angeles County with +10%.

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












Std_PM25_Bmore_LA <- Tot_PM25_Bmore_LA %>% 
  # filter(fips = "24510") %>% 
  select(everything()) %>% 
  mutate(
    Emissions_std = ifelse(
      fips == "24510", 
      Emissions_sum / Emiss_1999_Bmore,
      0
    ),
    Emissions_std = ifelse(
      fips == "06037",
      Emissions_sum / Emiss_1999_LA,
      0
    )
  )


Std_PM25_Bmore_LA <- transform(
  Tot_PM25_Bmore_LA, 
  Emissions_std = ifelse(
    fips == "24510",
      Emissions_sum / Emiss_1999_Bmore,
      Emissions_sum / Emiss_1999_LA
    )
  )







myfile %>% mutate(
  V5 = ifelse(V1 == 1 & V2 != 4, 1, 0), 
  V5 = ifelse(V2 == 4 & V3 != 1, 2, V5)
)













tmp <- SCC %>%
  filter(
    (grepl('[Cc]oal*', Short.Name) | grepl('[Cc]omb', Short.Name)) & 
    (grepl('[Cc]oal*', SCC.Level.One) | grepl('[Cc]omb', SCC.Level.One))
  ) %>% 
  arrange(SCC, Data.Category)



tmp <- SCC %>%
  filter(
      grepl('Coal', SCC.Level.One) | 
      grepl('[Cc]omb*', SCC.Level.One) | 
      grepl('Coal', SCC.Level.Two) | 
      grepl('[Cc]omb*', SCC.Level.Two) | 
      grepl('Coal', SCC.Level.Three) | 
      grepl('[Cc]omb*', SCC.Level.Three) | 
      grepl('Coal', SCC.Level.Four) | 
      grepl('[Cc]omb*', SCC.Level.Four)
  ) %>% 
  # tally (sort = TRUE)
  arrange(SCC, Data.Category)


  tmp <- SCC %>%
  filter(
      grepl('[Cc]omb*', SCC.Level.One) | 
      grepl('[Cc]omb*', SCC.Level.Two) | 
      grepl('[Cc]omb*', SCC.Level.Three) | 
      grepl('[Cc]omb*', SCC.Level.Four)
  ) %>% 
  # tally (sort = TRUE)
  arrange(SCC, Data.Category)