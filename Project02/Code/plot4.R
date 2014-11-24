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





# ==--Question 4 --==
# Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
# Ans: In general, emissions from coal combustion-related sources has decreased in years 1999-2008.  However, emissions from Point source has seen a decrease in years 1999-2008, while Non-Point source maintained relatively the same emission level.

# Using EI.Sector vector to filter for observations with coal combusion-related sources
tmp <- SCC %>% 
  filter(grepl('Coal', EI.Sector)) %>% 
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