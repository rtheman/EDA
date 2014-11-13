# Project 01
# <URL: https://class.coursera.org/exdata-008/human_grading/view/courses/972597/assessments/3/submissions>
# Last modified on Sat, 08Nov2014 by Rich Leung

library(datasets)
library(ggplot2)

# Initializing working directory
WD = "/Users/richleung/Dropbox/Projects/Coursera/EDA_local/Project01/Input"
setwd(WD)





# Import raw data file
raw_data <- read.table("household_power_consumption.txt", sep=";", header=TRUE)





# Create new data frame "raw_data2" with combined Date and Time
raw_data$DateTime = paste(raw_data$Date, raw_data$Time, sep = " ")

# Transform to Date-Time format
raw_data$DateTime_fmt <- strptime(raw_data$DateTime, format="%d/%m/%Y %H:%M:%S")

# Add Day based on given date
raw_data$Day <- weekdays(raw_data$DateTime_fmt)

# Organize columns
raw_data <- raw_data[, c(1,2,10,11,12,3,4,5,6,7,8,9)]





# Subset imported data set [raw_data] for Date = 2007-02-01 and 2007-02-02
Training_Set <- subset(raw_data, DateTime_fmt >= as.POSIXct('2007-02-01 00:00:00') & DateTime_fmt <= as.POSIXct('2007-02-02 23:59:00'))

# Transform [Global_active_power] from Factor type to Numeric type
Training_Set$Global_active_power <- as.numeric(as.character(Training_Set$Global_active_power))
Training_Set$Global_reactive_power <- as.numeric(as.character(Training_Set$Global_reactive_power))
Training_Set$Voltage <- as.numeric(as.character(Training_Set$Voltage))
Training_Set$Global_intensity <- as.numeric(as.character(Training_Set$Global_intensity))
Training_Set$Sub_metering_1 <- as.numeric(as.character(Training_Set$Sub_metering_1))
Training_Set$Sub_metering_2 <- as.numeric(as.character(Training_Set$Sub_metering_2))
Training_Set$Sub_metering_3 <- as.numeric(as.character(Training_Set$Sub_metering_3))





# Time to draw
# -- [Plot 2] Global Active Power between Feb 01-02 2007
plot(Training_Set$DateTime_fmt, Training_Set$Global_active_power, type = "l", main="", xlab="", ylab="Global Active Power (kilowatts)")
dev.copy(png, file = "plot2.png")
dev.off()
# using ggplot2
# g <- ggplot(Training_Set, aes(x=Time, y=Global_active_power))
# g + geom_line(color = "black")
#   + labs(y = "Global Active Power kilowatts")