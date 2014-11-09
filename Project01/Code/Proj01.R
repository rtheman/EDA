# Project 01
# <URL: https://class.coursera.org/exdata-008/human_grading/view/courses/972597/assessments/3/submissions>
# Last modified on Thurs, 06Nov2014 by Rich Leung

library(ggplot2)

# Initializing working directory
WD = "/Users/richleung/Dropbox/Projects/Coursera/Exploratory Data Analysis/Project01/Input"
# WD = "C:/Users/rich.leung.000/Documents/My Projects/Coursera/Exploratory Data Analysis/Project01/Proj01_RLeung/Code"
setwd(WD)





# Import raw data file
# raw_data <- read.table("C:/Users/rich.leung.000/Documents/My Projects/Coursera/Exploratory Data Analysis/Project01/Proj01_RLeung/Data/household_power_consumption.txt", sep=";", header=TRUE)
raw_data <- read.table("household_power_consumption.txt", sep=";", header=TRUE)





# Create new data frame "raw_data2" with combined Date and Time
raw_data$DateTime = paste(raw_data$Date, raw_data$Time, sep = " ")

# Transform to Date-Time format
raw_data$DateTime_fmt <- strptime(raw_data$DateTime, format="%d/%m/%Y %H:%M:%S")

# Add Day based on given date
raw_data$Day <- weekdays(raw_data$DateTime_fmt)

# Organize columns
raw_data <- raw_data[, c(1,2,10,11,12,3,4,5,6,7,8,9)]

# 
# # Transform to Date type [as.Date]
# raw_data$Date <- as.Date(raw_data$Date, "%d/%m/%Y")
# # Transform to Time type [strptime]
# raw_data$Time <- strptime(raw_data$Time, "%H:%M:%S")
# # Combine Date and Time as a Date-Time object
# raw_data$DateTime <- as.POSIXct(paste(raw_data$Date, raw_data$Time), format="%d/%m/%Y %H:%M:%S")





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
# -- [Plot 1] Distribution of GLobal Active Power during 2 day period (Feb 01-02 2007)
hist(Training_Set$Global_active_power, 
      main="Global Active Power", 
      xlab="Global Active Power (kilowatt)",
      ylab="Frequency", 
      col="red")
# # using ggplot2
# g <- ggplot(Training_Set, aes(x=Global_active_power))
# g + geom_histogram(fill = "red")

# -- [Plot 2] Global Active Power between Feb 01-02 2007
plot(Training_Set$DateTime_fmt, Training_Set$Global_active_power, type = "l", main="", xlab="", ylab="Global Active Power (kilowatts)")
# using ggplot2
# g <- ggplot(Training_Set, aes(x=Time, y=Global_active_power))
# g + geom_line(color = "black")
#   + labs(y = "Global Active Power kilowatts")

# -- [Plot 3] Energy Sub Metering 1 - 3 between Feb 01-02 2007
plot(Training_Set$DateTime_fmt, Training_Set$Sub_metering_1, type = "l", main="", xlab="", ylab="Energy sub metering", col="black")
lines(Training_Set$DateTime_fmt, Training_Set$Sub_metering_2, col = "red")
lines(Training_Set$DateTime_fmt, Training_Set$Sub_metering_3, col = "blue")
legend("topright", pch = 1, col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

# -- [Plot 4] Energy panel illustration
par(mfrow = c(1,3), mar = c(4,4,2,1), oma = c(0,0,2,0))
with(Training_Set, {
  plot(Training_Set$DateTime_fmt, Training_Set$Global_active_power, type = "l", main="", xlab="", ylab="Global Active Power (kilowatts)")
  
  plot(Training_Set$DateTime_fmt, Training_Set$Voltage, type = "l", main = "", xlab="datetime", ylab = "Voltage")
  
  plot(Training_Set$DateTime_fmt, Training_Set$Sub_metering_1, type = "l", main="", xlab="", ylab="Energy sub metering", col="black")
  lines(Training_Set$DateTime_fmt, Training_Set$Sub_metering_2, col = "red")
  lines(Training_Set$DateTime_fmt, Training_Set$Sub_metering_3, col = "blue")
  legend("topright", pch = 1, col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  
  plot(Training_Set$DateTime_fmt, Training_Set$Global_reactive_power, type = "l", main="", xlab="", ylab="Global_Reactive_power")
})



# References
length(Training_Set$Global_active_power)
str(Training_Set$Global_active_power)
class(Training_Set$Global_active_power_n)
names(Training_Set$Global_active_power)