#####################################################################
# Filename      : CourseProject1.R                                  #
# Author        : Robert Muwanga                                    #
# Purpose       : Generate 4 different kinds of plots in line with  #
#                 the instructions using the Electric Power         #
#                 Consumption dataset from the UC Irvine Machine    #
#                 Learning Repository                               #
# Last Updated  : 14 August 2021                                    #
#####################################################################

library(here)
setwd(here('Week1'))
options(scipen=999) # Remove scientific notation

### Download and extract source data

compressed_filename <- 'household_power_consumption.zip'
uncompressed_filename <- 'household_power_consumption.txt'
uri <- 'https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip'

if(!exists(compressed_filename)) {
  download.file(uri, compressed_filename)
}

if(!exists(uncompressed_filename)){
  unzip(compressed_filename)
}

### Load data into dataframe
column_classes <- c('character', 'character', rep('numeric', 7))
data <- read.delim(
  file = uncompressed_filename, 
  header= TRUE, 
  sep = ';', 
  na.strings = '?',
  colClasses = column_classes)

data$Date <- as.Date(data$Date, format='%d/%m/%Y')
data$Time <- format(strptime(data$Time, format = '%T'), '%T')
data$FullDateTime <- as.POSIXct(paste(data$Date, data$Time))

data <- subset(data, Date >= '2007-02-01' & Date <= '2007-02-02')

### Create plots

# Plot 1 - Global Active Power
plot1 <- function() { 
  hist(data$Global_active_power, 
       col= 'red', 
       main = 'Global Active Power', 
       xlab = 'Global Active Power (kilowatts)')
  
}

# Plot 2 - Global Active Power (kw) against Days
plot2 <- function() { 
  plot(x = data$FullDateTime, 
     y = data$Global_active_power, 
     type = 'l',
     xlab = '',
     ylab = 'Global Active Power (kilowatts)')
}

#Plot 3 - Sub_metering plot

plot3 <- function() { 
  plot(x = data$FullDateTime, 
       y = data$Sub_metering_1, 
       type = 'n',
       xlab = '',
       ylab = 'Energy sub metering')
  lines(y = data$Sub_metering_1, x = data$FullDateTime, col = 'black')
  lines(y = data$Sub_metering_2, x = data$FullDateTime, col = 'red')
  lines(y = data$Sub_metering_3, x = data$FullDateTime, col = 'blue')
  legend('topright', lty = 1,
         col = c('black', 'red', 'blue'), 
         legend = c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'))
}


#Plot4 - 2x2 image
plot4 <- function(x) {
  par(mfcol = c(2,2))
  plot2()
  plot3()
  
  plot(x = data$FullDateTime, 
       y = data$Voltage, 
       type = 'l',
       xlab = 'datetime',
       ylab = 'Voltage')
  
  plot(x = data$FullDateTime, 
       y = data$Global_reactive_power, 
       type = 'l',
       xlab = 'datetime',
       ylab = 'Global_reactive_power')
  
}


### Create images from plotting functions
number_of_functions <- 4
sapply(X = seq(number_of_functions), FUN = function(x) { 
  function_name <- paste('plot', x, sep = '')
  f <- match.fun(function_name)
  plot.new()
  png(paste(function_name, '.png', sep=''))
  f()
  dev.off()
})
