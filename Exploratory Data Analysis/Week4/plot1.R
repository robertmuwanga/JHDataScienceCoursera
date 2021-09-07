#####################################################################
# File name     : plot1.R                                           #
# Author        : Robert Muwanga                                    #
# Purpose       : Script for generating 'plot1.png'                 #
# Last Updated  : 09 September 2021                                 #
#                                                                   #
# Assignment question:                                              #
# Have total emissions from PM2.5 decreased in the United States    #
# from 1999 to 2008? Using the base plotting system, make a plot    #
# showing the total PM2.5 emissions from all sources for each of    #
# the years 199, 2002, 2005 and 2008.                               #
#####################################################################

### Install packages ----------------------------------------------------------

packages <- c('tidyverse', 'here', 'scales')
missed_packages <- which(!(packages %in% installed.packages()))

install.packages(missed_packages) # Install any missing packages
purrr::walk(packages, 
            function(package) library(package, character.only = TRUE))


### Download, extract load source data  ---------------------------------------
data_url <- 'https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip'
data_dir <- here('data')
data_name <- here(data_dir, 'FNEI_data.zip')

if(!dir.exists(data_dir)) {
  dir.create(data_dir)
}

if(!file.exists(data_name)) {
  download.file(url = data_url, destfile = data_name) # Download
}

unzip(zipfile = data_name, exdir = data_dir) # Extract

data <- sapply(X = grep('rds', dir(data_dir), value = TRUE), #Load
               FUN = function(data_source) 
                 readRDS(here(data_dir, data_source)))


### Assignment ----------------------------------------------------------------
options(scipen = 100) # Remove scientific notation

dplot1 <- data$summarySCC_PM25.rds %>% 
  group_by(year) %>% 
  summarise(total = sum(Emissions)) %>% ungroup()

# Plot graph and save in png file.
png(filename = 'plot1.png')
plot(dplot1$year, dplot1$total, type = 'o', 
     main = 'Total Emissions in the United States 1999 - 2008', 
     xlab = 'Year', ylab = expression(~ PM[2.5] ~ 'Emissions (in tons)'))

dev.off()
