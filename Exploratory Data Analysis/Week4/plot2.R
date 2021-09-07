#####################################################################
# File name     : plot2.R                                           #
# Author        : Robert Muwanga                                    #
# Purpose       : Script for generating 'plot2.png'                 #
# Last Updated  : 09 September 2021                                 #
#                                                                   #
# Assignment question:                                              #
# Have total emissions from PM2.5 decreased in Baltimore City,      #
# Maryland from 1999 - 2008? Use the base plotting system.          #
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

dplot2 <- data$summarySCC_PM25.rds %>% 
  select(fips, year, Emissions) %>% 
  filter(fips == '24510') %>% 
  group_by(year) %>% 
  summarize(total = sum(Emissions)) %>% 
  ungroup()

# Plot graph and save in png file.
png(filename = 'plot2.png')
barplot(dplot2$total ~ dplot2$year, 
        xlab = '', 
        ylab = expression(~ PM[2.5] ~ 'Emissions (in tons)'), 
        main = 'Total Emissions in the Baltimore City, Maryland \n1999 - 2008')
dev.off()