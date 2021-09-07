#####################################################################
# File name     : plot3.R                                           #
# Author        : Robert Muwanga                                    #
# Purpose       : Script for generating 'plot3.png'                 #
# Last Updated  : 09 September 2021                                 #
#                                                                   #
# Assignment question:                                              #
# Of the four types of sources indicated by the type (point,        # 
# nonpoint, onroad, nonroad) variable, which of these four sources  # 
# have seen decreases in emissions from 1999–2008 for Baltimore     #
# City? Which have seen increases in emissions from 1999–2008?      #
# Use the ggplot2 plotting system to make a plot answer this        #
# question.                                                         #
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

data$summarySCC_PM25.rds %>% 
  filter(fips == '24510') %>% 
  group_by(year, type) %>% 
  summarize(total = sum(Emissions, na.rm = TRUE)) %>% ungroup() %>%
  ggplot(aes(year, total, color = type)) + geom_point() + 
  geom_smooth(aes(color = type)) + 
  labs(title = 'Rates of Emissions of Baltimore City by Type of Source',
       caption = 'Time period: 1999 - 2008', 
       x = '', 
       y = expression(~PM[2.5]~ "Emissions (in tons)")) + 
  theme_classic() + 
  theme(text=element_text(size=10,  family="Cambria"))

ggsave('plot3.png')