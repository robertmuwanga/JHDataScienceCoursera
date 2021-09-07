#####################################################################
# File name     : plot5.R                                            #
# Author        : Robert Muwanga                                    #
# Purpose       : Script for generating 'plot5.png'                 #
# Last Updated  : 09 September 2021                                 #
#                                                                   #
# Assignment question:                                              #
# How have emissions from motor vehicle sources changed from        #
# 1999–2008 in Baltimore City                                       #
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
options(scipen = n)

# Assuming data with the feature Data.Category as 'Onroad' in 
# Source_Classification_Code as motor vehicle data

classifications <- data$Source_Classification_Code.rds %>% 
  select(SCC, Data.Category) %>% 
  filter(Data.Category == 'Onroad') %>% 
  .$SCC

onroad_data <- data$summarySCC_PM25.rds[
  classifications %in% data$summarySCC_PM25.rds$SCC, ]

# Filter out data that has variable 'type' as 'ON-ROAD' and summarize emissions
onroad_data <- onroad_data %>%
  filter(type == 'ON-ROAD', fips == "24510") %>% 
  select(Emissions, type, year) %>%
  group_by(year, type) %>% 
  summarize(total_emissions = sum(Emissions)) %>% 
  ungroup()

# Plot data
ggplot(onroad_data, aes(year, total_emissions)) + 
  geom_point() + 
  geom_line() + 
  labs(
    title = 'Baltimore City, Maryland Emissions from Motor Vehicles', 
    x = '',
    y = expression(~PM[2.5]~ "Emissions (in tons)"), 
    subtitle = 'Data from 1999- 2008') + 
  theme_classic(base_size = 10) + 
  guides(color=guide_legend(title = "Type", 
                            title.theme = element_text(face = 'bold'))) +
  theme(plot.title = element_text(size = 12, face = 'bold'),
        plot.subtitle = element_text(size = 10))

ggsave('plot5.png')
