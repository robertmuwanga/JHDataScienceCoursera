#####################################################################
# File name     : plot4.R                                            #
# Author        : Robert Muwanga                                    #
# Purpose       : Script for generating 'plot4.png'                 #
# Last Updated  : 09 September 2021                                 #
#                                                                   #
# Assignment question:                                              #
# Across the United States, how have emissions from coal            #
# combustion-related sources changed from 1999–2008?                #
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
# Identify which Classifications are based off coal.
classifications <- 
  data$Source_Classification_Code.rds$SCC[
    grepl(pattern = 'coal',
        x = data$Source_Classification_Code.rds$Short.Name,
        ignore.case = TRUE)]

classifications <- unique(classifications)

# Filter summary data based off classifications
coal_data <- data$summarySCC_PM25.rds[
  classifications %in% data$summarySCC_PM25.rds$SCC, ] %>% 
  group_by(year, type) %>%
  summarize(total_emissions = sum(Emissions, na.rm = TRUE)) %>% 
  ungroup()

# Plot data

ggplot(coal_data, aes(x = factor(year), y = total_emissions, fill = type)) + 
 geom_bar(stat = 'identity', position = 'dodge') + 
  labs(
    title = 'The US Emissions from Coal Combustion-related Sources', 
    x = '',
    y = expression(~PM[2.5]~ "Emissions (in tons)"), 
    subtitle = 'Data from 1999- 2008') + 
  theme_classic(base_size = 10) + 
  guides(color=guide_legend(title = "Type", 
                            title.theme = element_text(face = 'bold'))) +
  theme(plot.title = element_text(size = 12, face = 'bold'),
        plot.subtitle = element_text(size = 10))

ggsave('plot4.png')
