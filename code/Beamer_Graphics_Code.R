###############################################################################
#                                                                              #
#          Code for generating the plots for the beamer presentation           #
#                                                                              #
################################################################################

# ---------------------- Line Chart of the Fires Per Year ----------------------

# Read in the data
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
setwd("C:/Users/Mark/Dropbox/Graduate School/05) Courses/Stats 551/Final Project/551FinalProject/code")
data <- read.csv("../data/school_fire_cases_1998_2014.csv")

# Spread operations to set it up for plotting
data <- data %>% 
  select(Municipality, Cases, Year) %>% 
  tidyr::spread(Year, Cases) %>% 
  select(-Municipality) %>%
  colSums(na.rm = T) %>% 
  data.frame() 

# Tidying of the names
data$year = rownames(data)
colnames(data) <- c("Fires", "Year")

# Create the plot
ggplot(data) + 
  geom_bar(aes(Year, Fires), stat = "Identity", fill = "Red") + 
  ggtitle("Number of School Fires in Sweden Per Year", "Min = 120, Max = 270, Median = 167") +
  geom_text(aes(Year, Fires, label=Fires), nudge_y = 10)

# Save it down
ggsave("FiresPerYear.pdf", device = "pdf", path = "../doc/", width = 7.5, height = 4)
