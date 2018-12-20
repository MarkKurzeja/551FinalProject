install.packages("maps", dependencies = TRUE, repos = "https://cloud.r-project.org")
install.packages("ggmap", dependencies = TRUE)
library(maps)
library(ggmap)
library(tidyr)
library(dplyr)

setwd("C:/Users/Teerth/Downloads/551FinalProject-master/data")

joint_data <- readxl::read_xlsx("joint_data_table.xlsx", col_names = TRUE)

longitude <- c(max(joint_data$longitude) + 1, min(joint_data$longitude) - 1)
latitude <- c(max(joint_data$latitude) + 1, min(joint_data$latitude) - 1)

Sweden <- get_googlemap(center = c(lon = mean(longitude), lat = mean(latitude)), zoom = 5)

ggmap(Sweden)                        

totals <- joint_data[40:51] %>% rowSums() 
joint_data_new <- cbind(joint_data, totals)

heatmap <- ggmap(Sweden, extent = "device") + 
  geom_point(aes(x = longitude, y = latitude, size = totals), colour = "red", 
  alpha = 0.2, data = joint_data_new) +
  scale_color_gradient(totals)

joint_data_stockholm <- joint_data_new %>% 
  filter(longitude > 16, longitude < 20, latitude > 58.5, latitude < 60.5)

Stockholm <- get_googlemap("Stockholm", zoom = 8)

ggmap(Stockholm) +
  geom_point(aes(x = longitude, y = latitude, color = totals), size = 4, 
             alpha = .6, data = joint_data_stockholm) +
  scale_color_gradientn(colors = RColorBrewer::brewer.pal(7, "RdYlBu"))
