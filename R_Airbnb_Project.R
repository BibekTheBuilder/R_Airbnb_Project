# Team A, MIS 761, Spring 2023
# Bibek Satyal, William Houston, Mahesh Sangishetty


####################
# DEPENDENCIES

# Install packages
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("leaflet")
install.packages("leaflet.extras")

# Load libraries
library(dplyr)
library(ggplot2)
library(ggpubr)
library(leaflet)
library(leaflet.extras)


####################
# FUNCTIONS

# Draw density map (default), or heat map of prices
draw_map = function(df, radius=25, heatmap=FALSE) {
  map = leaflet(df) %>% 
    addTiles() %>%
    fitBounds(min(df$longitude), min(df$latitude), 
              max(df$longitude), max(df$latitude))
  if (heatmap) 
    map <- map %>% addHeatmap(~longitude, ~latitude, radius=radius, intensity = ~price)
  else 
    map <- map %>% addHeatmap(~longitude, ~latitude, radius=radius)
  return(map)
}


####################
# DATA PREP

# Load data from file
listings = read.csv("R_Airbnb_Project_Datatset.csv", header = TRUE)

# Remove dollar signs and commas from price and convert to numeric
listings$price = as.numeric(gsub("\\$|,", "", listings$price))

# Select only relevant columns
listings_new = listings %>% select(latitude, longitude, price)

# Add new columns calculating distances
listings_new$dist_strip = acos(sin(36.112950 * pi/180) * sin(listings_new$latitude * pi/180) +
                          cos(36.112950 * pi/180) * cos(listings_new$latitude * pi/180) *
                          cos((-115.174246 - listings_new$longitude)* pi/180)) * 6371

listings_new$dist_fremont = acos(sin(36.170754 * pi/180) * sin(listings_new$latitude * pi/180) +
                            cos(36.170754 * pi/180) * cos(listings_new$latitude * pi/180) *
                            cos((-115.144030 - listings_new$longitude)* pi/180)) * 6371

# Calculate distance to NEAREST tourism center
listings_new$distance <- pmin(listings_new$dist_strip, listings_new$dist_fremont)

# Check missing data
sum(is.na(listings_new))

# Drop listings outside Las Vegas cluster (Mesquite, Boulder City, etc)
listings_filtered = listings_new %>% filter(listings_new$distance <= 28)

# Filter outliers
Q1_price  = quantile(listings_filtered$price, 0.25)
Q3_price  = quantile(listings_filtered$price, 0.75)
IQR_price = Q3_price - Q1_price
listings_filtered = subset(listings_filtered, price > Q1_price - 1.5 * IQR_price & price < Q3_price + 1.5 * IQR_price)

# Create bins for distance intervals
listings_filtered <- listings_filtered %>% mutate(within=ceiling(distance))
listings_filtered$within <- factor(listings_filtered$within, levels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,
                                                                      19,20,21,22,23,24,25,26,27,28))


####################
# GRAPHS

# Draw density map of Nevada listings
draw_map(listings_new, 8)

# Display descriptive statistics, with outliers
nrow(listings_new)
summary(listings_new)

# Display descriptive statistics, outliers filtered
nrow(listings_filtered)
summary(listings_filtered)

# Draw histograms of Price and Distance
hist(listings_filtered$price, xlab="Price", main=NULL)
hist(listings_filtered$distance, xlab="Distance (km)", main=NULL)

# Scatter plot showing relationship between price and distance
ggscatter(listings_filtered, x = "distance", y = "price", add = "reg.line", conf.int = TRUE, cor.coef = FALSE, 
          cor.method = "pearson") + geom_smooth(method = lm, formula = y ~ x, color = "red", se = FALSE) + 
          labs(x = "Distance (km)", y = "Price")

# Draw binned box plots of Price vs Distance interval
ggplot(listings_filtered,aes(y=price,x=within)) + geom_jitter(shape=16, position=position_jitter(0.2), 
    color="lightgray") + geom_boxplot(fill=NA) + labs(y='Price', x='Distance (km)')

# Draw heat map of prices
draw_map(listings_filtered, 8, TRUE)


####################
# ANALYSIS

# Calculate the correlation
cor.test(listings_filtered$price, listings_filtered$distance)

# Run regression model and plot residuals
regression = lm(price ~ distance, data = listings_filtered)
summary(regression)
plot(regression, ask=FALSE)
