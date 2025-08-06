#Project 2
#Ryan Geisler
#Data set: "crime.csv"
#Questions
#  -Can patterns be identified in crime rates across different districts?
#  -Can clustering help pinpoint crime hotspots?
#  -What crimes are most prevalent in specific areas?


#load packagaes
install.packages("dplyr")
install.packages("ggplot2")
install.packages("purrr")
install.packages("tidyverse")
install.packages("corrplot")
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyverse)
library(corrplot)

#read in data set 
crime <- read.csv("crime.csv")
glimpse(crime)


#######################Exploratory Data Analysis###############################
crime %>%
  count(ward)

ward_crime_arrest_count <- crime %>%
  group_by(ward, arrest) %>%
  summarise(crime_count = n()) %>%
  arrange(ward)

#count of crimes with arrest status by Ward
ggplot(ward_crime_arrest_count, aes(x = factor(ward), y = crime_count, fill = as.factor(arrest))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Crime Count with Arrests by Ward", x = "Ward", y = "Crime Count", fill = "Arrested") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

crime %>% count(primary_type)

# distribution of crimes by district
district_crime_distribution <- crime %>%
  group_by(district) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  filter(district != 31)

#distribution of crimes by district
ggplot(district_crime_distribution, aes(x = factor(district), y = count)) +
  geom_bar(stat = "identity", fill = "lightcoral") +
  labs(title = "Distribution of Crimes by District", x = "District", y = "Crime Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#finding top 10 crimes
top_10_crimes <- crime %>%
  group_by(primary_type) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(10)

# filter data to include only the top 10 crime types
crime_data_top_10 <- crime %>%
  filter(primary_type %in% top_10_crimes$primary_type)

# find the top 10 locations with the most crimes
top_locations <- crime_data_top_10 %>%
  group_by(location_description) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(10)

# filter the data to include only the top locations
crime_data_top_10_locations <- crime_data_top_10 %>%
  filter(location_description %in% top_locations$location_description)

# count of primary type by location for top 10 crimes and locations
location_primary_type_distribution_top_10 <- crime_data_top_10_locations %>%
  group_by(location_description, primary_type) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

#relationship between location and the top 10 crime types
ggplot(location_primary_type_distribution_top_10, aes(x = reorder(location_description, count), y = count, fill = primary_type)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Crimes by Top 10 Locations", x = "Location Description", y = "Crime Count") +
  theme_minimal()

district_primary_type_distribution <- crime %>%
  group_by(district, primary_type) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>% 
  filter(district != 31)

# distribution of crimes by district
ggplot(district_primary_type_distribution, aes(x = factor(district), y = count, fill = primary_type)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Crime Types by District", x = "District", y = "Crime Count", fill = "Crime Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


crime %>% count(primary_type)

# categorize crimes by severity and group by district
crime_severity <- crime %>%
  mutate(severity = ifelse(primary_type %in% c("ASSAULT", "BATTERY", "HOMICIDE"), "High", "Low")) %>%
  group_by(district, severity) %>%
  summarise(count = n(), .groups = 'drop')%>%
  filter(district != 31)

# plot crime severity by district
ggplot(crime_severity, aes(x = factor(district), y = count, fill = severity)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Crime Severity by District", x = "District", y = "Crime Count", fill = "Severity") +
  theme_minimal() 



####################### Cleaning and Data Prep##################################

glimpse(crime$longitude)

crime_cluster <- crime %>% 
  select(longitude, latitude)
glimpse(crime_cluster)

crime_cluster <- crime_cluster %>%
  filter(!is.na(longitude) & !is.na(latitude)) %>%
  filter(longitude >= -91 & latitude >= 41.66)

summary(crime_cluster$longitude)
summary(crime_cluster$latitude)


#scaling data for clustering
crime_z <- as.data.frame(lapply(crime_cluster, scale))
glimpse(crime_cluster)

#elbow method to find to the K needed for clustering
crime_elbow <- map_dbl(1:10, function(k) {
  kmeans(crime_z, centers = k, nstart = 20)$tot.withinss})

plot(1:10, crime_elbow, type = "b", pch = 19,
     xlab = "Number of Clusters", ylab = "Within-Cluster Sum of Squares",
     main = "Elbow Method for Optimal k")

#######################Running the algorithm##################################
set.seed(4223)
crime_clusters <- kmeans(crime_z, 4 )

#looking at sizes of each cluster
crime_clusters$size
#cluster centers
crime_clusters$centers

#latitude by longitude, color by cluster
crime <- crime %>%
  filter(!is.na(longitude) & !is.na(latitude)) %>%
  filter(longitude >= -91 & latitude >= 41.66)

#putting clusters back in data
crime$cluster <- crime_clusters$cluster
glimpse(crime)

crime$cluster <- as.factor(crime$cluster)
crime[1:5,c("cluster","district","arrest")]

ggplot(crime, aes( x = longitude, y = latitude,  color = cluster)) + 
  geom_point()

#looking at crime clusters by district
crime_map <- crime %>%
  group_by(cluster, district) %>%
  summarise(lat = mean(latitude, na.rm = TRUE), lon = mean(longitude, na.rm = TRUE))

ggplot(crime_map, aes(x = lon, y = lat, color = as.factor(cluster))) +
  geom_point(size = 4) +
  labs(title = "Crime Clusters by District", x = "Longitude", y = "Latitude") +
  theme_minimal()


#comparing primary clusters and the top primary type of crime in that area
cluster_primaryType <- crime %>%
  group_by(cluster, primary_type) %>% 
  summarise(count = n()) %>%
  arrange(desc(count))%>%
  group_by(cluster) %>%
  top_n(1, count) %>%
  ungroup()

ggplot(cluster_primaryType, aes(x = cluster, y = count, fill = primary_type)) +
  geom_bar(stat = "identity") + 
  labs(title = "Top Crimes by Cluster", x = "Cluster", y = "Count of Primary Type") +
  theme_minimal()

#crime count by cluster
cluster_crime_count <- crime %>%
  group_by(cluster) %>%
  summarise(count = n())

ggplot(cluster_crime_count, aes(x = cluster, y = count, fill = as.factor(cluster))) +
  geom_bar(stat = "identity") + 
  labs(title = "Total Crime Count by Cluster", x = "Cluster", y = "Crime Count") +
  theme_minimal()

#calculate crime count per district within each cluster
cluster_district_crime <- crime %>%
  group_by(cluster, district) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(district != 31)
  

# distributions of districts crime in clusters 
ggplot(cluster_district_crime, aes(x = cluster, y = count, fill = as.factor(district))) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Crime Distribution by District Within Each Cluster", 
       x = "Cluster", y = "Crime Count by District", fill = "District") +
  theme_minimal() 

#crime type distribution by cluster
cluster_type_distribution <- crime %>%
  group_by(cluster, primary_type) %>%
  summarise(count = n()) 

ggplot(cluster_type_distribution, aes(x = cluster, y = count, fill = primary_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Crime Type Distribution Across Clusters", x = "Cluster", y = "Count of Crimes", fill = "Primary Type") +
  theme_minimal()


# calculate crime rate per cluster ###STILL NEED TO WORK on, add district if needed
cluster_crime_rate <- crime %>%
  group_by(cluster) %>%
  summarise(count = n()) %>%
  mutate(crime_rate = count / max(count)) # normalize by max crime count

ggplot(cluster_crime_rate, aes(x = cluster, y = crime_rate, fill = as.factor(cluster))) +
  geom_bar(stat = "identity") + 
  labs(title = "Crime Rate per Cluster", x = "Cluster", y = "Crime Rate (Normalized)") +
  theme_minimal()


# Crime type heatmap by cluster #STILL NEED TO WORK ON###################
# create the crime count by cluster and primary type
cluster_crime_heatmap <- crime %>%
  group_by(cluster, primary_type) %>%
  summarise(count = n()) %>%
  spread(key = primary_type, value = count, fill = 0)

# put into long format
cluster_crime_heatmap_long <- cluster_crime_heatmap %>%
  gather(key = "primary_type", value = "count", -cluster)

# plot the heatmap
ggplot(cluster_crime_heatmap_long, aes(x = cluster, y = primary_type)) +
  geom_tile(aes(fill = count), color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Heatmap of Crime Types by Cluster", x = "Cluster", y = "Primary Type", fill = "Crime Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# calculate the total crime count by cluster and district 
#crime rate is proportional to the number of districts, 
#which shows the intensity of crime across different clusters.
cluster_crime_rate <- crime %>%
  group_by(cluster, district) %>%
  summarise(count = n()) %>%
  # sum crimes per cluster by district
  group_by(cluster) %>%
  summarise(total_crimes = sum(count), total_districts = n_distinct(district)) %>%
  # calculate the crime rate as total crimes per district
  mutate(crime_rate = total_crimes / total_districts)

# plot the crime rate per cluster
ggplot(cluster_crime_rate, aes(x = cluster, y = crime_rate, fill = as.factor(cluster))) +
  geom_bar(stat = "identity") + 
  labs(title = "Crime Rate per Cluster",
       x = "Cluster", y = "Crime Rate (normalized)",
       fill = "Cluster") +
  theme_minimal()





