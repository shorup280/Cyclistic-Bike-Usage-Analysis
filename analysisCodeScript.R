# Load required libraries
library(tidyverse)   # Loads a collection of packages for data manipulation and visualization
library(conflicted)  # Resolves function name conflicts between packages
conflict_prefer("filter", "dplyr")  # Prioritize dplyr's filter function
conflict_prefer("lag", "dplyr")     # Prioritize dplyr's lag function
library(readxl)      # Load the package for reading Excel files

# Load data from two Excel files for 2019 and 2020
Divvy_2019 <- read_excel("workingDataset/Divvy_2019.xlsx")
View(Divvy_2019)  # View the data from Divvy_2019 in RStudio

Divvy_2020 <- read_excel("workingDataset/Divvy_2020.xlsx")
View(Divvy_2020)  # View the data from Divvy_2020 in RStudio

# Display column names to understand the structure of the datasets
colnames(Divvy_2019)
colnames(Divvy_2020)

# Rename columns to standardize names across the datasets
Divvy_2019 <- rename(Divvy_2019
                     ,ride_id = trip_id  # Rename trip_id to ride_id
                     ,rideable_type = bikeid  # Rename bikeid to rideable_type
                     ,started_at = start_time  # Rename start_time to started_at
                     ,ended_at = end_time  # Rename end_time to ended_at
                     ,start_station_name = from_station_name  # Rename from_station_name to start_station_name
                     ,start_station_id = from_station_id  # Rename from_station_id to start_station_id
                     ,end_station_name = to_station_name  # Rename to_station_name to end_station_name
                     ,end_station_id = to_station_id  # Rename to_station_id to end_station_id
                     ,member_casual = usertype  # Rename usertype to member_casual
)

# Check the structure of the datasets
str(Divvy_2019)
str(Divvy_2020)

# Convert certain columns to character type for consistency
Divvy_2019 <- mutate(Divvy_2019, ride_id = as.character(ride_id)
                     ,rideable_type = as.character(rideable_type))

# Combine the two datasets (2019 and 2020) into one big dataset
all_trips <- bind_rows(Divvy_2019, Divvy_2020)
View(all_trips)  # View the combined data

# Remove unnecessary columns from the data
all_trips <- all_trips %>%
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, "tripduration"))

# Check column names, number of rows, dimensions, and structure of the combined data
colnames(all_trips)
nrow(all_trips)
dim(all_trips)
head(all_trips)
str(all_trips)
summary(all_trips)

# Create a frequency table for the 'member_casual' column (number of members and casual users)
table(all_trips$member_casual)

# Recode member_casual: "Subscriber" becomes "member" and "Customer" becomes "casual"
all_trips <- all_trips %>%
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))

# Create new date-related columns
all_trips$date <- as.Date(all_trips$started_at)  # Convert started_at to Date type
all_trips$month <- format(as.Date(all_trips$date), "%m")  # Extract the month
all_trips$day <- format(as.Date(all_trips$date), "%d")  # Extract the day
all_trips$year <- format(as.Date(all_trips$date), "%Y")  # Extract the year
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")  # Extract the day of the week

# Calculate the ride length by finding the time difference between start and end times
all_trips$ride_length <- difftime(all_trips$ended_at, all_trips$started_at)
str(all_trips)

# Check if 'ride_length' is a factor, then convert it to numeric for calculations
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

# Remove rows where the start station is "HQ QR" or ride_length is negative
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

# Get a summary of ride_length after cleaning
summary(all_trips_v2$ride_length)

# Calculate the average ride length for each member type (casual or member)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# Calculate the average ride length for each member type by day of the week
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week,
          FUN = mean)

# Create a summary table of number of rides and average ride duration by weekday and member type
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%  # Create 'weekday' column using started_at
  group_by(member_casual, weekday) %>%  # Group by member type and weekday
  summarise(
    number_of_rides = n(),  # Calculate number of rides
    average_duration = mean(ride_length)  # Calculate average ride duration
  ) %>%
  arrange(member_casual, weekday)  # Sort by member type and weekday

# Create a bar plot to visualize the number of rides by weekday and member type
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%  # Create 'weekday' column
  group_by(member_casual, weekday) %>%  # Group by member type and weekday
  summarise(number_of_rides = n(),  # Calculate number of rides
            average_duration = mean(ride_length)) %>%  # Calculate average duration
  arrange(member_casual, weekday) %>%  # Sort data
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +  # Plot number of rides
  geom_col(position = "dodge")  # Create dodged bar chart

# Create a bar plot to visualize average ride duration by weekday and member type
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%  # Create 'weekday' column
  group_by(member_casual, weekday) %>%  # Group by member type and weekday
  summarise(number_of_rides = n(),  # Calculate number of rides
            average_duration = mean(ride_length)) %>%  # Calculate average ride duration
  arrange(member_casual, weekday) %>%  # Sort data
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +  # Plot average duration
  geom_col(position = "dodge")  # Create dodged bar chart

# Calculate average ride length for each member type and day of the week, and save the result to a CSV file
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual +
                      all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = 'avg_ride_length.csv')  # Save the output as a CSV file


# Use mutate to create a new column 'weekday' based on the 'started_at' column, which represents the day of the week
# The label = TRUE argument returns the weekday as a label (e.g., Mon, Tue, etc.)
all_trips_v2 %>%  
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  
  # Group the data by 'member_casual' and 'weekday' to calculate metrics for each group
  group_by(member_casual, weekday) %>%
  
  # Summarise the data by calculating the number of rides and the average ride duration for each group
  summarise(number_of_rides = n(),   # Count the number of rides for each group
            average_duration = mean(ride_length)) %>%   # Calculate the average ride duration for each group
  
  # Arrange the data in the order of 'member_casual' and 'weekday' for better readability
  arrange(member_casual, weekday) %>%
  
  # Create a bar chart using ggplot
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  
  # Add a column geometry for the bar chart with the "dodge" position for side-by-side bars
  geom_col(position = "dodge") +
  
  # Add labels for the chart
  labs(
    title = "Average Ride Duration by User Type and Weekday",   # Set the title of the chart
    x = "Weekday",   # Label for the x-axis
    y = "Average Duration (seconds)",   # Label for the y-axis
    fill = "User Type"   # Label for the legend
  ) +
  
  # Customize the colors in the chart for different user types
  scale_fill_manual(values = c("member" = "blue", "casual" = "green")) +
  
  # Apply a minimal theme for a clean and simple look
  theme_minimal()


