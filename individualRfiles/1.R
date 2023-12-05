library(readr)
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(dplyr)
accidents_data <- read.csv("~/Desktop/probbability/mini_research/archive/Accidents.csv")
bikers_data <- read.csv("~/Desktop/probbability/mini_research/archive/Bikers.csv")

missing_rows <- anti_join(bikers_data, accidents_data, by = "Accident_Index")

bikers_data <- filter(bikers_data, !Accident_Index %in% missing_rows$Accident_Index)

selected_col <- cbind(accidents_data[, c("Accident_Index",
                                     "Date",
                                     "Time",
                                     "Day",
                                     "Road_type",
                                     "Speed_limit",
                                     "Road_conditions",
                                     "Weather_conditions",
                                     "Light_conditions")],
                  bikers_data[, "Severity"])

colnames(selected_col)[ncol(selected_col)] <- "Severity"

selected_col_num <- cbind(accidents_data[, c("Accident_Index",
                                             "Date",
                                             "Time",
                                             "Day",
                                             "Speed_limit")])

Road_type_list <- c("Slip road", "One way sreet", "Single carriageway", "Dual carriageway", "Roundabout", "Unknown")
Road_conditions_list <- c("Dry", "Wet", "Snow", "Frost", "Flood", "Missing Data")
Weather_conditions_list <- c("Clear", "Clear and windy", "Fog", "Rain", "Rain and windy", "Snow", "Snow and windy", "Other", "Unknown", "Missing data")
Light_conditions_list <- c("Daylight", "Darkness lights lit", "Darkness no lights")
Severity_list <- c("Slight", "Serious", "Fatal")

convert_to_numeric <- function(selected_col, column_name, list) {
  unique_values <- unique(selected_col[[column_name]])
  
  name_numeric <- paste0(column_name, "_num")
  cat("Unique values in ", column_name, ":\n")
  k <- 1
  for (i in unique_values) {
    cat(unique_values[k], " = ", k, "\n")
    k <- k + 1
  }
  
  selected_col <- selected_col %>%
    mutate(!!name_numeric := match(selected_col[[column_name]], unique_values))
  
  return(selected_col %>% select(!!name_numeric))
}

# Applying the function to each column
selected_col_num <- cbind(selected_col_num, convert_to_numeric(selected_col, "Road_type"))
selected_col_num <- cbind(selected_col_num, convert_to_numeric(selected_col, "Road_conditions"))
selected_col_num <- cbind(selected_col_num, convert_to_numeric(selected_col, "Weather_conditions"))
selected_col_num <- cbind(selected_col_num, convert_to_numeric(selected_col, "Light_conditions"))
selected_col_num <- cbind(selected_col_num, convert_to_numeric(selected_col, "Severity"))

# Assuming selected_col is your data frame
library(ggplot2)

# Group by Speed_limit and calculate the mean Severity for each group
speed_severity_means <- selected_col_num %>%
  group_by(Speed_limit) %>%
  summarise(mean_severity = mean(Severity_num, na.rm = TRUE))

# Create the plot
ggplot(speed_severity_means, aes(x = Speed_limit, y = mean_severity)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Comparison of Speed Limits and Mean Severity",
       x = "Speed Limit",
       y = "Mean Severity") +
  theme_minimal() +
  coord_cartesian(ylim = c(0.5, 2), xlim = c(0, 61))


View(selected_col_num)


Severity_list <- c("Slight", "Serious", "Fatal")

convert_to_numeric <- function(selected_col, column_name, value_list) {
  unique_values <- unique(selected_col[[column_name]])
  
  name_numeric <- paste0(column_name, "_num")
  cat("Unique values in ", column_name, ":\n")
  
  # Print custom mapping
  for (i in seq_along(value_list)) {
    cat(value_list[i], " = ", i, "\n")
  }
  
  selected_col <- selected_col %>%
    mutate(!!name_numeric := match(selected_col[[column_name]], value_list))
  
  return(selected_col %>% select(!!name_numeric))
}

selected_col_num <- cbind(selected_col_num, convert_to_numeric(selected_col, "Severity", Severity_list))

