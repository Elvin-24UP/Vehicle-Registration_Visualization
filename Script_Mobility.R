library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

#For accessing it in a different system, use the data location accordingly
excel_file_path <- "D:\\MCP\\Sem3\\Internship Documents\\Work\\Week 1\\ER - Trends in Mobility 12May25.xlsx"

# Reading "only" the "combined" sheet from the excel file. Note that the sheets may be changed too!
mobility_data_raw <- read_excel(excel_file_path, sheet = "Ahmedabad")

# Displaying the first few rows and column names to understand the structure of the database
print("First few rows of the raw data:")
print(head(mobility_data_raw))
print("Column names in the raw data:")
print(colnames(mobility_data_raw))

# Select and Filter Data
required_cols <- c("fuel_rec", "Sum of n_regis", "Year", "veh_categ_rec2", "rto_code")
missing_cols <- setdiff(required_cols, colnames(mobility_data_raw))
mobility_data <- mobility_data_raw %>%
  select(fuel_rec, `Sum of n_regis`, Year, veh_categ_rec2, rto_code)
  #Type of unique datasets
print(unique(mobility_data$fuel_rec))

# Filter by 'fuel_rec' 
  # Define the desired fuel types, that are relevant to the study
valid_fuel_types <- c("EV", "Petrol", "Diesel") 
  # Check unique values in 'fuel_rec' 
print("Unique fuel types in 'fuel_rec' column (before filtering):")
print(unique(mobility_data$fuel_rec))
mobility_data <- mobility_data %>%
  filter(fuel_rec %in% valid_fuel_types)
print(unique(mobility_data$veh_categ_rec2))

# Filter by 'vehicle Category' ->This may be modified as required
valid_veh_categories <- c("2W", "4W", "3W") 
  # Check unique values 
print("Unique vehicle categories in 'veh_category' column (before filtering):")
print(unique(mobility_data$veh_categ_rec2))
mobility_data <- mobility_data %>%
  filter(veh_categ_rec2 %in% valid_veh_categories)

#Select only the required one. This code snippet may be ignored, if cumulative data is to studied. Else the following code
#snippet may be used. Using the case of Surat here
valid_rto<-c("GJ1")
  #Check for unique values 
print("Unique values for RTO Codes:")
print(unique(mobility_data$rto_code))
mobility_data<-mobility_data %>%
  filter(rto_code %in% valid_rto)

# Convert 'reg' to numeric if it's not already, handling potential NAs
mobility_data$`Sum of n_regis` <- as.numeric(as.character(mobility_data$`Sum of n_regis`))
mobility_data <- mobility_data %>% filter(!is.na(`Sum of n_regis`)) # Remove rows where 'reg' became NA

# Convert 'year' to numeric/integer if it's not already
mobility_data$Year <- as.integer(as.character(mobility_data$Year))
mobility_data <- mobility_data %>% filter(!is.na(Year)) # Remove rows where 'year' became NA


# Aggregate 'reg' values for the same 'year' (and other categories) ---
# The problem statement: "The year data set is problematic as, instead of being a single column,
# there are multiple values for the same year. So this should be added to form one column."
# This implies we need to group by year AND other relevant categories before summing registrations.

  # Checking the structure after filtering.
print("Structure of data before aggregation:")
print(head(mobility_data))
print(paste("Number of rows before aggregation:", nrow(mobility_data)))
  # Aggregate data: sum 'reg' for each combination of 'year', 'fuel_rec', and 'veh_category'
  # This step is crucial if there are multiple entries for the same year/fuel/category that need to be summed.
  # This is somewhat similar to Pivot table action in MS Excel
aggregated_data <- mobility_data %>%
  group_by(Year, fuel_rec, veh_categ_rec2, rto_code) %>%
  summarise(total_reg = sum(`Sum of n_regis`, na.rm = TRUE)) %>%
  ungroup() 
print("First few rows of aggregated data:")
print(head(aggregated_data))

#Calculate and Visualize Trends
## 1) Overall Trend of Vehicle Registration by Year
overall_trend <- aggregated_data %>%
  group_by(Year) %>%
  summarise(total_registrations = sum(total_reg, na.rm = TRUE)) %>%
  arrange(Year)
print("Overall Trend Data:")
print(overall_trend)

#Plotting
if(nrow(overall_trend) > 0) {
  plot_overall <- ggplot(overall_trend, aes(x = factor(Year), y = total_registrations)) +
    geom_bar(stat = "identity", fill = "steelblue", color = "black") +
    geom_text(aes(label = total_registrations), vjust = -0.5, size = 2) + # Add data labels
    labs(title = "Overall Trend of Vehicle Registrations by Year",
         x = "Year",
         y = "Total Registrations") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  print(plot_overall)
} else {
  print("No data to plot for overall trend.")
}
?geom_smooth

## 2) Trend by Vehicle Category (veh_category) over Years
category_trend <- aggregated_data %>%
  group_by(Year, veh_categ_rec2) %>%
  summarise(total_registrations = sum(total_reg, na.rm = TRUE)) %>%
  arrange(Year, veh_categ_rec2)
print("Trend by Vehicle Category Data:")
print(category_trend)

if(nrow(category_trend) > 0) {
  plot_category <- ggplot(category_trend, aes(x = factor(Year), y = total_registrations, fill = veh_categ_rec2)) +
    geom_bar(stat = "identity", position = "dodge", color = "black") +
    geom_text(aes(label = total_registrations, group = veh_categ_rec2),
              position = position_dodge(width = 0.9), vjust = -0.3, size = 2.5) + # Add data labels
    labs(title = "Vehicle Registrations by Category Over Years",
         x = "Year",
         y = "Total Registrations",
         fill = "Vehicle Category") +
    theme_void() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(plot_category)
  
  #Faceted plot for clearer individual trends.The previous plot was a combined.
  plot_category_faceted <- ggplot(category_trend, aes(x = factor(Year), y = total_registrations, fill = veh_categ_rec2)) +
    geom_bar(stat = "identity", color = "black") +
    facet_wrap(~veh_categ_rec2, scales = "free_y") + # Create separate plots for each category
    labs(title = "Vehicle Registrations by Category Over Years (Faceted)",
         x = "Year",
         y = "Total Registrations",
         fill = "Vehicle Category") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          legend.position = "none") # Hide legend as info is in facet titles
  print(plot_category_faceted)
  
} else {
  print("No data to plot for trend by vehicle category.")
}

## 3) Trend by Fuel Type (fuel_rec) over Years
fuel_trend <- aggregated_data %>%
  group_by(Year, fuel_rec) %>%
  summarise(total_registrations = sum(total_reg, na.rm = TRUE)) %>%
  arrange(Year, fuel_rec)
print("Trend by Fuel Type Data:")
print(fuel_trend)

if(nrow(fuel_trend) > 0) {
  plot_fuel <- ggplot(fuel_trend, aes(x = factor(Year), y = total_registrations, fill = fuel_rec)) +
    geom_bar(stat = "identity", position = "dodge", color = "black") +
     # Add data labels
    labs(title = "Vehicle Registrations by Fuel Type Over Years",
         x = "Year",
         y = "Total Registrations",
         fill = "Fuel Type") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  print(plot_fuel)
  
  #Faceted plot for clearer individual trends. Drill is the same as ##2
  plot_fuel_faceted <- ggplot(fuel_trend, aes(x = factor(Year), y = total_registrations, fill = fuel_rec)) +
    geom_bar(stat = "identity", color = "black") +
    facet_wrap(~fuel_rec, scales = "free_y") + # Create separate plots for each fuel type
    labs(title = "Vehicle Registrations by Fuel Type Over Years (Faceted)",
         x = "Year",
         y = "Total Registrations",
         fill = "Fuel Type") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          legend.position = "none")
  print(plot_fuel_faceted)
  
} else {
  print("No data to plot for trend by fuel type.")
}

# Save the cleaned and aggregated data
write.csv(aggregated_data, "aggregated_mobility_data.csv", row.names = FALSE)
?ggsave
# Save the plots as PNG files
ggsave("overall_trend_plot.png", plot = plot_overall, width = 10, height = 6, bg="white")
ggsave("category_trend_plot.png", plot = plot_category, width = 10, height = 6, bg="white")
ggsave("category_trend_faceted_plot.png", plot = plot_category_faceted, width = 12, height = 8, bg="white")
ggsave("fuel_trend_plot.png", plot = plot_fuel, width = 10, height = 6, bg="white")
ggsave("fuel_trend_faceted_plot.png", plot = plot_fuel_faceted, width = 12, height = 8, bg="white")

