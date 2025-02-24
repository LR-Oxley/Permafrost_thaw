##ALD Year axis ssps
library(tidyverse)
setwd("/Users/laraoxley/Desktop/data/CMIP6_corr/N_asymptotic")
maxN<-read.csv("TN_weighted_kg_per_m2.csv") ###without IPSL model, as this has a much lower ALD
str(maxN)
ggplot(maxN, aes(x = Year)) +
  # Add shaded area for variance (mean ± 1 standard deviation)
  geom_ribbon(aes(ymin = mean_370 - std_370, ymax = mean_370 + std_370), fill = "grey80", alpha = 0.5) +
  # Plot the mean line with colors changing based on Year
  geom_line(aes(y = mean_370), color = "green", linewidth = 0.8) +
  geom_ribbon(aes(ymin = mean_585 - std_585, ymax = mean_585 + std_585), fill = "grey80", alpha = 0.5) +
  # Plot the mean line with colors changing based on Year
  geom_line(aes(y = mean_585),  color = "blue", linewidth = 0.8) +
  geom_ribbon(aes(ymin = mean_126 - std_126, ymax = mean_126 + std_126), fill = "grey80", alpha = 0.5) +
  # Plot the mean line with colors changing based on Year
  geom_line(aes(y = mean_126), color = "red",  linewidth = 0.8) +
  labs(x = "Year", y = "N [kg N /m^2]", title = "Nitrogen released through permafrost thawing") +
  xlim(1850, 2100) +
  theme_bw() +
  theme(legend.position = "none")  # Remove the legend if not needed

#anomaly plot
ssp585<-maxN[, c("Year", "mean_585", "std_585")]
# Filter the data for Year between 1860 and 1920
filtered_585 <- ssp585[ssp585$Year >= 1860 & ssp585$Year <= 1900, ]
# Calculate the mean of the variable in the filtered data
mean_585<- mean(filtered_585$mean_585, na.rm = TRUE)

# Assuming 'Year' is a non-numeric column you want to exclude
ssp585_a <- ssp585
ssp585_a[ , -which(names(ssp585) %in% c("Year", "std_585"))] <- 
  ssp585_a[ , -which(names(ssp585) %in% c("Year", "std_585"))] - mean_585


#anomaly plot
ssp126<-maxN[, c("Year", "mean_126", "std_126")]
# Filter the data for Year between 1860 and 1920
filtered_126 <- ssp126[ssp126$Year >= 1860 & ssp126$Year <= 1900, ]
# Calculate the mean of the variable in the filtered data
mean_126<- mean(filtered_126$mean_126, na.rm = TRUE)

# Assuming 'Year' is a non-numeric column you want to exclude
ssp126_a <- ssp126
ssp126_a[ , -which(names(ssp126) %in% c("Year", "std_126"))] <- 
  ssp126_a[ , -which(names(ssp126) %in% c("Year", "std_126"))] - mean_126

#anomaly plot
ssp370<-maxN[, c("Year", "mean_370", "std_370")]
# Filter the data for Year between 1860 and 1920
filtered_370 <- ssp370[ssp370$Year >= 1860 & ssp370$Year <= 1900, ]
# Calculate the mean of the variable in the filtered data
mean_370<- mean(filtered_370$mean_370, na.rm = TRUE)

# Assuming 'Year' is a non-numeric column you want to exclude
ssp370_a <- ssp370
ssp370_a[ , -which(names(ssp370) %in% c("Year", "std_370"))] <- 
  ssp370_a[ , -which(names(ssp370) %in% c("Year", "std_370"))] - mean_370

anomaly_total<-data.frame(ssp585_a, ssp370_a, ssp126_a)
anomaly_total<-anomaly_total[,c("Year", "mean_585", "std_585","mean_370","std_370","mean_126","std_126")]


# Create a new column for color based on the Year
anomaly_total$color_period <- ifelse(anomaly_total$Year <= 2014, "black", "colored")

# Create separate dataframes for before and after 2015
before_2015 <- anomaly_total[anomaly_total$Year <= 2014, ]
after_2015 <- anomaly_total[anomaly_total$Year > 2014, ]

# Plot
ggplot() +
  # Plot for before 2015 (black color)
  geom_line(data = before_2015, aes(x = Year, y = mean_585), color = "black", linewidth = 0.8) +
  geom_ribbon(data = before_2015,aes(x = Year,ymin = mean_585 - std_585, ymax = mean_585 + std_585), fill = "grey", alpha = 0.5) +
  geom_line(data = before_2015, aes(x = Year, y = mean_370), color = "black", linewidth = 0.8) +
  geom_ribbon(data = before_2015,aes(x = Year,ymin = mean_370 - std_370, ymax = mean_370 + std_370), fill = "grey", alpha = 0.5) +
  geom_line(data = before_2015, aes(x = Year, y = mean_126), color = "black", linewidth = 0.8) +
  geom_ribbon(data = before_2015,aes(x = Year,ymin = mean_126 - std_126, ymax = mean_126 + std_126), fill = "grey", alpha = 0.5) +
  
  # Plot for after 2015 (colorful)
  geom_line(data = after_2015, aes(x = Year, y = mean_585), color = "blue", linewidth = 0.8) +
  geom_ribbon(data = after_2015,aes(x = Year,ymin = mean_585 - std_585, ymax = mean_585 + std_585), fill = "grey", alpha = 0.5) +
  geom_line(data = after_2015, aes(x = Year, y = mean_370), color = "darkgreen", linewidth = 0.8) +
  geom_ribbon(data = after_2015,aes(x = Year,ymin = mean_370 - std_370, ymax = mean_370 + std_370), fill = "grey", alpha = 0.5) +
  geom_line(data = after_2015, aes(x = Year, y = mean_126), color = "red", linewidth = 0.8) +
  geom_ribbon(data = after_2015,aes(x = Year,ymin = mean_126 - std_126, ymax = mean_126 + std_126), fill = "grey", alpha = 0.5) +
  
  # Add vertical line at 2015
  geom_vline(xintercept = 2015, linetype = "dashed", color = "black") +
  
  # Labels and title
  labs(x = "Year", y = "Nitrogen [kg N / m^2]", title = "N in soil after permafrost thaw, weighted") +
  xlim(1850, 2100) +
  
  # Theme and appearance
  theme_bw() +
  theme(legend.position = "none")


ggplot() +
  # Plot for before 2015 (black color)
  geom_line(data = before_2015, aes(x = Year, y = mean_585), color = "black", linewidth = 0.8) +
  geom_ribbon(data = before_2015, aes(x = Year, ymin = mean_585 - std_585, ymax = mean_585 + std_585), fill = "grey", alpha = 0.5) +
  geom_line(data = before_2015, aes(x = Year, y = mean_370), color = "black", linewidth = 0.8) +
  geom_ribbon(data = before_2015, aes(x = Year, ymin = mean_370 - std_370, ymax = mean_370 + std_370), fill = "grey", alpha = 0.5) +
  geom_line(data = before_2015, aes(x = Year, y = mean_126), color = "black", linewidth = 0.8) +
  geom_ribbon(data = before_2015, aes(x = Year, ymin = mean_126 - std_126, ymax = mean_126 + std_126), fill = "grey", alpha = 0.5) +
  # Plot for after 2015 (colorful, with legend)
  geom_line(data = after_2015, aes(x = Year, y = mean_585, color = "SSP 585"), linewidth = 0.8) +
  geom_ribbon(data = after_2015, aes(x = Year, ymin = mean_585 - std_585, ymax = mean_585 + std_585, color = "Standard Deviation"), fill = "grey", alpha = 0.5) +
  geom_line(data = after_2015, aes(x = Year, y = mean_370, color = "SSP 370"), linewidth = 0.8) +
  geom_ribbon(data = after_2015, aes(x = Year, ymin = mean_370 - std_370, ymax = mean_370 + std_370), fill = "grey", alpha = 0.5) +
  geom_line(data = after_2015, aes(x = Year, y = mean_126, color = "SSP 126"), linewidth = 0.8) +
  geom_ribbon(data = after_2015, aes(x = Year, ymin = mean_126 - std_126, ymax = mean_126 + std_126), fill = "grey", alpha = 0.5) +
  # Add vertical line at 2015
  geom_vline(xintercept = 2015, linetype = "dashed", color = "black") +
  # Labels and title
  labs(x = "Year", y = "Nitrogen [kg N / m^2]", title = "Amount of N in active layer following permafrost thaw") +
  xlim(1850, 2100) +
  # Add legend for after_2015
  scale_color_manual(values = c("SSP 585" = "blue", "SSP 370" = "darkgreen", "SSP 126" = "red", "Standard Deviation" = "grey"),
                     name = "Scenarios") +
  # Theme and appearance
  theme_bw() +
  theme(legend.position = "bottom")






data<-read.csv("TN_weighted_kg_per_m2.csv")
# Filter the data for the relevant time frames: 1880-1900, 1990-2010, 2080-2100
time_frames <- c('1880-1900', '1990-2010', '2080-2100')

filtered_data <- data[data$TF %in% time_frames, ]

# Aggregate the data by the mean nitrogen values for each time frame
agg_data <- filtered_data %>%
  group_by(TF) %>%
  summarize(
    mean_126 = mean(mean_126, na.rm = TRUE),
    mean_370 = mean(mean_370, na.rm = TRUE),
    mean_585 = mean(mean_585, na.rm = TRUE)
  )

# Reshape data for plotting
agg_data_melted <- agg_data %>%
  gather(key = "Scenario", value = "Nitrogen_Storage", mean_126, mean_370, mean_585)

# Plotting
ggplot(agg_data_melted, aes(x = TF, y = Nitrogen_Storage, fill = Scenario)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("mean_126" = "red", "mean_370" = "green", "mean_585" = "blue")) +
  labs(
    title = "Mean Nitrogen Storage for Different Scenarios and Time Frames",
    x = "Time Frame",
    y = "Mean Nitrogen Storage (kg N/m²)",
    fill = "Scenario"
  ) +
  theme_minimal() +
  theme(legend.title = element_text(size = 12), legend.position = "top")


###as anomaly plot

# Load necessary libraries
library(ggplot2)
library(tidyr)
library(dplyr)

# Filter the data for the relevant time frames: 1880-1900, 1990-2010, 2080-2100
time_frames <- c('1880-1900', '1990-2010', '2080-2100')
filtered_data <- data[data$TF %in% time_frames, ]

# Aggregate the data by the mean nitrogen values and standard deviation for each time frame
agg_data <- filtered_data %>%
  group_by(TF) %>%
  summarize(
    mean_126 = mean(mean_126, na.rm = TRUE),
    mean_370 = mean(mean_370, na.rm = TRUE),
    mean_585 = mean(mean_585, na.rm = TRUE),
    std_126 = mean(std_126, na.rm = TRUE),
    std_370 = mean(std_370, na.rm = TRUE),
    std_585 = mean(std_585, na.rm = TRUE)
  )

# Reshape the data for plotting
agg_data_melted <- agg_data %>%
  gather(key = "Scenario", value = "Nitrogen_Storage", mean_585, mean_370, mean_126) %>%
  mutate(
    Std_Dev = case_when(
      Scenario == "mean_126" ~ std_126,
      Scenario == "mean_370" ~ std_370,
      Scenario == "mean_585" ~ std_585
    ),
    Scenario = factor(Scenario, levels = c("mean_126", "mean_370", "mean_585"))
  )

# Plotting with error bars
ggplot(agg_data_melted, aes(x = TF, y = Nitrogen_Storage, fill = Scenario)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(
    aes(ymin = Nitrogen_Storage - Std_Dev, ymax = Nitrogen_Storage + Std_Dev),
    position = position_dodge(width = 0.7), width = 0.25
  ) +
  scale_fill_manual(values = c("mean_126" = "red", "mean_370" = "green", "mean_585" = "blue")) +
  labs(
    title = "Nitrogen Storage Anomalies with Standard Deviation Relative to 1880-1900 Reference Period",
    x = "Time Frame",
    y = "Anomaly in Nitrogen Storage (kg N/m²)",
    fill = "Scenario"
  ) +
  ylim(0, 10) +
  theme_minimal() +
  theme(legend.position = "none")

# Set the 1880-1900 values as the reference (i.e., 0 for anomalies)
reference_values <- agg_data[agg_data$TF == '1880-1900', c("mean_126", "mean_370", "mean_585")]

# Calculate anomalies for the timeframes 1990-2010 and 2080-2100
agg_data_anomalies <- agg_data %>%
  filter(TF != '1880-1900') %>%
  mutate(
    anomaly_n_126 = mean_126 - reference_values$mean_126,
    anomaly_n_370 = mean_370 - reference_values$mean_370,
    anomaly_n_585 = mean_585 - reference_values$mean_585,
    std_n_126 = std_126,
    std_n_370 = std_370,
    std_n_585 = std_585
  )

# Reshape the data for plotting
agg_data_anomalies_melted <- agg_data_anomalies %>%
  gather(key = "Scenario", value = "Nitrogen_Storage", anomaly_n_126, anomaly_n_370, anomaly_n_585) %>%
  mutate(
    Std_Dev = case_when(
      Scenario == "anomaly_n_126" ~ std_n_126,
      Scenario == "anomaly_n_370" ~ std_n_370,
      Scenario == "anomaly_n_585" ~ std_n_585
    ),
    Scenario = factor(Scenario, levels = c("anomaly_n_126", "anomaly_n_370", "anomaly_n_585"))
  )


# Plotting with error bars
ggplot(agg_data_anomalies_melted, aes(x = TF, y = Nitrogen_Storage, fill = Scenario)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = Nitrogen_Storage - Std_Dev, ymax = Nitrogen_Storage + Std_Dev),
                position = position_dodge(width = 0.7), width = 0.25) +
  scale_fill_manual(values = c("anomaly_n_126" = "red", "anomaly_n_370" = "green", "anomaly_n_585" = "blue")) +
  labs(
    title = "Nitrogen Storage relative to 1880-1900",
    x = "Time Frame",
    y = "Anomaly in Nitrogen Storage (kg N/m²)",
    fill = "Scenario"
  ) +
  theme_minimal() +
  theme(legend.title = element_text(size = 12), legend.position = "top")

