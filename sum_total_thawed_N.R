##### total thawed N 

library(terra)
library(dplyr)
library(tidyr)
library(ggplot2)

# Load the NetCDF file
file_path <- "/Users/laraoxley/Desktop/data/CMIP6_corr/Sum_total_N/N_asymptotic/N_thawed_mean_370.nc"
file_path <- "/Users/laraoxley/Desktop/data/CMIP6_corr/Sum_total_N/N_exp/N_thawed_fldsum_370_taxis.nc"# Replace with your file path
nc_data <- rast(file_path)

# Extract necessary layers
N_thawed <- nc_data[[grep("N_ALD", names(nc_data))]] # Extract all `N_thawed` layers
# Convert the SpatRaster object into a data frame
N_thawed_df <- as.data.frame(N_thawed, xy = FALSE, na.rm = TRUE)

# Add a column for the years (1850 to 2100)
N_thawed_df <- N_thawed_df %>%
  pivot_longer(cols = everything(), names_to = "Layer", values_to = "N_thawed") %>%
  mutate(Year = rep(1850:2100, each = n() / 251))

# Convert N_thawed to teragram (Tg)
N_thawed_df <- N_thawed_df %>%
  mutate(N_thawed_Tg = N_thawed / 1e12)
# Inspect the result
head(N_thawed_df)
tail(N_thawed_df)
N_thawed_df$Layer <- NULL
N_thawed_df$N_thawed <- NULL
# Load the NetCDF file for standard deviation
std_file_path <- "/Users/laraoxley/Desktop/data/CMIP6_corr/ssp126/N_exp/N_total_std_126.nc"  # Replace with your file path for the standard deviation
nc_std_data <- rast(std_file_path)

# Extract the relevant standard deviation layers (assuming similar naming convention)
N_thawed_std <- nc_std_data[[grep("N_ALD", names(nc_std_data))]] # Adjust as needed

# Convert the standard deviation SpatRaster object into a data frame
N_thawed_std_df <- as.data.frame(N_thawed_std, xy = FALSE, na.rm = TRUE)

# Add a column for the years (1850 to 2100)
N_thawed_std_df <- N_thawed_std_df %>%
  pivot_longer(cols = everything(), names_to = "Layer", values_to = "N_thawed_std") %>%
  mutate(Year = rep(1850:2100, each = n() / 251))
# Remove columns by name

# Convert N_thawed to Petagrams (Pg)
N_thawed_std_df <- N_thawed_std_df %>%
  mutate(N_thawed_Pg = N_thawed_std / 1e15)
# Inspect the result
head(N_thawed_std_df)
N_thawed_std_df$Layer <- NULL
N_thawed_std_df$N_thawed_std <- NULL
# Merge the N_thawed_df and N_thawed_std_df by Year
N_combined_df <- merge(N_thawed_df, N_thawed_std_df, by = "Year")
names(N_combined_df)<-c("Year", "Total", "STD")
setwd("/Users/laraoxley/Desktop/data/CMIP6_corr/ssp126/N_exp")
write.csv(N_combined_df, "sum_totalN_126.csv", row.names = FALSE)
# Plot with the standard deviation shaded
ggplot(N_combined_df, aes(x = Year, y = Total)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_ribbon(aes(ymin = Total - STD, ymax = Total + STD), fill = "blue", alpha = 0.2) +
  labs(
    title = "Total Nitrogen in Soil across Permafrost Region with Standard Deviation",
    x = "Year",
    y = "Total Nitrogen in soil (Pg)"
  ) +
  ylim(0.01, 0.09) +
  theme_minimal()

######################################################################################################
setwd("/Users/laraoxley/Desktop/data/CMIP6_corr/N_asymptotic")
maxN<-read.csv("total_thawed_N.csv") ###without IPSL model, as this has a much lower ALD
str(maxN)
ggplot(maxN, aes(x = Year)) +
  # Add shaded area for variance (mean ± 1 standard deviation)
  geom_ribbon(aes(ymin = Total_370 - STD_370, ymax = Total_370 + STD_370), fill = "grey80", alpha = 0.5) +
  # Plot the mean line with colors changing based on year
  geom_line(aes(y = Total_370), color = "green", linewidth = 0.8) +
  geom_ribbon(aes(ymin = Total_585 - STD_585, ymax = Total_585 + STD_585), fill = "grey80", alpha = 0.5) +
  # Plot the mean line with colors changing based on year
  geom_line(aes(y = Total_585),  color = "blue", linewidth = 0.8) +
  geom_ribbon(aes(ymin = Total_126 - STD_126, ymax = Total_126 + STD_126), fill = "grey80", alpha = 0.5) +
  # Plot the mean line with colors changing based on year
  geom_line(aes(y = Total_126), color = "red",  linewidth = 0.8) +
  labs(x = "Year", y = "N in Soil [Petagram]", title = "Nitrogen released through permafrost thawing") +
  xlim(1850, 2100) +
  theme_bw() +
  theme(legend.position = "none")  # Remove the legend if not needed



#anomaly plot
ssp585<-maxN[, c("Year", "Total_585", "STD_585")]
# Filter the data for years between 1860 and 1920
filtered_585 <- ssp585[ssp585$Year >= 1860 & ssp585$Year <= 1900, ]
# Calculate the mean of the variable in the filtered data
mean_585<- mean(filtered_585$Total_585, na.rm = TRUE)

# Assuming 'year' is a non-numeric column you want to exclude
ssp585_a <- ssp585
ssp585_a[ , -which(names(ssp585) %in% c("Year", "STD_585"))] <- 
  ssp585_a[ , -which(names(ssp585) %in% c("Year", "STD_585"))] - mean_585

#anomaly plot
ssp370<-maxN[, c("Year", "Total_370", "STD_370")]
# Filter the data for years between 1860 and 1920
filtered_370 <- ssp370[ssp370$Year >= 1860 & ssp370$Year <= 1900, ]
# Calculate the mean of the variable in the filtered data
mean_370<- mean(filtered_370$Total_370, na.rm = TRUE)

# Assuming 'year' is a non-numeric column you want to exclude
ssp370_a <- ssp370
ssp370_a[ , -which(names(ssp370) %in% c("Year", "STD_370"))] <- 
  ssp370_a[ , -which(names(ssp370) %in% c("Year", "STD_370"))] - mean_370

#anomaly plot
ssp126<-maxN[, c("Year", "Total_126", "STD_126")]
# Filter the data for years between 1860 and 1920
filtered_126 <- ssp126[ssp126$Year >= 1860 & ssp126$Year <= 1900, ]
# Calculate the mean of the variable in the filtered data
mean_126<- mean(filtered_126$Total_126, na.rm = TRUE)

# Assuming 'year' is a non-numeric column you want to exclude
ssp126_a <- ssp126
ssp126_a[ , -which(names(ssp126) %in% c("Year", "STD_126"))] <- 
  ssp126_a[ , -which(names(ssp126) %in% c("Year", "STD_126"))] - mean_126


anomaly_total<-data.frame(ssp585_a, ssp370_a, ssp126_a)
anomaly_total<-anomaly_total[,c("Year", "Total_585", "STD_585","Total_370","STD_370","Total_126","STD_126")]


# Create a new column for color based on the year
anomaly_total$color_period <- ifelse(anomaly_total$Year <= 2014, "black", "colored")

# Create separate dataframes for before and after 2015
before_2015 <- anomaly_total[anomaly_total$Year <= 2014, ]
after_2015 <- anomaly_total[anomaly_total$Year > 2014, ]

# Plot
ggplot() +
  # Plot for before 2015 (black color)
  geom_line(data = before_2015, aes(x = Year, y = Total_585), color = "black", linewidth = 0.8) +
  geom_ribbon(data = before_2015,aes(x = Year,ymin = Total_585 - STD_585, ymax = Total_585 + STD_585), fill = "grey", alpha = 0.3) +
  geom_line(data = before_2015, aes(x = Year, y = Total_370), color = "black", linewidth = 0.8) +
  geom_ribbon(data = before_2015,aes(x = Year,ymin = Total_370 - STD_370, ymax = Total_370 + STD_370), fill = "grey", alpha = 0.3) +
  geom_line(data = before_2015, aes(x = Year, y = Total_126), color = "black", linewidth = 0.8) +
  geom_ribbon(data = before_2015,aes(x = Year,ymin = Total_126 - STD_126, ymax = Total_126 + STD_126), fill = "grey", alpha = 0.3) +
  
  # Plot for after 2015 (colorful)
  geom_line(data = after_2015, aes(x = Year, y = Total_585), color = "blue", linewidth = 0.8) +
  geom_ribbon(data = after_2015,aes(x = Year,ymin = Total_585 - STD_585, ymax = Total_585 + STD_585), fill = "grey", alpha = 0.3) +
  geom_line(data = after_2015, aes(x = Year, y = Total_370), color = "darkgreen", linewidth = 0.8) +
  geom_ribbon(data = after_2015,aes(x = Year,ymin = Total_370 - STD_370, ymax = Total_370 + STD_370), fill = "grey", alpha = 0.3) +
  geom_line(data = after_2015, aes(x = Year, y = Total_126), color = "red", linewidth = 0.8) +
  geom_ribbon(data = after_2015,aes(x = Year,ymin = Total_126 - STD_126, ymax = Total_126 + STD_126), fill = "grey", alpha = 0.3) +
  
  # Add vertical line at 2015
  geom_vline(xintercept = 2015, linetype = "dashed", color = "black") +
  
  # Labels and title
  labs(x = "Year", y = "Nitrogen [Tg N]", title = "Nitrogen released in soil after permafrost thawing") +
  xlim(1850, 2100) +
  
  # Theme and appearance
  theme_bw() +
  theme(legend.position = "none")
