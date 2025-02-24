# Define the equation function
eq <- function(depth, a, b, k) {
  a + b * exp(-k * abs(depth))  # Use absolute depth to ensure exponent works correctly
}

# Define parameters for each biome
params <- list(
  Taiga  = list(a = 0.007, b = 0.097, k = 0.027),
  Tundra = list(a = 0.01,  b = 0.017, k = 0.019),
  Barren = list(a = 0,     b = 0.0161, k = 0.016)
)

# Set up the plot with depth from 0 to -300 cm
plot(NA, xlim = c(0, 0.12), ylim = c(-300, 0), xlab = "TN (kg/m²)", ylab = "Depth (cm)", main = "TN vs Depth")
grid()

# Add curves for each biome
colors <- c("darkgreen", "lightblue", "grey")
i <- 1

for (biome in names(params)) {
  depth_seq <- seq(0, 300, length.out = 100)  # Depth in cm (positive)
  TN_values <- eq(depth_seq, params[[biome]]$a, params[[biome]]$b, params[[biome]]$k)  # Compute TN
  
  lines(TN_values, -depth_seq, col = colors[i], lwd = 2)  # Flip depth to negative
  i <- i + 1
}
# Add Wetlands as a vertical line
wetlands_TN <- 0.022 # Constant TN value for wetlands
lines(rep(wetlands_TN, 100), seq(-300, 0, length.out = 100), col = "green", lwd = 2, lty = 1)

# Add legend
legend("topright", legend = c(names(params), "Wetlands"), col = c(colors, "green"), lwd = 2, lty = c(1, 1, 1, 1))






##############################################################################################################################
library(terra)
setwd("/Users/laraoxley/Desktop/data/CMIP6_corr/N_asymptotic/mean_ssp585")
# Load NetCDF files
ALD <- rast("/Users/laraoxley/Desktop/data/CMIP6_corr/N_asymptotic/mean_ssp585_corr.nc")
N_data <- rast("/Users/laraoxley/Desktop/data/CMIP6_corr/N_asymptotic/TN_30deg_corr.nc",lyr=1)
LC <- rast("/Users/laraoxley/Desktop/data/CMIP6_corr/N_asymptotic/LC_remapnn_corr.nc")

# Get latitude and longitude from the grid
lat <- unique(yFromCell(ALD, 1:ncell(ALD)))
lon <- unique(xFromCell(ALD, 1:ncell(ALD)))
plot(ALD)

# Set common extent
common_extent <- ext(-179.95, 179.95, 30, 90)
ext(ALD) <- ext(N_data) <- ext(LC) <- common_extent

# Ensure masks are numeric (not logical)
taiga_mask <- as.numeric(LC %in% c(1,2,3,4,5,8,9))
tundra_mask <- as.numeric(LC %in% c(6,7,10))
wetlands_mask <- as.numeric(LC == 11)
barren_mask <- as.numeric(LC %in% c(15,16))

# Land cover parameters
params <- list(
  taiga = c(a = 0.007, b = 0.097, k = 0.027),
  tundra = c(a = 0.01, b = 0.017, k = 0.019),
  barren = c(a = 0, b = 0.0161, k = 0.016)
)
### calculate wetlands like homogeneous 

# Normalize Nitrogen (replace ifel with terra::ifel)
normalize_N <- function(N, a, b, k) {
  A_3m <- 3 * a + (b / k) * (1 - exp(-3 * k))
  N / A_3m
}

taiga_N <- normalize_N(N_data * taiga_mask, params$taiga['a'], params$taiga['b'], params$taiga['k'])
tundra_N <- normalize_N(N_data * tundra_mask, params$tundra['a'], params$tundra['b'], params$tundra['k'])
barren_N <- normalize_N(N_data * barren_mask, params$barren['a'], params$barren['b'], params$barren['k'])
wetlands_N <- (N_data * wetlands_mask) / 3

# Compute thawed nitrogen
compute_thawed_N <- function(ALD, N, a, b, k) {
  A_ALD <- a * ALD + (b / k) * (1 - exp(-k * ALD))
  ifel(N == 0, NA, N * A_ALD)  # Avoid multiplying by zero
}

taiga_N[taiga_N == 0] <- NA
tundra_N[tundra_N == 0] <- NA
wetlands_N[wetlands_N == 0] <- NA
barren_N[barren_N == 0] <- NA
plot(taiga_N)

# Compute thawed nitrogen
thawed_taiga <- compute_thawed_N(ALD, taiga_N, params$taiga['a'], params$taiga['b'], params$taiga['k'])
thawed_tundra <- compute_thawed_N(ALD, tundra_N, params$tundra['a'], params$tundra['b'], params$tundra['k'])
thawed_barren <- compute_thawed_N(ALD, barren_N, params$barren['a'], params$barren['b'], params$barren['k'])
thawed_wetlands <- (wetlands_N * ALD)

plot(thawed_wetlands, main = "Thawed Nitrogen: Taiga", lyr=1)

# Calculate mean for each layer (year) in the SpatRaster object
taiga_thawed_N<- global(thawed_taiga, fun = "mean", na.rm = TRUE)
tundra_thawed_N<- global(thawed_tundra, fun = "mean", na.rm = TRUE)
barren_thawed_N<- global(thawed_barren, fun = "mean", na.rm = TRUE)
wetlands_thawed_N<- global(thawed_wetlands, fun = "mean", na.rm = TRUE)


# Save combined raster to a NetCDF file
writeCDF(
  thawed_taiga,
  "thawed_taiga.nc",
  varname = "Thawed_N",
  overwrite = TRUE
)
# Save combined raster to a NetCDF file
writeCDF(
  thawed_tundra,
  "thawed_tundra.nc",
  varname = "Thawed_N",
  overwrite = TRUE
)
# Save combined raster to a NetCDF file
writeCDF(
  thawed_wetlands,
  "thawed_wetlands.nc",
  varname = "Thawed_N",
  overwrite = TRUE
)
# Save combined raster to a NetCDF file
writeCDF(
  thawed_barren,
  "thawed_barren.nc",
  varname = "Thawed_N",
  overwrite = TRUE
)


### calculating the weighted average
#1.	Calculate the area of each grid cell based on latitude and resolution.
#2.	Multiply each thawed nitrogen raster by its grid cell area.
#3.	Sum all values over the Arctic region.
#4.	Divide by the total area of the Arctic region to get the final weighted mean in kg / m'2

library(terra)
setwd("/Users/laraoxley/Desktop/data/CMIP6_corr/N_asymptotic/mean_ssp585")


# Load NetCDF thawed nitrogen rasters
thawed_taiga <- rast("thawed_taiga.nc")
thawed_tundra <- rast("thawed_tundra.nc")
thawed_barren <- rast("thawed_barren.nc")
thawed_wetlands <- rast("thawed_wetlands.nc")

# Load total area raster (assumes area is in m^2 per grid cell)
total_area_raster <- rast("/Users/laraoxley/Desktop/data/CMIP6_corr/N_asymptotic/TN_30deg_corr.nc")
cell_area<-cellSize(total_area_raster, mask=TRUE, lyrs=FALSE, unit="m")
# Sum all the cell areas (total area of the grid)
total_area <- global(cell_area, fun = "sum", na.rm = TRUE)$sum

# Print the total area (in m²)
print(total_area)

#estimate of land surface area of the arctic (above 66.34°N) = 6–7 × 10¹² m².

# Get grid resolution (degrees)
res_x <- xres(thawed_taiga)  # Longitude resolution (°)
res_y <- yres(thawed_taiga)  # Latitude resolution (°)

# Earth's radius (in meters)
R <- 6371000  # meters

# Compute cell area for each latitude (m²)
latitudes <- yFromCell(thawed_taiga, 1:ncell(thawed_taiga))
cell_areas <- (R^2 * (res_x * pi/180) * (res_y * pi/180) * cos(latitudes * pi/180))  # m²

# Radius of Earth in meters
R <- 6378137
total_area <- global(cell_areas, fun = "sum", na.rm = TRUE)$sum


# Create area rasters for each region (only valid pixels count) = setValues(thawed_wetlands, cell_areas): 
#Creates a new raster with the same spatial structure as thawed_wetlands but with values from cell_areas.
cell_area_raster_taiga <- mask(setValues(thawed_taiga, cell_areas), thawed_taiga)
cell_area_raster_tundra <- mask(setValues(thawed_tundra, cell_areas), thawed_tundra)
cell_area_raster_barren <- mask(setValues(thawed_barren, cell_areas), thawed_barren)
cell_area_raster_wetlands <- mask(setValues(thawed_wetlands, cell_areas), thawed_wetlands)
plot(cell_area_raster_taiga)
cell_area_wetlands<-cellSize(cell_area_raster_wetlands, mask=TRUE, lyrs=FALSE, unit="m")
total_area_wt <- global(cell_area_wetlands, fun = "sum", na.rm = TRUE)$sum
cell_area_taiga<-cellSize(cell_area_raster_taiga, mask=TRUE, lyrs=FALSE, unit="m")
total_area_taiga <- global(cell_area_taiga, fun = "sum", na.rm = TRUE)$sum
cell_area_tundra<-cellSize(cell_area_raster_tundra, mask=TRUE, lyrs=FALSE, unit="m")
total_area_tundra <- global(cell_area_tundra, fun = "sum", na.rm = TRUE)$sum
cell_area_barren<-cellSize(cell_area_raster_barren, mask=TRUE, lyrs=FALSE, unit="m")
total_area_barren <- global(cell_area_barren, fun = "sum", na.rm = TRUE)$sum
plot(cell_area_raster_taiga)
??cellsize
# Compute total thawed nitrogen for each biome (kg N)
thawed_taiga_weighted <- thawed_taiga * cell_area_raster_taiga
thawed_tundra_weighted <- thawed_tundra * cell_area_raster_tundra
thawed_barren_weighted <- thawed_barren * cell_area_raster_barren
thawed_wetlands_weighted <- thawed_wetlands * cell_area_raster_wetlands

total_thawed_N_wt <- global(thawed_wetlands_weighted, "sum", na.rm = TRUE)  # kg N, for 251 years as a dataframe
total_thawed_N_tundra <- global(thawed_tundra_weighted, "sum", na.rm = TRUE)  # kg N, for 251 years as a dataframe
total_thawed_N_taiga <- global(thawed_taiga_weighted, "sum", na.rm = TRUE)  # kg N, for 251 years as a dataframe
total_thawed_N_barren <- global(thawed_barren_weighted, "sum", na.rm = TRUE)  # kg N, for 251 years as a dataframe
df<-data.frame(total_thawed_N_barren, total_thawed_N_taiga, total_thawed_N_tundra, total_thawed_N_wt)
# Sum total thawed nitrogen (kg N) as a spatraster
total_thawed_N <- sum(thawed_taiga_weighted, thawed_tundra_weighted, thawed_barren_weighted, thawed_wetlands_weighted, na.rm = TRUE)

total_thawed_N_global <- global(total_thawed_N, "sum", na.rm = TRUE)  # kg N, for 251 years as a dataframe
# Convert total thawed nitrogen from kg to teragrams (Tg)
total_thawed_N_Pg <- total_thawed_N_global / 1e12

# Print result in teragrams
print(total_thawed_N_Pg)

# Compute weighted average thawed nitrogen (kg N / m²)
weighted_avg_thawed_N <- total_thawed_N_global / total_area  # kg N/m²

df<-data.frame(weighted_avg_thawed_N)
df$Year<-rep(1851:2100)
write.csv(df, "TN_weighted_kg_per_m2_std_ssp585.csv")
ggplot(df, aes(x = Year, y = sum)) +
  geom_line(color = "blue", linewidth = 1) +
  labs(
    title = "Total Thawed Nitrogen Over Time",
    x = "Year",
    y = "Total Thawed Nitrogen (kg)"
  ) +
  #ylim(0,5)+
  theme_minimal()


## total thawed N in Tg
df<-data.frame(total_thawed_N_Pg)
df$Year<-rep(1851:2100)
write.csv(df, "total_thawed_N_Tg_std_ssp585.csv")


######### alternative solution
library(terra)
setwd("/Users/laraoxley/Desktop/data/CMIP6_corr/N_asymptotic/std_ssp585")

# Load NetCDF thawed nitrogen rasters
thawed_taiga <- rast("thawed_taiga.nc")
thawed_tundra <- rast("thawed_tundra.nc")
thawed_barren <- rast("thawed_barren.nc")
thawed_wetlands <- rast("thawed_wetlands.nc")
# Replace NA values with 0
thawed_taiga[is.na(thawed_taiga)] <- 0
thawed_tundra[is.na(thawed_tundra)] <- 0
thawed_barren[is.na(thawed_barren)] <- 0
thawed_wetlands[is.na(thawed_wetlands)] <- 0

# Combine the rasters for each year
combined_thawed <- thawed_taiga + thawed_tundra + thawed_barren + thawed_wetlands

combined_thawed <- ifel(combined_thawed == 0, NA, combined_thawed)
plot(combined_thawed)
# Calculate the area of each grid cell in m^2
??cellSize
cell_areas <- cellSize(combined_thawed, mask=TRUE, unit = "m")
plot(cell_areas)
# Sum all the cell areas (total area of the grid)
total_area <- global(cell_areas, fun = "sum", na.rm = TRUE)$sum

# Print the total area (in m²)
print(total_area)
# Weight the thawed nitrogen values by grid cell area
weighted_thawed <- combined_thawed * cell_areas
plot(weighted_thawed)
# Sum the weighted values for each year
total_thawed <- global(weighted_thawed, "sum", na.rm = TRUE)
total_thawed_Pg <- total_thawed / 1e12
# Convert to a data frame
total_thawed_df <- data.frame(
  Year = 1:nlyr(combined_thawed),  # Assuming 251 years
  Total_Thawed_Nitrogen_Pg = total_thawed_Pg$sum
)

# Print the first few rows
head(total_thawed_df)
tail(total_thawed_df)


# Compute weighted average thawed nitrogen (kg N / m²)
weighted_avg_thawed_N <- total_thawed / total_area  # kg N/m²

df<-data.frame(weighted_avg_thawed_N)
df$Year<-rep(1851:2100)
setwd("/Users/laraoxley/Desktop/data/CMIP6_corr/N_asymptotic/std_ssp585")
write.csv(df, "TN_weighted_kg_per_m2_std_ssp585.csv")
ggplot(df, aes(x = Year, y = sum)) +
  geom_line(color = "blue", linewidth = 1) +
  labs(
    title = "Total Thawed Nitrogen Over Time",
    x = "Year",
    y = "Total Thawed Nitrogen (kg)"
  ) +
  ylim(0,5)+
  theme_minimal()
