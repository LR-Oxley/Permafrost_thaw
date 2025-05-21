### calculating N2O emissions
# calculate ratio N20 emission per total dissolved N from permafrost thaw (data from Voigt et al 2017)

#Total dissolved N: mg N / kg DW
# dry - bare: 307 +/- 111 
# dry - vegetated: 570 +/- 199

# wet - bare: 219 +/- 40
# wet - vegetated: 314 +/- 75

# mass = density * volume
# convert total dissolved N: mg N / kg DW into g / cm'3 
# need bulk density: bare = 0.11 g / cm3 = 0.00011kg/cm3
# vegetated = 0.14 g / cm3 = 0.00014 kg/cm3

# dry bare: 307 mg N / kg DW --> how much is that in mg N / cm'3
# 307 mg N / kg * 0.00011 kg/cm'3 = 0.03377 mg N / cm'3
# dry vegetated: 570 mg N / kg DW * 0.00014 kg/ cm3 = 0.0798 mg N / cm3

# wet bare: 219 mg N / kg DW * 0.00011 kg / cm'3 = 0.02409 mg N / cm3
# wet vegetated: 314 mg N / kg DW * 0.00014 kg / cm3 = 0.04396 mg N / cm3

# to integrate this concentration of total dissolved N over 3cm deep soil slice that's 1m2
# mg N/ cm3 --> mg N/m2

# 1 m2 * 3cm = 10'000cm2 * 3 cm = 30'000cm3

# total dissolved N in /m2 for 3 cm deep slice
# dry bare = 0.03377 mg N / cm3 * 30'000 cm3 = 1'013.1 mg N / m2
# dry vegetated = 0.0798 mg N / cm3 * 30'000cm3 = 2'394 mg N / m2
# wet bare = 0.02409 mg N / cm3 * 30'000 cm3 = 722.7 mg N / m2 
# wet vegetated = 0.04396 mg N / cm3 * 30'000 cm3 = 1'318.8 mg N / m2

# to compare with 15 cm deep permafrost core: multiply by 5
# dry bare = 1'013.1 mg N / m2 * 5 = 5065.5 mg N / m2 
# dry vegetated = 2'394 mg N / m2 * 5= 11970 mg N / m2
# wet bare = 722.7 mg N / m2 * 5= 3613.5 mg N / m2
# wet vegetated = 1'318.8 mg N / m2 * 5 = 6594 mg N / m2

# palmtag et al: 0.5 kg N / m2 == 500'000 mg N / m2 (but this includes particulate organic N)
# for 50 cm depth increment

# to compare this with Voigt et al 15 cm deep permafrost core: multiply by 3
# dry bare = 5065.5 mg N / m2 * 3 = 15'195 mg N / m2
# dry vegetated = 11970 mg N / m2 * 3 = 35'910 mg N / m2
# wet bare = 3613.5 mg N / m2 *3 = 10'840.5 mg N / m2
# wet vegetated = 6590 mg N / m2 *3 = 19'770 mg N / m2

# as Voigt et al only included Total dissolved N, which is roughly 10% of the total N (particulate and dissolved) in the soil, the rest would be: 
# dry bare = 15195 / 0.1 = 151'950 mg N / m2
# dry vegetated = 35'910 /0.1 = 359'100 mg N / m2
# wet bare = 10'840 / 0.1 = 108'400 mg N / m2
# wet vegetated = 19'770 = 197'700 mg N / m2



# mean N2O emissions: mg N2O / m2*day for a 15cm slice of permafrost
# dry - bare: 2.81 +/- 0.6
# dry - vegetated: 0.20 +/- 0.03

# wet - bare: 0.21 +/- 0.03
# wet - vegetated: 0.13 +/- 0.02

# n20 emission rate per thawed total dissolved N for 15 cm deep permafrost slice

# dry bare = 2.81 mg N20 / m2 * day / 5065.5 mg N / m2 = 0.0005547 mg N20 / m2 *day per mg N / m2 
# dry vegetated = 0.2 mg N20 / m2 * day / 11970 mg N / m2 = 0.0000167 mg N20 / m2 *day per mg N / m2
# wet bare = 0.21 mg N20 / m2 * day / 3'613.5 mg N / m2 = 0.00005811 mg N20 / m2 *day per mg N / m2
# wet vegetated = 0.13 mg N20 / m2 / 6'594 mg N / m2 = 0.00001971 mg N20 / m2 *day per mg N / m2

# scale n20 emission rate per thawed total dissolved N for 100 cm deep permafrost slice
# dry bare = 0.0005547 mg N20 / m2 *day per mg N / m2 * 6.6 =  0.00366124 mg N20 / m2 * day per mg N / m2
# dry vegetated = 0.0000167 mg N20 / m2 *day per mg N / m2 * 6.6 = 0.00011022 
# wet bare = 0.00005811 mg N20 / m2 *day per mg N / m2 * 6.6 = 0.00038353 mg N20 / m2 * day per mg N / m2
# wet vegetated = 0.00001971 mg N20 / m2 *day per mg N / m2 * 6.6 = 0.00013009 mg N20 / m2 * day per mg N / m2



## calculating n20 emissions
library(terra)
setwd("/Users/laraoxley/Desktop/data/CMIP6_corr/N_asymptotic")
# Load NetCDF files
ALD <- rast("/Users/laraoxley/Desktop/data/CMIP6_corr/N_asymptotic/mean_ssp370_corr.nc")
N_data <- rast("/Users/laraoxley/Desktop/data/CMIP6_corr/N_asymptotic/TN_30deg_corr.nc",lyr=1)
LC <- rast("/Users/laraoxley/Desktop/data/CMIP6_corr/N_asymptotic/LC_remapnn_corr.nc")
plot(LC)
cell_area<-cellSize(LC, mask=TRUE, lyrs=FALSE, unit="m")
# Sum all the cell areas (total area of the grid)
total_area <- global(cell_area, fun = "sum", na.rm = TRUE)$sum

# Print the total area (in m²)
plot(N_data)
plot(ALD[[250]])

# Get latitude and longitude from the grid
lat <- unique(yFromCell(ALD, 1:ncell(ALD)))
lon <- unique(xFromCell(ALD, 1:ncell(ALD)))

# Set common extent
common_extent <- ext(-179.95, 179.95, 30, 90)
ext(ALD) <- ext(N_data) <- ext(LC) <- common_extent

# Ensure masks are numeric (not logical)
# Ensure masks are numeric (not logical)
taiga_mask <- as.numeric(LC %in% c(1,2,3,4,5,8,9))
tundra_mask <- as.numeric(LC %in% c(6,7,10))
wetlands_mask <- as.numeric(LC == 11)
other<- as.numeric(LC %in% c(12,13,14))
barren_mask <- as.numeric(LC %in% c(15,16))
total_mask<-taiga_mask+tundra_mask+wetlands_mask+other+barren_mask
plot(barren_mask)

# calculate N2O emissions for tundra und taiga = dry vegetated 
dry_veg_mask <- tundra_mask + taiga_mask  # Combine masks
dry_bare_mask<- barren_mask
wet_vegetated<- wetlands_mask

ALD_dry_vegetated <- ALD*dry_veg_mask
ALD_dry_vegetated[ALD_dry_vegetated == 0] <- NA
plot(ALD_dry_vegetated)

ALD_dry_bare<-ALD*dry_bare_mask
ALD_dry_bare[ALD_dry_bare == 0] <- NA
plot(ALD_dry_bare[[250]])
plot(dry_bare_mask)

ALD_wet_vegetated<-ALD*wet_vegetated
ALD_wet_vegetated[ALD_wet_vegetated == 0] <- NA
plot(ALD_wet_vegetated[[250]])

# Compute potential N₂O emissions
# Voigt factor: mg N₂O per m depth thaw per m² fläche for dry vegetated
emission_factor_dry_vegetated <- 0.00011022
emission_factor_wet_vegetated<-0.00013009
emission_factor_dry_bare<-0.00366124

n2o_emissions_per_day_dry_vegetated <- ALD_dry_vegetated * emission_factor_dry_vegetated  # mg N₂O / m²
n2o_emissions_per_day_wet_vegetated <- ALD_wet_vegetated * emission_factor_wet_vegetated  # mg N₂O / m²
n2o_emissions_per_day_dry_bare <- ALD_dry_bare * emission_factor_dry_bare  # mg N₂O / m²

n2o_emissions_per_year_dry_vegetated <- n2o_emissions_per_day_dry_vegetated * 112  # mg N₂O / m² for 100 growing season days
n2o_emissions_per_year_wet_vegetated <- n2o_emissions_per_day_wet_vegetated * 112  # mg N₂O / m² for 100 growing season days
n2o_emissions_per_year_dry_bare <- n2o_emissions_per_day_dry_bare * 112  # mg N₂O / m² for 112 growing season days

# Summarize: total emissions per year (dry vegetated area only)
total_emissions_dry_vegetated <- global(n2o_emissions_per_year_dry_vegetated, fun = "sum", na.rm = TRUE)
total_emissions_wet_vegetated <- global(n2o_emissions_per_year_wet_vegetated, fun = "sum", na.rm = TRUE)
total_emissions_dry_bare <- global(n2o_emissions_per_year_dry_bare, fun = "sum", na.rm = TRUE)
total_area_emissions<- total_emissions_dry_vegetated+total_emissions_wet_vegetated+total_emissions_dry_bare

# almost 55'800 mg N20 in 2100 over whole arctic area = 0.055 kg 

# estimates (Repo et al 2009): 0.1 Tg per year = 100'000'000 kg

# Convert mg/m² to metric tons (1 t = 1e9 mg, 1 km² = 1e6 m²)
area_km2 <- cellSize(n2o_emissions, unit = "km")
n2o_tonnes <- (n2o_emissions * area_km2 * 1e6) / 1e9  # → tonnes N₂O
total_emissions_per_year <- global(n2o_tonnes, fun = "sum", na.rm = TRUE)
plot(as.numeric(total_emissions$year), total_emissions$sum,
     type = "l", col = "blue", lwd = 2,
     xlab = "Year", ylab = "Total N2O emissions (mg/m'2)",
     main = "Estimated Arctic Wetland N2O Emissions")

