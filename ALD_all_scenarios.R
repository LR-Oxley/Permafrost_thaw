#all ssp scenarios
setwd("/Users/laraoxley/Desktop/data/CMIP6_corr")
library(tidyverse)
##ALD years axis ssps
ALD<-read.csv("ALD_all_ssp.csv") # excl. IPSL models
str(ALD)
ggplot(ALD, aes(x = years)) +
  # Add shaded area for variance (mean ± 1 standard deviation)
  geom_ribbon(aes(ymin = mean_126 - std_126, ymax = mean_126 + std_126), fill = "grey80", alpha = 0.5) +
  # Plot the mean line with colors changing based on years
  geom_line(aes(y = mean_126), linewidth = 1.5, colour= "red") +
  # Add shaded area for variance (mean ± 1 standard deviation)
  geom_ribbon(aes(ymin = mean_370 - std_370, ymax = mean_370 + std_370), fill = "grey80", alpha = 0.5) +
  # Plot the mean line with colors changing based on years
  geom_line(aes(y = mean_370), linewidth = 1.5, colour= "darkgreen") +
  # Add shaded area for variance (mean ± 1 standard deviation)
  geom_ribbon(aes(ymin = mean_585 - std_585, ymax = mean_585 + std_585), fill = "grey80", alpha = 0.5) +
  # Plot the mean line with colors changing based on years
  geom_line(aes(y = mean_585), linewidth = 1.5, colour= "blue") +
  # Customize the plot
  labs(x = "years", y = "Active Layer Depth [m]", title = "Mean Active Layer Depth") +
  xlim(1850, 2100) +
  theme_minimal() +
  theme(legend.position = "bottom") 




##ALD years axis ssps
setwd("/Users/laraoxley/Desktop/data/CMIP6_corr/ssp585")
ALD_585<-read.csv("ALD_ssp585.csv")

ggplot(ALD_585, aes(x=years)) + 
  geom_line(aes(y=MRI, color="MRI")) + 
  geom_line(aes(y=MIROC6, color="MIROC6")) +
  geom_line(aes(y=FGOALS, color="FGOALS")) +
  geom_line(aes(y=E3SM, color="E3SM")) +
  geom_line(aes(y=NOR, color="NOR")) +
  labs(x = "years", y = "Active Layer Depth [m]", title = "Active Layer Depth, ssp585") +
  xlim(1850,2100)+
  ylim(0.5,5)+
  xlab("years")
  
##calculate the yearsly thaw depth
# Calculate yearsly thaw rate
ALD_585_filtered <- ALD_585[ALD_585$years >= 2015 & ALD_585$years <= 2100, ]
  
ALD_585_filtered$yearly_Thaw_Rate <- c(NA, diff(ALD_585_filtered$mean))
  
# View the updated data
head(ALD_585_filtered)

total_change <- max(ALD_585_filtered$mean) - min(ALD_585_filtered$mean)
average_rate <- total_change / (max(ALD_585_filtered$years) - min(ALD_585_filtered$years))
print(average_rate)

# Plot Active Layer Depth (ALD) and yearsly Thaw Rate
ggplot(data = ALD_585_filtered, aes(x = years)) +
  geom_line(aes(y = mean, color = "mean")) +
  geom_line(aes(y = yearsly_Thaw_Rate, color = "yearsly Thaw Rate")) +
  labs(y = "Depth / Rate", color = "Legend") +
  ggtitle("Active Layer Depth and yearsly Thaw Rate") +
  theme_minimal()

#anomaly plot
E3SM_model<-ALD_585[, c("years", "E3SM")]
# Filter the data for yearss between 1860 and 1920
filtered_E3SM <- E3SM_model[E3SM_model$years >= 1860 & E3SM_model$years <= 1900, ]
# Calculate the mean of the variable in the filtered data
mean_E3SM <- mean(filtered_E3SM$E3SM, na.rm = TRUE)

# Assuming 'years' is a non-numeric column you want to exclude
E3SM_a <- E3SM_model
E3SM_a[ , -which(names(E3SM_model) == "years")] <- E3SM_model[ , -which(names(E3SM_model) == "years")] - mean_E3SM

#anomaly plot
NOR_model<-ALD_585[, c("years", "NOR")]
# Filter the data for yearss between 1860 and 1920
filtered_NOR <- NOR_model[NOR_model$years >= 1860 & NOR_model$years <= 1900, ]
# Calculate the mean of the variable in the filtered data
mean_NOR<- mean(filtered_NOR$NOR, na.rm = TRUE)

# Assuming 'years' is a non-numeric column you want to exclude
NOR_a <- NOR_model
NOR_a[ , -which(names(NOR_model) == "years")] <- NOR_model[ , -which(names(NOR_model) == "years")] - mean_NOR

#anomaly plot
FGOALS_model<-ALD_585[, c("years", "FGOALS")]
# Filter the data for yearss between 1860 and 1920
filtered_FGOALS <- FGOALS_model[FGOALS_model$years >= 1860 & FGOALS_model$years <= 1900, ]
# Calculate the mean of the variable in the filtered data
mean_FGOALS<- mean(filtered_FGOALS$FGOALS, na.rm = TRUE)

# Assuming 'years' is a non-numeric column you want to exclude
FGOALS_a <- FGOALS_model
FGOALS_a[ , -which(names(FGOALS_model) == "years")] <- FGOALS_model[ , -which(names(FGOALS_model) == "years")] - mean_FGOALS

#anomaly plot
MIROC6_model<-ALD_585[, c("years", "MIROC6")]
# Filter the data for yearss between 1860 and 1920
filtered_MIROC6 <- MIROC6_model[MIROC6_model$years >= 1860 & MIROC6_model$years <= 1900, ]
# Calculate the mean of the variable in the filtered data
mean_MIROC6<- mean(filtered_MIROC6$MIROC6, na.rm = TRUE)

# Assuming 'years' is a non-numeric column you want to exclude
MIROC6_a <- MIROC6_model
MIROC6_a[ , -which(names(MIROC6_model) == "years")] <- MIROC6_model[ , -which(names(MIROC6_model) == "years")] - mean_MIROC6

#anomaly plot
MRI_model<-ALD_585[, c("years", "MRI")]
# Filter the data for yearss between 1860 and 1920
filtered_MRI <- MRI_model[MRI_model$years >= 1860 & MRI_model$years <= 1900, ]
# Calculate the mean of the variable in the filtered data
mean_MRI<- mean(filtered_MRI$MRI, na.rm = TRUE)

# Assuming 'years' is a non-numeric column you want to exclude
MRI_a <- MRI_model
MRI_a[ , -which(names(MRI_model) == "years")] <- MRI_model[ , -which(names(MRI_model) == "years")] - mean_MRI


anomaly_total<-data.frame(MRI_a, MIROC6_a, FGOALS_a,E3SM_a, NOR_a)
anomaly_585<-anomaly_total[,c("years", "MRI", "MIROC6","NOR","FGOALS","E3SM")]
names(anomaly_585)<-c("years", "MRI_585", "MIROC6_585","NOR_585","FGOALS_585","E3SM_585")
anomaly_585 <- anomaly_585[1:251, ]
anomaly_585$mean_col_585 <- rowMeans(anomaly_585[, c('MRI_585','MIROC6_585',"NOR_585","FGOALS_585" ,"E3SM_585")], na.rm = TRUE)
anomaly_585$std_row_585 <- apply(anomaly_585[, c('MRI_585','MIROC6_585',"NOR_585","FGOALS_585" ,"E3SM_585")], 1, sd, na.rm = TRUE)
anomaly_585<-anomaly_585[, c("mean_col_585", "std_row_585")]

ggplot(anomaly_585, aes(x=years)) + 
  geom_line(aes(y=MRI, color="MRI")) + 
  geom_line(aes(y=MIROC6, color="MIROC6")) +
  geom_line(aes(y=FGOALS, color="FGOALS")) +
  geom_line(aes(y=E3SM, color="E3SM")) +
  geom_line(aes(y=NOR, color="NOR")) +
  labs(x = "years", y = "Active Layer Depth [m]", title = "Active Layer Depth, ssp585") +
  theme_minimal() +
  xlab("years")

setwd("/Users/laraoxley/Desktop/data/CMIP6_corr")
##ALD years axis ssps
ALD_126<-read.csv("ALD_ssp126.csv")
#ALD_126 <- ALD_126 %>% select(-E3SM)
#ALD_126 <- na.omit(ALD_126)
ggplot(ALD_126, aes(x=years)) + 
  geom_line(aes(y=MRI, color="MRI")) + 
  geom_line(aes(y=MIROC6, color="MIROC6")) +
  geom_line(aes(y=FGOALS, color="FGOALS")) +
  geom_line(aes(y=E3SM, color="E3SM")) +
  geom_line(aes(y=NOR, color="NOR")) +
  labs(x = "years", y = "Active Layer Depth [m]", title = "Active Layer Depth, ssp126") +
  xlim(1850,2100)+xlab("years")+
  ylim(0.5,5)

str(ALD_126)
ALD_126$mean<-as.numeric(ALD_126$mean)
ALD_126$std<-as.numeric(ALD_126$std)
##calculate the yearsly thaw depth
# Calculate yearsly thaw rate
ALD_126_filtered <- ALD_126[ALD_126$years >= 2015 & ALD_126$years <= 2100, ]

ALD_126_filtered$yearsly_Thaw_Rate <- c(NA, diff(ALD_126_filtered$mean))

# View the updated data
head(ALD_126_filtered)

total_change <- max(ALD_126_filtered$mean) - min(ALD_126_filtered$mean)
average_rate <- total_change / (max(ALD_126_filtered$years) - min(ALD_126_filtered$years))
print(average_rate)

# Plot Active Layer Depth (ALD) and yearsly Thaw Rate
ggplot(data = ALD_126_filtered, aes(x = years)) +
  geom_line(aes(y = mean, color = "mean")) +
  geom_line(aes(y = yearsly_Thaw_Rate, color = "yearsly Thaw Rate")) +
  labs(y = "Depth / Rate", color = "Legend") +
  ggtitle("Active Layer Depth and yearsly Thaw Rate") +
  theme_minimal()

#anomaly plot
E3SM_model<-ALD_126[, c("years", "E3SM")]
# Filter the data for yearss between 1860 and 1920
filtered_E3SM <- E3SM_model[E3SM_model$years >= 1860 & E3SM_model$years <= 1900, ]
# Calculate the mean of the variable in the filtered data
mean_E3SM <- mean(filtered_E3SM$E3SM, na.rm = TRUE)

# Assuming 'years' is a non-numeric column you want to exclude
E3SM_a <- E3SM_model
E3SM_a[ , -which(names(E3SM_model) == "years")] <- E3SM_model[ , -which(names(E3SM_model) == "years")] - mean_E3SM

#anomaly plot
NOR_model<-ALD_126[, c("years", "NOR")]
# Filter the data for yearss between 1860 and 1920
filtered_NOR <- NOR_model[NOR_model$years >= 1860 & NOR_model$years <= 1900, ]
# Calculate the mean of the variable in the filtered data
mean_NOR<- mean(filtered_NOR$NOR, na.rm = TRUE)

# Assuming 'years' is a non-numeric column you want to exclude
NOR_a <- NOR_model
NOR_a[ , -which(names(NOR_model) == "years")] <- NOR_model[ , -which(names(NOR_model) == "years")] - mean_NOR

#anomaly plot
FGOALS_model<-ALD_126[, c("years", "FGOALS")]
# Filter the data for yearss between 1860 and 1920
filtered_FGOALS <- FGOALS_model[FGOALS_model$years >= 1860 & FGOALS_model$years <= 1900, ]
# Calculate the mean of the variable in the filtered data
mean_FGOALS<- mean(filtered_FGOALS$FGOALS, na.rm = TRUE)

# Assuming 'years' is a non-numeric column you want to exclude
FGOALS_a <- FGOALS_model
FGOALS_a[ , -which(names(FGOALS_model) == "years")] <- FGOALS_model[ , -which(names(FGOALS_model) == "years")] - mean_FGOALS

#anomaly plot
MIROC6_model<-ALD_126[, c("years", "MIROC6")]
# Filter the data for yearss between 1860 and 1920
filtered_MIROC6 <- MIROC6_model[MIROC6_model$years >= 1860 & MIROC6_model$years <= 1900, ]
# Calculate the mean of the variable in the filtered data
mean_MIROC6<- mean(filtered_MIROC6$MIROC6, na.rm = TRUE)

# Assuming 'years' is a non-numeric column you want to exclude
MIROC6_a <- MIROC6_model
MIROC6_a[ , -which(names(MIROC6_model) == "years")] <- MIROC6_model[ , -which(names(MIROC6_model) == "years")] - mean_MIROC6

#anomaly plot
MRI_model<-ALD_126[, c("years", "MRI")]
# Filter the data for yearss between 1860 and 1920
filtered_MRI <- MRI_model[MRI_model$years >= 1860 & MRI_model$years <= 1900, ]
# Calculate the mean of the variable in the filtered data
mean_MRI<- mean(filtered_MRI$MRI, na.rm = TRUE)

# Assuming 'years' is a non-numeric column you want to exclude
MRI_a <- MRI_model
MRI_a[ , -which(names(MRI_model) == "years")] <- MRI_model[ , -which(names(MRI_model) == "years")] - mean_MRI



anomaly_total<-data.frame(MRI_a, MIROC6_a, FGOALS_a,E3SM_a, NOR_a)
anomaly_126<-anomaly_total[,c("years","MRI", "MIROC6","NOR","FGOALS","E3SM")]
names(anomaly_126)<-c("years", "MRI_126", "MIROC6_126","NOR_126","FGOALS_126","E3SM_126")
anomaly_126 <- anomaly_126[1:251, ]
anomaly_126$mean_col_126 <- rowMeans(anomaly_126[, c('MRI_126','MIROC6_126',"NOR_126","FGOALS_126" ,"E3SM_126")], na.rm = TRUE)
anomaly_126$std_row_126 <- apply(anomaly_126[, c('MRI_126','MIROC6_126',"NOR_126","FGOALS_126" ,"E3SM_126")], 1, sd, na.rm = TRUE)

anomaly_126<-anomaly_126[, c("mean_col_126", "std_row_126")]
ggplot(anomaly_126, aes(x=years)) + 
  geom_line(aes(y=MRI, color="MRI")) + 
  geom_line(aes(y=MIROC6, color="MIROC6")) +
  geom_line(aes(y=FGOALS, color="FGOALS")) +
  geom_line(aes(y=E3SM, color="E3SM")) +
  geom_line(aes(y=NOR, color="NOR")) +
  labs(x = "years", y = "Active Layer Depth [m]", title = "Active Layer Depth, ssp126") +
  theme_minimal() +
  xlab("years")

##ALD years axis ssps
ALD_370<-read.csv("ALD_ssp370.csv")

ggplot(ALD_370, aes(x=years)) + 
  geom_line(aes(y=MRI, color="MRI")) + 
  geom_line(aes(y=MIROC6, color="MIROC6")) +
  geom_line(aes(y=FGOALS, color="FGOALS")) +
  geom_line(aes(y=E3SM, color="E3SM")) +
  geom_line(aes(y=NOR, color="NOR")) +
  labs(x = "years", y = "Active Layer Depth [m]", title = "Active Layer Depth, ssp370") +
  theme_minimal() +
  xlab("years")

str(ALD_370)
ALD_370$mean<-as.numeric(ALD_370$mean)
ALD_370$std<-as.numeric(ALD_370$std)


#anomaly plot
E3SM_model<-ALD_370[, c("years", "E3SM")]
# Filter the data for yearss between 1860 and 1920
filtered_E3SM <- E3SM_model[E3SM_model$years >= 1860 & E3SM_model$years <= 1900, ]
# Calculate the mean of the variable in the filtered data
mean_E3SM <- mean(filtered_E3SM$E3SM, na.rm = TRUE)

# Assuming 'years' is a non-numeric column you want to exclude
E3SM_a <- E3SM_model
E3SM_a[ , -which(names(E3SM_model) == "years")] <- E3SM_model[ , -which(names(E3SM_model) == "years")] - mean_E3SM

#anomaly plot
NOR_model<-ALD_370[, c("years", "NOR")]
# Filter the data for yearss between 1860 and 1920
filtered_NOR <- NOR_model[NOR_model$years >= 1860 & NOR_model$years <= 1900, ]
# Calculate the mean of the variable in the filtered data
mean_NOR<- mean(filtered_NOR$NOR, na.rm = TRUE)

# Assuming 'years' is a non-numeric column you want to exclude
NOR_a <- NOR_model
NOR_a[ , -which(names(NOR_model) == "years")] <- NOR_model[ , -which(names(NOR_model) == "years")] - mean_NOR

#anomaly plot
FGOALS_model<-ALD_370[, c("years", "FGOALS")]
# Filter the data for yearss between 1860 and 1920
filtered_FGOALS <- FGOALS_model[FGOALS_model$years >= 1860 & FGOALS_model$years <= 1900, ]
# Calculate the mean of the variable in the filtered data
mean_FGOALS<- mean(filtered_FGOALS$FGOALS, na.rm = TRUE)

# Assuming 'years' is a non-numeric column you want to exclude
FGOALS_a <- FGOALS_model
FGOALS_a[ , -which(names(FGOALS_model) == "years")] <- FGOALS_model[ , -which(names(FGOALS_model) == "years")] - mean_FGOALS

#anomaly plot
MIROC6_model<-ALD_370[, c("years", "MIROC6")]
# Filter the data for yearss between 1860 and 1920
filtered_MIROC6 <- MIROC6_model[MIROC6_model$years >= 1860 & MIROC6_model$years <= 1900, ]
# Calculate the mean of the variable in the filtered data
mean_MIROC6<- mean(filtered_MIROC6$MIROC6, na.rm = TRUE)

# Assuming 'years' is a non-numeric column you want to exclude
MIROC6_a <- MIROC6_model
MIROC6_a[ , -which(names(MIROC6_model) == "years")] <- MIROC6_model[ , -which(names(MIROC6_model) == "years")] - mean_MIROC6

#anomaly plot
MRI_model<-ALD_370[, c("years", "MRI")]
# Filter the data for yearss between 1860 and 1920
filtered_MRI <- MRI_model[MRI_model$years >= 1860 & MRI_model$years <= 1900, ]
# Calculate the mean of the variable in the filtered data
mean_MRI<- mean(filtered_MRI$MRI, na.rm = TRUE)

# Assuming 'years' is a non-numeric column you want to exclude
MRI_a <- MRI_model
MRI_a[ , -which(names(MRI_model) == "years")] <- MRI_model[ , -which(names(MRI_model) == "years")] - mean_MRI


anomaly_total<-data.frame(MRI_a, MIROC6_a, FGOALS_a,E3SM_a, NOR_a)
anomaly_370<-anomaly_total[,c("years", "MRI", "MIROC6","NOR","FGOALS","E3SM")]
names(anomaly_370)<-c("years", "MRI_370", "MIROC6_370","NOR_370","FGOALS_370","E3SM_370")
anomaly_370 <- anomaly_370[1:251, ]
anomaly_370$mean_col_370 <- rowMeans(anomaly_370[, c('MRI_370','MIROC6_370',"NOR_370","FGOALS_370" ,"E3SM_370")], na.rm = TRUE)
anomaly_370$std_row_370 <- apply(anomaly_370[, c('MRI_370','MIROC6_370',"NOR_370","FGOALS_370" ,"E3SM_370")], 1, sd, na.rm = TRUE)
anomaly_370<-anomaly_370[, c("mean_col_370", "std_row_370")]
years<-(1850:2100)
anomaly_total<-data.frame(anomaly_370, anomaly_126, anomaly_585,years)

ggplot(anomaly_370, aes(x=years)) + 
  geom_line(aes(y=MRI_370, color="MRI")) + 
  geom_line(aes(y=MIROC6_370, color="MIROC6")) +
  geom_line(aes(y=FGOALS_370, color="FGOALS")) +
  geom_line(aes(y=E3SM_370, color="E3SM")) +
  geom_line(aes(y=NOR_370, color="NOR")) +
  labs(x = "years", y = "Active Layer Depth [m]", title = "Active Layer Depth, ssp370") +
  theme_minimal() +
  xlab("years")


######plotting mean up to a years and then the individual time series
library(dplyr)



library(reshape2)
#transformed
dfm <- melt(anomaly_total[,c("mean_col_370", "std_row_370", "mean_col_126","std_row_126", "mean_col_585", "std_row_585", "years")], id.vars = "years")
names(dfm)[1]<-"years"
names(dfm)[2]<-"Model"
names(dfm)[3]<-"ALD"
head(dfm)


#anomaly mean
# Add a new column 'mean_col' that contains the mean of 'col1', 'col2', and 'col3'
anomaly_total$mean_col <- rowMeans(anomaly_total[, c("mean_col_370", "std_row_370", "mean_col_126","std_row_126", "mean_col_585", "std_row_585")], na.rm = TRUE)
# Assuming your dataframe is 'df' and you want to calculate row-wise standard deviation for 'col1', 'col2', 'col3'
anomaly_total$std_row <- apply(anomaly_total[, c("mean_col_370", "std_row_370", "mean_col_126","std_row_126", "mean_col_585", "std_row_585")], 1, sd, na.rm = TRUE)
anomaly_mean<-anomaly_total[, c("years","mean_col", "std_row")]

# Create a new column to distinguish yearss before and after 2015
anomaly_mean$color_group <- ifelse(anomaly_mean$years > 2015, "After 2015", "Before 2015")

# Define the time cutoff
time_cutoff <- 2014

# Filter `anomaly_mean` up to the cutoff
mean_df <- anomaly_mean %>%
  filter(years <= time_cutoff)
tail(mean_df)
# Filter `anomaly_total` after the cutoff
individual_df <- anomaly_total %>%
  filter(years > time_cutoff)
head(individual_df)

ggplot() +
  # Plot the mean values up to the cutoff
  geom_line(data = mean_df, aes(x = years, y = mean_col), color = "black", linewidth = 1.2, linetype = "solid") +
  geom_ribbon(data = mean_df,aes(x = years, ymin = mean_col - std_row, ymax = mean_col + std_row), fill = "grey80", alpha = 0.5) +
  geom_line(data = individual_df, aes(x = years, y = mean_col_126, color = "ssp126"), linewidth = 0.8) +
  geom_ribbon(data = individual_df,aes(x = years, ymin = mean_col_126 - std_row_126, ymax = mean_col_126 + std_row_126), fill = "grey80", alpha = 0.5) +
  geom_line(data = individual_df, aes(x = years, y = mean_col_370, color = "ssp370"), linewidth = 0.8) +
  geom_ribbon(data = individual_df,aes(x = years, ymin = mean_col_370 - std_row_370, ymax = mean_col_370 + std_row_370), fill = "grey80", alpha = 0.5) +
  geom_line(data = individual_df, aes(x = years, y = mean_col_585, color = "ssp585"), linewidth = 0.8) +
  geom_ribbon(data = individual_df,aes(x = years, ymin = mean_col_585 - std_row_585, ymax = mean_col_585 + std_row_585), fill = "grey80", alpha = 0.5) +
  geom_vline(xintercept = 2015, linetype = "dashed", color = "black") +
  labs(title = "Active Layer Depth, different SSPs",
       x = "Time",
       y = "Active Layer Depth") +
  theme_minimal() +
  xlim(1850,2100)+
  theme(legend.position = "bottom")



#compare average of 1880-1900, 1990-2010 and 2080-2100
library(dplyr)

# Group data by years
ALD_tf0 <- ALD_126 %>%
  filter(years >= 1880 & years <= 1900) %>%
  group_by(years)

# Group data by years
ALD_tf1 <- ALD_126 %>%
  filter(years >= 1990 & years <= 2010) %>%
  group_by(years)

ALD_tf2 <- ALD_126 %>%
  filter(years >= 2080 & years <= 2100) %>%
  group_by(years) 

n<-21
TF0<-"1880-1900"
TF1<-"1990-2010"
TF2<-"2080-2100"
x<-rep(c(TF0,TF1,TF2), each=n)
ALD_tf3<-rbind(ALD_tf0,ALD_tf1, ALD_tf2)
ALD_tf3$TF<-x
write.csv(ALD_tf3, "ALD_bars.csv", row.names = FALSE)

#install.packages("ggpubr")
library(ggpubr)

ggbarplot(ALD_tf3, x = "TF", y = "mean",
          add = "mean_sd", fill = "TF", ylim=c(0,4))

# Perform one-way ANOVA
anova_result <- aov(mean ~ TF, data = ALD_tf3)
mean<- anova_result$xlevels
summary(anova_result)
options(digits = 15)
TukeyHSD(anova_result)
library(dplyr)

group_means <- ALD_tf3 %>%
  group_by(TF) %>%
  summarize(mean_ALD = mean(mean, na.rm = TRUE))  # `na.rm = TRUE` removes any NA values

# Print the result
print(group_means)
