#MESI data from Stocker et al 2024
setwd("/Users/laraoxley/Desktop/data/MESI")
#install.packages("metafor")
library(metafor)
# Read in data in (default) wide format
MainTable = read.csv("mesi_main.csv",header=T)

library(tidyverse)

df_filtered <- MainTable[MainTable$lat >= 60, ]
df_filtered<-df_filtered[df_filtered$npk== "_100", ]
str(df_filtered)
table(df_filtered$ecosystem_type)
df_filtered <- df_filtered[!is.na(df_filtered$npk), ]
df_short<-df_filtered[,c("citation", "response", "ecosystem_type", "vegetation_type", "experiment_type", "treatment", 
                         "x_c","x_t","x_units","sd_c","sd_t","se_c","se_t", "rep_c","rep_t")]
nrow(df_short)
print(df_short)
str(df_short)
table(df_short$ecosystem_type)
table(df_tundra$response)
df_tundra <- df_short[df_short$ecosystem_type == "tundra", ]
df_tundra_bgb <- df_tundra[df_tundra$response == "bgb", ]

df_tundra_bgb <- df_tundra %>%
  filter(response %in% c("agb", "total_biomass", "anpp", "bgb", "fine_root_biomass","gpp"))
table(df_tundra_bgb$ecosystem_type)



analyse_meta <- function(df, nam_target) {
  
  # Check for sufficient data
  if (nrow(dplyr::filter(df, response == !!nam_target)) < 3) {
    
    warning(paste("Too little data for meta analysis for", nam_target))
    modl <- NA
    df_box <- tibble()
    
  } else {
    
    # Add effect size and variance columns
    df <- df %>%
      mutate(
        logr = log(x_t / x_c),
        logr_var = (sd_t^2 / (rep_t * (x_t^2))) + (sd_c^2 / (rep_c * (x_c^2))),
        exp = citation  # use citation as experiment ID
      )
    
    # Fit meta-analysis model
    modl <- try(metafor::rma.mv( 
      logr, 
      logr_var,
      method = "REML", 
      random = ~ 1 | exp, 
      slab = exp,
      control = list(stepadj = 0.3), 
      data = df %>% filter(response == !!nam_target)
    ))
    
    # Handle model fitting failure
    if (inherits(modl, "try-error")) {
      modl <- NA
      df_box <- tibble()
      
    } else {
      modl_scaled <- predict(modl, transf = exp)
      
      df_box <- tibble(
        var = nam_target,
        middle = modl$b[1,1],
        ymin   = modl$ci.lb,
        ymax   = modl$ci.ub,
        middle_scaled = modl_scaled$pred,
        ymin_scaled   = modl_scaled$ci.lb,
        ymax_scaled   = modl_scaled$ci.ub
      )
    }
  }
  
  return(list(df_box = df_box, modl = modl))  
}

result <- analyse_meta(df_tundra_bgb, nam_target = "response")  # Replace "SOM" with the variable you want

# Step 3: View results
result$df_box  # Summary tibble
result$modl    # Meta-analysis model object


