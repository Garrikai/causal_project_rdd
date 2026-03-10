# load dataset
library(haven)
library(rdd)
library(tidyverse)
library(ggplot2)
library(rdrobust)
library(rddensity)
library(rdlocrand)
library(rdmulti)
library(pander)
library(stargazer)
library(rio)
library(AER)
library(lmtest)
library(sandwich)
library(modelsummary)
library(broom)
library(kableExtra)
library(huxtable)



intial <- read_dta("data/preprocessing/intial.dta")


nds_dfyr <- intial %>%
    select(pid, w5_best_age_yrs, w5_best_race,w5_best_gen, w5_a_hldes, w5_expf, w5_hhincome, w5_a_hllfsmkqnt, w5_best_dob_y, w5_best_marstt, w5_best_edu,
           w5_hhsizer, w5_expenditure, w5_best_dob_m) %>%
    mutate(agemonth = w5_best_age_yrs * 12 + w5_best_dob_m)

# Define the cutoff age for pension eligibility

cutoff_month <- 720


# Create binary for pension treatment
nids_datayr <- nds_dfyr %>%
    mutate(treatment = ifelse(agemonth >= cutoff_month,1,0))
#nds_df$treatment <-ifelse(nds_df$w5_best_age_yrs > 60, 1,0 )

# Filter the data to select individuals just below and just above the threshold
month_range <- 48 #60 for robustness


filtered_datayr <- nids_datayr %>%
    filter(agemonth >= (cutoff_month - month_range) & agemonth <= (cutoff_month + month_range)) # 794 observations







#####data cleaning 19 Nov####
# Convert the gender column to a factor variable with appropriate levels
filtered_datayr$w5_best_gen <- factor(filtered_datayr$w5_best_gen, levels = c(-9, -8, -3, 1, 2), labels = c("Don't know", "Refused", "Missing", "Male", "Female"))
filtered_datayr$gen <- ifelse(filtered_datayr$w5_best_gen == "Male", 1, ifelse(filtered_datayr$w5_best_gen == "Female", 0, NA))
# Convert the Race, we will only use african
filtered_datayr$w5_best_race <- factor(filtered_datayr$w5_best_race, levels = c(-9, -8, -3, 1, 2, 3, 4), labels = c("Don't know", "Refused", "Missing", "African", "Coloured", "Asian", "White"))
filtered_datayr$race_african <- ifelse(filtered_datayr$w5_best_race == "African", 1, 0)
# Catergorize the health assessment status to healthy and unhealthy
filtered_datayr$w5_a_hldes <- factor(filtered_datayr$w5_a_hldes, levels = c(-9, -8, -5, -3, 1, 2, 3, 4, 5), labels = c("Don't know", "Refused", "Not applicable", "Missing", "Excellent", "Very good", "Good", "Fair", "Poor"))
filtered_datayr$health_binary <- ifelse(filtered_datayr$w5_a_hldes %in% c("Excellent", "Very good", "Good"), 1, ifelse(filtered_datayr$w5_a_hldes%in% c("Fair", "Poor"), 0, NA))
# Convert the cigarettes consumed
filtered_datayr$w5_a_hllfsmkqnt <- ifelse(filtered_datayr$w5_a_hllfsmkqnt %in% c(-9, -8, -5, -3), NA, filtered_datayr$w5_a_hllfsmkqnt)
filtered_datayr$w5_a_hllfsmkqnt <- as.numeric(filtered_datayr$w5_a_hllfsmkqnt)


# More conversions
filtered_datayr <- filtered_datayr %>%
    mutate(year = as.numeric(w5_best_dob_y))

# Function to remove attributes
remove_attributes <- function(x) {
    attributes(x) <- NULL
    return(x)
}

# Remove specific attribute 'label'
remove_label <- function(x) {
    attr(x, "label") <- NULL
    return(x)
}

# Function to remove all attributes from a variable
remove_all_attributes <- function(x) {
    attributes(x) <- NULL
    return(x)
}

str(filtered_datayr)

# Create a new dataset
filtered_data1yr <- filtered_datayr %>%
    select(pid, w5_hhincome, w5_a_hllfsmkqnt, health_binary,w5_expf, gen, race_african, treatment,agemonth, w5_best_marstt, w5_best_edu,
           w5_hhsizer, w5_expenditure)
datayr <- as.data.frame(lapply(filtered_data1yr, remove_all_attributes))
#data2 <- data %>%filter(gen == 0) # only females
data2yr <- datayr %>% mutate(month_cen = agemonth - cutoff_month)