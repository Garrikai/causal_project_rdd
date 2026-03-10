rm(list = ls())
# Old Age Pension Programs: Datasets that include information on pension receipt, household demographics, and health outcomes, with a clear cutoff for eligibility (e.g., age 60).

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

intial <- read_dta("data/preprocessing/intial.dta")


#nids_df <- intial %>% select(pid, w5_best_age_yrs, )

nds_df <- intial %>%
    select(pid, w5_best_age_yrs, w5_best_race,w5_best_gen, w5_a_hldes, w5_expf, w5_hhincome, w5_a_hllfsmkqnt, w5_best_dob_y, w5_best_marstt, w5_best_edu,
           w5_hhsizer, w5_expenditure)

# Define the cutoff age for pension eligibility
cutoff_age <- 60


# Create binary for pension treatment
nids_data <- nds_df %>%
    mutate(treatment = ifelse(w5_best_age_yrs >= cutoff_age,1,0))
#nds_df$treatment <-ifelse(nds_df$w5_best_age_yrs > 60, 1,0 )

# Filter the data to select individuals just below and just above the threshold
age_range <- 3 #5 for robustness


filtered_data <- nids_data %>%
    filter(w5_best_age_yrs >= (cutoff_age - age_range) & w5_best_age_yrs <= (cutoff_age + age_range)) # 794 observations







#####data cleaning 19 Nov####
# Convert the gender column to a factor variable with appropriate levels
filtered_data$w5_best_gen <- factor(filtered_data$w5_best_gen, levels = c(-9, -8, -3, 1, 2), labels = c("Don't know", "Refused", "Missing", "Male", "Female"))
filtered_data$gen <- ifelse(filtered_data$w5_best_gen == "Male", 1, ifelse(filtered_data$w5_best_gen == "Female", 0, NA))
# Convert the Race, we will only use african
filtered_data$w5_best_race <- factor(filtered_data$w5_best_race, levels = c(-9, -8, -3, 1, 2, 3, 4), labels = c("Don't know", "Refused", "Missing", "African", "Coloured", "Asian", "White"))
filtered_data$race_african <- ifelse(filtered_data$w5_best_race == "African", 1, 0)
# Catergorize the health assessment status to healthy and unhealthy
filtered_data$w5_a_hldes <- factor(filtered_data$w5_a_hldes, levels = c(-9, -8, -5, -3, 1, 2, 3, 4, 5), labels = c("Don't know", "Refused", "Not applicable", "Missing", "Excellent", "Very good", "Good", "Fair", "Poor"))
filtered_data$health_binary <- ifelse(filtered_data$w5_a_hldes %in% c("Excellent", "Very good", "Good"), 1, ifelse(filtered_data$w5_a_hldes%in% c("Fair", "Poor"), 0, NA))
# Convert the cigarettes consumed
filtered_data$w5_a_hllfsmkqnt <- ifelse(filtered_data$w5_a_hllfsmkqnt %in% c(-9, -8, -5, -3), NA, filtered_data$w5_a_hllfsmkqnt)
filtered_data$w5_a_hllfsmkqnt <- as.numeric(filtered_data$w5_a_hllfsmkqnt)


# More conversions
filtered_data <- filtered_data %>%
    mutate(age = as.numeric(w5_best_age_yrs))

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



# Create a new dataset
filtered_data1 <- filtered_data %>%
    select(pid, w5_hhincome, w5_a_hllfsmkqnt, health_binary,w5_expf, gen, race_african, age, treatment, w5_best_marstt, w5_best_edu,
           w5_hhsizer, w5_expenditure) %>%
    mutate(linc = log(w5_hhincome))
data <- as.data.frame(lapply(filtered_data1, remove_all_attributes))
#data2 <- data %>%filter(gen == 0) # only females
data2 <- data %>% mutate(Age_Centered = age - 60)




head(data2) %>% pander()

stargazer( data2,
           type = "html",
           omit.summary.stat = c("p25", "p75"),
           digits = 2 ) # summary statistics


stargazer( filtered_data,
           type = "html",
           omit.summary.stat = c("p25", "p75"),
           keep = c("treatment", "Age_Centered"),
           digits = 2 ) #

####################################### Visuals#############################################
# Testing for manipulation
# We draw a histogram to find how many of the individuals in our sample
# find themselves having an age above the cutoff as a first test. Where does the bulk of
# individuals lie. We need to make sure there is not much manipulation around the threshold of 65.
# We can then infer exogeneity between people around the threshold.
hist(data2$age)


## Check for Take up. Precisely will the take up be higher for the treated group
#Is there really a discontinuity around the age score. Is there a jump around the threshold
#g1 <-
ggplot(data2,
       aes(x = age,
           y = treatment,
           color = factor(treatment))) +
    geom_point(alpha = 0.5) +
    geom_smooth(se = FALSE) +
    labs(x = "Age of individuals",
         y = "Cumulative probability of take up") +
    scale_color_discrete(name = " ",
                         labels = c("No treatment", "Treatment")) +
    geom_vline(xintercept = 60, linetype = "dotted") +
    theme_minimal()


ggplot(data2, aes(x = age, y = w5_expf)) +
    geom_point() +
    geom_smooth(method = "loess", se = FALSE) +# Add a smoothed line
    labs(title = "Treatment vs. Age", x = "Age (years)", y = "Treatment Variable")


#g2 <-
ggplot(data2, aes(x = age, y = w5_expf, group = treatment)) +
    geom_vline(xintercept = 60, linetype = "dashed") +
    geom_point(alpha=0.5) +
    geom_smooth(method = "loess") +
    theme_bw() +
    labs(title = "Relationship between Age (Centered at 60) and Household Food Expenditure",
         x = "Centered Age (years)",
         y = "Average_Household_Food_Expenditure")

#############################################
binned2 <- data2 %>%
    group_by(age) %>%
    summarize(Average_Household_Food_Expenditure = mean(w5_expf, na.rm = TRUE))

ggplot(binned2, aes(x = age, y = Average_Household_Food_Expenditure)) +
    geom_line() +
    geom_point() + # Add points to the line for better visibility
    geom_smooth(method = "loess") +
    labs(title = "Average_Household_Food_Expenditure by Age", x = "Age (years)", y = "Average_Household_Food_Expenditure ")

# Split the data into two parts: below and above the cutoff age of 60
binned_below <- binned2 %>% filter(age <= 60)
binned_above <- binned2 %>% filter(age >= 60)

# Create the plot
g3<-
ggplot() +
    # Line for ages below 60
    geom_line(data = binned_below, aes(x = age, y = Average_Household_Food_Expenditure ), color = "blue") +
    geom_point(data = binned_below, aes(x = age, y = Average_Household_Food_Expenditure ), color = "blue") +
    # Line for ages above 60
    geom_line(data = binned_above, aes(x = age, y = Average_Household_Food_Expenditure ), color = "red") +
    geom_point(data = binned_above, aes(x = age, y = Average_Household_Food_Expenditure ), color = "red") +
    # Add a cutoff line at 60
    geom_vline(aes(xintercept = 60), linetype = 'dashed') +
    labs(title = "Average_Household_Food_Expenditure  by Age with Cutoff at 60",
         x = "Age (years)",
         y = "Average_Household_Food_Expenditure ") +
    theme_minimal()

#############################################################################################
############Estimation of the models#####################################################
# Linear term and a squared term with "treated" interactions
m <- lm(Support ~ Income_Centered*Participation +
            I(Income_Centered^2)*Participation, data = gt)

# Add a triangular kernel weight
kweight <- function(x) {
    # To start at a weight of 0 at x = 0, and impose a bandwidth of .01,
    # we need a "slope" of -1/.01 = 100,
    # and to go in either direction use the absolute value
    w <- 1 - 100*abs(x)
    # if further away than .01, the weight is 0, not negative
    w <- ifelse(w < 0, 0, w)
    return(w)
}

# Run the same model but with the weight
mw <- lm(Support ~ Income_Centered*Participation, data = gt,
         weights = kweight(Income_Centered))

# See the results with heteroskedasticity-robust SEs
msummary(list('Quadratic' = m, 'Linear with Kernel Weight' = mw),
         stars = c('*' = .1, '**' = .05, '***' = .01), vcov = 'robust')
###########################################################################################

### Running linear model with same slope
lm_same <- lm(w5_expf ~ treatment + Age_Centered, data = data2)
stargazer::stargazer(lm_same, type = "text")



