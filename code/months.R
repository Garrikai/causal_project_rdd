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
library(rio)
library(AER)
library(lmtest)
library(sandwich)
library(modelsummary)
library(broom)
library(kableExtra)



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




#head(data2yr) %>% pander()
##################################### Data Description##################################
descriptive <- data2yr %>%
    select(w5_expf, w5_hhincome, gen, race_african, health_binary, w5_best_edu, w5_best_marstt, w5_hhsizer )

descriptive1 <- data2yr %>% #Splitting the tables
    select(w5_expf, w5_hhincome, gen, race_african, health_binary, w5_best_edu, w5_best_marstt, w5_hhsizer, treatment )
treated_desc <- descriptive1 %>% filter(treatment == 1)
untreated_desc <- descriptive1 %>% filter(treatment == 0 )


stargazer( descriptive,
           type = "latex",
           omit.summary.stat = c("p25", "p75"),
           covariate.labels = c("Household Food Expenditure", "Household Income", "Male",  "African", "Perceived health status", "Education", "Marital Status","Household Size"),
           digits = 2,
           title = "Descriptive Statistics for full sample")# summary statistics

stargazer( untreated_desc,
           type = "latex",
           omit.summary.stat = c("p25", "p75"),
           covariate.labels = c("Household Food Expenditure", "Household Income", "Male",  "African", "Perceived health status", "Education", "Marital Status","Household Size"),
           digits = 2,
           title = "Descriptive Statistics for Pr-Old age pension eligible")# summary statistics


stargazer( treated_desc,
           type = "latex",
           omit.summary.stat = c("p25", "p75"),
           covariate.labels = c("Household Food Expenditure", "Household Income", "Male",  "African", "Perceived health status", "Education", "Marital Status","Household Size"),
           digits = 2,
           title = "Descriptive Statistics for Post Old-age pension eligible")# summary statistics

table1 <- capture.output(stargazer(descriptive, type = "latex",style = "qje", omit.summary.stat = c("p25", "p75"),covariate.labels = c("Household Food Expenditure", "Household Income", "Male",  "African", "Perceived health status", "Education", "Marital Status","Household Size"),
                                   digits = 2,
                                   title = "Descriptive Statistics for full sample"))
table2 <- capture.output(stargazer(treated_desc, type = "latex",style = "qje", omit.summary.stat = c("p25", "p75"),
                                   covariate.labels = c("Household Food Expenditure", "Household Income", "Male",  "African", "Perceived health status", "Education", "Marital Status","Household Size"),
                                   digits = 2,
                                   title = "Descriptive Statistics for Post Old-age pension eligible"))
table3 <- capture.output(stargazer(untreated_desc, type = "latex", style = "qje", omit.summary.stat = c("p25", "p75"),
                                   covariate.labels = c("Household Food Expenditure", "Household Income", "Male",  "African", "Perceived health status", "Education", "Marital Status","Household Size"),
                                   digits = 2,
                                   title = "Descriptive Statistics for Pre-Old age pension eligible"))
# Combine the tables
combined_table <- paste0( "<div style='display: flex; flex-direction: row;'>", "<div style='margin-right: 20px;'>", paste(table1, collapse = "\n"), "</div>", "<div style='margin-right: 20px;'>", paste(table2, collapse = "\n"), "</div>", "<div>", paste(table3, collapse = "\n"), "</div>", "</div>" )
# Print the combined table
cat(combined_table, sep = "\n")

####################################### Visuals#############################################
# Testing for manipulation
# We draw a histogram to find how many of the individuals in our sample
# find themselves having an age above the cutoff as a first test. Where does the bulk of
# individuals lie. We need to make sure there is not much manipulation around the threshold of 65.
# We can then infer exogeneity between people around the threshold.



## Check for Take up. Precisely will the take up be higher for the treated group
#Is there really a discontinuity around the age score. Is there a jump around the threshold
g1 <- ggplot(data2yr,
       aes(x = agemonth,
           y = treatment,
           color = factor(treatment))) +
    geom_point(alpha = 0.5) +
    geom_smooth(se = FALSE) +
    labs(x = "Age of individuals",
         y = "Cumulative probability of take up") +
    scale_color_discrete(name = " ",
                         labels = c("No treatment", "Treatment")) +
    geom_vline(xintercept = 720, linetype = "dotted") +
    theme_minimal()


g2 <- ggplot(data2yr, aes(x = agemonth, y = w5_expf, group = treatment)) +
    geom_vline(xintercept = 720, linetype = "dashed") +
    geom_point(alpha=0.5) +
    geom_smooth(method = "loess") +
    theme_bw() +
    labs(title = "Relationship between Age (at 720 months) and Household Food Expenditure",
         x = "Centered Age (months)",
         y = "Average_Household_Food_Expenditure")

################################################################################
binned2yr <- data2yr %>%
    group_by(agemonth) %>%
    summarize(Average_Household_Food_Expenditure = mean(w5_expf, na.rm = TRUE))

ggplot(binned2yr, aes(x = agemonth, y = Average_Household_Food_Expenditure)) +
    geom_line() +
    geom_point() + # Add points to the line for better visibility
    geom_smooth(method = "loess") +
    labs(title = "Average_Household_Food_Expenditure by binned Age", x = "Age (years)", y = "Average_Household_Food_Expenditure ")

# Split the data into two parts: below and above the cutoff age of 60
binned_belowyr <- binned2yr %>% filter(agemonth <= 720)
binned_aboveyr <- binned2yr %>% filter(agemonth >= 720)

# Create the plot
g3<-
ggplot() +
    # Line for ages below 60
    geom_line(data = binned_belowyr, aes(x = agemonth, y = Average_Household_Food_Expenditure ), color = "blue") +
    geom_point(data = binned_belowyr, aes(x = agemonth, y = Average_Household_Food_Expenditure ), color = "blue") +
    # Line for ages above 60
    geom_line(data = binned_aboveyr, aes(x = agemonth, y = Average_Household_Food_Expenditure ), color = "red") +
    geom_point(data = binned_aboveyr, aes(x = agemonth, y = Average_Household_Food_Expenditure ), color = "red") +
    # Add a cutoff line at 60
    geom_vline(aes(xintercept = 720), linetype = 'dashed') +
    labs(title = "Average_Household_Food_Expenditure  by Age with Cutoff at 720 months",
         x = "Age (months)",
         y = "Average_Household_Food_Expenditure ") +
    theme_minimal()

################################################################################
############ Parametric Estimation of the models  ##############################
################################################################################

### Running linear model with same slope
lm_same <- lm(w5_expf ~ treatment + agemonth, data = data2yr)
stargazer::stargazer(lm_same, type = "latex")

#### linear with different slops
lm_diff <- lm(w5_expf ~ treatment*agemonth, data = data2yr)
stargazer::stargazer(lm_diff, type = "latex")

#### Non linear model
lm_quad <- lm(w5_expf ~ agemonth +
                    I(agemonth^2) + # I tells R to interpret "as is"
                    treatment +
                    I(agemonth * treatment) +
                    I((agemonth^2) * treatment),
                data = data2yr)
stargazer::stargazer(lm_quad, type = "latex")



################################################################################
##########  Non-Parametric Estimation using RDrobust ###########################################

rd_mod1 <- rdrobust::rdrobust(data2yr$w5_expf,
                          data2yr$agemonth,
                          c = cutoff_month,
                          kernel = "tri",
                          bwselect = "mserd")
summary(rd_mod1)

rdrobust::rdplot(data2yr$w5_expf,
                 data2yr$agemonth,
                 c = 720,
                 kernel = "tri",
                 title = "Retirement Consumption relationship",
                 x.label = "Age from 720 months",
                 y.label =  "Household Monthly food expenditure  \n (R1000)"
)


quad_rdrobust <- rdrobust::rdrobust(data2yr$w5_expf,
                                         data2yr$agemonth,
                                         c = 720,
                                         kernel = "tri",
                                         bwselect = "mserd",
                                         p = 2) #polynomial 2
summary(quad_rdrobust)
rdrobust::rdplot(data2yr$w5_expf,
                 data2yr$agemonth,
                 c = 720,
                 kernel = "tri",
                 p = 2,
                 title = "Quadriatic estimation: polynomial of order 2",
                 x.label = "Age from 720 months",
                 y.label =  "Household Monthly food expenditure  \nin Rands (000s)"
)
################################################################################

###############################################################################
################# Sensitivity #################################################

############# Data Manipulation of the  running variable #######################
test_density <- rddensity(data2yr$agemonth, c = 720)
summary(test_density)

plot_density_test <- rdplotdensity(rdd = test_density,
                                   X = data2yr$agemonth,
                                   type = "both")

############### Optimal bandwidth for non parametric #############################################
IKbandwidth(X = data2yr$agemonth, Y = data2yr$w5_expf, cutpoint = 720, verbose = T, kernel = "rectangular")

nlm2 <- lm(w5_expf ~ agemonth * treatment,
          data =data2yr,
          subset = (agemonth >= -780 & agemonth <= 780)) # 5 years(55-65years)
summary(nlm2)


nlmopt <- lm(w5_expf ~ agemonth * treatment,
           data =data2yr,
           subset = (agemonth >= -764 & agemonth <= 764)) # 3 years(55-65years)
summary(nlmopt)

################ Change windows/bandwidth of parametric ################################
#### Parametric
lm_same_bw3 <- lm(w5_expf ~ treatment + agemonth,
              data = filter(data2yr,
                            agemonth >= -756 & agemonth <= 756))
tidy(lm_same_bw3)

modelsummary(list("Full data" = lm_same,
                  "Bandwidth = 3" = lm_same_bw3))

### Non-Parametric
models <- list()
models[['Ideal']] <- rdrobust(y = data2yr$w5_expf, x = data2yr$agemonth, c = cutoff_month, h = 16.861)


models[['Twice']] <-rdrobust(y = data2yr$w5_expf, x = data2yr$agemonth, c = cutoff_month, h = 16.861*2)


models[['Half']] <-rdrobust(y = data2yr$w5_expf, x = data2yr$agemonth, c = cutoff_month, h = 16.861/2)

# Function to extract bandwidth and effect size
extract_info <- function(model) {
    list( Bandwidth = model$bws[1], `Effect size` = model$coef[1,1] ) }
# Apply function to each model and convert to a data frame
results <- lapply(models, extract_info) %>%
    bind_rows(.id = "Model") %>%
    rename("Bandwidth (ideal)" = Bandwidth, "Effect size" = `Effect size`)
# Create a nicely formatted table
results %>%
    kable("latex", caption = "Bandwidth selection using common approach") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"))


