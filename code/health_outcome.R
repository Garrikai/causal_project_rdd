rm(list = ls())
# Old Age Pension Programs: Datasets that include information on pension receipt, household demographics, and health outcomes, with a clear cutoff for eligibility (e.g., age 60).


# load dataset
library(haven)
library(rdd)
library(tidyverse)
library(ggplot2)
intial <- read_dta("data/preprocessing/intial.dta")
View(intial)

nids_df <- intial %>% select(pid, w5_best_age_yrs, w5_a_hlbp,w5_a_hlbp_bf, w5_a_hlbp_med, w5_a_hlbp_stl, w5_a_hllfexer, w5_a_hllfsmk, w5_a_bpsys_1, w5_a_bpdia_1)

# Create binary for pension treatment
nids_df$treatment <-ifelse(nds_df$w5_best_age_yrs > 60, 1,0 )

nds_df <- nds_df %>%
    filter(nds_df$w5_best_age_yrs >=  40) #12827


# Data preprocessing
df <- nds_df # subset to keep 1 and 2 then change to 1, 0 binary
filtered_df <- df %>% filter(w5_a_incgovpen %in% c("1", "2")) #9426
filtered_df$w5_a_incgovpen <- ifelse(filtered_df$w5_a_incgovpen == 1, 1,0)
unique(filtered_df$w5_a_incgovpen_v) # remove 9,8,5,3 and NA
df2 <- drop_na(filtered_df) #2865
sum(is.na(filtered_df)) #6561 NAs
sum(df2$treatment == 1) #1734.....60%
sum(df2$w5_a_incgovpen == 1) #2865
sum(df2$w5_best_age_yrs == 60) # 172 at the threshold



# Visuals
# Testing for manipulation
# We draw a histogram to find how many of the individuals in our sample
# find themselves having an age above the cutoff as a first test. Where does the bulk of
# individuals lie. We need to make sure there is not much manipulation around the threshold of 65.
# We can then infer exogeneity between people around the threshold.
hist(df2$w5_best_age_yrs)


## Check for Take up. Precisely will the take up be higher for the treated group
#Is there really a discontinuity around the age score. Is there a jump around the threshold
ggplot(df2,
       aes(x = w5_best_age_yrs,
           y = treatment,
           color = factor(treatment))) +
    geom_point() +
    labs(x = "Age of individuals",
         y = "Cumulative probability of take up") +
    scale_color_discrete(name = " ",
                         labels = c("No treatment", "Treatment")) +
    geom_vline(xintercept = 60, linetype = "dotted") +
    theme_minimal()









# Testing for smoothness

