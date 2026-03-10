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
