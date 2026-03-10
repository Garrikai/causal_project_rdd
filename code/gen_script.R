library(tidyverse)

# Load dataset and clean it
cct_df <- readr::read_csv("https://raw.githubusercontent.com/seramirezruiz/stats-ii-lab/master/Session%206/data/cct_data.csv") # loading simulated data frame of the program
View(cct_df)
sum(cct_df$treatment == 1) #2575
sum(cct_df$treatment != 1) #2425


names(cct_df)
cct_df <- rename(cct_df, "id" = "...1")


ggplot(cct_df,
       aes(x = hh_income,
           y = years_of_schooling,
           color = factor(treatment))) +
    geom_point() +
    labs(x = "Household Income",
         y = "Years of Schooling") +
    scale_color_discrete(name = " ",
                         labels = c("No treatment", "Treatment")) +
    geom_vline(xintercept = 20000, linetype = "dotted") +
    theme_minimal()

ggplot(cct_df,
       aes(x = hh_income,
           y = treatment,
           color = factor(treatment))) +
    geom_point() +
    labs(x = "Household Income",
         y = "Treatment") +
    scale_color_discrete(name = " ",
                         labels = c("No treatment", "Treatment")) +
    geom_vline(xintercept = 20000, linetype = "dotted") +
    theme_minimal()


cct_df %>% distinct()



# Apply the function to the relevant columns
filtered_data1$pid <- remove_attributes(filtered_data1$pid)
filtered_data1$w5_hhincome <- remove_attributes(filtered_data1$w5_hhincome)
filtered_data1$linc <- remove_attributes(filtered_data1$linc)
filtered_data1$w5_a_hllfsmkqnt  <- remove_attributes(filtered_data1$w5_a_hllfsmkqnt)
filtered_data1$health_binary  <- remove_attributes(filtered_data1$health_binary)
filtered_data1$gen  <- remove_attributes(filtered_data1$gen)
filtered_data1$race_african  <- remove_attributes(filtered_data1$race_african)
filtered_data1$age  <- remove_attributes(filtered_data1$age)
filtered_data1$treatment  <- remove_attributes(filtered_data1$treatment)

filtered_data1$pid <- remove_label(filtered_data1$pid)
filtered_data1$w5_hhincome  <- remove_label(filtered_data1$w5_hhincome)
filtered_data1$w5_a_hllfsmkqnt  <- remove_label(filtered_data1$w5_a_hllfsmkqnt)
filtered_data1$health_binary  <- remove_label(filtered_data1$health_binary)
filtered_data1$gen  <- remove_label(filtered_data1$gen)
filtered_data1$race_african  <- remove_label(filtered_data1$race_african)
filtered_data1$age  <- remove_label(filtered_data1$age)
filtered_data1$treatment  <- remove_label(filtered_data1$treatment)
filtered_data1$linc  <- remove_label(filtered_data1$linc)

############ Centering the Age variable at 60 years

# Creating Bins for Centered Age and Grouping by Bins
binned <- data2 %>%
    mutate(Age_Bins = cut(Age_Centered,
                          breaks = seq(min(Age_Centered), max(Age_Centered), by = 1))) %>%
    group_by(Age_Bins) %>%
    summarize(Household_exp = mean(w5_expf, na.rm = TRUE),
              Age_Centered = mean(Age_Centered))

# Creating the plot
#g3 <-
ggplot(binned, aes(x = Age_Centered, y = Household_exp)) +
    geom_line() +
    # Add a cutoff line at 0 (which corresponds to the centered age of 60)
    geom_vline(aes(xintercept = 0), linetype = 'dashed') +
    labs(title = "Relationship between Age (Centered at 60) and Household Food Expenditure",
         x = "Centered Age (years)",
         y = "Average Household Food Expenditure")

