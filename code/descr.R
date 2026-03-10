library(huxtable)
descriptive <- data2yr %>%
    select(w5_expf, w5_hhincome, gen, race_african, health_binary, w5_best_edu, w5_best_marstt, w5_hhsizer, treatment) %>%
    rename( Expenditure = w5_expf, Income = w5_hhincome, Gender = gen, AfricanRace = race_african, HealthBinary = health_binary, Education = w5_best_edu, MaritalStatus = w5_best_marstt, HouseholdSize = w5_hhsizer, TreatmentGroup = treatment )

# Create the table
stargazer(descriptive, type = "latex", title = "Descriptive Statistics",
          summary = TRUE, digits = 2)


