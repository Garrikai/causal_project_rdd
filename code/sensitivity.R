###############################################################################
################# Sensitivity #################################################

############# Data Manipulation of the  running variable #######################
test_density <- rddensity(data2yr$agemonth, c = 720)
summary(test_density)

plot_density_test <- rdplotdensity(rdd = test_density,
                                   X = data2yr$agemonth,
                                   type = "both",
                                   title = "McCrary Density Test",
                                   xlabel = "Age (months)")



############### Optimal bandwidth for non parametric #############################################
IKbandwidth(X = data2yr$agemonth, Y = data2yr$w5_expf, cutpoint = 720, verbose = T, kernel = "rectangular")

################ Change windows/bandwidth of parametric ################################
#### Parametric
lm_same_bw3 <- lm(w5_expf ~ treatment + agemonth,
                  data = filter(data2yr,
                                agemonth >= -756 & agemonth <= 756))
lm_same_bw5 <- lm(w5_expf ~ treatment + agemonth,
                  data = filter(data2yr,
                                agemonth >= -780 & agemonth <= 780))
modelsummary(list("Full data" = lm_same,
                  "Bandwidth = 3" = lm_same_bw3,
                  "Bandwidth = 5" = lm_same_bw5))

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
    kable("latex", caption = "Bandwidth selection using Non-parametric estimation") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

