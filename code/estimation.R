# Parametric

### Running linear model with same slope
lm_same <- lm(w5_expf ~ treatment + agemonth, data = data2yr)
lm_diff <- lm(w5_expf ~ treatment*agemonth, data = data2yr)
lm_quad <- lm(w5_expf ~ agemonth +
                  I(agemonth^2) + # I tells R to interpret "as is"
                  treatment +
                  I(agemonth * treatment) +
                  I((agemonth^2) * treatment),
              data = data2yr)

# LaTeX tables
stargazer::stargazer(lm_same, lm_diff, lm_quad, type = "latex", title = "Regression Results")


# Non Parametric
rd_mod1 <- rdrobust::rdrobust(data2yr$w5_expf,
                              data2yr$agemonth,
                              c = cutoff_month,
                              kernel = "tri",
                              bwselect = "mserd")
#summary(rd_mod1)

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
#summary(quad_rdrobust)
rdrobust::rdplot(data2yr$w5_expf,
                 data2yr$agemonth,
                 c = 720,
                 kernel = "tri",
                 p = 2,
                 title = "Retirement Consumption relationship",
                 x.label = "Age from 720 months",
                 y.label =  "Household Monthly food expenditure  \nin Rands (000s)"
)


######################################################################
