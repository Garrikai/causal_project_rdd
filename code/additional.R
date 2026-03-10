# Using summarytools
## dfsummary, desc(), freq()

pacman::p_load(summarytools)

library(tidyverse)


data(trees)
trees %>% glimpse()

trees %>%
    descr() %>%
    stview()


trees %>%
    dfSummary(
        graph.col = TRUE,
        style = "grid",
        graph.magnif = 0.75
    ) %>%
    stview()

### DAGs
install.packages('dagitty')

library(ggdag)
library(dagitty)

df <- dagify(
    earn ~ edu + year + Bkgd + Loc + JobCx,
    Edu ~ Req + Loc + Bkgd + Year,
    JobCx ~ Edu,
    Bkgd ~ U1,
    Loc ~ U1
)

ggdag(df) +
    theme_dag()

# Conditional Indeoendence assumption