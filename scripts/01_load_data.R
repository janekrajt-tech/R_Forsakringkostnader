library(tidyverse)
library(here)
data <- read.csv(here("data", "insurance_costs.csv"))

glimpse(data)

colSums(is.na(data))

summary(data)
# annual_checkups och bmi saknar värde