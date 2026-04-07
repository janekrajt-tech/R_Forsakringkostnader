library(tidyverse)

data <- read.csv("insurance_costs.csv")

glimpse(data)

colSums(is.na(data))

summary(data)
# annual_checkups och bmi saknar värde