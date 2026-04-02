library(tidyverse)

data <- read.csv("insurance_costs.csv")

glimpse(data)
nrow(data)
ncol(data)

colSums(is.na(data))

# annual_checkups och bmi saknar värde

summary(data)

# smoker och chronic_condition kan ändras till logical
data_clean <- data |>
  filter(!is.na(bmi), !is.na(annual_checkups)) |>
  mutate(smoker = trimws(tolower(smoker))) |>
  mutate(smoker = smoker == "yes")

#Jag valde att ta bort rader med NA i både bmi och annual_checkups, då eftersom andel sakande värden var ganska låg


clean_text_col <- function(x) {
  trimws(tolower(x))
  
}
#Funktionen trimmar mellanrum och gör till bara små bokstäver

names(data_clean)[!sapply(data_clean, is.numeric)]

#Kollar om det finns fler icke numeriska kolumner som behövs fixas

data_clean <- data_clean |>
  mutate(chronic_condition = clean_text_col(chronic_condition))|>
  mutate(chronic_condition = chronic_condition == "yes")

prop.table(table(data_clean$chronic_condition))

#Fixat chronic_condition så den är logical ist

data_clean$annual_checkups <- as.integer(data_clean$annual_checkups)

#Ändrat datatyp på annual_checkups till int 

glimpse(data_clean)


