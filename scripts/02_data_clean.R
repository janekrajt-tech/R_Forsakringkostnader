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


#Fixat chronic_condition så den är logical ist

data_clean$annual_checkups <- as.integer(data_clean$annual_checkups)

#Ändrat datatyp på annual_checkups till int 


data_clean <- data_clean %>% 
  mutate(age_group = 
           case_when(
             age <= 30  ~ "young",
             age > 30 & age <= 50  ~ "middle",
             age > 50   ~ "older"
           )
  )
#Skapat age_group för att förenkla jamförelser mellan åldersgrupper

data_clean <- data_clean |>
  mutate(risk_score = as.numeric(smoker) + 
           as.numeric(chronic_condition) + 
           prior_claims)

#Kombination av flera risk faktorer som skapar en risk_score. Fler risker - högre


check_alter <- function(data, col) {
  data %>%
    count({{ col }}, sort = TRUE)
}
#Funkionen kollar de olika svarsalternativ i kolumnen, ifall något inte stämmer
check_alter(data_clean, region)

data_clean <- data_clean %>%
  mutate(exercise_level = ifelse(trimws(exercise_level) == "", "missing", exercise_level))

data_clean <- data_clean %>% 
  mutate(region = clean_text_col(region))

cat_col_in_data_clean <- names(data_clean)[
  sapply(data_clean, is.character)  &
    names(data_clean) != "customer_id"]
# Alla kategoriska kolumner som ska checkas om allt stämmer

for (col in cat_col_in_data_clean) {
  cat("\n---", col, "---\n")
  print(check_alter(data_clean, !!sym(col)))
}
# En loop som gör det enklare att kolla alla kategoriska kolumner på en gång 
data_clean <- data_clean %>% 
  mutate(plan_type = clean_text_col(plan_type))


data_clean <- data_clean %>%
  mutate(across(c(sex, region, smoker, chronic_condition,
                  exercise_level, plan_type, age_group), as.factor))

#Ändrat alla char till factor kolumner 

glimpse(data_clean)