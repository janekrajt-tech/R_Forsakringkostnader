r_model <- lm(charges ~ age + bmi + smoker + chronic_condition + prior_claims, data = data_clean)
summary(r_model)
