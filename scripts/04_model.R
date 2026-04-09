r_model <- lm(charges ~ age + bmi + smoker + chronic_condition + prior_claims, data = data_clean)
print(summary(r_model))