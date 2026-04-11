r_model <- lm(charges ~ age + bmi + smoker + chronic_condition + prior_claims, data = data_clean)
print(summary(r_model))

r_model_2 <- lm(charges ~ age + bmi + risk_score, data = data_clean)
print(summary(r_model_2))

