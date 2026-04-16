model_1 <- lm(charges ~ smoker + chronic_condition, data = data_clean)
model_2 <- lm(charges ~ smoker + chronic_condition + prior_claims, data = data_clean)
model_3 <- lm(charges ~ smoker + chronic_condition + prior_claims + age + bmi, data = data_clean)

summary(model_1)
summary(model_3)




model_comparison <- tibble(
  model = c(
    "Model 1: smoker + chronic_condition ",
    "Model 2: + prior_claims",
    "Model 3: + age + bmi"
  ),
  r_squared = c(
    summary(model_1)$r.squared,
    summary(model_2)$r.squared,
    summary(model_3)$r.squared
    
  ),
  adjusted_r_squared = c(
    summary(model_1)$adj.r.squared,
    summary(model_2)$adj.r.squared,
    summary(model_3)$adj.r.squared

  ),
  residual_se = c(
    summary(model_1)$sigma,
    summary(model_2)$sigma,
    summary(model_3)$sigma
   
  )
)

model_comparison

model_3_diagnotics <- data_clean %>% 
  mutate(
    fitted_value = fitted(model_3),
    residual = resid(model_3)
  )


act_res_diff <- model_3_diagnotics %>% 
  select(charges, fitted_value, residual) %>% 
  slice_head(n = 10)


act_vs_pred <- ggplot(model_3_diagnotics, aes(x = fitted_value, y = charges)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Faktiskt pris mot predikterat pris",
    x = "Predikterat charges",
    y = "Faktiskt charges"
  ) +
  theme_minimal()






