p_customers_q <- ggplot(data_clean, aes(x = region)) +
  geom_bar() +
  labs(
    title = "Antal kunder per region",
    x = "Region",
    y = "Antal") + 
  theme_minimal()

p_agegroup_q <- ggplot(data_clean, aes(x = age_group)) +
  geom_bar() +
  labs(
    title = "Antal kunder per åldersgrupp",
    x = "Åldersgrupp",
    y = "Antal")+ 
  theme_minimal()

p_charges_hist <- ggplot(data_clean, aes(x = charges)) +
  geom_histogram(binwidth = 1000) +
  labs(
    title = "Histogram över charges",
    x = "charges",
    y = "Antal")+ 
  theme_minimal()

p_charges_vs_age <- ggplot(data_clean, aes(x = charges, y = age, colour = age_group)) + 
  geom_point() +
  labs(
    title = "Charges vs age",
    x = "charges",
    y = "age") + 
  theme_minimal()

p_charges_bmi <- ggplot(data_clean, aes(x = charges, y = bmi)) + 
  geom_point() +
  labs(
    title = "Charges vs bmi",
    x = "Charges",
    y = "Bmi")+ 
  theme_minimal()

mean_charges_sex <- data_clean %>%
  group_by(sex) %>%
  summarise(mean_charges = mean(charges, na.rm = TRUE)) %>%
  arrange(desc(mean_charges))

mean_charges_region <- data_clean %>%
  group_by(region) %>%
  summarise(mean_charges = mean(charges, na.rm = TRUE)) %>%
  arrange(desc(mean_charges))

mean_charges_agegroup <- data_clean %>%
  group_by(age_group) %>%
  summarise(mean_charges = mean(charges, na.rm = TRUE)) %>%
  arrange(desc(mean_charges))


mean_charges_plantype <- data_clean %>%
  group_by(plan_type) %>%
  summarise(mean_charges = mean(charges, na.rm = TRUE)) %>%
  arrange(desc(mean_charges))

p_agegroup_charges_hist <- ggplot(data_clean, aes( x = age_group, y = charges)) +
  geom_boxplot() +
  labs(
    title = "Boxplot Åldersgrupp vs Charges",
    x = "Åldersgrupp",
    y = "Charges")+ 
  theme_minimal()

mean_charges_smoker <- data_clean %>%
  group_by(smoker) %>%
  summarise(mean_charges = mean(charges, na.rm = TRUE)) %>% 
  arrange(desc(mean_charges))

smoker_charges_boxplot <- ggplot(data_clean, aes(x = smoker, y= charges, fill = smoker)) +
  geom_boxplot() +
  labs(
    title = "Boxplot över charges för rökare och ickerökare",
    x = "Rökare",
    y = "Charges") + 
    theme_minimal()
  

p_charges_chronic <- ggplot(data_clean, aes(x = chronic_condition, y = charges)) +
  geom_boxplot() +
  labs(
    title = "Boxplot över kronisk sjukdom",
    x = "Kronisk Sjukdom",
    y = "Charges")+ 
  theme_minimal()

p_prior_claims_charges <- ggplot(data_clean, aes(x = factor(prior_claims), y = charges))  +
  geom_boxplot() +
  labs(
    title = "Boxplot för tidigare claims", 
    x = "Antal tidigare claims",
    y = "Charges")+ 
  theme_minimal()

ggsave(here("output", "figures", "p_customers_q.png"), p_customers_q)
ggsave(here("output", "figures", "p_agegroup_q.png"), p_agegroup_q)
ggsave(here("output", "figures", "p_charges_hist.png"), p_charges_hist)
ggsave(here("output", "figures", "p_charges_vs_age.png"), p_charges_vs_age)
ggsave(here("output", "figures", "p_charges_bmi.png"), p_charges_bmi)
ggsave(here("output", "figures", "p_agegroup_charges_hist.png"), p_agegroup_charges_hist)
ggsave(here("output", "figures", "p_charges_chronic.png"), p_charges_chronic)
ggsave(here("output", "figures", "p_prior_claims_charges.png"), p_prior_claims_charges)








