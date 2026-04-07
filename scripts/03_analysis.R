p_customers_q <- ggplot(data_clean, aes(x = region)) +
  geom_bar() +
  labs(
    title = "Antal kunder per region",
    x = "Region",
    y = "Antal"
  )
p_agegroup_q <- ggplot(data_clean, aes(x = age_group)) +
  geom_bar() +
  labs(
    title = "Antal kunder per åldersgrupp",
    x = "Åldersgrupp",
    y = "Antal"
  )

p_charges_hist <- ggplot(data_clean, aes(x = charges)) +
  geom_histogram() +
  labs(
    title = "Histogram for charges",
    x = "charges"
  )
p_charges_vs_age <- ggplot(data_clean, aes(x = charges, y = age, colour = smoker)) + 
  geom_point() +
  labs(
    title = "Charges vs age",
    x = "charges",
    y = "age"
  )

p_charges_bmi <- ggplot(data_clean, aes(x = charges, y = bmi, colour = age_group)) + 
  geom_point() +
  labs(
    title = "Charges vs bmi",
    x = "bmi",
    y = "age"
  )

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
    title = "Boxplot Age Group vs Charges",
    x = "Age Group",
    y = "Charges"
  )
ggsave("../output/figures/p_customers_q.png", p_customers_q)
ggsave("../output/figures/p_agegroup_q.png", p_agegroup_q)
ggsave("../output/figures/p_charges_hist.png", p_charges_hist)
ggsave("../output/figures/p_charges_vs_age.png", p_charges_vs_age)
ggsave("../output/figures/p_charges_bmi.png", p_charges_bmi)
ggsave("../output/figures/p_agegroup_charges_hist.png", p_agegroup_charges_hist)
print(p_customers_q)
print(p_agegroup_q)
print(p_charges_vs_age)
print(p_charges_hist)
print(p_charges_bmi)
print(p_agegroup_charges_hist)
print(mean_charges_agegroup)
print(mean_charges_plantype)
print(mean_charges_region)
print(mean_charges_sex)












