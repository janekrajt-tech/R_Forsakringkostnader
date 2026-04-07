ggplot(data_clean, aes(x = region)) +
  geom_bar() +
  labs(
    title = "Antal kunder per region",
    x = "Region",
    y = "Antal"
  )
ggplot(data_clean, aes(x = age_group)) +
  geom_bar() +
  labs(
    title = "Antal kunder per åldersgrupp",
    x = "Åldersgrupp",
    y = "Antal"
  )

ggplot(data_clean, aes(x = charges)) +
  geom_histogram() +
  labs(
    title = "Histogram for charges",
    x = "charges"
  )
ggplot(data_clean, aes(x = charges, y = age, colour = smoker)) + 
  geom_point() +
  labs(
    title = "Charges vs age",
    x = "charges",
    y = "age"
  )

ggplot(data_clean, aes(x = charges, y = bmi, colour = age_group)) + 
  geom_point() +
  labs(
    title = "Charges vs bmi",
    x = "bmi",
    y = "age"
  )

data_clean %>%
  group_by(sex) %>%
  summarise(mean_charges = mean(charges, na.rm = TRUE)) %>%
  arrange(desc(mean_charges))

data_clean %>%
  group_by(region) %>%
  summarise(mean_charges = mean(charges, na.rm = TRUE)) %>%
  arrange(desc(mean_charges))

data_clean %>%
  group_by(age_group) %>%
  summarise(mean_charges = mean(charges, na.rm = TRUE)) %>%
  arrange(desc(mean_charges))


data_clean %>%
  group_by(plan_type) %>%
  summarise(mean_charges = mean(charges, na.rm = TRUE)) %>%
  arrange(desc(mean_charges))

ggplot(data_clean, aes( x = age_group, y = charges)) +
  geom_boxplot() +
  labs(
    title = "Boxplot Age Group vs Charges",
    x = "Age Group",
    y = "Charges"
  )