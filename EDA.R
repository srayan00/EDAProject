#some hypothesis about the countries in Indian Subcontinent - Pakistan, India, Sri Lanka, Bangladesh - known to be best cricket teams and there is a lot of rivalry

cricket_cleaned %>%
  filter (strike_rate != Inf | average != Inf) %>%
  filter (country %in% c("India", "Bangladesh", "Sri Lanka", "Pakistan")) %>%
  group_by(country) %>%
  summarise(avg_economy = sum(economy)/ n(), 
            avg_strikerate = sum(strike_rate)/n(),
            avg_maidenratio = sum(maiden_ratio)/n(),
            avg_average = sum(average)/n()) %>%
  pivot_longer(avg_economy:avg_average,
               names_to= "stat",
               values_to = "value") %>%
  ggplot() +
  geom_col(aes(x = country, y = value, fill = stat)) +
  facet_wrap(~stat, ncol=1, scales = "free_y") +
  theme_bw() +
  labs(x = "Country", y = "Values",
       title = "Which team in the Indian Subcontinent has the best bowlers?") +
  theme(strip.background = element_blank(),
        legend.position = "none")

#scatter plot with usage_rate and economy
cricket_cleaned %>%
  ggplot(aes(x = usage_rate, y = maiden_ratio)) +
  geom_point() + 
  geom_smooth() +
  theme_bw()
