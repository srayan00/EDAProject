#2D Bar graph with year_active and avg_maiden ratio
cricket_cleaned %>% 
  group_by(years_active) %>%
  summarize(avg_ratio = sum(maiden_ratio)/n()) %>%
  ggplot() +
  geom_bar(aes(x = years_active, y = avg_ratio), stat = "identity")

#Scatter plot - maiden over/overs and strike_rate
cricket_cleaned %>% 
  filter(strike_rate != Inf) %>%
  ggplot() +
  geom_point(aes(x= maiden_ratio, y = strike_rate)) 

#scatter plot between economy and strikerate
cricket_cleaned %>% 
  filter (strike_rate != Inf)
ggplot() +
  geom_point(aes(x = economy, y = strike_rate))

#1D  bar chart of year_active
cricket_cleaned %>% 
  ggplot() +
  geom_bar(aes(x = years_active), fill = "deepskyblue3") +
  theme_bw() +
  labs(x = "Years Active",
       title = "The rise in popularity of T20 Internation Cricket",
       caption = "More newer players joined recently")

#1D bar chart of Starting Years
cricket_cleaned %>% 
  ggplot() +
  geom_bar(aes(x = start), fill = "deepskyblue3") +
  theme_bw() +
  labs(x = "Starting Year",
       title = "The rise in popularity of T20 International Cricket",
       caption = "More newer players joined recently")

#1D density for Usage Rate
cricket_cleaned %>%
  ggplot() +
  geom_density(aes(x = usage_rate))