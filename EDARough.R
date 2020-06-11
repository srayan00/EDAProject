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
  ggplot(aes(x = usage_rate, y = wickets)) +
  geom_point() + 
  geom_smooth() +
  theme_bw()

#scatter plot with average wickets and economy
cricket_cleaned %>%
  mutate(avg_wicket = wickets/innings) %>%
  ggplot() +
  geom_point(aes(x=economy, y=avg_wicket))

#Hierarchial clustering with complete linkage
cricket_complete_hclust <- hclust(dist(dplyr::select(cricket_cleaned, country, years_active, innings, overs, runs, wickets)), 
                                  method ="complete")
ggdendrogram(cricket_complete_hclust, rotate = FALSE, size =4, leaf_labels = FALSE, labels=FALSE)

#Hierarchial Clustering with complete linkage by country
cricket_by_country <- cricket_cleaned %>%
  group_by(country) %>%
  summarise(avg_yearsactive = sum(years_active)/n(),
            avg_innings = sum(innings)/n(),
            avg_overs = sum(overs)/n(),
            avg_runs = sum(runs)/n(),
            avg_wickets = sum(wickets)/n(),
            avg_maidens = sum(maidens)/n())

country_names <- cricket_by_country$country
cricket_by_country <- cricket_by_country %>% 
  dplyr::select(-c("country")) %>%
  scale() %>% 
  as.data.frame() 
     
clust_by_country <- hclust(dist(cricket_by_country),
                           method = "complete")
clust_by_country$labels <- country_names

ggdendrogram(clust_by_country) + 
  labs(title = "Dendogram with clusters of countries") +
  ylab("Height")
five_clusters <-cutree(clust_by_country, k=3)
cricket_by_country %>%
  mutate(cluster = as.factor(five_clusters)) %>%
  ggplot() +
  geom_point(aes(x = avg_runs, y = avg_wickets, color = cluster)) +
  theme_bw()
  
#minimax linkage 
cricket_country_minimax <- protoclust(dist(dplyr::select(cricket_by_country, avg_yearsactive, 
                                                     avg_innings, avg_overs, avg_runs,
                                                     avg_wickets)))
cricket_country_minimax$labels <- cricket_by_country$country
ggdendrogram(cricket_country_minimax, rotate = TRUE)