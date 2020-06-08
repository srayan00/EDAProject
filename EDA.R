library(tidyverse)
cricket <- read_csv("http://www.stat.cmu.edu/cmsac/sure/materials/data/eda_projects/womens_cricket_bowling.csv")
# Read in the file

cricket_cleaned <- cricket %>% 
  filter((player != "CR Seneviratna (SL-W/UAE-W)"))  %>% # Remove player who has caps for more than one nation (Sahana, thoughts on what to do with this observation?)
  drop_na() %>%
  select(-"X1") %>% 
  mutate(player = gsub(' \\(\\)$', '', player),                 # Remove trailing parentheses
         country = fct_recode(country,                          # Replace country abbreviations
                              Thailand = "THI",
                              Indonesia = "INA",
                              China = "CHN",
                              Malawi = "MLW",
                              Uganda = "UGA",
                              Nepal = "NEP",
                              Mozabique = "MOZ",
                              Botswana = "BOT",
                              Vanuatu = "VAN",
                              Brazil = "BRA",
                              Rwanda = "RWA",
                              Myanmar = "MYA",
                              Nigeria = "NGA",
                              Chile = "CHI",
                              Kenya = "KEN",
                              Tanzania = "TZN",
                              Malaysia = "MAL",
                              Korea = "KOR",
                              Singapore = "SIN",
                              Belize = "BLZ",
                              France = "FRA",
                              Mexico = "MEX",
                              Oman = "OMA",
                              Jersey = "JEY",
                              "Sierra Leone" = "Sri LankaE",
                              Kuwait = "KUW",
                              Germany = "GER",
                              Peru = "PER",
                              Argentina = "ARG",
                              Austria = "AUT",
                              Fiji = "FJI",
                              Norway = "NOR",
                              "Costa Rica" = "CRC",
                              Bhutan = "BHU",
                              Philippines = "PHI",
                              Mali = "MLI",
                              Guernsey = "GUN",
                              Maldives = "MDV",
                              Samoa = "South AfricaMwn"),
         region = case_when(country %in% c("Argentina", "Belize", "Brazil", "Canada", "Chile", "Costa Rica", "Mexico", "Peru", "United States of America", "West Indies") ~ "Americas",
                            country %in% c("South Africa", "Zimbabwe", "Namibia", "Botswana", "Kenya", "Malawi", "Mali", "Mozambique", "Nigeria", "Rwanda", "Sierra Leone", "Tanzania", "Uganda") ~ "Africa",
                            country %in% c("India", "Pakistan", "Sri Lanka", "Hong Kong", "Bangladesh", "Bhutan", "China", "Kuwait", "Malaysia", "Maldives", "Myanmar", "Nepal", "Oman", "Qatar", "Singapore", "Thailand", "United Arab Emirates") ~ "Asia",
                            country %in% c("Australia", "New Zealand", "Papau New Guinea", "Fiji", "Indonesia", "Japan", "Philippines", "Samoa", "Korea", "Vanuatu") ~ "East Asia - Pacific",
                            country %in% c("England", "Ireland", "Scotland", "Netherlands", "France", "Jersey", "Germany", "Austria", "Norway", "Guernsey") ~ "Europe"),
         years_active = end -start,
         ratio_maiden = maidens/overs)


#2D Bar graph with year_active and avg_maiden ratio
cricket_with_variables %>% 
  group_by(years_active) %>%
  summarize(avg_ratio = sum(ratio_maiden)/n()) %>%
  ggplot() +
  geom_bar(aes(x = years_active, y = avg_ratio), stat = "identity")

#Scatter plot - maiden over/overs and strike_rate
cricket_cleaned %>% 
  filter(strike_rate != Inf) %>%
  ggplot() +
  geom_point(aes(x= ratio_maiden, y = strike_rate)) 

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
       title = "The rise in popularity of T20 Internation Cricket",
       caption = "More newer players joined recently")

table(cricket_with_variables$country)

#some hypothesis about the countries in Indian Subcontinent - Pakistan, India, Sri Lanka, Bangladesh - known to be best cricket teams and there is a lot of rivalry

cricket_cleaned %>%
  filter (strike_rate != Inf | average != Inf) %>%
  filter (country %in% c("India", "Bangladesh", "Sri Lanka", "Pakistan")) %>%
  group_by(country) %>%
  summarise(avg_economy = sum(economy)/ n(), 
            avg_strikerate = sum(strike_rate)/n(),
            avg_maidenratio = sum(ratio_maiden)/n(),
            avg_average = sum(average)/n()) %>%
  ggplot() +
  geom_col(aes(x = country, y = avg_maidenratio), fill = "darkorchid4") +
  theme_bw() +
  labs(x = "Country", y = "Average Maiden Ratio",
       title = "Which team in the Indian Subcontinent has the best average Maiden Ratio?")
  
