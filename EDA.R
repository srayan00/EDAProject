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
                              Samoa = "South AfricaMwn"))
cricket_cleaned %>% 
  mutate (years = end - start) %>%
  group_by(years) %>%
  summarize(avg_econ = sum(strike_rate)/n()) %>%
  ungroup() %>%
  mutate (avg_econ = avg_econ) %>%
  ggplot() +
  geom_bar(aes(x = years, y = avg_econ), stat = "identity")
