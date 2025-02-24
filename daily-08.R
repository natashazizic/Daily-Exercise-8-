#Name: Natasha Zizic 
#Date: 02/23/2-20225
#Purpose: Daily Exercise 8 - Joins and Pivots 

library(tidyverse)
url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-recent.csv'
covid = read_csv(url)
glimpse(covid)
state_info <- data.frame(
  state = state.abb,
  state_name = state.name,
  region = state.region
)
head(state_info)

covid_region_data <- covid %>%
  left_join(state_info, by = "state") 

head(covid_region_data)

unique(covid$state)

state_info <- data.frame(
  state = state.name, 
  region = state.region)

covid_region_data <- covid %>%
  left_join(state_info, by = "state")

head(covid_region_data)

covid_region_summary <- covid_region_data %>%
  group_by(region, date) %>%
  summarize(
    daily_cases = sum(cases, na.rm = TRUE),
    daily_deaths = sum(deaths, na.rm = TRUE)
  ) %>%
  arrange(region, date) %>%
  mutate(
    total_cases = cumsum(daily_cases),
    total_deaths = cumsum(daily_deaths)
  ) %>%
  ungroup()


head(covid_region_summary)

covid_long <- covid_region_summary %>%
  pivot_longer(cols = c(total_cases, total_deaths), 
               names_to = "metric", 
               values_to = "count")

head(covid_long)

plot <- ggplot(covid_long, aes(x = date, y = count, color = region)) +
  geom_line(size = 1) + 
  facet_grid(metric ~ region, scales = "free_y") +  
  labs(
    title = "Cumulative Cases and Deaths: Region",
    subtitle = "COVID-19 Data: NY-Times",
    x = "Date",
    y = "Daily Cumulative Count"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),  
    legend.position = "none"  
  )
print(plot)




