# Tidy Tuesday 2020-08-04
# European Energy
# Richard Bamattre

library(tidyverse)
library(tidytuesdayR)

# Import fonts 
library(extrafont)
#font_import() # - one time right?
loadfonts(device = "win")

library(janitor)
library(gganimate)

# Get the Data

tuesdata <- tidytuesdayR::tt_load('2020-08-04')

energy_type <- tuesdata$energy_types

ctry_total <- tuesdata$country_totals

energy_type %>% tabyl(level, type) # pumped hydro power the only level 2

ctry_total %>% tabyl(level) # extract of larger table - only total

energy <- energy_type %>%
  select(-level) %>%
  pivot_longer(`2016`:`2018`, names_to = "year", values_to = "energy_gwh") %>%
  bind_rows(ctry_total %>%
              select(-level) %>%
              pivot_longer(`2016`:`2018`, names_to = "year", values_to = "energy_gwh")
  )

energy %>%
  filter(country_name == "Belgium") %>%
  ggplot(aes(year, energy_gwh, group = type)) +
  geom_line() +
  facet_wrap(~ type, scales = "free")

energy_total <- energy %>%
  pivot_wider(names_from = "type", 
              values_from = "energy_gwh") %>%
  clean_names() %>%
  select(-c(total_net_production:energy_supplied)) %>%
  mutate(total = rowSums(select(., conventional_thermal:other))) %>%
  # total net production doesn't add up for countries like Luxembourg, Austria, Malta
  mutate(`Renewable Energy` = rowSums(select(., hydro:geothermal)) / total,
         `Conventional Energy` = rowSums(select(., conventional_thermal, other)) / total,
         `Nuclear Energy` = nuclear / total) %>%
  select(Country = country_name, Year = year, `Renewable Energy`:`Nuclear Energy`) %>%
  mutate(renew_num = `Renewable Energy`) %>%
  pivot_longer(`Renewable Energy`:`Nuclear Energy`, 
               names_to = "Energy Type",
               values_to = "Percent of Total")

anim <- energy_total %>%
  filter(!is.na(Country)) %>%
  ggplot(aes(y = reorder(Country, renew_num), x = `Percent of Total`, fill = `Energy Type`)) +
  geom_col(position = "stack") +
  scale_x_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(margin = margin(r = -20)),
        text = element_text(size = 12, family = "Bahnschrift"),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 1),
        plot.background = element_rect(fill = "#F9F9ED",
                                       color = NA)) +
  labs(title = "European Energy Production",
       subtitle = "Year: {closest_state}",
       y = "", x = "") +
  scale_fill_manual(values = c("#dddddd", "#ee8866", "#bbcc33"),
                    guide = guide_legend(reverse = TRUE)) +
  transition_states(Year,
                    transition_length = 2,
                    state_length = 1) +
  ease_aes("cubic-in-out")

animate(
  anim,
  renderer = gifski_renderer(),
  width = 700, height = 550
)

anim_save("European Energy Production.gif", last_animation())
