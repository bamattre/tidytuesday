# Tidy Tuesday 2020-10-13
# Datasaurus Dozen
# Richard Bamattre

# load
library(tidytuesdayR)

# wrangle
library(tidyverse)
library(janitor)

# Import fonts 
library(extrafont)

# viz
library(gganimate)

dino <- tt_load('2020-10-13')$datasaurus

dino %>%
  ggplot(aes(x, y)) +
    geom_point() +
    facet_wrap(~ dataset) +
    geom_smooth(method = "lm", se = FALSE) +
    theme_void()

dino2 <- dino %>%
  group_by(dataset) %>%
  mutate(x_mean = mean(x),
         y_mean = mean(y))

plot <- ggplot(dino2, aes(x, y)) +
  geom_point(shape = 19, color = "#2a9d8f", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "#e76f51") +
  geom_rug(color = "#2a9d8f") +
  geom_point(aes(x_mean, y_mean),
             shape = 15, color = "#e9c46a", size = 3) +
  xlim(-25, 125) +
  ylim(-25, 125) +
  theme_void() +
  annotate("label", x = -10, y = -10, label = "Distribution",
           color = "#2a9d8f", family = "Bahnschrift",
           fill = "#264653", size = 6) +
  annotate("label", x = 100, y = 50, label = "Trend line",
           color = "#e76f51", family = "Bahnschrift",
           fill = "#264653", size = 6) +
  annotate("label", x = 60, y = 40, label = "Mean",
           color = "#e9c46a", family = "Bahnschrift",
           fill = "#264653", size = 6) +
  theme(text = element_text(family = "Bahnschrift",
                            color = "#2a9d8f", size = 16),
        plot.background = element_rect(fill = "#264653"),
        plot.margin = margin(20, 20, 20, 20)) +
  labs(title = "The Datasaurus Dozen",
       subtitle = "Tidy Tuesday | @rbamattre (Data Source: Alberto Cairo)")

plot

plot +
  facet_wrap(~ dataset)

animation <- plot +
  transition_states(dataset,
                    transition_length = 2,
                    state_length = 3) +
  ease_aes(y = "cubic-in-out")

animation
