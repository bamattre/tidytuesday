# Tidy Tuesday

# 2020-07-28

#install.packages("palmerpenguins")
library(palmerpenguins)
library(tidyverse)
library(gridExtra)
library(ggthemes)

# Import fonts 
library(extrafont)
#font_import() # - one time right?
loadfonts(device = "win")

p <- penguins

one <- p %>%
  ggplot(aes(body_mass_g, fill = species)) +
  geom_dotplot(stackgroups = TRUE, 
                 stackdir = "center",
                 method = "histodot",
               dotsize = .3,
               color = NA) +
  labs(y = "", x = "Body Mass (g)") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        axis.text.y = element_blank(),
        text = element_text(size = 12, family = "Bahnschrift"),
        plot.background = element_rect(fill = "#F9F9ED",
                                       color = NA)) +
  guides(fill = guide_legend(title = "Penguin Species")) +
  scale_fill_ptol()

two <- p %>%
  ggplot(aes(flipper_length_mm, species, fill = species)) +
  geom_violin(color = NA) +
  geom_jitter(alpha = 0.2, height = 0.1, shape = 1) +
  labs(y = "", x = "Flipper Length (mm)",
       title = "Palmer Penguins (Tidy Tuesday)") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        text = element_text(size = 12, family = "Bahnschrift"),
        plot.background = element_rect(fill = "#F9F9ED",
                                       color = NA)) +
  scale_fill_ptol()

grid.arrange(two, one)