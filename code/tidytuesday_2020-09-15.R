# Tidy Tuesday 2020-09-15
# Govt Spending on Kids
# Richard Bamattre

# load
library(tidytuesdayR)

# wrangle
library(tidyverse)
library(janitor)

tuesdata <- tt_load('2020-09-15')

kids <- tuesdata$kids

# explore data

# example of spending over time for one state
kids %>%
  filter(state == "California") %>%
  ggplot(aes(year, inf_adj_perchild)) +
    geom_line() +
    facet_wrap(~ variable)

# relationship between two spending categories?
kids %>%
  #filter(year == max(year)) %>%
  select(-c(raw, inf_adj)) %>%
  pivot_wider(names_from = "variable", values_from = "inf_adj_perchild") %>%
  select(-c(year, state)) %>%
  cor(use = "pairwise.complete.obs") %>%
  corrplot::corrplot()
