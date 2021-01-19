# Tidy Tuesday 2020-12-01
# Toronto Shelters
# Richard Bamattre @rbamattre

# load
library(tidytuesdayR) # Get tidy tuesday data

# wrangle
library(tidyverse) # of course
library(lubridate)
library(janitor) # clean column names and nice tables
library(tsibble)

# model
library(fable)
library(feasts)

# Import fonts 
library(extrafont) # use fonts

# viz
library(ggthemes)
library(gridExtra)
library(grid)

# set font
font = "Bahnschrift"

# load data
tuesdata <- tidytuesdayR::tt_load('2020-12-01')

shelter <- tuesdata$shelters %>%
  mutate(free_beds = capacity - occupancy,
         over_capacity = occupancy > capacity) %>%
  mutate(free_beds = if_else(free_beds < 0, 0, free_beds)) # no negative beds

# Odd occupancy over time in 2017 (drops at the end of the month?)
# Most are in Toronto

shelter %>%
  #filter(year(occupancy_date) > 2017) %>%
  group_by(shelter_city, occupancy_date, sector) %>%
  summarize(occupancy = sum(occupancy, na.rm = TRUE)) %>%
  ggplot(aes(occupancy_date, occupancy, color = sector)) +
    geom_line() +
    facet_wrap(~ shelter_city)
    
# coed might be hard to forecast, some change over time

shelter_to <- shelter %>%
  filter(shelter_city == "Toronto",
         sector != "Co-ed") %>%
  filter(year(occupancy_date) > 2017) %>%
  group_by(occupancy_date, sector) %>%
  summarize(occupancy = sum(occupancy, na.rm = TRUE),
            capacity = sum(capacity, na.rm = TRUE),
            free_beds = sum(free_beds, na.rm = TRUE),
            n_over_capacity = sum(over_capacity, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(occupancy_date = as.Date(occupancy_date))

# convert to tsibble

shelter_to_ts <- shelter_to %>%
  as_tsibble(key = sector, index = occupancy_date)

# check for gaps
has_gaps(shelter_to_ts)

autoplot(shelter_to_ts) 

gg_season(shelter_to_ts, y = occupancy, period = "month") # seasonal patterns?

# using weeks as seasons?
shelter_to_ts %>%
  model(classical_decomposition(occupancy, type = "additive")) %>%
  components() %>%
  autoplot()

# Look at autocorrelation
shelter_to_ts %>%
  ACF(occupancy) %>%
  autoplot()

# Clearly not white noise - but no clear seasonality here

# Try various forecasts
model <- shelter_to_ts %>%
  model(
    snaive = SNAIVE(occupancy),
    arima = ARIMA(occupancy), 
    lm = TSLM(occupancy ~ trend() + season()),
    nn = NNETAR(box_cox(occupancy, 0.15)) # how to add predictors?
  )

model %>%
  accuracy() %>%
  arrange(MASE)

# neural network & arima have the lowest MASE

forecast <- model %>%
  forecast(h = 30)

plot1 <- forecast %>%
  filter(.model == "nn") %>%
  autoplot(shelter_to_ts %>% filter_index("2019-09-01" ~ .)) +
  theme_fivethirtyeight() +
  theme(legend.position = "none",
        text = element_text(family = font)) +
  labs(title = "Forecasting Daily Occupancy for Toronto Shelters by Type",
       subtitle = "Using Feed-Forward Neural Networks")

g <- arrangeGrob(plot1,
                 bottom = textGrob("Tidy Tuesday | @rbamattre | Source: open.toronto.ca", 
                                   gp = gpar(fontfamily = font,
                                             col = "black",
                                             hjust = 0,
                                             cex = 0.8)))

ggsave("2020-12-01 toronto shelters.png", g, width = 8, height = 6, dpi = 150,
       bg = "#f4f4f4")
