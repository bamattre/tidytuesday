# Tidy Tuesday 2020-10-06
# NCAA Women's Basketball
# Richard Bamattre

# load
library(tidytuesdayR)

# text
library(tidytext)

# wrangle
library(tidyverse)
library(janitor)

# Import fonts 
library(extrafont)

# viz
library(gt)

tues <- tt_load('2020-10-06')$tournament # not in package yet?

# seed points - from https://github.com/schmid07/TT-2020-Week-41

seed_point_table <- 
  tribble(
    ~seed, ~seed_points,
    1, 	100,
    2, 	72.7,
    3, 	54.5,
    4,	48.5,
    5,	33.3,
    6,	33.3,
    7,	27.3,
    8,	21.2,
    9,	18.2,
    10,	18.2,
    11,	18.2,
    12,	15.2,
    13,	9.09,
    14,	6.06,
    15,	3.03,
    16,	0
  )

ncaa <- tues %>%
  left_join(seed_point_table, by = "seed")

ncaa %>%
  filter(conference == "Big Ten") %>%
  ggplot(aes(y = seed, x = year, group = school, color = school)) +
    geom_line()

ncaa %>%
  ggplot(aes(full_percent, seed)) +
    geom_point() +
    geom_smooth(method = "lm")

ncaa_summary <- ncaa %>%
  arrange(school, year) %>%
  group_by(school) %>%
  filter(any(year >= 2015)) %>% # only schools with recent data
  summarize(years = if_else(n() > 1, # years of data
                            paste0(min(year), "-", max(year)),
                            as.character(year[1])),
            latest_conference = max(conference), # schools change conf over time?
            avg_seed = mean(seed_points, na.rm = TRUE),
            avg_reg_p = mean(reg_percent, na.rm = TRUE),
            avg_conf_p = mean(conf_percent, na.rm = TRUE),
            avg_full_p = mean(full_percent, na.rm = TRUE)
            )

ncaa_summary %>%
  arrange(desc(avg_seed), desc(avg_full_p)) %>% 
  slice(1:20) %>%
  gt(rowname_col = "school") %>% # use school as the stub
  tab_header(
    title = md("**NCAA Women's Basketball Teams**"),
    subtitle = "Top 20 Schools Based on Average Seed Points"
  ) %>%
  tab_stubhead(label = "School") %>%
  cols_align("left", # align certain columns
             vars(latest_conference)) %>%
  cols_align("center",
             vars(avg_seed, avg_reg_p, avg_conf_p, avg_full_p)) %>%
  opt_align_table_header("left") %>%
  cols_label( # rename columns
    years = "Years",
    latest_conference = "Conference",
    avg_seed = html("Average<br>Seed Points"),
    avg_reg_p = "Season",
    avg_conf_p = "Conference",
    avg_full_p = "Total"
  ) %>%
  cols_move_to_start( # rearrange columns
    columns = vars(latest_conference)
  ) %>%
  tab_spanner( # title to span similar columns
    label = "Average Win Percentages",
    columns = vars(avg_reg_p, avg_conf_p, avg_full_p)
  ) %>%
  fmt_number( # adjust the format of certain numbers
    columns = vars(avg_seed),
    decimals = 1
  ) %>%
  fmt_number(
    columns = vars(avg_reg_p, avg_conf_p, avg_full_p),
    decimals = 0
  ) %>%
  tab_source_note( # create a note at the bottom
    source_note = "Tidy Tuesday submission - @rbamattre | Data Source: FiveThirtyEight"
  ) %>%
  data_color( # use conditional formatting
    columns = vars(avg_reg_p, avg_conf_p, avg_full_p),
    colors = scales::col_numeric(
      palette = viridis::viridis(n = 10),
      domain = NULL
    )
  ) %>%
  tab_style( # right align row names
    style = list( # change style - specify style changes and where to apply
      cell_text(align = "right")
    ),
    locations = list(
      cells_stub(rows = TRUE),
      cells_stubhead()
      )
  ) %>%
  tab_style(
    style = cell_text(font = "Candara",
                      weight = "bold"),
    locations = cells_title("title")
  ) %>%
  tab_style( # change font
    style = list(
      cell_text(font = "Bahnschrift")
    ),
    locations = list(
      cells_body(),
      cells_stub(),
      cells_column_labels(everything()),
      cells_column_spanners(everything()),
      cells_stubhead(),
      cells_title("subtitle")
    )
  ) %>%
  tab_style( # bold school names and seed
    style = list(
      cell_text(weight = "bold")
    ),
    locations = list(
      cells_stub(),
      cells_body(vars(avg_seed))
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(color = "gray")
    ),
    locations = cells_body(vars(years))
  ) %>%
  tab_options(
    table.border.bottom.color = "white",
    table.border.top.style = "white",
    heading.border.bottom.color = "white",
    table_body.border.bottom.color = "white"
  ) #%>%
#gtsave("2020-10-06 ncaa womens basketball.png", vwidth = 1000, vheight = 2000, zoom = 2)
  