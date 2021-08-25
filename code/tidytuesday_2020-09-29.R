# Tidy Tuesday 2020-09-29
# Beyonce & Taylor Swift lyrics
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
library(thematic)
library(gridExtra)
library(ggwordcloud)

tuesdata <- tt_load('2020-09-29') # not in package yet?

# read in data manually

beyonce_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/beyonce_lyrics.csv')
taylor_swift_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/taylor_swift_lyrics.csv')
sales <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/sales.csv')
charts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/charts.csv')

ts <- taylor_swift_lyrics %>%
  unnest_tokens(ngram, Lyrics, "ngrams", n = 3)

# get collaborators
bey_collab <- beyonce_lyrics %>%
  distinct(song_id, song_name, artist_name) %>%
  mutate(feature = if_else(str_detect(song_name, "Ft\\."), 
                           str_extract(song_name, "\\(Ft.([^\\)]+)\\)"), 
                           NA_character_) %>%
           str_remove_all("\\(|\\)|Ft\\.")) %>%
  separate_rows(feature, sep = "&|\\,") %>%
  mutate(feature = str_squish(feature)) #%>%
  # pivot_longer(artist_name:feature, names_to = "type", values_to = "artist") %>%
  # filter(!is.na(artist))

# get lyrics
bey <- beyonce_lyrics %>%
  filter(!str_detect(song_name, "Greek|Spanish|Spanglish")) %>% # remove translations
  unnest_tokens(ngram, line, "ngrams", n = 3)

ts %>%
  count(ngram, sort = TRUE)

bey %>%
  count(ngram, sort = TRUE)

bey_collab %>%
  count(feature, sort = TRUE)

bey_charts <- charts %>%
  filter(artist == "Beyoncé") %>%
  mutate(chart_position = as.numeric(chart_position)) %>%
  mutate(number_one = if_else(chart_position == 1, "*", NA_character_))

# set theme
# pulling colors from https://beyoncepalettes.tumblr.com/
# there's an R package too! https://github.com/dill/beyonce

thematic_on(bg = "#43152B", fg = "#CF7F46", accent = "#598444", 
            font = "Bahnschrift",
            sequential = sequential_gradient(.5, .5))

a <- bey_charts %>%
  ggplot(aes(chart_position, chart, fill = chart_position,
             label = number_one)) +
  geom_col() +
  geom_text(color = "orange", size = 7, nudge_y = -.4) +
  facet_wrap(~title) +
  #theme_minimal() +
  theme(
        legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_blank(),
        #panel.grid.major.x = element_line(color = "lightgray"),
        #plot.background = element_rect(fill = "#333333"),
        text = element_text(family = "Bahnschrift")
        ) +
  #scale_fill_viridis(direction = -1) +
  labs(title = "Number One Albums by Country",
       x = "Chart Position (Number 1 with Stars)", y = "")

bey_collab2 <- bey_collab %>%
  filter(!is.na(feature)) %>%
  distinct(artist_name, feature, song_id) %>%
  rename(artist1 = artist_name,
         artist2 = feature) %>%
  group_by(artist1, artist2) %>%
  summarize(songs = n_distinct(song_id))

b <- bey_collab2 %>%
  arrange(desc(songs)) %>%
  slice(1:50) %>%
  mutate(rank = row_number()) %>%
  ggplot(aes(label = artist2)) +
    geom_text_wordcloud_area(aes(size = songs, color = rank),
                             area_corr_power = .8,
                             family = "Bahnschrift") +
    scale_size_area(max_size = 20) +
    theme(panel.background = element_blank(),
          text = element_text(family = "Bahnschrift")) +
    labs(title = "Top Artists Featured")

c <- bey %>%
  count(ngram, sort = TRUE) %>%
  filter(!is.na(ngram)) %>%
  slice(1:50) %>%
  mutate(rank = row_number()) %>%
  ggplot(aes(label = ngram)) +
    geom_text_wordcloud_area(aes(size = n, color = rank),
                            area_corr_power = .8,
                            family = "Bahnschrift") +
    scale_size_area(max_size = 15) +
    theme(panel.background = element_blank(),
          text = element_text(family = "Bahnschrift")) +
    labs(title = "Top Phrases")

g <- arrangeGrob(a, b, c, 
                 ncol = 1,
                 top = textGrob("Beyoncé's Albums, Features, and Phrases", 
                                gp = gpar(fontfamily = "Bahnschrift",
                                          fontface = "bold",
                                          col = "#598444",
                                          hjust = -1,
                                          cex = 2)),
                 bottom = textGrob("Tidy Tuesday - @rbamattre", 
                                   gp = gpar(fontfamily = "Bahnschrift",
                                             col = "#598444",
                                             hjust = 0,
                                             cex = 0.8)))

ggsave("beyonce_plot.png", g, width = 8, height = 11, dpi = 150,
       bg = "#43152B")
