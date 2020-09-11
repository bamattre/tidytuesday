# Tidy Tuesday 2020-09-08
# Friends
# Richard Bamattre

#install.packages("friends")

# load
library(friends)
library(curl)

# wrangle
library(tidyverse)
library(janitor)

# text
library(tidytext)
library(textclean)

# Import fonts 
library(extrafont)
#font_import() # - one time right?
loadfonts(device = "win")

# viz
library(ggthemes)
library(grid)
library(gridExtra)
library(ggwordcloud)
library(ggTimeSeries)

# All utterances by speaker, season, episode, scene
head(friends)

# Entities - who the characters are talking about?
head(friends_entities$entities)

# Emotions of each utterance (assuming 1:1?)
head(friends_emotions)

# Episode information
head(friends_info)

# List of friends
list_fr <- c("Rachel Green", "Ross Geller", "Chandler Bing",
             "Monica Geller", "Joey Tribbiani", "Phoebe Buffay")

# Just curious - ratings over time
friends_info %>%
  mutate(se = paste0(str_pad(season, 2, side = "left", pad = "0"), 
                     str_pad(episode, 2, side = "left", pad = "0"))) %>%
    # unique id for episode
  ggplot(aes(se, imdb_rating, group = season, color = as.factor(season))) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    theme_fivethirtyeight() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = "none",
          panel.grid.major.x = element_blank()) +
    labs(title = "Friends IMDB ratings by season",
         x = "Season and Episode",
         y = "IMDB Rating") +
    scale_y_continuous(breaks = seq(0, 10, 0.5))

# Need to handle contractions, otherwise tidytext counts 't' as a word in "don't"

friends_clean <- friends %>%
  mutate(text_cl = replace_contraction(text, contraction.key = lexicon::key_contractions),
         text_abb = str_remove_all(text, "[[:punct:]]"))

# What character has the highest lexical diversity?
# i.e. unique words divided by total words

friends_ld <- friends %>%
  filter(speaker %in% list_fr) %>%
  unnest_tokens(word, text) %>%
  group_by(speaker, season, episode) %>%
  summarize(lex_div = length(unique(word)) / length(word),
            total_words = length(word),
            total_utt = length(unique(utterance))) %>%
  mutate(se = paste0(str_pad(season, 2, side = "left", pad = "0"), 
                     str_pad(episode, 2, side = "left", pad = "0"))) %>%
  ungroup()
  
plot1 <- friends_ld %>%
  ggplot(aes(y = reorder(speaker, lex_div), x = lex_div, color = speaker)) +
    geom_jitter(alpha = 0.2) +
    geom_boxplot(alpha = 0.5, outlier.alpha = 0) +
    theme_fivethirtyeight() +
    theme(legend.position = "none",
          panel.grid.major.y = element_blank(),
          text = element_text(size = 12, family = "Bahnschrift")) +
    labs(title = "The One With the Biggest Vocabulary",
         subtitle = "Unique words out of total words spoken, by episode") +
    scale_color_brewer(palette = "Dark2") +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                   breaks = seq(.3, .9, by = .1))

plot2 <- friends_ld %>%
  ggplot(aes(y = reorder(speaker, lex_div), x = total_words, color = speaker)) +
  geom_jitter(alpha = 0.2) +
  geom_boxplot(alpha = 0.5, outlier.alpha = 0) +
  theme_fivethirtyeight() +
  theme(legend.position = "none",
        #axis.title.x = element_text(),
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        text = element_text(size = 12, family = "Bahnschrift")) +
  labs(title = "The One With the Most Words",
       subtitle = "Total words spoken by episode") +
  scale_color_brewer(palette = "Dark2")

# Interesting - this changes when contractions are dealt with

# What are the most common phrases or catch phrases by character?

# pull out bigrams/ngrams
friends_bg <- friends_clean %>%
  filter(speaker %in% list_fr) %>% # only dialogue for six friends
  unnest_tokens(ngram, text_abb, token = "ngrams", n = 4) %>%
  filter(!is.na(ngram)) %>%
  separate(ngram, c("word1", "word2", "word3", "word4"), remove = FALSE) %>%
  mutate(all_stop = word1 %in% stop_words$word
           & word2 %in% stop_words$word
           & word3 %in% stop_words$word
           & word4 %in% stop_words$word
         ) %>%
  filter(all_stop == FALSE) %>%
  unite(ngram, word1, word2, word3, word4, sep = " ")

# remove ngrams that are completely or mostly stop words?

friends_bg %>%
  count(ngram, sort = TRUE)

# Looking at tf-idf by speaker
friends_tf_idf <- friends_bg %>%
  count(speaker, ngram) %>%
  bind_tf_idf(ngram, speaker, n) %>%
  arrange(desc(tf_idf))

friends_tf_idf %>%
  group_by(speaker) %>%
  arrange(speaker, desc(tf_idf)) %>%
  slice(1:10) %>%
  ggplot(aes(tf_idf, ngram, fill = speaker)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ speaker, scales = "free")

plot3 <- friends_tf_idf %>%
  group_by(speaker) %>%
  arrange(speaker, desc(tf_idf)) %>%
  slice(1:15) %>%
  ggplot(aes(x = 1, y = 1, size = tf_idf, label = ngram,
             color = speaker)) +
    geom_text_wordcloud(family = "Bahnschrift") +
    scale_radius(range = c(3, 8)) +
    theme_fivethirtyeight() +
    theme(
      panel.grid.major = element_blank(),
      axis.text = element_blank(),
      text = element_text(size = 12, family = "Bahnschrift")
      ) +
    facet_wrap(~ speaker) +
    scale_color_brewer(palette = "Dark2") +
    labs(title = "The One With the Catch Phrases",
         subtitle = "Most important 4 letter phrases for each character")

# Who is speaking more or less over seasons and episodes?

plot4 <- friends_ld %>%
  group_by(speaker, season) %>%
  summarize(n = sum(total_utt)) %>%
  group_by(season) %>%
  mutate(perc = n / sum(n)) %>%
  ggplot(aes(x = season, y = perc, group = speaker, 
             fill = speaker, color = speaker)) +
    geom_smooth(se = FALSE, size = 2) +
    geom_text_repel(data = . %>% filter(season == 10),
              aes(label = speaker, color = speaker),
              #direction = "x",
              family = "Bahnschrift",
              nudge_x = 0.6,
              hjust = 0.5,
              segment.alpha = 0) +
    theme_fivethirtyeight() +
    theme(legend.position = "none",
          text = element_text(size = 12, family = "Bahnschrift"),
          panel.grid.major.y = element_blank(),
          plot.margin = unit(c(1, 5, 1, 1), "lines")) +
    scale_fill_brewer(palette = "Dark2") +
    scale_x_continuous(breaks = seq(1, 10, by = 1), limits = c(1, 11.5)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(title = "The One About Who Spoke the Most",
         subtitle = "Percent of Utterances by Season")

# Generate grid of all plots

g <- arrangeGrob(plot1, plot2, plot3, plot4,
             layout_matrix = rbind(c(1, 2),
                                   c(3, 3),
                                   c(4, 4)),
             top = textGrob("Insights from 'Friends' Dialogue", 
                            gp = gpar(fontfamily = "Bahnschrift",
                                      fontface = "bold",
                                      hjust = -1,
                                      cex = 2)),
             bottom = textGrob("Tidy Tuesday - @rbamattre", 
                               gp = gpar(fontfamily = "Bahnschrift",
                                         hjust = 0,
                                         cex = 0.8)))

ggsave("friends_plot.png", g, width = 11, height = 15, dpi = 150,
       bg = "#F0F0F0")

