# Tidy Tuesday 2020-09-22
# Himalayan Expeditions
# Richard Bamattre

# load
library(tidytuesdayR)

# wrangle
library(tidyverse)
library(janitor)

# viz
library(ggpubr)
library(grid)
library(gridExtra)

# Import fonts 
library(extrafont)

tuesdata <- tt_load('2020-09-22')

peaks <- tuesdata$peaks

mem <- tuesdata$members

exp <- tuesdata$expeditions

# join peak information to expeditions
exp2 <- exp %>%
  left_join(peaks, by = c("peak_id", "peak_name")) %>%
  mutate(diff_metres = if_else(height_metres > highpoint_metres,
                               highpoint_metres - height_metres, 0),
         perc_surv = ((members + hired_staff) - (member_deaths + hired_staff_deaths)) / 
           (members + hired_staff))

# plot by highpoint - metres from the peak
exp2 %>%
  filter(!str_detect(termination_reason, "Success")) %>%
  ggplot(aes(basecamp_date, diff_metres, color = termination_reason)) +
    geom_point(alpha = 0.5, shape = 16) +
    labs(x = "Date of Basecamp", "Meters from Peak Height")

# see how many days it took for each ascent
# this would be more interesting if we knew the base camp height
exp2 %>%
  filter(year == 2019) %>%
  ggplot(aes(basecamp_date, highpoint_metres, group = expedition_id)) +
    geom_segment(aes(x = basecamp_date, xend = termination_date,
                     y = highpoint_metres, yend = highpoint_metres)) +
    geom_point(aes(x = highpoint_date, y = highpoint_metres))

mem2 <- mem %>%
  left_join(peaks, by = c("peak_id", "peak_name")) %>%
  left_join(exp %>%
              distinct(expedition_id, highpoint_date),
            by = "expedition_id") %>%
  mutate(diff_metres = if_else(height_metres > highpoint_metres,
                               highpoint_metres - height_metres, 0))
                               
# scatter histogram?
mem2 %>%
  filter(sex == "F") %>%
  ggscatterhist(x = "highpoint_date", y = "diff_metres",
                #color = "sex", 
                alpha = 0.5, shape = 16,
                margin.params = list(fill = "gray", size = 0.2)
                #margin.params = list(fill = "sex",
                #                     color = "black", size = 0.2)
                )

p <- mem2 %>%
  filter(sex == "F") %>%
  ggplot(aes(highpoint_date, diff_metres)) +
  geom_point(data = . %>% filter(solo == FALSE, died == FALSE,
                                 oxygen_used == FALSE), 
             alpha = 0.1, shape = 16) +
  geom_point(data = . %>% filter(oxygen_used == TRUE), alpha = 0.4,
             shape = 16, color = "#66ccee", size = 2) +
  geom_point(data = . %>% filter(solo == TRUE), alpha = 1,
             shape = 17, color = "#228833", size = 3) +
  geom_point(data = . %>% filter(died == TRUE), alpha = 0.8,
             shape = 16, color = "#ee6677", size = 2) +
  annotate("label", x = as.Date("1956-01-01"), y = -1000, 
           color = "white", fill = "#ee6677", 
           label = "Died", family = "Bahnschrift") +
  annotate("label", x = as.Date("1993-01-01"), y = -450, 
           color = "white", fill = "#228833",
           label = "Solo Climber", family = "Bahnschrift") +
  annotate("label", x = as.Date("1975-01-01"), y = -200, 
           color = "white", fill = "#66ccee",
           label = "Used Oxygen", family = "Bahnschrift") +
  annotate("text", x = as.Date("1935-01-01"), y = -3000, 
           color = "black", family = "Bahnschrift", hjust = 0,
           label = "7,044 women attempted to climb Himalayan peaks.\nOne third of them were successful.\n611 led expeditions.\n4 solo climbed.") +
  theme(legend.position = "none",
        text = element_text(size = 12, family = "Bahnschrift"),
        plot.title = element_text(face = "bold"),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "#dddddd")) +
  labs(x = "Date Reaching Highpoint", y = "Meters from Peak Height",
       title = "Women on Himalayan Expeditions",
       subtitle = "Points on the top indicate a successful climb to the peak") 

ggsave("2020-09-22_himalayan_exp.png", p, 
       device = "png", width = 9, height = 6, dpi = 150)

# facet wrapped

g1 <- mem2 %>%
  filter(sex == "F") %>% 
  ggplot(aes(highpoint_date, diff_metres)) +
  geom_point(alpha = 0.1, shape = 16) +
  theme(legend.position = "none",
        text = element_text(size = 12, family = "Bahnschrift"),
        plot.title = element_text(face = "bold"),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "#f8f8f8", color = NA)) +
  labs(y = "Meters from Peak Height", x = "",
       title = "All Women Climbers") 

g2 <- mem2 %>%
  filter(sex == "F", died == TRUE) %>% 
  ggplot(aes(highpoint_date, diff_metres)) +
  geom_point(alpha = 0.7, shape = 16, color = "#ee6677") +
  theme(legend.position = "none",
        text = element_text(size = 12, family = "Bahnschrift"),
        plot.title = element_text(face = "bold"),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "#f8f8f8", color = NA)) +
  labs(x = "", y = "", title = "Deaths") 

g3 <- mem2 %>%
  filter(sex == "F", solo == TRUE) %>% 
  ggplot(aes(highpoint_date, diff_metres)) +
  geom_point(alpha = 0.7, shape = 16, color = "#228833") +
  theme(legend.position = "none",
        text = element_text(size = 12, family = "Bahnschrift"),
        plot.title = element_text(face = "bold"),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "#f8f8f8", color = NA)) +
  labs(x = "", y = "", title = "Solo Climbed") 

g4 <- mem2 %>%
  filter(sex == "F", oxygen_used == TRUE) %>% 
  ggplot(aes(highpoint_date, diff_metres)) +
  geom_point(alpha = 0.7, shape = 16, color = "#4477aa") +
  theme(legend.position = "none",
        text = element_text(size = 12, family = "Bahnschrift"),
        plot.title = element_text(face = "bold"),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "#f8f8f8", color = NA)) +
  labs(x = "", y = "Meters from Peak Height", title = "Used Oxygen") 

g5 <- mem2 %>%
  filter(sex == "F", str_detect(expedition_role, "Leader")) %>% 
  ggplot(aes(highpoint_date, diff_metres)) +
  geom_point(alpha = 0.7, shape = 16, color = "#ccbb44") +
  theme(legend.position = "none",
        text = element_text(size = 12, family = "Bahnschrift"),
        plot.title = element_text(face = "bold"),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "#f8f8f8", color = NA)) +
  labs(x = "Date Reaching Highpoint", y = "", title = "Led Expedition") 

g6 <- mem2 %>%
  filter(sex == "F", age >= 50) %>% 
  ggplot(aes(highpoint_date, diff_metres)) +
  geom_point(alpha = 0.7, shape = 16, color = "#aa3377") +
  theme(legend.position = "none",
        text = element_text(size = 12, family = "Bahnschrift"),
        plot.title = element_text(face = "bold"),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "#f8f8f8", color = NA)) +
  labs(x = "", y = "", title = "Aged 50 or Older") 
  
grid <- arrangeGrob(g1, g2, g3, g4, g5, g6, ncol = 3,
                     top = textGrob("Women in Himalayan Expeditions", 
                                    gp = gpar(fontfamily = "Bahnschrift",
                                              fontface = "bold",
                                              hjust = -1,
                                              cex = 2)),
                     bottom = textGrob("Points on the top indicate a successful climb to the peak\nTidy Tuesday - @rbamattre", 
                                       gp = gpar(fontfamily = "Bahnschrift",
                                                 hjust = 0,
                                                 cex = 0.8)))

ggsave("himalaya.png", grid, device = "png", 
       width = 11, height = 9, bg = "#f8f8f8")
