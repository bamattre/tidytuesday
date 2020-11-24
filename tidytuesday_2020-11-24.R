# Tidy Tuesday 2020-11-24
# Washington Hiking
# Richard Bamattre @rbamattre

# load
library(tidytuesdayR) # Get tidy tuesday data

# wrangle
library(tidyverse) # of course
library(janitor) # clean column names and nice tables

# model
library(tidymodels)
library(textrecipes)
library(vip)

# Import fonts 
library(extrafont) # use fonts

# viz
library(ggthemes)
library(corrplot)
library(gridExtra)
library(grid)

# create custom color theme
colors = c("#59886b", "#c05555")

# set font
font = "Bahnschrift"

# change theme
theme_tufte2 <- theme_tufte() +
  theme(text = element_text(family = font))

theme_set(theme_tufte2)

# load data
tuesdata <- tidytuesdayR::tt_load('2020-11-24')

hike <- tuesdata$hike_data %>%
  mutate(across(.cols = c(gain:rating), as.numeric)) %>%
  distinct(name, .keep_all = TRUE) # remove duplicates

# Could we predict whether a hike is good for kids based on the features?

# Clean data - one column per feature

hike2 <- hike %>%
  unnest(features) %>%
  mutate(value = 1) %>% 
  pivot_wider(names_from = "features", values_from = "value") %>%
  mutate(across(.cols = c(`Dogs allowed on leash`:Summits), ~ replace_na(.x, 0))) %>%
  separate(length, into = c("length", "length_type"), sep = "\\smiles,\\s") %>%
  mutate(length_type = if_else(str_detect(length, "miles of trails"), "miles of trails",
                               length_type),
         length = str_remove(length, " miles of trails") %>% as.numeric()) %>%
  clean_names() %>%
  mutate(Outcome = if_else(good_for_kids == 1, "Good for kids",
                                 "Not good for kids") %>% factor()) %>% # outcome should be a factor
  select(-good_for_kids)
  
# visualize data

plot_number <- hike2 %>%
  ggplot(aes(Outcome, fill = Outcome)) +
    geom_bar() +
    scale_fill_manual(values = colors) +
    labs(x = "", y = "Number of hikes",
         title = "Number of trails") +
    theme(legend.position = "none")
    
plot_variables <- hike2 %>%
  filter(gain < 10000, length < 50) %>% # don't plot outliers
  rename("Gain in elevation (feet)" = gain,
         "Highest point in feet" = highpoint,
         "User submitted rating (out of 5)" = rating,
         "Length of trail" = length) %>%
  pivot_longer(c("Length of trail", 
                 "Gain in elevation (feet)":"User submitted rating (out of 5)"), 
               values_to = "value", names_to = "indicator") %>%
  ggplot(aes(value, fill = Outcome)) +
    geom_density(color = NA, alpha = 0.8) +
    facet_wrap(~ indicator, scales = "free") +
    scale_fill_manual(values = colors) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    labs(y = "", x = ""
         #title = "Relationship of various measures with trails being good for kids"
         )

# correlation plot
hike2 %>%
  select(where(is.numeric)) %>%
  cor() %>%
  corrplot()

# Test/train split

set.seed(321)

# 80/20 train test split
split <- initial_split(hike2, prop = 8/10) 

train <- training(split)
test <- testing(split)

# Create a recipe to transform data

hike_recipe <- recipe(Outcome ~ ., data = train) %>%
  step_rm(location) %>%
  update_role(name, new_role = "ID") %>%
  step_dummy(length_type) %>% # make dummy variables from length type
  step_tokenize(description) %>% # tokenize the description (separate words)
  step_stopwords(description) %>% # remove stop words
  step_stem(description) %>% # stem words
  step_tokenfilter(description, min_times = 10) %>%
  # remove tokens that are used less than n times
  step_tfidf(description, prefix = "tf_") %>% # make tfidf
  step_center(all_predictors()) %>%
  step_scale(all_predictors())
  
# preview recipe
hike_recipe

# see what features are generated
hike_recipe %>%
  prep() %>%
  bake(train) %>%
  str()

# set up model(s)

forest <- 
  rand_forest() %>%
  set_engine("ranger",
             importance = "impurity") %>%
  set_mode("classification")

knn <-
  nearest_neighbor() %>%
  set_engine("kknn") %>%
  set_mode("classification")

# set up workflow

hike_workflow <- 
  workflow() %>%
  add_model(forest) %>%
  add_recipe(hike_recipe)

# preview model
hike_workflow
  
# model

# set crossfolds
folds <- vfold_cv(train, v = 4)

# create a fit using workflow, feed in training data
hike_fit <-
  hike_workflow %>%
  fit_resamples(folds)
  
# metrics
collect_metrics(hike_fit)

# last fit

last_fit <-
  hike_workflow %>%
  last_fit(split)
  
collect_metrics(last_fit)

# nice custom visuals from https://www.benjaminsorensen.me/post/modeling-with-parsnip-and-tidymodels/

plot_conf_mat <- last_fit %>%
  collect_predictions() %>%
  conf_mat(truth = Outcome, estimate = .pred_class) %>%
  .[[1]] %>%
  as.data.frame() %>%
  ggplot(aes(Prediction, Truth, fill = Freq)) +
    geom_tile() +
    geom_text(aes(label = Freq), color = "white", size = 10) +
    theme(legend.position = "none") +
    scale_fill_gradient(high = "#59886b", low = "#c05555")

plot_prob <- last_fit %>%
  collect_predictions() %>%
  ggplot(aes(`.pred_Good for kids`, fill = Outcome)) +
    geom_density(alpha = 0.8, color = NA) +
    scale_fill_manual(values = colors) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    labs(y = "", x = "Probability",
         title = "Probability of being good for kids versus actual outcome")

# variable importance

plot_vip <- last_fit %>%
  pluck(".workflow", 1) %>%
  pull_workflow_fit() %>%
  vip(num_features = 10, aesthetics = list(fill = "#59886b")) +
    labs(title = "Variable importance from Random Forest")

# add text

text_1 <- ggplot() +
  annotate("text",
           label = paste("It's pretty easy to know if a trail is good for kids\n",
                 "if someone has labeled it. However, is it possible\n",
                 "to predict whether a trail is kid-friendly using\n",
                 "the trail length, gain, features, ratings, and descriptions?"),
           x = 0, y = 0, size = 4, 
           family = font) +
  theme_void()

text_2 <- ggplot() +
  annotate("text",
           label = paste("We can see there's\n already some relationships\n",
                         "between how long, steep,\n and high a trail is\n",
                         "and whether it's kid-friendly.\n Rating doesn't seem\n",
                         "to be helpful."),
           x = 0, y = 0, size = 3, 
           family = font) +
  theme_void()

text_3 <- ggplot() +
  annotate("text",
           label = paste("A random forest model, trained on 80% of the trails,\n",
                         "is 82% accurate in predicting whether it's good\n",
                         "for kids."),
           x = 0, y = 0, size = 4, 
           family = font) +
  theme_void()

text_4 <- ggplot() +
  annotate("text",
           label = paste("Certain attributes are \nmore useful in predicting\n",
                         "a trail's kid-friendliness.\n The 'tf_description'\n",
                         "variables are words taken\n from the description"),
           x = 0, y = 0, size = 3, 
           family = font) +
  theme_void()

# bring together the plots

g <- arrangeGrob(text_1, plot_number, 
                 plot_variables, text_2,
                 text_3, plot_conf_mat,
                 plot_prob, 
                 text_4, plot_vip,
                 layout_matrix = rbind(c(1, 1, 2, 2),
                                       c(3, 3, 3, 4),
                                       c(3, 3, 3, 4),
                                       c(5, 5, 6, 6),
                                       c(7, 7, 7, 7),
                                       c(8, 9, 9, 9)),
                 top = textGrob("Predicting Whether Washington Trails are Good for Kids", 
                                gp = gpar(fontfamily = font,
                                          fontface = "bold",
                                          col = "black",
                                          hjust = -1,
                                          cex = 1.7)),
                 bottom = textGrob("Tidy Tuesday | @rbamattre | Source: Washington Trails Association", 
                                   gp = gpar(fontfamily = font,
                                             col = "black",
                                             hjust = 0,
                                             cex = 0.8)))

ggsave("2020-11-24 washington trails.png", g, width = 8, height = 11, dpi = 150,
       bg = "#f4f4f4")

