---
title: "Olympics and Income"
output: html_notebook
---

The Olympics are about talent, hard work, and dreams. I think they're also about wealth, infrastructure, and resources.

The guiding question for this exploration is:

**Is participation and medals in the Olympics disproportionately related to country's wealth?**

Or in other words: if you have immense talent, are you more likely to make it into the Olympics and win because of being born in a wealthy industrialized country?

## Load some packages

```{r}
# load
library(tidytuesdayR) # Get tidy tuesday data
library(WDI) # World Bank World Development Indicators: https://datatopics.worldbank.org/world-development-indicators/

# wrangle
library(tidyverse) # of course
library(lubridate)
library(janitor) # clean column names and nice tables
```

## Read in the data

```{r}
# load data from tidy tuesday package
tuesdata <- tidytuesdayR::tt_load('2021-07-27')
```
