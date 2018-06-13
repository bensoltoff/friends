## tidy.R
## 6/11/18 BCS
## Convert Friends transcripts to tidytext data frame

library(tidyverse)
library(stringr)
library(rvest)
library(tidytext)

# get list of file names
episodes <- list.files(path = "season", full.names = TRUE)

## remove duplicate episodes
episodes <- episodes[!str_detect(episodes, pattern = "outtakes|uncut")]

# scrape the transcript of a single episode
episode <- episodes[[1]]

## collect text into a corpus
episode_corpus <- read_html(episode) %>%
  html_nodes("p") %>%
  html_text(trim = TRUE)

## convert to one-line-per-row data frame
episode_lines <- data_frame(episode = parse_number(episode),
           line = episode_corpus) %>%
  # detect scene transitions
  mutate(scene = str_detect(line, "Scene"),
         scene_num = cumsum(scene)) %>%
  # remove scene transition lines
  filter(scene_num != 0,
         !scene) %>%
  # determine which character is speaking
  mutate(character = str_extract(line, "\\w+:"),
         character = str_remove(character, ":")) %>%
  # remove lines that are not speech
  filter(!is.na(character))

