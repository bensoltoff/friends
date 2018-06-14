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

# function to scrape a transcript and convert to tidytext data frame
# url: url of episode transcript
# n: n-gram
tidy_episode <- function(url, ngram = 1) {
  # collect text into a corpus
  episode_corpus <- read_html(url) %>%
    html_nodes("p")
  
  # most transcripts work fine as is
  if(length(episode_corpus) > 10){
    episode_corpus <- episode_corpus %>%
      html_text(trim = TRUE)
  } else {
    # for corpa which do not conform to "p" selector tag,
    # implement manual parsing method
    episode_corpus <- episode_corpus %>%
      str_replace_all(pattern = "<.*?>", replacement = "|") %>%
      str_split(pattern = "\\|") %>%
      nth(2) %>%
      str_replace(pattern = "\\n", replacement = "")
  }
  
  # fix unicode apostrophes
  episode_corpus <- str_replace_all(episode_corpus, "\\u0092", "'")
  
  # convert to one-line-per-row data frame
  episode_lines <- data_frame(line = episode_corpus) %>%
    filter(line != "") %>%
    # detect scene transitions
    mutate(scene = str_detect(line, "Scene"),
           scene_num = cumsum(scene)) %>%
    # remove scene transition lines
    ## some transcripts don't have scene markers, so only filter
    ## if they exist
    {if(sum(.$scene) != 0) filter(., scene_num != 0, !scene) else .} %>%
    select(-scene) %>%
    # determine which character is speaking
    mutate(character = str_extract(line, "\\w+:") %>%
             str_remove(":") %>%
             str_to_title,
           line = str_remove(line, "\\w+:")) %>%
    # deduplicate a few character names
    mutate(character = recode(character,
      Rach = "Rachel",
      Mnca = "Monica",
      Phoe = "Phoebe"
    )) %>%
    # remove lines that are not speech
    filter(!is.na(character))
  
  # convert to tidytext data frame
  episode_tidy <- episode_lines %>%
    unnest_tokens(output = word,
                  input = line,
                  token = "ngrams",
                  n = ngram)
  
  return(episode_tidy)
}

# scrape all the transcripts
episodes_all <- data_frame(episode = episodes) %>%
  # setup to create ngrams 1:5
  expand(episode, ngram = 1:5) %>%
  # safely tokenize each episode
  mutate(tidy = map2(episode, ngram, safely(tidy_episode)),
         tidy_results = transpose(tidy)$result)

episodes_tidy_all <- episodes_all %>%
  # expand to one row per token
  filter(!map_lgl(tidy_results, is.null)) %>%
  unnest(tidy_results) %>%
  # convert episode to two columns
  mutate(episode = parse_number(episode),
         episode = formatC(episode, width = 4, format = "d", flag = "0")) %>%
  # two part episodes lose the second number
  separate(col = episode, into = c("season", "episode"),
           sep = 2, convert = TRUE) %>%
  # create token ID column
  mutate(id = row_number()) %>%
  select(id, everything())

# remove stop words
friends_stop_words <- episodes_tidy_all %>%
  # separate ngrams into separate columns
  separate(col = word,
           into = c("word1", "word2", "word3", "word4", "word5"),
           sep = " ") %>%
  # find last word
  mutate(last = if_else(ngram == 5, word5,
                        if_else(ngram == 4, word4,
                                if_else(ngram == 3, word3,
                                        if_else(ngram == 2, word2, word1))))) %>%
  # remove tokens where the first or last word is a stop word
  filter(word1 %in% stop_words$word |
           last %in% stop_words$word) %>%
  # keep only id variables
  select(id:scene_num)

episodes_tidy_final <- episodes_tidy_all %>%
  anti_join(friends_stop_words) %>%
  write_csv("data/friends-tidytext.csv")

