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
tidy_episode <- function(url) {
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
                  input = line)
  
  return(episode_tidy)
}

# scrape all the transcripts
episodes_tidy_all <- data_frame(episode = episodes) %>%
  # safely tokenize each episode
  mutate(tidy = map(episode, safely(tidy_episode)),
         tidy_results = transpose(tidy)$result) %>%
  # expand to one row per token
  filter(! map_lgl(tidy_results, is.null)) %>%
  unnest(tidy_results) %>%
  # convert episode to two columns
  mutate(episode = parse_number(episode),
         episode = formatC(episode, width = 4, format = "d", flag = "0")) %>%
  ## two part episodes lose the second number
  separate(col = episode, into = c("season", "episode"),
           sep = 2, convert = TRUE)
