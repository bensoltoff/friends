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