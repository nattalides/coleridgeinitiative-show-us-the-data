
# Packages ----------------------------------------------------------------

library(tidyverse)
library(data.table)
library(parallel)
library(tidytext)
library(textcat)
library(stopwords)

# Stop Words --------------------------------------------------------------

include_words <- stringr::str_extract_all(string = train$cleaned_label,
                                          pattern = "\\w+") %>%
  unlist() %>%
  unique() %>%
  tolower() %>%
  tibble::tibble(word = .)

labels <- include_words

substr(include_words$word, 1, 1) <- toupper(substr(include_words$word, 1, 1))

labels <- dplyr::bind_rows(labels, include_words) %>%
  dplyr::distinct()

stop_words_1 <- stop_words %>%
  dplyr::select(word)

substr(stop_words_1$word, 1, 1) <- toupper(substr(stop_words_1$word, 1, 1))

stop_words_1 <- dplyr::bind_rows(dplyr::select(stop_words, word), stop_words_1) %>%
  dplyr::anti_join(labels, by = "word")

stop_words_2 <- stopwords(language = "en") %>%
  tibble::tibble(word = .)

substr(stop_words_2$word, 1, 1) <- toupper(substr(stop_words_2$word, 1, 1))

stop_words_2 <- dplyr::bind_rows(stopwords(language = "en") %>%
                                   tibble::tibble(word = .), stop_words_2) %>%
  dplyr::anti_join(labels, by = "word")

my_stop_words <- dplyr::bind_rows(stop_words_1, stop_words_2) %>%
  dplyr::distinct()

saveRDS(my_stop_words, "my_stop_words.rds")

my_stop_words <- readRDS("my_stop_words.rds")