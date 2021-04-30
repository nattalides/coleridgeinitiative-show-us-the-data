
# Packages ----------------------------------------------------------------

library(tidyverse)
library(data.table)

# Data --------------------------------------------------------------------

train_res_clean <- readRDS("train_res_clean.rds")

train <- data.table::fread("train.csv") %>%
  dplyr::mutate(file_name = stringr::str_c("train/", Id, ".json")) %>%
  dplyr::left_join(train_res_clean, by = "file_name")

saveRDS(train, "train.rds")

test_res_clean <- readRDS("test_res_clean.rds")

test <- data.table::fread("sample_submission.csv") %>% 
  dplyr::mutate(file_name = stringr::str_c("test/", Id, ".json")) %>% 
  dplyr::left_join(test_res_clean, by = "file_name")

saveRDS(test, "test.rds")

test <- readRDS("test.rds")

# Rules -------------------------------------------------------------------

set_ind <- function(ind, sub, add) {
  
  ind_df <- tibble::tibble(ind) %>% 
    dplyr::mutate(ind_start = ind - sub,
                  ind_stop = ind + add) %>% 
    dplyr::mutate(ind_start = dplyr::if_else(ind_start < 0, 0, ind_start)) %>% 
    dplyr::mutate(ind_stop = dplyr::if_else(ind_stop < 0, 0, ind_stop))
  
  return(ind_df)
  
}

get_words <- function(ind, word_ind, temp_text) {
  
  res <- temp_text %>% 
    dplyr::slice(word_ind$ind_start[ind]:word_ind$ind_stop[ind]) %>% 
    dplyr::pull(word) %>% 
    stringr::str_c(., collapse = " ") %>% 
    stringr::str_replace_all(string = ., pattern = "\\s+", replacement = " ") %>%
    stringr::str_remove_all(string = ., pattern = "\\. \\w+") %>%
    stringr::str_remove_all(string = ., pattern = "\\.\\w+") %>% 
    stringr::str_extract_all(string = ., pattern = "\\b[A-Z]\\w+|\\(|\\)") %>% 
    unlist() %>% 
    stringr::str_c(., collapse = " ") %>% 
    tibble::tibble(res = .)
  
  return(res)
  
}

get_words_text <- function(x, data, sub, add) {
  
  temp <- data %>% 
    dplyr::filter(Id == data$Id[x]) 
  
  temp_text <- temp %>%
    dplyr::select(text) %>% 
    dplyr::slice(1) %>% 
    tidytext::unnest_tokens(output = word,
                            input = text,
                            token = stringr::str_split, pattern = " ",
                            to_lower = FALSE)
  
  ind <- stringr::str_detect(string = temp_text$word, pattern = "data") %>% 
    which
  
  word_ind <- set_ind(ind, sub = sub, add = add)
  
  res <- parallel::mclapply(1:nrow(word_ind),
                            get_words,
                            word_ind = word_ind,
                            temp_text = temp_text,
                            mc.cores = 1) %>%
    dplyr::bind_rows() %>% 
    distinct()
  
  return(res)
  
}


tt <- parallel::mclapply(1:20,
                         get_words_text,
                         data = train,
                         sub = 0,
                         add = 100,
                         mc.cores = 4) %>%
  dplyr::bind_rows() %>% 
  distinct()














