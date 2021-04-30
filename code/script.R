
# Packages ----------------------------------------------------------------

library(tidyverse)
library(data.table)
library(parallel)
library(tidytext)

# Load TRAIN and TEST -----------------------------------------------------

train <- data.table::fread("train.csv")

# Read and Clean ----------------------------------------------------------

read_clean_title <- function(dataset_title) {
  
  text <- stringr::str_c(dataset_title, collapse = " ") %>% 
    stringr::str_remove_all(string = ., pattern = "[^\\w\\.\\s\\(\\)]") %>% 
    stringr::str_remove_all(string = ., pattern = "\n") %>% 
    stringr::str_remove_all(string = ., pattern = "\\d") %>% 
    stringr::str_replace_all(string = ., pattern = "\\s+", replacement = " ")
  
  res <- tibble::tibble(dataset_title = dataset_title, cleaned_title = text)
  
  return(res)
  
}

read_clean_label <- function(dataset_label) {
  
  text <- stringr::str_c(dataset_label, collapse = " ") %>% 
    stringr::str_remove_all(string = ., pattern = "[^\\w\\.\\s\\(\\)]") %>% 
    stringr::str_remove_all(string = ., pattern = "\n") %>% 
    stringr::str_remove_all(string = ., pattern = "\\d") %>% 
    stringr::str_replace_all(string = ., pattern = "\\s+", replacement = " ")
  
  res <- tibble::tibble(dataset_label = dataset_label, cleaned_label = text)
  
  return(res)
  
}

label_df <- train %>% 
  dplyr::select(dataset_title, dataset_label, cleaned_label) %>% 
  dplyr::distinct()

label_title_clean <- parallel::mclapply(unique(label_df$dataset_title),
                                        read_clean_title,
                                        mc.cores = parallel::detectCores()) %>%
  dplyr::bind_rows()

label_title_label <- parallel::mclapply(unique(label_df$dataset_label),
                                        read_clean_label,
                                        mc.cores = parallel::detectCores()) %>%
  dplyr::bind_rows()

label_clean <- label_df %>% 
  dplyr::rename(output_label = cleaned_label) %>% 
  dplyr::left_join(label_title_clean, by = "dataset_title") %>% 
  dplyr::left_join(label_title_label, by = "dataset_label") %>% 
  dplyr::select(dataset_title, cleaned_title, dataset_label, cleaned_label, 
                output_label)

read_clean_json <- function(Id) {
  
  publication <- data.table(jsonlite::fromJSON(paste0('train/', Id, '.json')))
  
  text <- stringr::str_c(publication$text, collapse = " ") %>% 
    stringr::str_remove_all(string = ., pattern = "[^\\w\\.\\s\\(\\)]") %>% 
    stringr::str_remove_all(string = ., pattern = "\n") %>% 
    stringr::str_remove_all(string = ., pattern = "\\d") %>% 
    stringr::str_replace_all(string = ., pattern = "\\s+", replacement = " ")
  
  res <- tibble::tibble(Id = Id, text = text)
  
  return(res)
  
}

train_clean <- parallel::mclapply(unique(train$Id)[1:10],
                                  read_clean_json,
                                  mc.cores = parallel::detectCores()) %>%
  dplyr::bind_rows()

train <- train %>%
  dplyr::left_join(train_clean, by = "Id") %>% 
  dplyr::select(-cleaned_label) %>% 
  dplyr::left_join(label_clean, by = c("dataset_title", "dataset_label"))

saveRDS(train, "train.rds")

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
  
  if(nrow(word_ind) > 0) {
  res <- parallel::mclapply(1:nrow(word_ind),
                            get_words,
                            word_ind = word_ind,
                            temp_text = temp_text,
                            mc.cores = 1) %>%
    dplyr::bind_rows() %>% 
    distinct()
  } else {
    res <- tibble::tibble()
  }
  
  return(res)
  
}

data <- train %>% 
  dplyr::distinct(file_name, .keep_all = TRUE)

res <- parallel::mclapply(1:nrow(data),
                         get_words_text,
                         data = data,
                         sub = 5,
                         add = 50,
                         mc.cores = 8) %>%
  dplyr::bind_rows() %>% 
  distinct()

saveRDS(res, "res.rds")

new_labels <- res %>% 
  dplyr::mutate(label = stringr::str_remove_all(string = res, pattern = "\\( \\)")) %>% 
  dplyr::mutate(label = stringr::str_replace_all(string = label, pattern = "\\( ", replacement = "\\(")) %>% 
  dplyr::mutate(label = stringr::str_replace_all(string = label, pattern = "\\) ", replacement = "\\)")) %>% 
  dplyr::mutate(label = stringr::str_trim(string = label, side = "both")) %>% 
  dplyr::filter(label != "") %>% 
  dplyr::distinct(label, .keep_all = TRUE) %>% 
  dplyr::mutate(char_len = nchar(label)) %>% 
  dplyr::filter(char_len >= 10)
  
# Last step ---------------------------------------------------------------

train_labels <- c(unique(train$cleaned_label))

predict <- function(Id, data, train_labels){
  
  text <- data %>% 
    dplyr::filter(Id == !!Id) %>% 
    dplyr::select(text) %>% 
    dplyr::distinct() %>% 
    dplyr::pull(text)

  labels <- vector()
  
  for (label in train_labels) {
    if (sum(grepl(label, text)) > 0) {
      labels <- c(labels, label)
    }
  }
  
  res <- tibble::tibble(Id = Id, PredictionString = tolower(paste(labels, collapse = '|')))
  
  return(res)
}

test <- data.table::fread("sample_submission.csv")

test_clean <- parallel::mclapply(unique(test$Id),
                                  read_clean_json,
                                  mc.cores = parallel::detectCores()) %>%
  dplyr::bind_rows()

test_res <- parallel::mclapply(unique(test$Id),
                   predict,
                   data = test_clean,
                   train_labels = train_labels,
                   mc.cores = parallel::detectCores()) %>%
  dplyr::bind_rows()

fwrite(test_res, 'submission.csv')

                