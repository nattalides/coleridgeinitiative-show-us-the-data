
# Packages ----------------------------------------------------------------

library(tidyverse)
library(data.table)
library(parallel)
library(tidytext)
library(textcat)

# Load TRAIN and TEST -----------------------------------------------------

train <- data.table::fread("train.csv") %>% 
  dplyr::mutate(file_name = stringr::str_c("train/", Id, ".json"))

test <- data.table::fread("sample_submission.csv") %>% 
  dplyr::mutate(file_name = stringr::str_c("test/", Id, ".json"))

# Read JSON ---------------------------------------------------------------

read_json <- function(file_name) {
  
  doc <- jsonlite::fromJSON(file_name)
  
  text <- stringr::str_c(tolower(doc$text), collapse = " ")
  
  res <- tibble::tibble(file_name = !!file_name, text = text)
  
  return(res)
  
}

# Read all TRAIN documents ------------------------------------------------

# train_res <- parallel::mclapply(unique(train$file_name),
#                           read_json,
#                           mc.cores = parallel::detectCores()) %>% 
#   dplyr::bind_rows()
# saveRDS(train_res, "train_res.rds")

# char_vec <- stringr::str_extract_all(string = train_res$text, pattern = "\\W") %>% 
#   unlist() %>%
#   unique()
# saveRDS(char_vec, "char_vec.rds")

# train_res <- readRDS("train_res.rds")

# Read and Clean ----------------------------------------------------------

read_clean_json <- function(file_name) {
  
  doc <- jsonlite::fromJSON(file_name)
  
  text <- stringr::str_c(doc$text, collapse = " ") %>% 
    stringr::str_remove_all(string = ., pattern = "[^\\w\\.\\s\\(\\)\\,\\:]") %>% 
    stringr::str_remove_all(string = ., pattern = "\n") %>% 
    stringr::str_remove_all(string = ., pattern = "\\d") %>% 
    stringr::str_remove_all(string = ., pattern = "\\(\\)") %>% 
    stringr::str_replace_all(string = ., pattern = "\\s+", replacement = " ") %>% 
    tibble::tibble(text = .)
  
  res_text <- text %>% 
    tidytext::unnest_tokens(output = word,
                            input = text,
                            token = stringr::str_split, pattern = " ",
                            to_lower = FALSE)
  #dplyr::anti_join(my_stop_words, by = "word")
  
  clean_text <- stringr::str_c(res_text$word, collapse = " ")
  
  language <- textcat(clean_text)
  
  res <- tibble::tibble(file_name = file_name, text = clean_text, lan = language)
  
  return(res)
  
}

train_res_clean <- parallel::mclapply(unique(train$file_name)[1:20],
                                      read_clean_json,
                                      mc.cores = parallel::detectCores()) %>%
  dplyr::bind_rows()
saveRDS(train_res_clean, "train_res_clean.rds")

train_res_clean <- readRDS("train_res_clean.rds")


test_res_clean <- parallel::mclapply(unique(test$file_name),
                                     read_clean_json,
                                     mc.cores = parallel::detectCores()) %>%
  dplyr::bind_rows()
# saveRDS(test_res_clean, "test_res_clean.rds")

# Detect function ---------------------------------------------------------

detect_word <- function(file_name, data) {
  
  data <- train_res
  file_name <- data$file_name[10]
  
  text <- data %>% 
    dplyr::filter(file_name == !!file_name) %>% 
    dplyr::pull(text)
  
  stringr::str_detect(string = text, pattern = "data")
  
  stringr::str_extract_all(string = text, pattern = "(\\w+.){2}data.(\\w+....){11}")
  
  # stringr::str_detect(string = text, pattern = "dataset")
  # stringr::str_detect(string = text, pattern = "survey")
  # stringr::str_detect(string = text, pattern = "report")
  # stringr::str_detect(string = text, pattern = "\\(.*\\)")
  
  
}

