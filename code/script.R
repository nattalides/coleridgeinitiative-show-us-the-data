
# Packages ----------------------------------------------------------------

library(tidyverse)
library(data.table)
library(parallel)
library(tidytext)

my_stop_words <- stop_words
substr(my_stop_words$word, 1, 1) <- toupper(substr(my_stop_words$word, 1, 1))
my_stop_words <- dplyr::bind_rows(stop_words, my_stop_words)

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

train_res <- readRDS("train_res.rds")

char_vec <- readRDS("char_vec.rds")

char_rm <- char_vec[!(char_vec %in% c(" ", ".", "(", ")", ",", ":"))]

# Read and Clean ----------------------------------------------------------

read_clean_json <- function(file_name, char_rm) {
  
  doc <- jsonlite::fromJSON(file_name)
  
  text <- stringr::str_c(doc$text, collapse = " ") %>% 
    stringr::str_remove_all(string = ., pattern = "[^\\w\\.\\s\\(\\)\\,\\:]") %>% 
    stringr::str_remove_all(string = ., pattern = "\n") %>% 
    tibble::tibble(text = .)
  
  res_text <- text %>% 
    tidytext::unnest_tokens(output = word,
                            input = text,
                            token = stringr::str_split, pattern = " ",
                            to_lower = FALSE) %>% 
    dplyr::anti_join(my_stop_words, by = "word") %>% 
    dplyr::distinct()
  
  clean_text <- stringr::str_c(res_text$word, collapse = " ")
  
  res <- tibble::tibble(file_name = !!file_name, text = clean_text)
  
  return(res)
  
}

# stringr::str_extract_all(tt, "\\W") %>% table %>% data.frame %>% View()

# Read all TEST documents -------------------------------------------------

test_res <- readRDS("test_res.rds")

# test_res <- parallel::mclapply(unique(test$file_name),
#                           read_json,
#                           mc.cores = parallel::detectCores()) %>% 
#   dplyr::bind_rows()

# saveRDS(test_res, "test_res.rds")

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

