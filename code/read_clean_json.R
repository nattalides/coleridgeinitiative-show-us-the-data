read_clean_json <- function(file_name) {
  
  doc <- jsonlite::fromJSON(file_name)
  
  text <- stringr::str_c(doc$text, collapse = " ") %>% 
    #stringr::str_remove_all(string = ., pattern = "[^\\w\\.\\s\\(\\)\\,\\:]") %>% 
    stringr::str_remove_all(string = ., pattern = "[^\\w\\.\\s\\(\\)]") %>% 
    stringr::str_remove_all(string = ., pattern = "\n") %>% 
    stringr::str_remove_all(string = ., pattern = "\\d") %>% 
    stringr::str_replace_all(string = ., pattern = "\\s+", replacement = " ") %>% 
    tibble::tibble(text = .)
  
  # res_text <- text %>% 
  #   tidytext::unnest_tokens(output = word,
  #                           input = text,
  #                           token = stringr::str_split, pattern = " ",
  #                           to_lower = FALSE)
  # dplyr::anti_join(my_stop_words, by = "word")
  
  # clean_text <- stringr::str_c(res_text$word, collapse = " ")
  
  # language <- textcat(text)
  
  res <- tibble::tibble(file_name = file_name, text = text)
  
  return(res)
  
}