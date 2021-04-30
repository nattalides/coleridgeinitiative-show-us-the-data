result <- jsonlite::fromJSON("train/d0fa7568-7d8e-4db9-870f-f9c6f668c17b.json")
length(result)
head(result)

library("rjson")
result <- jsonlite::fromJSON("train/01e4e08c-ffea-45a7-adde-6a0c0ad755fc.json")
length(result)
head(result)

result[[1]]$text


train$pub_title[1]
train$dataset_title[1]

library(tidyverse)
result <- jsonlite::fromJSON(stringr::str_c("train/", train$Id[2],".json")) %>% tibble::tibble()


