## load libraries
library(data.table)
library(jsonlite)
## read train data labels
train <- fread('train.csv')
train_labels <- c(unique(train$cleaned_label))
## function to read data and match strings
predict <- function(Id){
  publication <- data.table(fromJSON(paste0('test/', Id, '.json')))
  publication[, clean_text := gsub('[^A-Za-z0-9]+', ' ', tolower(text))]
  
  labels <- vector()
  
  for (label in train_labels)
  {
    if (sum(grepl(label, publication$clean_text)) > 0)
    {
      labels <- c(labels, label)
    }
  }
  
  return (paste(labels, collapse = '|'))
}
## iterate test data
sample_submission <- fread('sample_submission.csv')
sample_submission[, PredictionString := lapply(Id, predict)]
## save predictions
fwrite(sample_submission[, .SD, .SDcols = c('Id', 'PredictionString')], 'submission.csv')