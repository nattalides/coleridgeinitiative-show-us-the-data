jaccard = function(str1, str2) {
  a = strsplit(tolower(str1), ' ')[[1]]
  b = strsplit(tolower(str2), ' ')[[1]]
  c = intersect(a, b)
  return((length(c)) / (length(a) + length(b) - length(c)))
}

precision = function(tp, fp) {
  return(tp/(tp + fp))
}

recall = function(tp, fn) {
  return(tp/(tp + fn))
}

fbeta = function(precision, recall, beta) {
  fbeta = (1 + beta^2) * (precision * recall) / (beta^2 * precision + recall)
  return(fbeta)
}