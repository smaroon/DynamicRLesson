test <-function()
  cat("working\n on it")

parse_template <- function() { 
  library(yaml)
  newrow <- function(element){
    temp <- data.frame(raw_yaml)
  }
  raw_yaml <- yaml.load_file("template.yaml")
  #temp <- lapply(raw_yaml[-1], newrow)
  #df <- NULL
  s <- toString(raw_yaml)
  writeLines(s , "out.yaml")
  #writeLines(temp , "out.yaml")
  #}
  #writeLines(readLines("template.txt" , n=-1), "out.txt")
}
