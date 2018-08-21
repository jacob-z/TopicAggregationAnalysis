
library(dplyr)
library(stringr)

source("helpers/utils.R")

args <- commandArgs(trailingOnly=TRUE)
if (length(args) < 4) {
  cat("You must specify two models, terms, and the number of documents to print!\n")
  caT("It is recommended that you specify an output file as this method can generate a large output.")
  cat("USAGE: Rscript error_analysis_documents.R path_to_1 path_to_2 path_to_terms n_docs > out.txt\n")
} else {
  
  file1  <- args[[1]]
  file2  <- args[[2]]
  terms  <- args[[3]]
  n_docs <- as.integer(args[[4]])
  
  # file1  <- "../dat/models/ex5/Flat_V5K200_STM.RData"
  # file2  <- "../dat/models/ex5/Flat_V5K300_STM.RData"
  # terms  <- "../dat/models/ex5/selected_topics.txt"
  # n_docs <- 10
  
  selected_topics <- readChar(terms, file.info(terms)$size) %>%
    str_split("\n") %>% unlist()
  
  dfs <- parse.files(file1, file2) %>%
    filter.models(selected_topics) %>%
    score.models(raw_docs=TRUE) %>%
    get.worst.docs(n_docs) %>%
    print.worst.docs(n_docs) %>%
    print.assignments(n_docs)
  
  # get.worst.docs should load the data frame of docnames and error scores across the three metrics
  # print... should just print out the top rows of this frame
  # print assignments should just take the top and generate lots of output
}