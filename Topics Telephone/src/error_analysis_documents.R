
library(dplyr)
library(stringr)

source("helpers/utils.R")

args <- commandArgs(trailingOnly=TRUE)
if (length(args) < 5) {
  cat("BAD ARGUMENTS\n")
  cat("You must specify two models, terms, the number of documents to print,\n")
  cat("and a path to the corpus texts! It is recommended that you specify an\n")
  cat("output file as this method can generate a large output.\n")
  cat("USAGE: Rscript error_analysis_documents.R path_to_1 path_to_2 \n")
  cat("\tpath_to_terms n_docs path_to_corpus > out.txt\n")
} else {
  
  file1  <- args[[1]]
  file2  <- args[[2]]
  terms  <- args[[3]]
  n_docs <- as.integer(args[[4]])
  source_dir <- args[[5]]
  
  # file1  <- "../dat/models/ex5/Flat_V5K200_STM.RData"
  # file2  <- "../dat/models/ex5/Flat_V5K300_STM.RData"
  # terms  <- "../dat/models/ex5/selected_topics.txt"
  # n_docs <- 10
  # source_dir <- "../dat/corpora/ex5/"
  
  selected_topics <- readChar(terms, file.info(terms)$size) %>%
    str_split("\n") %>% unlist()
  
  dfs <- parse.files(file1, file2) %>%
    filter.models(selected_topics) %>%
    score.models(raw_docs=TRUE) %>%
    rank.worst.docs() %>%
    print.worst.docs(n_docs) %>%
    print.assignments(n_docs, source_dir)
}