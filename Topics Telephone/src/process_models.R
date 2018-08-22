### process_models.R
### Jacob Zimmer
### Report error scores on two aggregated models using a list of selected terms

library(dplyr)
library(stringr)

source("helpers/utils.R")

args <- commandArgs(trailingOnly=TRUE)
if (length(args) < 3) {
  cat("You must specify two flat models and a file containing a subset of topic labels!\n")
  cat("USAGE: Rscript process_models.R path_to_model1 path_to_model2 path_to_terms\n")
} else {
  
  file1 <- args[[1]]
  file2 <- args[[2]]
  terms <- args[[3]]
  
  # file1 <- "../dat/models/ex5/Flat_V5K200_STM.RData"
  # file2 <- "../dat/models/ex5/Flat_V5K300_STM.RData"
  # terms <- "../dat/models/ex5/selected_topics.txt"
  
  file1 <- "~/Desktop/arxiv_k200_initial_flattened.RData"
  file2 <- "~/Desktop/arxiv_k300_reconstruction_flattened.RData"
  terms <- "~/Desktop/arxiv_reconstruction_selected.txt"

  selected_topics <- readChar(terms, file.info(terms)$size) %>%
    str_split("\n") %>% unlist()
  
  dfs <- parse.files(file1, file2) %>%
    filter.models(selected_topics) %>%
    score.models()
  
  cat(sprintf("Error scores:\n"))
  cat(sprintf("MAD:\t%.4f\n", dfs[['mad']]))
  cat(sprintf("MSE:\t%.4f\n", dfs[['mse']]))
  cat(sprintf("ROC:\t%.4f\n", dfs[['roc']]))
  
}