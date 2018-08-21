### lda_comparison.R
### Jacob Zimmer
### Run the LDA equivalent of Topics Telephone for comparison to Trellis/aggregation


library(dplyr)
library(stringr)

source("helpers/utils.R")
source("helpers/utils_optimize.R")

pipeline <- function(model1, model2) {
  t1 <- data.frame(model1$theta)
  t2 <- data.frame(model2$theta)
  
  lst <- create.cost.matrix(list("t1"=t1, "t2"=t2), stability_test=TRUE) %>%
    solve.LSAP() %>% 
    merge.reconstruction() %>%
    score.models(stability_test=TRUE)
  
  return(lst)
}

args <- commandArgs(trailingOnly=TRUE)
if (length(args) < 1) {
  cat("You must specify a directory of STM models to process!\n")
  cat("USAGE: Rscript stability_test.R path_to_dir\n")
} else {
  
  dir_to_process <- args[[1]]
  
  # dir_to_process <- "../dat/models/ex5-lda_comparison/22"
  
  files_to_process <- list.files(dir_to_process, full.names = TRUE)
  
  # Load models
  cat("Loading models...\n")
  models <- vector("list", length(files_to_process))
  for (i in 1:length(files_to_process)) {
    load(files_to_process[i])
    models[[i]] <- model
    rm(model)
  }
  
  # Select best based on convergence
  cat("Selecting best model...\n")
  convs <- vector("integer", 21)
  for (i in 1:21) {
    convs[i] <- models[[i]]$convergence$bound[length(models[[i]]$convergence$bound)]
  }
  
  model_best <- models[[which.max(convs)]]
  models[[which.max(convs)]] <- NULL
  
  res_dfs <- lapply(models, function(i) pipeline(model_best, i))
  
  res_mad <- unlist(lapply(res_dfs, `[`, 'mad'))
  res_mse <- unlist(lapply(res_dfs, `[`, 'mse'))
  res_roc <- unlist(lapply(res_dfs, `[`, 'roc'))
  
  cat(sprintf("LDA comparison results:\n"))
  cat(sprintf("MAD:\t%.4f +/- %.4f\n", mean(res_mad), sd(res_mad)))
  cat(sprintf("MSE:\t%.4f +/- %.4f\n", mean(res_mse), sd(res_mse)))
  cat(sprintf("ROC:\t%.4f +/- %.4f\n", mean(res_roc), sd(res_roc)))
  
}