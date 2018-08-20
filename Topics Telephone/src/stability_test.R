
library(dplyr)
library(stringr)

source("helpers/utils.R")
source("helpers/utils_optimize.R")

stability.test <- function(files) {
  file1 <- files[1]; print(file1)
  file2 <- files[2]; print(file2)
  
  dfs <- parse.files(file1, file2, stability_test=TRUE) %>%
    create.cost.matrix(stability_test=TRUE) %>%
    solve.LSAP() %>%
    merge.reconstruction() %>%
    score.models(stability_test=TRUE)
  
  return(list("mad"=dfs[['mad']],
              "mse"=dfs[['mse']],
              "roc"=dfs[['roc']]))
}

args <- commandArgs(trailingOnly=TRUE)
if (length(args) < 2) {
  print("You must specify a directory of models to test and k!")
  print("USAGE: Rscript stability_test.R path_to_dir")
} else {
  
  dir_to_process <- args[[1]]
  k <- args[[2]]
  
  # dir_to_process <- "../dat/models/ex5-stability_test/"
  # k <- "200"
  
  # select files
  files_to_process <- list.files(dir_to_process, full.names = TRUE)
  files_to_process <- str_subset(files_to_process, k)
  
  pairs_to_process <- combn(files_to_process, 2)
  
  res_dfs <- apply(pairs_to_process, 2, stability.test)
  
  res_mad <- mean(unlist(lapply(res_dfs, `[`, 'mad')))
  res_mse <- mean(unlist(lapply(res_dfs, `[`, 'mse')))
  res_roc <- mean(unlist(lapply(res_dfs, `[`, 'roc')))
  
  print(sprintf("Stability results at K = %s:", k))
  cat(sprintf("MAD:\t%.4f\n", res_mad))
  cat(sprintf("MSE:\t%.4f\n", res_mse))
  cat(sprintf("ROC:\t%.4f\n", res_roc))
}
