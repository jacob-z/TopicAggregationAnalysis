
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

print.assignments <- function(dfs, n_docs, source_dir) {
  t1 <- dfs[["t1"]]
  t2 <- dfs[["t2"]]
  ranked_docs <- dfs[["worst_docs"]]
  
  queries <- vector("character", n_docs)
  j <- 1
  for (i in 1:dim(t1)[1]) {
    docs <- ranked_docs[i,]
    for (cat in c("mad_doc", "mse_doc", "roc_doc")) {
      doc <- as.character(unlist(unname(docs[cat])))
      if (!(doc %in% queries)) {
        queries[j] <- doc
        j <- j + 1
      }
    }
    if (j == n_docs + 1) {break}
  }
  
  for (query in queries) {
    idx <- match(query, row.names(t1))
    t1_dist <- t1[idx,]; row.names(t1_dist) <- c()
    t2_dist <- t2[idx,]; row.names(t2_dist) <- c()
    
    k <- dim(t1)[2]
    mad_score <- mad(t1, t2, raw_docs=T)[idx]
    roc_score <- roc(t1, t2, raw_docs=T)[idx]
    
    cat(sprintf("Document Title: %s\n", query))
    
    cat(sprintf("Sample Text: "))
    filename <- paste(source_dir, str_replace(query, " ", "_"), sep="")
    file_text <- readChar(filename, ifelse(file.info(filename)$size > 2000, 2000, file.info(filename)$size))
    cat(sprintf("%s\n\n", str_squish(file_text)))
    
    cat(sprintf("Error metrics:\t\tMAD = %.4f\tROC = %.4f\n\n", mad_score, roc_score))
    cat(sprintf("Document-Topic Proportions:\n"))
    cat(sprintf("Topic Label\t\tOriginal\tReconstruction\n"))
    
    k <- length(t1_dist)
    for (i in 1:k) {
      docname <- names(t1_dist)[i] %>% str_trunc(15) %>% str_pad(15, "right")
      cat(sprintf("%s\t\t%.4f\t\t%.4f\n", docname, unname(t1_dist)[i], unname(t2_dist)[i]))
    }
    cat("\n\n")
  }
  
  
  return(dfs)
}









