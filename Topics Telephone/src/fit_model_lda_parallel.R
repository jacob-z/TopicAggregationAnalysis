### fit_model_stm.R
### Jacob Zimmer
### Fit a corpus of text files with STM (adapted from ajbc/trellis on Github)

library(stm)
library(data.table)
library(foreign)
library(parallel)

fit.lda <- function(out, k, i, out_type, out_file) {
  model <- stm(documents=out$documents, vocab=out$vocab, K=k, init.type="LDA",
               seed=round(runif(10, min = 0, max = 1000)))
  
  beta <- exp(model$beta$logbeta[[1]])
  theta <- model$theta
  vocab <- out$vocab
  
  filenames <- lapply(titles$title, function (x) gsub("^(\\s|\\r|\\n|\\t)+|(\\s|\\n|\\r|\\t)+$", "", x))
  titles <- lapply(filenames, function (x) URLdecode(gsub("_", " ", x)))
  
  out_file <- gsub("\\.", paste0("_", i, "."), out_file)
  
  if (out_type == "trellis") {
    save(beta, theta, vocab, titles, filenames, file = out_file)
  } else {
    save(model, file = out_file)
  }
}

args <- commandArgs(trailingOnly=TRUE)
if (length(args) < 5) {
  cat("BAD ARGUMENTS\n")
  cat("You must specify a directory of text files, number of topics, type of\n")
  cat("output (trellis or stm) and an output file.\n")
  cat("USAGE: Rscript fit_model_lda.R path_to_dir k n out_type out_file \n")
} else {
  
  dir <- args[[1]]
  k <- as.integer(args[[2]])
  n <- as.integer(args[[3]])
  out_type <- args[[4]]
  out_file <- args[[5]]
  
  if (!(out_type %in% c("stm", "trellis"))) {
    stop("Invalid argument to out_type.  Only \"stm\" or \"trellis\" are valid.")
  }
  
  filenames <- list.files(dir)
  files <- lapply(filenames, function(x) { readLines(file.path(dir, x)) })
  files <- lapply(files, function(x) paste(x, sep="", collapse=" "))
  files <- t(data.frame(files))
  rownames(files) <- c()
  
  titles <- data.frame(filenames)
  docs <- data.frame(files)
  
  colnames(titles) <- c("title")
  colnames(docs) <- c("documents")
  
  processed <- textProcessor(docs$documents, metadata=titles)
  out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
  
  
  mclapply(1:n, function(i) fit.lda(out, k, i, out_type, out_file), mc.cores=2)
}