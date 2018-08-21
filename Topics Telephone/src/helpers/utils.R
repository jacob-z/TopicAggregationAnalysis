### utils.R
### Jacob Zimmer
### Tools for working with the output of Topics Telephone experiments

parse.files <- function(file1, file2, model="trellis", stability_test=FALSE) {
  cat("Loading files...\n")

  if (model == "trellis") {

    load(file1)
    t1 <- data.frame(theta)
    if (! stability_test) {
      names(t1) <- unlist(mantitles)
    }
    row.names(t1) <- titles

    load(file2)
    t2 <- data.frame(theta)
    if (dim(t2)[2] <= dim(t1)[2] && ! stability_test) {
      names(t2) <- unlist(mantitles)
    }
    row.names(t2) <- titles

  } else if (model == "stm") {



  } else if (model == "tm-lda") {



  } else {
    stop(sprintf("Error: Model type %s is not valid", model))
  }
  
  return(list(t1=t1, t2=t2))
}

filter.models <- function(dfs, selected_topics) {
  cat("Filtering models...\n")

  x1 <- dfs[['t1']][names(dfs[['t1']]) %in% selected_topics]
  x2 <- dfs[['t2']][names(dfs[['t2']]) %in% selected_topics]
  y1 <- rowSums(dfs[['t1']][ ! (names(dfs[['t1']]) %in% selected_topics)])
  y2 <- rowSums(dfs[['t2']][ ! (names(dfs[['t2']]) %in% selected_topics)])
  
  dfs[['t1']] <- cbind(x1, "Other"=y1)
  dfs[['t2']] <- cbind(x2, "Other"=y2)
  dfs[['t1']] <- dfs[['t1']][, order(names(dfs[['t1']]))]
  dfs[['t2']] <- dfs[['t2']][, order(names(dfs[['t2']]))]
  
  return(dfs)
}

word.count <- function(str1) {
  return(sapply(gregexpr("[[:alpha:]]+", str1), function(x) sum(x > 0)))
}

mad <- function(df_t1, df_t2, raw_docs=FALSE, raw_tops=FALSE) {
  t1 <- unname(as.matrix(df_t1))
  t2 <- unname(as.matrix(df_t2))

  if (raw_docs && raw_tops) {
    print("Arguments are exclusive")
    return(-1)
  }

  res <- -1

  if (raw_docs) {
    res <- rowMeans(abs(t1-t2))
    names(res) <- row.names(df_t1)
    return(res)
  } else if (raw_tops) {
    res <- colMeans(abs(t1-t2))
    names(res) <- names(df_t1)
    return(res)
  } else {
    return(mean(colMeans(abs(t1-t2))))
  }
}

mse <- function(df_t1, df_t2, raw_docs=FALSE, raw_tops=FALSE) {
  t1 <- unname(as.matrix(df_t1))
  t2 <- unname(as.matrix(df_t2))

  if (raw_docs && raw_tops) {
    print("Arguments are exclusive")
    return(-1)
  }

  res <- -1

  if (raw_docs) {
    res <- rowMeans((t1-t2)*(t1-t2))
    names(res) <- row.names(df_t1)
    return(res)
  } else if (raw_tops) {
    res <- colMeans((t1-t2)*(t1-t2))
    names(res) <- names(df_t1)
    return(res)
  } else {
    return(mean(colMeans((t1-t2)*(t1-t2))))
  }
}

roc <- function(t1, t2, raw_docs=FALSE) {
  N <- dim(t1)[1]
  k <- dim(t1)[2]

  res <- sapply(1:N, function(i) {
    x <- t1[i,]; y <- t2[i,]

    a <- x[order(x, decreasing = T)][1:k]
    b <- y[order(y, decreasing = T)][1:k]
  
    # get numeric ranks
    l <- unique(c(as.character(names(a)), as.character(names(b))))
    a <- as.numeric(factor(names(a), levels=l))
    b <- as.numeric(factor(names(b), levels=l))

    return(unname(cor.test(a, b, method="spearman")[["estimate"]]))
  })

  if (raw_docs) {
    names(res) <- row.names(t1)
    return(res)
  }
  
  return(mean(res))
}

score.models <- function(dfs, raw_docs=FALSE, raw_tops=FALSE, stability_test=FALSE) {
  cat("Scoring models...\n")

  t1 <- dfs[['t1']]

  if (stability_test) {
    t2 <- dfs[['t3']]
  } else {
    t2 <- dfs[['t2']]
  }

  dfs[['mad']] <- mad(t1, t2, raw_docs, raw_tops)
  dfs[['mse']] <- mse(t1, t2, raw_docs, raw_tops)
  dfs[['roc']] <- roc(t1, t2, raw_docs)

  return(dfs)
}

rank.worst.docs <- function(dfs) {
  cat("Ranking documents...\n")

  dat <- data.frame("mad_doc" = names(sort(dfs[["mad"]], decreasing=TRUE)),
                    "mad"     = sort(dfs[["mad"]], decreasing=TRUE),
                    "mse_doc" = names(sort(dfs[["mse"]], decreasing=TRUE)),
                    "mse"     = sort(dfs[["mse"]], decreasing=TRUE),
                    "roc_doc" = names(sort(dfs[["roc"]], decreasing=FALSE)),
                    "roc"     = sort(dfs[["roc"]], decreasing=FALSE))

  row.names(dat) <- c()
  dfs[['worst_docs']] <- dat
  return(dfs)
}

print.worst.docs <- function(dfs, n_docs) {
  cat(sprintf("Printing top %d worst documents by error metric:\n", n_docs))

  dat <- dfs[["worst_docs"]]

  cat("Rank \t Document \t\t MAD \t\t Document \t\t MSE \t\t Document \t\t ROC\n")
  for (i in 1:n_docs) {
    cat(sprintf("%d \t %15s \t %.4f \t %15s \t %.4f \t %15s \t %.4f \n", i,
                substr(dat$mad_doc[i], start=1, stop=15), dat$mad[i],
                substr(dat$mse_doc[i], start=1, stop=15), dat$mse[i], 
                substr(dat$roc_doc[i], start=1, stop=15), dat$roc[i]))
  }
  cat("\n\n")

  return(dfs)
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
    filename <- paste(source_dir, gsub(" ", "_", query), sep="")
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















