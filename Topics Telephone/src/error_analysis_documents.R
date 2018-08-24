
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
    score.models(raw_docs=TRUE) #%>%
    # rank.worst.docs() %>%
    # print.worst.docs(n_docs) %>%
    # print.assignments(n_docs, source_dir)
  
  
}

human1 <- c("EuroWordNet.txt", "EXtended WordNet.txt", "IndoWordNet.txt", "WordNet.txt")

library(ggplot2)

t1 <- dfs[["t1"]]
t2 <- dfs[["t2"]]
queries <- c("Darknet market.txt")
docs <- vector("character", length(queries))
for (i in 1:length(queries)) {
  docs[i] <- match(queries[i], row.names(t1))
}

topic <- match("Internet", names(t1))
dat <- data.frame("original"=t1[,topic], "reconstruction"=t2[,topic])
ggplot(dat, aes(x=original, y=reconstruction)) + geom_point() +
  geom_point(data = dat[docs,], aes(x=original, y=reconstruction), color="red")

# dat <- data.frame(cbind("mad"=top_mad, "roc"=top_res))
# ggplot(dat, aes(x=mad, y=roc)) + geom_hex()



graph.worst <- function(dfs, n_docs) {
  t1 <- dfs[["t1"]]
  t2 <- dfs[["t2"]]
  ranked_docs <- dfs[["worst_docs"]]
  
  queries <- vector("character", n_docs)
  j <- 1
  for (i in 1:dim(t1)[1]) {
    docs <- ranked_docs[i,]
    for (cat in c("mad_doc", "mse_doc", "roc_doc")) {
      doc <- as.character(unlist(unname(docs[cat])))
      if (!(doc %in% queries) && j < n_docs+1) {
        queries[j] <- doc
        j <- j + 1
      }
    }
    if (j > n_docs + 1) {break}
  }
  
  dat <- matrix(-1, nrow = length(queries), ncol=2)
  for (i in 1:length(queries)) {
    idx <- match(queries[i], row.names(t1))
    idy <- unname(which.max(t1[idx,]))
    
    dat[i,] <- c(t1[idx, idy], t2[idx, idy])
  }
  dat <- as.data.frame(dat); names(dat) <- c("Original", "Reconstructed")
  
  g <- ggplot(dat, aes(x=Original, y=Reconstructed)) + geom_point() + xlim(0,1) + ylim(0,1)
  g
  
  ### TEMP
  green  <- c("Ivy Bridge (microarchitecture).txt", "Tijuana.txt", "FC Akademiya Tolyatti.txt",
              "Health Information Technology for Economic and Clinical Health Act.txt",
              "Pat Green.txt", "Data erasure.txt", "San Jose, California.txt")
  yellow <- c("Seatrain Lines.txt", "Tianhe-1.txt", "Recycling in Japan.txt", "GlobalEnglish.txt",
              "Certified Information Systems Security Professional.txt", "Zettabox.txt",
              "Information Systems Professional.txt", "AdMob.txt", "Internet in Australia.txt",
              "Regulation on Wholesale Energy Market Integrity and Transparency.txt",
              "Automated Border Control systems.txt", "Waste management in Switzerland.txt",
              "Alpha Data.txt", "Delta Force: Urban Warfare.txt")
  red    <- c("Darknet market.txt", "Alan Turing (sculpture).txt", "Alan Turing Memorial.txt",
              "Recovery of an MMO Junkie.txt", "Address Verification System.txt")
  orange <- c("HECToR.txt", "GPU (disambiguation).txt", "Source code.txt", "Odyssey.txt",
              "Jonestown.txt", "Recovery.txt", "National Information Technology Agency.txt",
              "Bra size.txt", "Codebreaker (film).txt")
  blue   <- c("EuroWordNet.txt", "EXtended WordNet.txt", "IndoWordNet.txt", "WordNet.txt",
              "Warrant officer (United Kingdom).txt", "News International phone hacking scandal.txt", 
              "Byte order mark.txt", "50 Most Influential (Bloomberg Markets ranking).txt",
              "Homeostasis.txt", "Quorum (distributed computing).txt", "Kabbalah.txt",
              "Talmud.txt", "Merlin (2008 TV series).txt", "BabelNet.txt", "Deadlock.txt",
              "EPU.txt", "Variable-width encoding.txt", "Escape sequences in C.txt",
              "Eventual consistency.txt", "Arduin Glaber.txt", "MacBook family.txt")
  purple <- c("La Academia 6: Última Generación.txt", "Priority ceiling protocol.txt", 
              "Charlotte Brontë.txt", "M4 Sherman variants.txt", "Priority inheritance.txt",
              "Carrier frequency.txt", "Type 03 Chū-SAM.txt", "Radeon HD 7000 Series.txt",
              "Frigate.txt", "MIM-104 Patriot.txt", "Laurence Olivier.txt", "AAM-4.txt",
              "AMD Radeon Rx 200 series.txt", "Hector Monsegur.txt")
  color_codes <- list(green, yellow, red, orange, blue, purple)
  
  # Create subsets for graphing
  code_dists <- vector("list", length(color_codes))
  for (i in 1:length(color_codes)) {
    code <- color_codes[[i]]
    
    dat_dist <- matrix(-1, nrow = length(code), ncol = 2)
    for (j in 1:length(code)) {
      idx <- match(code[j], row.names(t1))
      idy <- unname(which.max(t1[idx,]))
      dat_dist[j,] <- c(t1[idx, idy], t2[idx, idy])
    }
    dat_dist <- as.data.frame(dat_dist); names(dat_dist) <- c("original", "reconstructed")
    
    code_dists[[i]] <- dat_dist
  }
  
  # Graph subsets
  colors <- c("green", "yellow", "red", "turquoise1", "blue", "purple")
  g <- ggplot(dat, aes(x=Original, y=Reconstructed)) + geom_point() + xlim(0,1) + ylim(0,1)
  for (i in 1:length(code_dists)) {
    g <- g + geom_point(data = code_dists[[i]], aes(x=original, y=reconstructed), color = colors[i], size=3)
  }
  g + geom_abline(intercept = 0, slope = 1, linetype="dashed")
  ### END TEMP
  
  return(g)
}
