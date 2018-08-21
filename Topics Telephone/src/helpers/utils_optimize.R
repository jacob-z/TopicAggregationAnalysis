### utils_optimize.R
### Jacob Zimmer
### Tools for building an optimal reconstrution and testing stability

library(clue)

merge.reconstruction <- function(dfs) {
  cat("Merging matched columns...\n")

  t1 <- dfs[['t1']]; t2 <- dfs[['t2']]
  t1 <- unname(as.matrix(t1)); t2 <- unname(as.matrix(t2))
  m <- dfs[['m']][1:dim(t2)[2]]
  
  t3 <- t(rowsum(t(t2), m))
  
  t3 <- data.frame(t3)
  names(t3) <- names(dfs[['t1']])
  row.names(t3) <- row.names(dfs[['t1']])
  
  dfs[['t3']] <- t3
  return(dfs)
}

create.cost.matrix <- function(dfs, expand=FALSE, stability_test=FALSE) {
  t1 <- dfs[['t1']]
  t2 <- dfs[['t2']]

  N     <- dim(t1)[1]
  k_agg <- dim(t1)[2]
  k_raw <- dim(t2)[2]
  
  cat(paste0("Creating ", k_agg, " by ", k_raw, " cost matrix (MSE)...\n"))
  cost_mat <- matrix(data = 0, nrow = k_agg, ncol = k_raw)
  for(i in 1:k_agg){
    for (j in 1:k_raw) {
      cost_mat[i,j] <- mean(abs(t1[,i] - t2[,j]))
    }
  }
  
  #replicate columns for multiple assignments in matching
  if (expand) {
    cat(paste0("Modifying to ", k_raw, " by ", k_agg*k_raw, " for multiple assignment matching...\n"))
    cost_mat <- t(cost_mat)
    cost_mat <- do.call(cbind, replicate(k_raw, cost_mat, simplify=FALSE))
  }
  if (stability_test) {
    cost_mat <- t(cost_mat)
  }

  dfs[['c']] <- cost_mat
  
  return(dfs)
}

solve.LSAP <- function(dfs) {
  cat("Matching columns...\n")
  
  dfs[['m']] <- solve_LSAP(dfs[['c']])
  return(dfs)
}

subsample.solve <- function(lst, cutoff=1000, prop=1) {
  
  c <- lst[["c"]]
  k_agg <- dim(lst[["t1"]])[2]
  k_raw <- dim(lst[["t2"]])[2]
  size <- k_agg*prop
  
  m <- matrix(data = 0, nrow = k_raw, ncol = k_agg)
  s <- sum(rowSums(m) > cutoff)
  
  i <- 0
  while (s < k_raw) {
    # subsample columns into small cost matrix
    cols <- sample(1:ncol(c), size, replace = FALSE) # [1:12]
    samp <- c[,cols]   #[1:12, 1:12]
    
    # apply solve_LSAP
    sol <- solve_LSAP(samp)[1:k_agg] #[1:12]
    
    # sorts cols and subsets
    row_idx <- cols[sol]
    col_idx <- c(1:k_agg)
    
    # new way...
    m[row_idx + nrow(m)*(col_idx-1)] <- m[row_idx + nrow(m)*(col_idx-1)] + 1
    
    # exit conditions
    s <- sum(rowSums(m) > cutoff)
    if (i %% 10000 == 0) {
      print(sprintf("%7d iterations: %6d done, max: %6d, min: %d", i, s, max(rowSums(m)), min(rowSums(m))))
    }
    i <- i + 1
    
    if (max(rowSums(m)) > 10 * cutoff) {
      print(sprintf("Warning: cutoff not reached, min = %d", min(rowSums(m))))
      break
    }
  }
  print(sprintf("%7d iterations: %3d rows satisfied", i, s))
  
  return(apply(m, 1, which.max))
}

random.search <- function(lst, iter=1000) {
  
  c <- lst[["c"]]
  t1 <- lst[["t1"]]
  t2 <- lst[["t2"]]
  k_agg <- dim(t1)[2]
  k_raw <- dim(t2)[2]
  
  m <- matrix(data = 0, nrow = k_raw, ncol = 0)
  scores <- c()
  best <- 100
  
  i <- 0
  t3 <- t(t2)
  while (i < iter) {
    # randomly assign topics in t2
    assignments <- sample(1:k_agg, k_raw, replace = TRUE)
    
    # condense t2 by topic
    t4 <- rowsum(t3, assignments)
    
    score  <- mad(t1, t(t4))
    if (score < best) {
      best <- score
      m <- assignments
    }
    
    if (i %% 100 == 0) {
      print(sprintf("%5d iterations complete", i))
    }
    i <- i+1
  }
  
  return(m)
}

priority.solve <- function(file1, file2) {
  
  lst <- create_cost_matrix(file1, file2)
  
  while (dim(lst[["t2"]])[2] > 0) {
    
    c <- lst[["c"]]
    t1 <- lst[["t1"]]
    t2 <- lst[["t2"]]
    k_agg <- lst[["k_agg"]]
    k_raw <- lst[["k_raw"]]
    
    # run matching with k_agg (returns in K' sorted order 1:12)
    if (dim(c)[1] > dim(c)[2]) {
      tmp <- solve_LSAP(t(c))[1:dim(c)[2]] #assignments 
      
      # update t1 and break (last iter)
      for (i in 1:k_agg) {
        #if (all(t1[,tmp[i]] - t2[,i] >= 0)) {
          t1[,tmp[i]] <- t1[,tmp[i]] - t2[,i]
        # } else {
        #   print("BUG")
        #   tmp[-i]
        # }
      }
      t1 <- abs(t1)
      break
    } else {
      tmp <- solve_LSAP(c)[1:k_agg]
    }
    
    # adjust k_agg topics in t1 to subtract matched t2 cols
    # for column in t2[,tmp] subtract from corresponding col in t1
    for (i in 1:k_agg) {
      if (all(t1[,i] - t2[,tmp[i]] >= 0)) {
        t1[,i] <- t1[,i] - t2[,tmp[i]]
      } else {
        tmp[-i]
      }
    }
    t2 <- t2[,-tmp]
    
    # rerun create new cost matrix from t1 and t2
    N <- dim(t1)[1]
    k_raw <- k_raw - length(tmp)
    
    if (k_raw == 0) {break}
    print(paste0("Creating ", k_agg, " by ", k_raw, " cost matrix (MSE)..."))
    cost_mat <- matrix(data = 0, nrow = k_agg, ncol = k_raw)
    for(i in 1:k_agg){
      for (j in 1:k_raw) {
        cost_mat[i,j] <- mean((t1[,i] - t2[,j])^2)
      }
    }
    
    lst = list("c"=cost_mat, "t1"=t1, "t2"=t2, "N"=N, "k_agg"=k_agg, "k_raw"=k_raw)
  }
  
  m_a_d <- mean(apply(   t1, 2, mean))
  m_s_e <- mean(apply(t1*t1, 2, mean))
  
  return(list("mad"=m_a_d, "mse"=m_s_e))
}

