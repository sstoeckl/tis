as.list.keepClass <- function(x, ...){
  res <- vector("list", length(x))
  for (i in seq_along(x)) res[[i]] <- x[i]
  res
}
