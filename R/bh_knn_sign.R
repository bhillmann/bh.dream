library(foreach)

#' Run KNN
#'
#' @param train matrix or data frame of training set cases
#' @param test matrix or data frame of test set cases
#' @param labels multiclass factor of true classifcations
#'
#' @export
#'
bh_knn_sign = function (train, test, labels, k=1) {
  train <- as.matrix(train)

  if (is.null(dim(test)))
    dim(test) <- c(1, length(test))

  test <- as.matrix(test)

  if (any(is.na(train)) || any(is.na(test)) || any(is.na(labels)))
    stop("no missing values are allowed")

  p <- ncol(train)
  ntr <- nrow(train)
  if (length(labels) != ntr)
    stop("'train' and 'labels' have different lengths")
  if (ntr < k) {
    warning(gettextf("k = %d exceeds number %d of patterns",
                     k, ntr), domain = NA)
    k <- ntr
  }

  if (k < 1)
    stop(gettextf("k = %d must be at least 1", k), domain = NA)

  nte <- nrow(test)
  if (ncol(test) != p)
    stop("dims of 'test' and 'train' differ")

  results = rep(0, nte)
  for(i in 1:nte) {
    pairwise.dists = apply(train, 1, function(a) {pcc(a, test[i,])})
    k.order = order(pairwise.dists, decreasing=T)[1:k]
    k.sign = sapply(labels[k.order], sign)
    results[i] = sum(k.sign * pairwise.dists[k.order])
  }
  results
}

sign = function(x) {
  if(x == 0) {
     return(-1)
     }
  return(1)
}

pcc = function(x,y) {
  abs(cor(x,y))
}
