library(foreach)

#' Run KNN
#'
#' @param train matrix or data frame of training set cases
#' @param test matrix or data frame of test set cases
#' @param labels multiclass factor of true classifcations
#' @param pred the class to predict
#' @param k number of neighbours considered
#'
#' @export
#'
bh_knn = function (train, test, labels, k=1, dist=function(x,y) {abs(cor(x,y))}) {
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

  results = foreach(i=1:nte, .combine = 'c') %do% {
    pairwise.dists = apply(train, 1, function(a) {dist(a, test[i,])})
    sum(labels[order(pairwise.dists, decreasing=T)[1:k]] == 1)/k
  }
  results
}
