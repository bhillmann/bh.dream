#' Run Leave One Out Cross-Validation
#'
#' @param classifer the classifier function
#' @param train matrix or data frame of training set cases
#' @param labels multiclass factor of true classifcations
#'
#' @export
#'
bh_loocv = function(classifier, train, labels) {
  results = rep(NA, nrow(train))
  for (i in 1:nrow(train)) {
    results[i] = classifier(train[-i,], train[i,], labels[-i])
  }
  results
}

