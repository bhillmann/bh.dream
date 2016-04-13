library(ROCR)

#' Run Leave One Out Cross-Validation
#'
#' @param target_pred the binary predictions
#' @param target_class the labeled data
#'
#' @export
#'
bh_plot_auc = function(target_pred, target_class) {
  # calculating the values for ROC curve
  pred <- prediction(target_pred, target_class)
  perf <- performance(pred,"tpr","fpr")
  # changing params for the ROC plot - width, etc
  par(mar=c(5,5,2,2),xaxs = "i",yaxs = "i",cex.axis=1.3,cex.lab=1.4)
  # plotting the ROC curve
  plot(perf,col="black",lty=3, lwd=3)
  abline(0,1)
  # calculating AUC
  auc <- performance(pred,"auc")
  results = list()
  # now converting S4 class to vector
  results$auc = unlist(slot(auc, "y.values"))
  results$tp = unlist(slot(pred, "tp"))
  results$fp = unlist(slot(pred, "fp"))
  results$tn = unlist(slot(pred, "tn"))
  results$fn = unlist(slot(pred, "fn"))
  results$cutoffs = unlist(slot(pred, "cutoffs"))
  results
}
