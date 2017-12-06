
#p_load(PRROC)
area_under_pr_curve_metric <- function(preds,dtrain){
  preds <- 1/(1 + exp(-preds))
  labels <- getinfo(dtrain, "label")
  pr <- pr.curve( preds, labels )
  return(list(metric="PRROC", value=pr$auc.integral))
}


logregobj <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  preds <- 1/(1 + exp(-preds))
  grad <- preds - labels
  hess <- preds * (1 - preds)
  return(list(grad = grad, hess = hess))
} 