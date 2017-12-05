



library(xgboost)
train_an_xb <- function(xy_train,
                        xy_test,
                        vars_x,
                        param,
                        #savefile="/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar/inst/extdata/toponym_xb_model.bin",
                        use_weights=T,
                        missing=NA,
                        weight=NULL,
                        extract_features=F,
                        early_stopping_rounds=10,
                        nrounds=200
){
  
  #https://github.com/dmlc/xgboost/blob/master/demo/kaggle-higgs/higgs-train.R
  
  #install.packages("drat", repos="https://cran.rstudio.com")
  #drat:::addRepo("dmlc")
  #install.packages("xgboost", repos="http://dmlc.ml/drat/", type = "source")
  #install.packages("xgboost")
  
  label=xy_train$rex_match
  weight <- xy_train$weights # as.numeric(dtrain[[32]]) * testsize / length(label)
  
  if(use_weights){
    dtrain <- xgb.DMatrix(data=as.matrix( as.data.frame(xy_train)[,vars_x] ), label = as.numeric(as.data.frame(xy_train)$rex_match),
                          weight = xy_train$weights, missing = missing )
    dtest <- xgb.DMatrix(data= as.matrix(as.data.frame(xy_test)[,vars_x] ),
                         label = as.numeric(as.data.frame(xy_test)$rex_match),
                         weight = xy_test$weights,missing = missing )
  } else {
    dtrain <- xgb.DMatrix(data=as.matrix( as.data.frame(xy_train)[,vars_x] ), label = as.numeric(as.data.frame(xy_train)$rex_match),
                          missing = missing )
    dtest <- xgb.DMatrix(data= as.matrix(as.data.frame(xy_test)[,vars_x] ),
                         label = as.numeric(as.data.frame(xy_test)$rex_match),
                         missing = missing )
  }
  
  base_score = sum(xy_train$rex_match) / nrow(xy_train)
  watchlist <- list("dtrain" = dtrain, "dtest"=dtest)
  
  xb <- xgb.train(params=param,
                  data=dtrain,
                  nrounds = nrounds,
                  watchlist=watchlist,
                  early_stopping_rounds=early_stopping_rounds, 
                  maximize=T
                  #feval = area_under_pr_curve_metric
                  #base_score = base_score, #the initial prediction score of all instances, global bias
                  #weight=weight
  )
  
  return(xb)
  
  # if(extract_features){
  #   new.features.train <- xgb.create.features(model = xb, as.matrix( as.data.frame(xy_train)[,vars_x]))
  #   new.features.test <- xgb.create.features(model = xb, as.matrix( as.data.frame(xy_test)[,vars_x]))
  #   
  #   new.dtrain <- xgb.DMatrix(data = new.features.train, label = xy_train$rex_match, missing=missing)
  #   new.dtest <- xgb.DMatrix(data = new.features.test, label = xy_test$rex_match, missing=missing)
  #   watchlist <- list(train = new.dtrain, test=new.dtest)
  # 
  #   xb2 <- xgb.train(params=param,
  #                 data=new.dtrain,
  #                 nrounds = nrounds,
  #                 watchlist=watchlist,
  #                 early_stopping_rounds=early_stopping_rounds, 
  #                 maximize=T
  #                 #feval = area_under_pr_curve_metric
  #                 #base_score = base_score, #the initial prediction score of all instances, global bias
  #                 #weight=weight
  #                 )
  #   
  #   xgb.save(xb2, savefile)
  #   
  #   return(xb2)
  
  # } else {
  #   
  #   return(xb)
  #   
  # }
  
}

