




plot_partial_effects <- function(rf=rf_mapcoordinate_clean_missing,
                                 outcome="mapcoordinate_clean_missing",
                                 var="document_district_clean",
                                 minsize=100,
                                 train=pred_cords$x_all_pre_dummy,
                                 histogram=F,
                                 scale=4) {
  
  #p_load(Hmisc)
  sentence_case <- function(x) Hmisc::capitalize(tolower(gsub("_"," ",x)))
  
  
  #Dummies packagr broke
  #options(na.action='na.pass')
  #x_all <-  model.matrix(~ . - 1, pred_cords$x_all_pre_dummy)
  #options(na.action='na.omit')
  
  x_all <- dummies::dummy.data.frame(pred_cords$x_all_pre_dummy,
              dummy.classes=c('character','factor','ordered'))
  dtrain <- xgboost::xgb.DMatrix(data=as.matrix( x_all ),  missing = NA )
  #hist(predict(rf, dtrain)) #ok very different results
  
  uniquevalues <- table(train[,var])
  uniquevalues <- names(uniquevalues[uniquevalues>100])
  
  
  predictions_list <- list()
  for(q in uniquevalues){
    print(q)
    
    #options(na.action='na.pass')
    #testdata_dummy <-  model.matrix(~ . - 1, train)
    #options(na.action='na.omit')
    
    testdata_dummy <- dummies::dummy.data.frame(train,
      dummy.classes=c('character','factor','ordered'))
    #dtest <- xgb.DMatrix(data=as.matrix( testdata_dummy ),  missing = NA )
    #hist(  predict(rf, dtest )  )
    
    #testdata[,outcome] <- NULL
    if(is.character(train[,var]) | is.factor(train[,var])){
      
      testdata_dummy[,grepl( var, names(testdata_dummy) )] <- 0
      testdata_dummy[,grepl( q, names(testdata_dummy) )] <- 1
      
    } else {
      testdata_dummy[,var ] <- as.numeric(q)
    }
    
    
    #dtest <- xgb.DMatrix(data=as.matrix( testdata_dummy ), missing = NA )
    #hist(predict(rf, dtest)) #ok very different results
    
    
    dtest <- xgboost::xgb.DMatrix(data=as.matrix( testdata_dummy ),  missing = NA )
    predictions_list[[q]] <- data.frame( predict(rf, dtest ) )
    predictions_list[[q]]$xvar <- q
    predictions_list[[q]]$yvar <- outcome
  }
  predictions <- rbindlist(predictions_list)
  
  #boxplot(TRUE.~xvar, predictions) #I thought I understood how this works but I clearly don't.
  temp <- aggregate(predictions, by=list(predictions$xvar), FUN=median)
  
  predictions$xvar_ordered <- factor(sentence_case(predictions$xvar),
                                     levels=  sentence_case(temp$xvar[order(temp$predict.rf..dtest.)])
  )
 
  if(histogram){
    p <- predictions %>% 
            ggplot(  aes(x=xvar_ordered,
                                 y=predict.rf..dtest.
                                 )
                ) +  geom_boxplot(notch=T) + 
                     coord_flip()  + 
                     theme_bw() +
      theme(axis.text=element_text(size=8),
            plot.margin = unit(c(0,0,0,0), "lines")) +
            xlab('') + ylab('')
    
  } else {
    p <- ggplot(predictions, aes(x=predict.rf..dtest. ,y=xvar)) + 
      #geom_boxplot(notch=T) + 
      geom_density_ridges(scale = scale) + theme_ridges() +
      #coord_flip()  + 
      #theme_bw() +
      theme(axis.text=element_text(size=8), plot.margin = unit(c(0,0,0,0), "lines")) + xlab('') + ylab('')
  }
  
  
  return(p)
}