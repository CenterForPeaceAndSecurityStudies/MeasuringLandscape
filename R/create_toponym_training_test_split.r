
#This function loads a prelabeled file of toponym-suggestion diads
#It returns a training test split
create_toponym_split_training_test <- function(
                                                handlabeled,
                                                vars_id,
                                                vars_weights,
                                                vars_y,
                                                vars_x,
                                                neg_count=0,
                                                fromscratch=F) {
  

  xy_all <- handlabeled
  
  xy_all$a <- xy_all$name_cleaner_a
  xy_all$b <- xy_all$name_cleaner_b
  
  postfixes=geonames_postfixes()
  print("Stemming A")
  stem_results_a <- strip_postfixes(to_be_striped=xy_all$a , postfixes=postfixes, whitelist="fort hall", verbose=T) 
  print("Stemming B")
  stem_results_b <- strip_postfixes(to_be_striped=xy_all$b , postfixes=postfixes, whitelist="fort hall", verbose=T) 
  stem_ab <- data.table(a=stem_results_a$name_cleaner_stemmed,b=stem_results_b$name_cleaner_stemmed)
  
  xy_all <- toponym_add_features(xy_all) #requires the two columns to be called a and b appends all the columns necessary for toponym matching
  temp <- toponym_add_features(stem_ab) #requires the two columns to be called a and b appends all the columns necessary for toponym matching
  names(temp) <- paste0(names(temp),"_stemmed")
  
  xy_all <- cbind(xy_all, temp)
  xy_all$postfix_has_a <- stem_results_a$name_cleaner_suffix!=""
  xy_all$postfix_has_b <- stem_results_b$name_cleaner_suffix!=""
  
  xy_all$weights <- 1
  xy_all$weights[handlabeled$rex_match==1] <- sum(xy_all$rex_match==0, na.rm=T)/sum(xy_all$rex_match==1, na.rm=T)
  table(xy_all$weights)
  
  dim(xy_all)
  #saveRDS(handlabeled,
  #        file=glue(getwd(), "/../inst/extdata/handlabeled.Rds")

  
  #handlabeled <- readRDS(system.file("extdata", "handlabeled.Rds", package = "MeasuringLandscape"))

  vars_id_y_x_weights <- c(vars_id,vars_y,vars_x,vars_weights)
  
  #Load preselected random sample of stems to serve as test diads
  id_test <- readRDS(system.file("extdata", "id_test.Rds", package = "MeasuringLandscape"))
  length(id_test)
  
  xy_all$test <- F
  xy_all$test[xy_all$a_stemmed %in% id_test | xy_all$b_stemmed %in% id_test] <- T
  table(xy_all$test)
  
  #table(handlabeled[,'q_gram_2']==0, handlabeled$rex_match)
  #handlabeled <- subset(handlabeled, q_gram_2>0)
  
  xy_all <- subset(xy_all, !is.na(rex_match))[,vars_id_y_x_weights,with=F]
  xy_all[is.na(xy_all)] <- NA
  xy_train <- subset(xy_all, !test)
  xy_test <- subset(xy_all, test)
  dim(xy_test)
  dim(xy_train)
  
  return(
    list(handlabeled=handlabeled,
         xy_all=xy_all,
         xy_train=xy_train,
         xy_test=xy_test
    )
  )
  
}
