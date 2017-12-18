
toponym_training_dataset_load <- function(
                                          drop_zero_dist=F,
                                          drop_identical=T,
                                          inverted_diads=T
                                          ){
  
  #Load the hand labels
  handlabeled <- fread(
    system.file("extdata", "event_flatfile_matches_for_hand_labeling - event_flatfile_matches_for_hand_labeling.csv",
                package = "MeasuringLandscape"),
    data.table=T) 

  #Drop any diad missing a label on either side  
  handlabeled <- subset(handlabeled, name_cleaner_a!="" & name_cleaner_b!="")
  
  print("Hand labels original")
  print(dim(handlabeled))
  print(table(handlabeled$rex_match))

  #Create some features
  handlabeled$extranegative <- F ;  dim(handlabeled)
  handlabeled$exact_match <- handlabeled$name_cleaner_a== handlabeled$name_cleaner_b ;  dim(handlabeled)
  
  
  if(drop_identical){
    handlabeled <- subset(handlabeled, !exact_match)
    print("Drop identical labels")
    print(dim(handlabeled))
    print(table(handlabeled$rex_match))
  }
  
  if(drop_zero_dist){
    print("Drop labels with low similarity")
    handlabeled[,temp_q_cos:= stringsim(name_cleaner_a,name_cleaner_b,"cos", nthread= parallel::detectCores(),q=2),]
    handlabeled <- handlabeled[temp_q_cos>.3] ; dim(handlabeled)
    
    #handlabeled[,temp_q_gram_2:= stringsim(name_cleaner_a,name_cleaner_b,"qgram", nthread=48,q=2),]
    #handlabeled <- handlabeled[temp_q_gram_2 != 0] ; dim(handlabeled)
  }
  
  if(inverted_diads){
    #Need the model to be indifferent to ordering so add the training obs back in flipped as well
    handlabeled2 <- handlabeled
    handlabeled2$temp <- handlabeled2$name_cleaner_a
    handlabeled2$name_cleaner_a <- handlabeled2$name_cleaner_b
    handlabeled2$name_cleaner_b <- handlabeled2$temp
    handlabeled2$temp <- NULL
    handlabeled <- rbind(handlabeled, handlabeled2) ; dim(handlabeled)
    
    #Remove duplicates with the same exact entries on both sides
    handlabeled <- subset(handlabeled, !duplicated(paste(name_cleaner_a,name_cleaner_b)))
    handlabeled <- subset(handlabeled, !duplicated(paste(name_cleaner_b,name_cleaner_a)))

    print("Invert Diads")
    print(dim(handlabeled))
    +print(table(handlabeled$rex_match))
  }
  
  return(handlabeled)
  
}


