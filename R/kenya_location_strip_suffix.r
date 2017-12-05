

kenya_location_strip_suffix_train_classifier_lstm <- function(token_features_vectors, timesteps=5){

  library(keras)
  xy_traintest_A <- kenya_location_strip_suffix_train_trainingsplit(features_local=T , features_upos=T , features_global=T, features_word2vec=T ,
                                                                    timesteps=timesteps, validation_size=.2)

  MAXLEN=5

  weights <- as.data.frame(t(apply(xy_traintest_A[['y_train']], 1, colSums)))
  weights[,1:ncol(weights)] <- lapply(weights, scale)
  #weights <- weights/rowSums(weights)
  d_weights <- dist(weights)
  hc_weights <- hclust(d_weights, method="ward.D2")
  plot(hc_weights, cex=.5)
  cuts <- cutree(hc_weights, 20)
  table(cuts)
  sample_weights <- (1/(table(cuts)/length(cuts)))[cuts]
  sample_weights <- sample_weights/min(sample_weights)
  #sample_weights <- sample_weights/sum(sample_weights)
  BATCH_SIZE <- 128 # 128
  HIDDEN = 128
  reg=   regularizer_l1( 0.001)

  epochs=100 #30 was sufficient for all but the biggest models, but they performed badly so don't need to do anyway
  dropout=0 #drop out significantly hurt my results even on validation

  xy_traintest <- xy_traintest_A

  dimnames(xy_traintest[['x_all']])
  y_features_k <- dim(xy_traintest[['y_all']])[3]
  max_words = max(xy_traintest[['x_all']][,,'global_word_clean_integer']) + 1000 # I don't know why but somehow higher number integers appear so I give it a buffer

  #Model with just words themselves nothing else
  model <- keras_model_sequential()
  model %>%
    layer_embedding(input_dim = max_words , output_dim = 30, input_length = 5, mask_zero=T) %>%
    layer_dropout(rate = 0.2) %>%
    bidirectional(layer_lstm(units = 50, return_sequences = T)) %>%
    layer_dropout(rate = 0.5) %>%
    time_distributed(layer_dense(units = 10, activation = 'softmax'))

  model %>% keras::compile(
      loss = 'binary_crossentropy',
      optimizer = 'rmsprop',
      metrics = c('accuracy')
    )

  model %>% fit(x = xy_traintest[['x_train']][,,"global_word_clean_integer", drop=T] ,
                    y = xy_traintest[['y_train']],
                    batch_size = BATCH_SIZE, epochs = 100 ,
                    validation_data = list(xy_traintest[['x_test']][,,"global_word_clean_integer", drop=T],
                                           xy_traintest[['y_test']] ),
                    sample_weight=sample_weights
  )


  x_features <- setdiff( dimnames(xy_traintest[['x_train']])[[3]] , "global_word_clean_integer")
  x_features_k <- length(x_features)

  # inputs <- layer_input(shape = c( 5 , 1 ), dtype = 'int32')
  l0 <- layer_input(shape = c(5), dtype = 'int32', name = 'main_input')
  l1 <- l0 %>% layer_embedding(input_dim = max_words , output_dim = 30, input_length = 5, mask_zero=T)

  l0b <- layer_input(shape = c(5,x_features_k),  name = 'main_input2')
  l1b <- l0b %>% layer_masking(mask_value = 0.0 , input_shape= c(5,x_features_k) )
  l2 <- layer_concatenate(c(l1, l1b))

  l3 <- l2 %>% layer_dropout(rate = 0.2)
  l4 <- l3 %>% bidirectional(layer_lstm(units = 100, return_sequences = T, kernel_regularizer=reg))
  l5 <- l4 %>% layer_dropout(rate = 0.5)
  l6 <- l5 %>% time_distributed(layer_dense(units = 10, activation = 'softmax', kernel_regularizer=reg))

  vqa_model <- keras_model(inputs = list(l0,l0b), outputs = l6)

  vqa_model %>%
    keras::compile(
      loss = 'binary_crossentropy',
      optimizer = 'rmsprop',
      metrics = c('accuracy')
    )

  vqa_model %>% fit(x = list( xy_traintest[['x_train']][,,"global_word_clean_integer", drop=T] ,
                              xy_traintest[['x_train']][,,x_features, drop=F]),
                    y = xy_traintest[['y_train']],
                    batch_size = BATCH_SIZE,
                    epochs = 20 , #past 20 over fits
                    validation_data = list(list( xy_traintest[['x_test']][,,"global_word_clean_integer", drop=T] ,
                                                 xy_traintest[['x_test']][,,x_features, drop=F] ),
                                           xy_traintest[['y_test']] )  ,

                    sample_weight=sample_weights
  )


  winning_model = vqa_model

  #sudo pip install h5py
  save_model_hdf5(winning_model,
                  filepath='/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/Violent Events/JPR_Paper_RandR/code/MeasuringLandscapeCivilWar/inst/extdata/strip_suffix_keras_model.hdf5',
                  overwrite = TRUE,include_optimizer = TRUE)

  return(model)
}

#location_names<- unique(flatfiles$name_clean)

#location_names<- dimnames(xy_traintest[['x_train']])[[1]]


kenya_location_strip_suffix_predict <- function(location_names,timesteps=5) {

  y_features <- c("label_posesive","label_corporate","label_person","label_location","label_type","label_postfix_location",
   "label_prefix","label_null","label_postfix_numeric" , "label_garbage"  )
  location_names=dimnames(xy_traintest[['x_train']])[[1]]
  location_names <- unique(flatfiles$name_clean)
  library(keras)

  #model <- load_model_hdf5(filepath='/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/Violent Events/JPR_Paper_RandR/code/MeasuringLandscapeCivilWar/inst/extdata/strip_suffix_keras_model.hdf5')
  model <- model

  #xy_traintest_A <- kenya_location_strip_suffix_train_trainingsplit(features_local=T , features_upos=T , features_global=T, features_word2vec=F ,
  #                                                                  timesteps=timesteps, validation_size=.2)

  x_predict <- kenya_location_strip_suffix_train_trainingsplit_yx(features_local=T , features_upos=T , features_global=T, features_word2vec=T,
                                                                  location_names=location_names)
  x_predict_flat <- kenya_location_strip_suffix_train_trainingsplit_yx(features_local=T , features_upos=T , features_global=T, features_word2vec=T,
                                                                       array3d=F, location_names=location_names)
  setkeyv(x_predict_flat,'location_name_clean_word_number')

  dim(x_predict[[2]])
  dim(x_predict_flat)

  y_features_k <- dim(model$output)[[3]]
  dimnames(x_predict[['x_all_padded_array']])[[1]]

  x_features <- setdiff( dimnames(x_predict$x_all_padded_array)[[3]] , "global_word_clean_integer")
  x_features_k <- length(x_features)

  #predictions <- predict(model, x_predict[["x_all_padded_array"]])
  #predictions <- predict(model, x_predict$x_all_padded_array[,,"global_word_clean_integer", drop=T])
  predictions <- predict(winning_model,
                         list(x_predict$x_all_padded_array[,,"global_word_clean_integer", drop=T],
                              x_predict$x_all_padded_array[,,x_features, drop=F])
  )


  predictions_dt <- rbindlist( lapply(1:dim(predictions)[[1]], FUN=function(i) data.frame(round(predictions[i,,],2)) ))
  #don't trust this right now
  #dim(predictions)<- c( dim(predictions)[[1]]*timesteps , dim(predictions)[[3]] ) #Flatten https://stackoverflow.com/questions/4022195/transform-a-3d-array-into-a-matrix-in-r
  names(predictions_dt) <- y_features
  predictions_dt$prediction <- y_features[max.col(predictions_dt)]
  #predictions_dt$location_name_clean <- sort(rep(dimnames(x_predict[['x_all_padded_array']])[[1]], timesteps))
  #rownames(predictions_dt) <- paste0(predictions_dt$location_name_clean, "_", 1:timesteps)
  predictions_dt$location_name_clean_word_number <- x_predict_flat$location_name_clean_word_number
  predictions_dt$location_name_clean <- gsub( "_[0-9]$","", x_predict_flat$location_name_clean_word_number )

  predictions_dt$word <- x_predict_flat$word

  predictions_dt <- subset(predictions_dt, !is.na(word))


  predictions_readable <- predictions_dt[, list(
        paste_posesive=paste(word[prediction %in% "label_posesive"], collapse=" "),
        paste_corporate=paste(word[prediction %in% "label_corporate"], collapse=" "),
        paste_person=paste(word[prediction %in%  "label_person"], collapse=" "),
        paste_location=paste(word[prediction %in%  "label_location"], collapse=" "),
        paste_type=paste(word[prediction %in%  "label_type"], collapse=" "),
        paste_postfix_location=paste(word[prediction %in%  "label_postfix_location"], collapse=" "),
        paste_prefix=paste(word[prediction %in%  "label_prefix"], collapse=" "),
        paste_null=paste(word[prediction %in%  "label_null"], collapse=" "),
        paste_postfix_numeric=paste(word[prediction %in%  "label_postfix_numeric"], collapse=" "),
        paste_garbage=paste(word[prediction %in%  "label_garbage"], collapse=" ") #,
        #paste_start=paste(word[prediction %in%  "label_start"], collapse=" "),
        #paste_end=paste(word[prediction %in%  "label_end"], collapse=" ")
  ), by=list(location_name_clean)] ; dim(predictions_readable)

  cols=names(predictions_readable)
  predictions_readable[ , (cols) := lapply(.SD, trimws), .SDcols = cols]

  return(predictions_readable)
}




kenya_location_strip_suffix_train_classifier_rf <- function(token_features_vectors){


  inpath='/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/Violent Events/JPR_Paper_RandR/code/MeasuringLandscapeCivilWar/inst/extdata/tokens_hand_labeled.csv'

  library(data.table)
  tokens_hand_labeled <- fread(inpath)

  y_features <- grep("label_",names(tokens_hand_labeled), value=T)
  y_features_k <- length(y_features)

  tokens_hand_labeled[, location_name_clean := tolower(location_name),]
  tokens_hand_labeled[, location_name_length := length(location_name_clean_word_number), by=location_name]
  tokens_hand_labeled$labeled <- rowSums(tokens_hand_labeled[,grepl("label_",names(tokens_hand_labeled)), with=F], na.rm=T)
  tokens_hand_labeled[, labeled_count := sum(labeled), by=location_name]

  handlabels <- subset(tokens_hand_labeled, labeled_count==location_name_length) #Limit only to names that have been fully labeled by hand

  features <- kenya_location_strip_suffix_generate_features(unique(handlabels$location_name_clean))
  setdiff(handlabels$location_name_clean_word_number,   features$location_name_clean_word_number)

  namesword2vec <- c('word2vec_has', paste0("word2vec_dim",1:300))
  x_features <- c('location_name_clean_word_number','word_number','count', 'word_number_max_average', 'position_mean' ,
                  'position_max' ,'position_fromend_mean','word_nchar',namesword2vec)
  data_all <- merge(handlabels, features[,eval(quote(x_features)),with=F], by.x="location_name_clean_word_number",
                    by.y="location_name_clean_word_number", all.x=T, all.y=F)
  setdiff(handlabels$location_name_clean_word_number, tolower(features$location_name_word))
  x_train <- data_all[,eval(quote(c('word_number','count', 'word_number_max_average', 'position_mean' ,
                                    'position_max' ,'position_fromend_mean','word_nchar',namesword2vec))),with=F]
  y_train <- data_all[,eval(quote(y_features)),with=F]
  y_train[is.na(y_train)] <-0
  all_train <- cbind(x_train,y= as.factor(names(y_train)[ max.col(y_train) ]))

  p_load(randomForestSRC)
  rf <- rfsrc(y~.,data=all_train)

  data_predictions <- kenya_location_strip_suffix_generate_features(location_names)
  predictions <- predict(rf, newdata=data_predictions[,
                                                      eval(quote(c('word_number','count', 'word_number_max_average', 'position_mean' ,
                                                                   'position_max' ,'position_fromend_mean','word_nchar',namesword2vec))),with=F])

  #predictions$class
  #Ok when I come back , look at how we did here

  data_predictions$prediction <- predictions$class

  predictions_readable <- data_predictions[, list(
    paste_posesive=paste(word[prediction %in% "label_posesive"], collapse=" "),
    paste_corporate=paste(word[prediction %in% "label_corporate"], collapse=" "),
    paste_person=paste(word[prediction %in%  "label_person"], collapse=" "),
    paste_location=paste(word[prediction %in%  "label_location"], collapse=" "),
    paste_type=paste(word[prediction %in%  "label_type"], collapse=" "),
    paste_postfix_location=paste(word[prediction %in%  "label_postfix_location"], collapse=" "),
    paste_prefix=paste(word[prediction %in%  "label_prefix"], collapse=" "),
    paste_null=paste(word[prediction %in%  "label_null"], collapse=" "),
    paste_postfix_numeric=paste(word[prediction %in%  "label_postfix_numeric"], collapse=" "),
    paste_garbage=paste(word[prediction %in%  "label_garbage"], collapse=" ")
  ), by=list(location_name_clean)] ; dim(predictions_readable)
  cols=names(predictions_readable)
  predictions_readable[ , (cols) := lapply(.SD, trimws), .SDcols = cols]


}


trash <- function() {

  #Model A Basic Features Only
  xy_traintest <- xy_traintest_A
  x_features_k=dim(xy_traintest[['x_train']])[3]; y_features_k=dim(xy_traintest[['y_train']])[3]; MAXLEN <- dim(xy_traintest[['x_train']])[2]
  modelA <- keras_model_sequential()  %>%
    layer_masking(mask_value = 0.0 , input_shape=c(MAXLEN, x_features_k))  %>% layer_dropout(dropout)  %>%
    bidirectional(layer_lstm(units=HIDDEN , kernel_regularizer=reg , return_sequences = TRUE , dropout= dropout )) %>%
    time_distributed(layer_dense(units = y_features_k, activation="softmax")) %>%
    keras::compile(loss = "categorical_crossentropy", optimizer = "adam", metrics = "accuracy")
  histA <- modelA %>% fit(x = xy_traintest[['x_train']], y = xy_traintest[['y_train']], batch_size = BATCH_SIZE, epochs = epochs ,
                          validation_data = list(xy_traintest[['x_test']],xy_traintest[['y_test']] )  , sample_weight=sample_weights)

  #Model B Basic Features + Pos
  xy_traintest <- xy_traintest_B
  x_features_k=dim(xy_traintest[['x_train']])[3]; y_features_k=dim(xy_traintest[['y_train']])[3]; MAXLEN <- dim(xy_traintest[['x_train']])[2]
  modelB <- keras_model_sequential()  %>%
    layer_masking(mask_value = 0.0 , input_shape=c(MAXLEN, x_features_k))  %>% layer_dropout(dropout)  %>%
    bidirectional(layer_lstm(units=HIDDEN , kernel_regularizer=reg , return_sequences = TRUE , dropout= dropout )) %>%
    time_distributed(layer_dense(units = y_features_k, activation="softmax")) %>%
    keras::compile(loss = "categorical_crossentropy", optimizer = "adam", metrics = "accuracy")
  histB <- modelB %>% fit(x = xy_traintest[['x_train']], y = xy_traintest[['y_train']], batch_size = BATCH_SIZE, epochs = epochs ,
                          validation_data = list(xy_traintest[['x_test']],xy_traintest[['y_test']] )   , sample_weight=sample_weights)

  #Model C Basic Features + Pos
  xy_traintest <- xy_traintest_C
  x_features_k=dim(xy_traintest[['x_train']])[3]; y_features_k=dim(xy_traintest[['y_train']])[3]; MAXLEN <- dim(xy_traintest[['x_train']])[2]
  modelC <- keras_model_sequential()  %>%
    layer_masking(mask_value = 0.0 , input_shape=c(MAXLEN, x_features_k))  %>% layer_dropout(dropout)  %>%
    bidirectional(layer_lstm(units=HIDDEN , kernel_regularizer=reg , return_sequences = TRUE , dropout= dropout )) %>%
    time_distributed(layer_dense(units = y_features_k, activation="softmax")) %>%
    keras::compile(loss = "categorical_crossentropy", optimizer = "adam", metrics = "accuracy")
  histC <- modelC %>% fit(x = xy_traintest[['x_train']], y = xy_traintest[['y_train']], batch_size = BATCH_SIZE, epochs = epochs ,
                          validation_data = list(xy_traintest[['x_test']],xy_traintest[['y_test']] )   , sample_weight=sample_weights)

  #Model D Basic Features + Pos
  xy_traintest <- xy_traintest_C
  x_features_k=dim(xy_traintest[['x_train']])[3]; y_features_k=dim(xy_traintest[['y_train']])[3]; MAXLEN <- dim(xy_traintest[['x_train']])[2]
  modelD <- keras_model_sequential()  %>%
    layer_masking(mask_value = 0.0 , input_shape=c(MAXLEN, x_features_k))  %>% layer_dropout(dropout)  %>%
    bidirectional(layer_lstm(units=HIDDEN , kernel_regularizer=reg , return_sequences = TRUE , dropout= dropout )) %>%
    bidirectional(layer_lstm(units=HIDDEN , kernel_regularizer=reg , return_sequences = TRUE , dropout= dropout )) %>%
    time_distributed(layer_dense(units = y_features_k, activation="softmax")) %>%
    keras::compile(loss = "categorical_crossentropy", optimizer = "adam", metrics = "accuracy")
  histD <- modelD %>% fit(x = xy_traintest[['x_train']], y = xy_traintest[['y_train']], batch_size = BATCH_SIZE, epochs = epochs ,
                          validation_data = list(xy_traintest[['x_test']],xy_traintest[['y_test']] )  , sample_weight=sample_weights )

  #Model E Basic Features + Pos
  xy_traintest <- xy_traintest_C
  x_features_k=dim(xy_traintest[['x_train']])[3]; y_features_k=dim(xy_traintest[['y_train']])[3]; MAXLEN <- dim(xy_traintest[['x_train']])[2]
  modelE <- keras_model_sequential()  %>%
    layer_masking(mask_value = 0.0 , input_shape=c(MAXLEN, x_features_k))  %>% layer_dropout(dropout)  %>%
    bidirectional(layer_lstm(units=HIDDEN , kernel_regularizer=reg , return_sequences = TRUE , dropout= dropout )) %>%
    layer_lstm(units=HIDDEN , kernel_regularizer=reg , return_sequences = TRUE , dropout= dropout ) %>%
    time_distributed(layer_dense(units = y_features_k, activation="softmax")) %>%
    keras::compile(loss = "categorical_crossentropy", optimizer = "adam", metrics = "accuracy")
  histE <- modelE %>% fit(x = xy_traintest[['x_train']], y = xy_traintest[['y_train']], batch_size = BATCH_SIZE, epochs = epochs ,
                          validation_data = list(xy_traintest[['x_test']],xy_traintest[['y_test']] )   , sample_weight=sample_weights)

  #Model F  Shrinking looks better
  xy_traintest <- xy_traintest_C
  x_features_k=dim(xy_traintest[['x_train']])[3]; y_features_k=dim(xy_traintest[['y_train']])[3]; MAXLEN <- dim(xy_traintest[['x_train']])[2]
  modelF <- keras_model_sequential()  %>%
    layer_masking(mask_value = 0.0 , input_shape=c(MAXLEN, x_features_k))  %>% layer_dropout(dropout)  %>%
    bidirectional(layer_lstm(units=HIDDEN , kernel_regularizer=reg , return_sequences = TRUE , dropout= dropout )) %>%
    layer_lstm(units=HIDDEN , kernel_regularizer=reg , return_sequences = TRUE , dropout= .5 ) %>%
    time_distributed(layer_dense(units = y_features_k, activation="softmax")) %>%
    keras::compile(loss = "categorical_crossentropy", optimizer = "adam", metrics = "accuracy")
  histF <- modelF %>% fit(x = xy_traintest[['x_train']], y = xy_traintest[['y_train']], batch_size = BATCH_SIZE, epochs = epochs ,
                          validation_data = list(xy_traintest[['x_test']],xy_traintest[['y_test']] )  , sample_weight=sample_weights )

  #Model G Twice as many units is better than half
  xy_traintest <- xy_traintest_C
  x_features_k=dim(xy_traintest[['x_train']])[3]; y_features_k=dim(xy_traintest[['y_train']])[3]; MAXLEN <- dim(xy_traintest[['x_train']])[2]
  modelG <- keras_model_sequential()  %>%
    layer_masking(mask_value = 0.0 , input_shape=c(MAXLEN, x_features_k))  %>% layer_dropout(dropout)  %>%
    bidirectional(layer_lstm(units=HIDDEN*2 , kernel_regularizer=reg , return_sequences = TRUE , dropout= dropout )) %>%
    layer_lstm(units=HIDDEN*2 , kernel_regularizer=reg , return_sequences = TRUE , dropout= dropout ) %>%
    time_distributed(layer_dense(units = y_features_k, activation="softmax")) %>%
    keras::compile(loss = "categorical_crossentropy", optimizer = "adam", metrics = "accuracy")
  histG <- modelG %>% fit(x = xy_traintest[['x_train']], y = xy_traintest[['y_train']], batch_size = BATCH_SIZE, epochs = epochs ,
                          validation_data = list(xy_traintest[['x_test']],xy_traintest[['y_test']] )   , sample_weight=sample_weights )

  #Model H    #Biderectional is better than just lstm
  xy_traintest <- xy_traintest_C
  x_features_k=dim(xy_traintest[['x_train']])[3]; y_features_k=dim(xy_traintest[['y_train']])[3]; MAXLEN <- dim(xy_traintest[['x_train']])[2]
  modelH <- keras_model_sequential()  %>%
    layer_masking(mask_value = 0.0 , input_shape=c(MAXLEN, x_features_k))  %>% layer_dropout(dropout)  %>%
    layer_lstm(units=HIDDEN*2 , kernel_regularizer=reg , return_sequences = TRUE , dropout= dropout ) %>%
    layer_lstm(units=HIDDEN*2 , kernel_regularizer=reg , return_sequences = TRUE , dropout= dropout ) %>%
    time_distributed(layer_dense(units = y_features_k, activation="softmax")) %>%
    keras::compile(loss = "categorical_crossentropy", optimizer = "adam", metrics = "accuracy")
  histH <- modelH %>% fit(x = xy_traintest[['x_train']], y = xy_traintest[['y_train']], batch_size = BATCH_SIZE, epochs = epochs ,
                          validation_data = list(xy_traintest[['x_test']],xy_traintest[['y_test']] )   , sample_weight=sample_weights )

  #Model I  Shrinking looks better  #3 Layers is worse
  xy_traintest <- xy_traintest_C
  x_features_k=dim(xy_traintest[['x_train']])[3]; y_features_k=dim(xy_traintest[['y_train']])[3]; MAXLEN <- dim(xy_traintest[['x_train']])[2]
  modelI <- keras_model_sequential()  %>%
    layer_masking(mask_value = 0.0 , input_shape=c(MAXLEN, x_features_k))  %>% layer_dropout(dropout)  %>%
    bidirectional(layer_lstm(units=HIDDEN , kernel_regularizer=reg , return_sequences = TRUE , dropout= dropout )) %>%
    layer_lstm(units=HIDDEN , kernel_regularizer=reg , return_sequences = TRUE , dropout= dropout ) %>%
    layer_lstm(units=HIDDEN/2 , kernel_regularizer=reg , return_sequences = TRUE , dropout= dropout ) %>%
    time_distributed(layer_dense(units = y_features_k, activation="softmax")) %>%
    keras::compile(loss = "categorical_crossentropy", optimizer = "adam", metrics = "accuracy")
  histI <- modelI %>% fit(x = xy_traintest[['x_train']], y = xy_traintest[['y_train']], batch_size = BATCH_SIZE, epochs = epochs ,
                          validation_data = list(xy_traintest[['x_test']],xy_traintest[['y_test']] )   , sample_weight=sample_weights )

  histA #
  histB # With more data this performed better
  histC #
  histD #D for the win with val_acc: 0.9426
  histE #
  histF
  histG
  histH #not great
  histI #Not great

}
