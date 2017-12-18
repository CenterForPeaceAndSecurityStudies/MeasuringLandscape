library(pacman)

kenya_location_strip_suffix_generate_annotations <- function(location_names, save=F){


  #download_core_nlp(  output_loc=path,
  #                    force = T) #rcurl strips off the zip extension for some reason, renamed by hand and ran again
  #setup_tokenizers_backend()
  #init_backend(type = "tokenizers")

  if(save){
    saveRDS(annotations, '/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/Violent Events/JPR_Paper_RandR/code/MeasuringLandscapeCivilWar/inst/extdata/annotations_name.Rdata')
  }

  return(annotations)
}



kenya_location_strip_suffix_create_data_for_labeling <-
  function(location_names,
           outpath='/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/Violent Events/JPR_Paper_RandR/code/MeasuringLandscapeCivilWar/inst/extdata/tokens_for_labeling.csv'){

    print("Initializing spaCy")
    init_spaCy(vector_flag = F)
    print("Generating Annotations")
    annotations <- run_annotators(unique(location_names),as_strings = TRUE, doc_id_offset=0)
    library(keras)
    p_load(ptw)
    require(utils)
    #Read Training Data

    tokens_for_labeling <- annotations$token
    tokens_for_labeling[,c('label_posesive','label_corporate','label_person',
                           'label_location','label_type','label_postfix_location','label_prefix','label_null')] <- NA
    tokens_for_labeling <- as.data.table(tokens_for_labeling)

    tokens_for_labeling <- subset(tokens_for_labeling, tid!=0) #Kill Root
    tokens_for_labeling$location_name <- unique(location_names)[tokens_for_labeling$id]
    tokens_for_labeling[, word_number := seq_len(.N), by=id]

    tokens_for_labeling[, location_name_clean_word_number := tolower(paste(location_name,word_number, sep="_")),]
    tokens_for_labeling <- subset(tokens_for_labeling, !duplicated(location_name_clean_word_number))

    tokens_for_labeling <- tokens_for_labeling[,c('location_name_clean_word_number','id','word_number',
                                                  'location_name','word',
                                                  'label_posesive','label_corporate','label_person',
                                                  'label_location','label_type','label_postfix_location','label_prefix','label_null'),with=F]

    if(!is.null(outpath)){
      fwrite(tokens_for_labeling, outpath)
    }
    return(tokens_for_labeling)
  }


kenya_location_strip_suffix_generate_features_unsupervised <- function(fromscratch=F){

  if(fromscratch) {
    flatfiles <- fread('/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/Violent Events/JPR_Paper_RandR/code/MeasuringLandscapeCivilWar/inst/extdata/flatfiles.csv')
    global_features <- kenya_location_strip_suffix_generate_features(location_names= unique(flatfiles$name_clean) ,
                                                                     features_upos=F ,
                                                                     features_local=T,
                                                                     features_global=F,
                                                                     features_word2vec=F  , timesteps=5 , scaleT=F)
    global_features$word_clean <- tolower(global_features$word)
    global_features <- subset(global_features,!duplicated(word_clean) )
    global_features <- subset(global_features,!is.na(word_clean) & word_clean!="")
    global_features <- global_features[,c('word_clean','word_number_max','word_frequency','word_number_max_average',
                                          'word_position_mean','word_position_fromend_mean'),with=F]
    global_features$word_clean_integer <- as.numeric(as.factor(global_features$word_clean))+1 #reserve 0 for masked and 1 for unknown
    names(global_features)[2:7] <- paste0("global_", names(global_features)[2:7])

    fwrite(global_features,
           '/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/Violent Events/JPR_Paper_RandR/code/MeasuringLandscapeCivilWar/inst/extdata/global_features.csv')
  }
  return(fread('/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/Violent Events/JPR_Paper_RandR/code/MeasuringLandscapeCivilWar/inst/extdata/global_features.csv'))

}


kenya_location_strip_suffix_generate_features <- function(location_names, save=F,
                                                          features_upos=F ,
                                                          features_local=F,
                                                          features_global=F,
                                                          features_word2vec=F ,
                                                          timesteps=5 ,
                                                          scaleT=T
){

  location_names <- unique(location_names)
  p_load(cleanNLP)
  print("Initializing spaCy")
  init_spaCy(vector_flag = features_word2vec)
  print("Generating Annotations")
  annotations <- run_annotators(location_names,
                                as_strings = TRUE, doc_id_offset=0)

  token <- as.data.table(annotations$token)   # Start from scratch here

  token <- subset(token , tid !=0 ) #Kill Root

  #Identity Work
  token$location_name <- unique(location_names)[token$id]
  token[, word_number := seq_len(.N), by=id]
  token[,location_name_clean_word_number:=tolower(paste(location_name,word_number, sep="_")),]
  token[,location_name_word:=paste(location_name,word, sep="_"),]

  token[,location_name_clean:=tolower(location_name)]

  token[,word_number_max:=max(word_number), by=list(id)]
  token[,word_clean:=tolower(word),]

  #Start Grabbing Features
  x_features <- vector()
  token_features <- token

  #UPOS Features
  if(features_upos) {
    upos <- as.data.frame(to_categorical(as.factor(token$upos))); names(upos) <- paste0("upos_",levels(as.factor(token$upos)))
    upos$"NA" <- NULL
    token_features <- cbind(token_features, upos[,1:16])
    x_features <- c(x_features, grep('^upos_', names(upos), value = T))
  }

  #Local Features
  if(features_local){

    local_features <- token_features[,list(
      word_frequency =length(id), #number of times it appears
      word_number_max_average=mean(word_number_max),
      word_position_mean=mean(word_number),
      word_position_max=max(word_number) ,
      word_position_fromend_mean=mean(word_number_max-word_number),
      word_nchar=nchar(word_clean)[1]
      #upos_mode=names(sort(-table(upos)))[1]
    ), by=list(word_clean)]

    token_features <- merge(token_features,
                            local_features, by="word_clean", all.x=T, all.y=T)
    setkey(token_features, "location_name_clean_word_number")
    token_features[is.na(token_features)] = 0 #ugly way to do it  https://stackoverflow.com/questions/20535505/replacing-all-missing-values-in-r-data-table-with-a-value

    #Local Features by Hand
    token_features$word_hasnumbers <- as.numeric(grepl('[0-9]', token_features$word)) ; table(token_features$word_hasnumbers)
    token_features$word_haspunct <- as.numeric(grepl('[[:punct:]]', token_features$word, perl = T)) ; table(token_features$word_haspunct)
    p_load(sylcount)
    token_features$word_syllables <- sapply(sylcount(token_features$word, counts.only = F), FUN=function(x) sum(x$syllables) )

    x_features <- c(x_features, setdiff( grep('^word_', names(token_features), value = T) , "word_clean") )
  }

  #Global Features
  if(features_global) {
    global_features <- kenya_location_strip_suffix_generate_features_unsupervised(F)
    token_features <- merge(token_features,global_features, by="word_clean", all.x=T, all.y=F)
    token_features$global_word_clean_integer[is.na(token_features$global_word_clean_integer)] <- 1 #imput missing as 1, leave 0 for masked
    setkeyv(token_features, 'location_name_clean_word_number')

    x_features <- unique(c(x_features, names(global_features) ))
  }

  # install.packages("devtools")
  #devtools::install_github("sjackman/uniqtag") #shorten it and then hash it as an integer
  #library(uniqtag)
  #token_features_vectors$word_uniqtag <- uniqtag(token_features_vectors$word)

  #p_load(hash)
  #token_features_vectors$word_hash <- sapply(token_features_vectors$word, text_hashing_trick, n=10, hash_function='md5')
  #p_load(R.oo)
  #token_features_vectors$word_hash <- hashCode(token_features_vectors$word)

  if(features_word2vec){
    vector <- as.data.table(annotations$vector)
    names(vector) <- c('id','sid','tid', paste0("word2vec_dim",1:300))
    token_features <- merge(token_features, vector, by=c('id','sid','tid'), all.x=T)
    setkey(token_features,location_name_clean_word_number)

    x_features <- unique(c(x_features, paste0("word2vec_dim",1:300)))
  }

  if(scaleT) {
    for (col in setdiff(x_features,c("word_clean","global_word_clean_integer")  ) )  token_features[, (col) := scale(as.numeric(col)) ]
  } #scale if requested

  for (col in setdiff(x_features,c("word_clean","global_word_clean_integer"))  )  { token_features[is.na(get(col)), (col) := 0.0] } #replace NA with zeros to be masked

  final <- token_features[, unique( c('location_name_clean_word_number','location_name_clean','word','word_number',x_features) ),with=F]
  return(final)

}



kenya_location_strip_suffix_train_trainingsplit_y <- function(timesteps=5,
                                                              inpath='/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/Violent Events/JPR_Paper_RandR/code/MeasuringLandscapeCivilWar/inst/extdata/tokens_hand_labeled.csv'
) {
  library(data.table)

  if(is.null(inpath)) {
    tokens_hand_labeled <- fread(system.file("extdata", "tokens_hand_labeled.csv", package = "MeasuringLandscape"))
  } else {
    tokens_hand_labeled <- fread(inpath)
  }

  y_features <- c(grep("label_",names(tokens_hand_labeled), value=T)) #,'label_end'
  y_features_k <- length(y_features)

  tokens_hand_labeled[, location_name_clean := tolower(location_name),]
  tokens_hand_labeled[, location_name_length := length(word_number), by=location_name_clean]

  tokens_hand_labeled$labeled <- rowSums(tokens_hand_labeled[,grep("label_",names(tokens_hand_labeled), value=T), with=F], na.rm=T)
  tokens_hand_labeled[, labeled_count := sum(labeled), by=location_name]

  tokens_hand_labeled_fully <- subset(tokens_hand_labeled, labeled_count==location_name_length) #Limit only to names that have been fully labeled by hand
  tokens_hand_labeled_fully <- subset(tokens_hand_labeled_fully, !duplicated(tokens_hand_labeled_fully$location_name_clean_word_number)) #further subset to only unique names

  #Generate features for these
  setkey(tokens_hand_labeled_fully, location_name_clean_word_number) #This ensures both are in the same order

  y_all <- tokens_hand_labeled_fully
  y_all <- subset(y_all, word_number <=timesteps) ; dim(y_all)
  dim(y_all)

  for (col in c(y_features)) { y_all[is.na(get(col)), (col) := 0.0] } #replace NA with zeros

  return(y_all)
}


kenya_location_strip_suffix_train_trainingsplit_yx <- function(features_upos=F , features_local=F, features_global=F, features_word2vec=F ,
                                                               timesteps=5 , scaleT=F , array3d=T, location_names=NULL) {

  if(is.null(location_names)) {

    y_all <- kenya_location_strip_suffix_train_trainingsplit_y() ; dim(y_all)
    y_features <- grep('^label_', names(y_all), value = T)

    location_names=unique(tolower(y_all$location_name_clean))
    x_all <- kenya_location_strip_suffix_generate_features(location_names=unique(y_all$location_name_clean),
                                                           features_upos=features_upos,
                                                           features_local=features_local,
                                                           features_global=features_global,
                                                           features_word2vec=features_word2vec,
                                                           scaleT=scaleT ,
                                                           timesteps=timesteps) ; dim(x_all)

    x_features <- setdiff( names(x_all) , c("location_name_clean_word_number","location_name_clean","word","word_clean"))

    ykeep <- c('word_number',
               grep('label_', names(y_all), value = T) )
    yx_all <- merge(y_all[, c('location_name_clean_word_number',ykeep), with=F],
                    x_all[,  setdiff( names(x_all),ykeep), with=F ],
                    by= c('location_name_clean_word_number'), all.x=T, all.y=F)  ; dim(yx_all)

  } else {
    yx_all <- kenya_location_strip_suffix_generate_features(location_names=unique(location_names),
                                                            features_upos=features_upos,
                                                            features_local=features_local,
                                                            features_global=features_global,
                                                            features_word2vec=features_word2vec,
                                                            scaleT=scaleT ,
                                                            timesteps=timesteps
    )  ; dim(yx_all)
    yx_all$location_name_clean <- gsub("_[0-9]$", "",  yx_all$location_name_clean_word_number )
    y_features <- NULL
    x_features <- setdiff( names(yx_all) , c("location_name_clean_word_number","location_name_clean","word","word_clean"))
  }

  padding <- as.data.table(expand.grid( location_name_clean = location_names,  word_number = 1:(timesteps) ))  ; dim(padding)

  yx_all_padded <- merge(padding, yx_all, by=c('location_name_clean','word_number'), all.x=T, all.y=F )
  yx_all_padded$location_name_clean_word_number <- with(yx_all_padded,  paste0(location_name_clean,'_',word_number))
  setkeyv(yx_all_padded,'location_name_clean_word_number')
  yx_all_padded$word_number[is.na(yx_all_padded$word)] <- 0 #This makes sure that we zero out rows entirely to be masked

  yx_all_padded_subset <- yx_all_padded[,unique( c('location_name_clean_word_number','word_number','word',y_features,x_features) ),with=F]
  setkeyv(yx_all_padded_subset,'location_name_clean_word_number')

  yx_all_padded_subset <- subset(yx_all_padded_subset, word_number<=timesteps)
  yx_all_padded_subset$location_name_clean <- gsub("_[0-9]$", "",  yx_all_padded_subset$location_name_clean_word_number ) #placeholder, something's broken'
  for (col in c(y_features,x_features)) { yx_all_padded_subset[is.na(get(col)), (col) := 0.0] } #Impute zeros a second time now. Unrelated to scale. So padded are all zeros

  if(!array3d){
    return(yx_all_padded_subset)
  } else {
    x_all_padded_array <- array(t(as.matrix(yx_all_padded_subset[,x_features,with=F])),
                                dim=c(length(x_features),timesteps,length(unique(yx_all_padded_subset$location_name_clean)))) #it fills it in by row so have to do this transpose and then 300,100 thing
    x_all_padded_array <- aperm(x_all_padded_array, c(3,2,1))
    dim(x_all_padded_array)
    dimnames(x_all_padded_array) <- list()
    dimnames(x_all_padded_array)[[1]] <-  as.character(unique(padding$location_name_clean))
    dimnames(x_all_padded_array)[[2]] <- paste("TimeStep",1:(timesteps),sep="_")
    dimnames(x_all_padded_array)[[3]] <- x_features
    #attributes(x_all_padded_array) <- list( words= y_all$word)

    y_all_padded_array <- array(t(as.matrix(yx_all_padded_subset[,y_features,with=F])),
                                dim=c(length(y_features),timesteps,length(unique(yx_all_padded_subset$location_name_clean)) ) )
    y_all_padded_array <- aperm(y_all_padded_array, c(3,2,1))

    if(dim(y_all_padded_array)[3]!=0) {
      dimnames(y_all_padded_array) <- list()
      dimnames(y_all_padded_array)[[1]] <- as.character(unique(padding$location_name_clean))
      dimnames(y_all_padded_array)[[2]] <- paste("TimeStep",1:(timesteps),sep="_")
      dimnames(y_all_padded_array)[[3]] <- y_features
      #attributes(y_all_padded_array) <- list( words= y_all$word)
    }
    return(list(y_all_padded_array=y_all_padded_array,
                x_all_padded_array=x_all_padded_array))
  }
}



kenya_location_strip_suffix_train_trainingsplit <- function(validation_size=.1,
                                                            features_upos=F , features_local=F, features_global=F, features_word2vec=F ,
                                                            timesteps=5 ,scaleT=F ){

  yx_list <- kenya_location_strip_suffix_train_trainingsplit_yx( features_upos=features_upos,
                                                                 features_local=features_local,
                                                                 features_global=features_global,
                                                                 features_word2vec=features_word2vec,
                                                                 scaleT=scaleT ,
                                                                 timesteps=timesteps)

  y_all_padded_array <- yx_list[['y_all_padded_array']] ; dim(y_all_padded_array)
  x_all_padded_array <- yx_list[['x_all_padded_array']]  ; dim(x_all_padded_array)

  labeled=dimnames(y_all_padded_array)[[1]]

  #Split into train and validation
  i_train <- sample(labeled, length(labeled)*(1-validation_size))
  i_test <- setdiff(labeled,i_train)

  x_train_padded_array <- x_all_padded_array[labeled %in% i_train,,] ;dim(x_train_padded_array)
  x_test_padded_array <- x_all_padded_array[labeled %in% i_test,,] ;dim(x_test_padded_array)

  y_train_padded_array <- y_all_padded_array[labeled %in% i_train,,] ;dim(y_train_padded_array)
  y_test_padded_array <- y_all_padded_array[labeled %in% i_test,,] ;dim(y_test_padded_array)

  return(list(x_all=x_all_padded_array,
              y_all=y_all_padded_array,
              x_train=x_train_padded_array,
              x_test=x_test_padded_array,
              y_train=y_train_padded_array,
              y_test=y_test_padded_array)
  )
}

