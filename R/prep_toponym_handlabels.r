

#This function takes in a pair of datasets, A and B, and pulls points in B that are nearby A.
#This gives us a list of possible matches of toponyms that might be considered the same
#Saving this as a csv file lets a human go back and hand label them
create_toponym_dataset_forlabeling <- function(){
  #Create some UTM versions because we want to be precise about distances in meters
  
  crs_m <- "+proj=utm +zone=27 +datum=NAD83 +units=m +no_defs" 
  flatfiles_sf_roi_utm_centroid <-  st_centroid(
    st_transform( flatfiles_sf_roi, crs=crs_m)
  ) ; dim(flatfiles_sf_roi_utm_centroid)
  
  events_sf_utm <-  st_transform(
    events_sf,
    crs=crs_m) ; dim(events_sf_utm)
  
  #We're not doing this anymore we're sampling
  #This is going to be our main pairwise dataset, I think we'll add columns to it as necessary rather than just try to rbind the two main ones over and over again
  #events_flatfiles <- as.data.table( expand.grid(event_hash = events_sf_utm$event_hash,
  #                                               place_hash = flatfiles_sf_roi_utm_centroid$place_hash) ) #this takes a while, might want to mcapply over a
  #
  #
  #dim(events_flatfiles) #549,004,829 it's half a billion observations
  #
  
  #You know, centroids should be enough. If the river or polygon is much bigger than this then I shouldn't be merging it anyway
  p_load(RANN)
  coords_events <- st_coordinates(events_sf_utm)
  coords_events[!is.finite(coords_events)] <- NA
  condition_events <- !is.na(coords_events[,1]); table(condition_events)
  
  coords_flatfiles <- st_coordinates(flatfiles_sf_roi_utm_centroid)
  coords_flatfiles[!is.finite(coords_flatfiles)] <- NA
  condition_flatfiles <- !is.na(coords_flatfiles[,1]); table(condition_flatfiles)
  
  nearest_gaz <- nn2(
    data  = na.omit(coords_flatfiles),
    query = na.omit(coords_events) ,
    k = 10, #wow this actually ended up mattering
    #treetype = c("kd", "bd"),
    searchtype = "standard" #,
    #radius = 4000 #meters
  ) #damn that's fast
  
  #table(nearest$nn.dists>100) #These are missing, no match within the radius
  #summary(nearest_gaz$nn.dists[nearest_gaz$nn.dists<100])
  nearest_long <- as.data.table(nearest_gaz$nn.idx)
  nearest_long$event_hash <- events_sf_utm$event_hash[condition_events]
  library(reshape2)
  m_within <- melt(nearest_long, id.vars=c("event_hash"))
  #m_within$variable <- NULL
  m_within$place_hash <- flatfiles_sf_roi_utm_centroid$place_hash[condition_flatfiles][m_within$value]
  m_within$value <- NULL
  
  setkey(flatfiles_dt, place_hash)
  
  events_dt <- as.data.table(events_sf)
  setkey(events_dt, event_hash)
  
  m_within$name_cleaner_a <-  events_dt[m_within$event_hash, ]$name_cleaner
  m_within$name_cleaner_b <-  flatfiles_dt[m_within$place_hash, ]$name_cleaner
  
  setkey(m_within, event_hash, variable)
  
  m_within$rex_match <- NA
  m_within$rex_match[m_within$name_cleaner_a==m_within$name_cleaner_b] <- 1
  
  m_within <- subset(m_within, !duplicated(paste(name_cleaner_a,name_cleaner_b)))
  m_within$string_dist_osa <-  stringdist(m_within$name_cleaner_a, m_within$name_cleaner_b, method ="osa", nthread=48)
  
  m_within <- subset(m_within, !is.na(name_cleaner_a) & !is.na(name_cleaner_b) )
  
  write.csv(m_within, 
            glue(getwd(), "/../inst/extdata/event_flatfile_matches_for_hand_labeling.csv")
  )
}



#This function loads a prelabeled file of toponym-suggestion diads
#It returns a training test split
create_training_dataset <- function(vars_id, vars_weights, vars_y, vars_x, neg_count=0, fromscratch=F, drop_zero_dist=T, drop_identical=T) {
  
  if(fromscratch | neg_count>0){
    handlabeled <- fread(
      system.file("extdata", "event_flatfile_matches_for_hand_labeling - event_flatfile_matches_for_hand_labeling.csv",
                  package = "MeasuringLandscapeCivilWar"),
      data.table=T) 
    #handlabeled <- fread("/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar/inst/extdata/name_cleaner_pairs.csv")
    
    handlabeled$extranegative <- F
    handlabeled <- subset(handlabeled, name_cleaner_a!="" & name_cleaner_b!="")
    handlabeled$exact_match <- handlabeled$name_cleaner_a== handlabeled$name_cleaner_b
    dim(handlabeled)
    
    temp <- handlabeled %>% group_by(name_cleaner_a) %>% summarize_at('rex_match', sum, na.rm = TRUE)
    dim(temp)
    
    #Remove the identicals. That's giving it a false confidence
    handlabeled <- subset(handlabeled, name_cleaner_a != name_cleaner_b)
    table(handlabeled$rex_match)
    
    #Stopped including examples outside the ROI
    # I think our move here now is to supplement it with lots and lots of randomly sampled zeros that are sure to be more than 10km away
    # outside_roi <- subset(flatfiles_dt, !region_of_interest_within)$place_hash #one of those things that should take no time but is getting hung up for some reason
    # #Intentionally throws a warning
    # neg_count <- neg_count #how many artificial negative examples to include
    # neg1 <- data.table( event_hash=sample(events_sf_utm$event_hash[!is.na(events_sf_utm$name_cleaner)], size=neg_count, replace = T),
    #                     place_hash=sample(outside_roi, size=neg_count, replace = T) ) #intentionally letting it recycle to match places length
    # setkey(events_dt, event_hash)
    # neg1$name_cleaner_a <-  events_dt[neg1$event_hash, ]$name_cleaner
    # neg1$name_cleaner_b <-  flatfiles_dt[neg1$place_hash, ]$name_cleaner
    # neg1$rex_match <- 0
    # dim(neg1)
    # neg1$extranegative <- T
    # 
    # handlabeled <- rbindlist(list(handlabeled, neg1), fill=T) ; dim(handlabeled) #combine the two
    
    if(drop_identical){
      handlabeled <- handlabeled[name_cleaner_a != name_cleaner_b] ; dim(handlabeled)
    }
    if(drop_zero_dist){
      
      handlabeled[,temp_q_cos:= stringsim(name_cleaner_a,name_cleaner_b,"cos", nthread=48,q=2),]
      handlabeled <- handlabeled[temp_q_cos>.3] ; dim(handlabeled)
      
      #handlabeled[,temp_q_gram_2:= stringsim(name_cleaner_a,name_cleaner_b,"qgram", nthread=48,q=2),]
      #handlabeled <- handlabeled[temp_q_gram_2 != 0] ; dim(handlabeled)
    }
    
    #Need the model to be indifferent to ordering so add the training obs back in flipped as well
    handlabeled2 <- handlabeled
    handlabeled2$temp <- handlabeled2$name_cleaner_a
    handlabeled2$name_cleaner_a <- handlabeled2$name_cleaner_b
    handlabeled2$name_cleaner_b <- handlabeled2$temp
    handlabeled2$temp <- NULL
    handlabeled <- rbind(handlabeled, handlabeled2) ; dim(handlabeled)
    handlabeled <- subset(handlabeled, !duplicated(paste(name_cleaner_a,name_cleaner_b)))
    
    #Do it again after the rbind
    handlabeled <- subset(handlabeled, name_cleaner_a!="" & name_cleaner_b!="")
    dim(handlabeled)
    #Remove the identicals. That's giving it a false confidence
    
    table(handlabeled$rex_match) #2,123 positive examples, but half of those are just the mirror
    
    
    handlabeled$a <- handlabeled$name_cleaner_a
    handlabeled$b <- handlabeled$name_cleaner_b
    
    postfixes=geonames_postfixes()
    print("Stemming A")
    stem_results_a <- strip_postfixes(to_be_striped=handlabeled$a , postfixes=postfixes, whitelist="fort hall", verbose=T) 
    print("Stemming B")
    stem_results_b <- strip_postfixes(to_be_striped=handlabeled$b , postfixes=postfixes, whitelist="fort hall", verbose=T) 
    stem_ab <- data.table(a=stem_results_a$name_cleaner_stemmed,b=stem_results_b$name_cleaner_stemmed)
    
    handlabeled <- toponym_add_features(handlabeled) #requires the two columns to be called a and b appends all the columns necessary for toponym matching
    temp <- toponym_add_features(stem_ab) #requires the two columns to be called a and b appends all the columns necessary for toponym matching
    names(temp) <- paste0(names(temp),"_stemmed")
    
    handlabeled <- cbind(handlabeled, temp)
    handlabeled$postfix_has_a <- stem_results_a$name_cleaner_suffix!=""
    handlabeled$postfix_has_b <- stem_results_b$name_cleaner_suffix!=""
    
    handlabeled$weights <- 1
    handlabeled$weights[handlabeled$rex_match==1] <- sum(handlabeled$rex_match==0, na.rm=T)/sum(handlabeled$rex_match==1, na.rm=T)
    table(handlabeled$weights)
    
    dim(handlabeled)
    saveRDS(handlabeled,
            file=glue(getwd(), "/../inst/extdata/handlabeled.Rds")
            )
  }
  
  handlabeled <- readRDS(system.file("extdata", "handlabeled.Rds", package = "MeasuringLandscapeCivilWar"))
  
  vars_id_y_x_weights <- c(vars_id,vars_y,vars_x,vars_weights)
  
  #id_test <- sample(unique(handlabeled$a_stemmed),500) #sample stems instead as a harder test
  #need to save these now and never create them again. We're evaluating multiple models and if we ever do a different split we'll be cheating
  #saveRDS(id_test, file="/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar/inst/extdata/id_test.Rds")
  id_test <- readRDS(system.file("extdata", "id_test.Rds", package = "MeasuringLandscapeCivilWar"))
  
  handlabeled$test <- F
  handlabeled$test[handlabeled$a_stemmed %in% id_test | handlabeled$b_stemmed %in% id_test] <- T
  table(handlabeled$test)
  
  #table(handlabeled[,'q_gram_2']==0, handlabeled$rex_match)
  handlabeled <- subset(handlabeled, q_gram_2>0)
  
  xy_all <- subset(handlabeled, !is.na(rex_match))[,vars_id_y_x_weights,with=F]
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

