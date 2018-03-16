
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
  #p_load(RANN)
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
  ) #wow that's fast
  
  #table(nearest$nn.dists>100) #These are missing, no match within the radius
  #summary(nearest_gaz$nn.dists[nearest_gaz$nn.dists<100])
  nearest_long <- as.data.table(nearest_gaz$nn.idx)
  nearest_long$event_hash <- events_sf_utm$event_hash[condition_events]
  
  #library(reshape2)
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
  m_within$string_dist_osa <-  stringdist(m_within$name_cleaner_a, m_within$name_cleaner_b, method ="osa", nthread= parallel::detectCores())
  
  m_within <- subset(m_within, !is.na(name_cleaner_a) & !is.na(name_cleaner_b) )
  
  write.csv(m_within, 
            glue(getwd(), "/../inst/extdata/event_flatfile_matches_for_hand_labeling.csv")
  )
}
