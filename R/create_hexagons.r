

## Create hexagons dataset of a given size

# region_of_interest_sf_utm <- create_roi(bottom_left_x=35.67,
#                                        bottom_left_y=-1.43285,
#                                        top_right_x=38.19,
#                                        top_right_y=0.54543)

create_hexagon_df <- function(
                              cellsize_km=1, # km
                              region_of_interest=NULL,
                              type="hexagonal",
                              crs) {

  # crs_m <- "+proj=utm +zone=27 +datum=NAD83 +units=m +no_defs"
  # flatfiles_sf_utm <- st_transform(flatfiles_sf, crs=crs_m) #actually takes a sec
  # crs(regionofinterest) <- crs(pop_raster)

  print(cellsize_km)
  HexPts <- spsample(
    region_of_interest %>% as("Spatial"),
    type = "hexagonal",
    cellsize = cellsize_km / 111
  )
  HexPols <- HexPoints2SpatialPolygons(HexPts)
  # plot(HexPols)
  HexPols_sf <- st_as_sf(HexPols)
  HexPols_sf$hexid <- 1:length(HexPols)
  HexPols_sf$hex_longitude <- coordinates(HexPols)[, 1]
  HexPols_sf$hex_latitude <- coordinates(HexPols)[, 2]

  return(HexPols_sf)
}


#
#
#   print("Over")
#   hex_cad <- over(HexPols,cadastral)
#   hex_cad$hexid <- 1:nrow(hex_cad)
#
#   hex_tribes <- over(HexPols,tribes)
#   hex_tribes$hexid <- 1:nrow(hex_tribes)
#
#   hex_language <- over(HexPols,language)
#   hex_language$hexid <- 1:nrow(hex_language)
#
#   #hex_agzones <- over(HexPols,agzones)
#   #hex_agzones$hexid <- 1:nrow(hex_agzones)
#
#   hex_landuse <- over(HexPols,landuse)
#   hex_landuse$hexid <- 1:nrow(hex_landuse)
#
#   #hex_rainfall <- over(HexPols,rainfall)
#   #hex_rainfall$hexid <- 1:nrow(hex_rainfall)
#
#
#   merge2 <- function(x,y) merge(x,y, by='hexid', all.x=T)
#   hex_covariates <- Reduce(merge2,list(
#     hex_cad,
#     hex_tribes,
#     hex_language,
#     #hex_agzones,
#     hex_landuse#,
#     #hex_rainfall
#   )) ; dim(hex)
#
#
#   dim(hex_cad)
#
#   #What if foreached this?
#   p_load(foreach)
#   p_load(doMC); # install.packages('doMC')
#   registerDoMC(cores=8)
#   #voronoi_lists <-  lapply(1:100,FUN=function(x) fit_voronoi())
#   #Yup that was the trick
#   print("Rasters")
#   hex_pop <- foreach(i=1:length(HexPols)) %dopar% {  raster::extract( pop_raster,HexPols[i],fun=sum  )  } #Ok this is multithreaded now
#   hex_rugged <- foreach(i=1:length(HexPols)) %dopar% {  raster::extract(raster_rugged,HexPols[i],fun=sum)  } #Ok this is multithreaded now
#   hex_roads <- foreach(i=1:length(HexPols)) %dopar% {  raster::extract(raster_roads,HexPols[i],fun=sum)  } #Ok this is multithreaded now
#   hex_rain <- foreach(i=1:length(HexPols)) %dopar% {  raster::extract(raster_rain,HexPols[i],fun=sum)  } #Ok this is multithreaded now
#   hex_forest <- foreach(i=1:length(HexPols)) %dopar% {  raster::extract(raster_forest,HexPols[i],fun=sum)  } #Ok this is multithreaded now
#
#
#
#   hex_covariates$pop_mean     <- unlist(hex_pop) / cellsize
#   hex_covariates$rugged_mean  <- unlist(hex_rugged)/ cellsize
#   hex_covariates$roads_mean   <- unlist(hex_roads)/ cellsize
#   hex_covariates$rain_mean    <- unlist(hex_rain)/ cellsize
#   hex_covariates$forest_mean  <- unlist(hex_forest)/ cellsize
#
#   hex_covariates$pop_log     <- log(unlist(hex_pop)+1)     #/ cellsize
#   hex_covariates$rugged_log  <- log(unlist(hex_rugged)+1)  #/ cellsize
#   hex_covariates$roads_log   <- log(unlist(hex_roads)+1)   #/ cellsize
#   hex_covariates$rain_log    <- log(unlist(hex_rain)+1)    #/ cellsize
#   hex_covariates$forest_log  <- log(unlist(hex_forest)+1)    #/ cellsize
#
#   events_temp <- subset(events, !is.na(best_latitude) & !is.na(best_longitude))
#   events_best_sp <- SpatialPointsDataFrame(coords=events_temp[,c('best_longitude','best_latitude')], data=events_temp)
#   crs(events_best_sp) <- crs(HexPols)
#   events_best_sp$hexid <- over(events_best_sp,HexPols)
#
#   #now collapse on hexid however you want and merge on the hexagons
#   table(events_best_sp$type_clean_aggmed)
#   table(events_best_sp$type_clean_agghigh)
#
#   print("Events")
#   p_load(plyr)
#   events_best_sp_hex <- ddply(data.frame(events_best_sp), .(hexid), summarize,
#
#                               governmentkilled_clean_log = log( sum(governmentkilled_clean, na.rm=T) +1),
#                               rebelskilled_clean_log = log( sum(rebelskilled_clean, na.rm=T) +1),
#                               rebelsgovernment_killed_clean_log = log( sum(governmentkilled_clean, na.rm=T) + sum(rebelskilled_clean, na.rm=T)+1),
#
#                               ratio_killed_gov2all = sum(governmentkilled_clean, na.rm=T) / ( sum(governmentkilled_clean, na.rm=T) + sum(rebelskilled_clean, na.rm=T)   ),
#
#                               governmentkilledwounded_clean_log = log( sum(governmentkilledwounded_clean, na.rm=T) +1),
#                               rebelskilledwounded_clean_log = log( sum(rebelskilledwounded_clean, na.rm=T) +1),
#                               rebelsgovernment_killedwounded_clean_log = log(  sum(rebelsgovernment_killedwounded_clean, na.rm=T) +1),
#
#                               ratio_killedwounded_gov2all = sum(governmentkilledwounded_clean, na.rm=T) / ( sum(governmentkilledwounded_clean, na.rm=T) + sum(rebelskilledwounded_clean, na.rm=T) ),
#
#                               type_clean_agghigh_all =   sum( type_clean_agghigh %in% "government activity") +  sum( type_clean_agghigh %in% "rebel activity"),
#                               type_clean_agghigh_gov =  sum( type_clean_agghigh %in% "government activity"),
#                               type_clean_agghigh_reb =   sum( type_clean_agghigh %in% "rebel activity") ,
#                               type_clean_agghigh_gov_log = log ( sum( type_clean_agghigh %in% "government activity") + 1),
#                               type_clean_agghigh_reb_log = log (  sum( type_clean_agghigh %in% "rebel activity") + 1),
#                               type_clean_agghigh_any_log = log (  sum( type_clean_agghigh %in% "government activity") +
#                                                                     sum( type_clean_agghigh %in% "rebel activity") + 1),
#                               type_clean_agghigh_netgov = sum( type_clean_agghigh %in% "government activity") - sum( type_clean_agghigh %in% "rebel activity")
#   )
#
#   events_best_sp_hex$type_clean_agghigh_ratio_reb2all <- events_best_sp_hex$type_clean_agghigh_reb/events_best_sp_hex$type_clean_agghigh_all
#
#   hex_all <- merge( hex, hex_covariates, all.x=T)
#   hex_all <- merge( hex_all, events_best_sp_hex, all.x=T)
#
#   hex_all[,names(events_best_sp_hex)][is.na(hex_all[,names(events_best_sp_hex)])] <- 0
#
#   #head(hex_all)
#
#   #with(hex_cad_events, plot(log(type_clean_agghigh_gov), log(pop)))
#   return(hex_all)
# }
#
#
# ```
#
# # Create hexagons
#
# #2km, 4km,8km,16km,32km
#
# ```{r}
#
# p_load(foreach)
# p_load(doMC); # install.packages('doMC'); install.packages("doMC", repos="http://R-Forge.R-project.org")
# registerDoMC(cores=8)
#
# onekm <- 0.008
# test=create_hexagon_df(cellsize=17*onekm) #column names error is ok
# head(test)
# save(test, file="D:/Dropbox (UCSD_MATH)/Kenya Article Drafts/Violent Events/code/hextest.Rdata")
#
# fromscratch=F
# if(fromscratch) {
#   sizes=rev(c(5:30*onekm))
#   #hex_all_list <- foreach(i=sizes ) %dopar%  {  create_hexagon_df(i)  } #This takes a very long time.
#   options(warn=-1)
#   hex_all_list <- list()
#   for(i in sizes) {
#     print(i)
#     if(is.null(hex_all_list[[as.character(i)]] )) {
#       print("Starting")
#       hex_all_list[[as.character(i)]] <- create_hexagon_df(i)
#     }
#   }
#   options(warn=1)
#   save(hex_all_list, file="D:/Dropbox (UCSD_MATH)/Kenya Article Drafts/Violent Events/code/hex_all_list.Rdata")
# } else {
#   load(file="D:/Dropbox (UCSD_MATH)/Kenya Article Drafts/Violent Events/code/hex_all_list.Rdata")
# }
#
# sortednames <- names(hex_all_list)[order(as.numeric(names(hex_all_list)))]
# hex_all_list <- hex_all_list[sortednames]
# names(hex_all_list)
# lapply(hex_all_list, dim)
#
# #Pull covariates by lat/longs. Need to update all of these so that we pull by gaz.
#
# ```{r}
# events$pop_density_mapcoordinate <- raster::extract(pop_raster, cbind(events$mapcoordinate_clean_longitude, events$mapcoordinate_clean_latitude))
# hist(events$pop_density_mapcoordinate)
#
# events$pop_density_gaz_ensemble <- raster::extract(pop_raster, cbind(events$longitude_ensemble, events$latitude_ensemble))
# hist(events$pop_density_gaz_ensemble)
# summary(events$pop_density_gaz_ensemble)
#
# hist(events$pop_density_gaz_ensemble-events$pop_density_mapcoordinate, breaks=50)
#
# events$rugged_mapcoordinate <- raster::extract(raster_rugged, cbind(events$mapcoordinate_clean_longitude, events$mapcoordinate_clean_latitude))
# hist(events$rugged_mapcoordinate)
#
# events$rugged_gaz_ensemble <- raster::extract(raster_rugged, cbind(events$gaz_ensemble_longitude, events$gaz_ensemble_latitude))
# hist(events$rugged_gaz_ensemble)
#
# events$roads_mapcoordinate <- raster::extract(raster_roads, cbind(events$mapcoordinate_clean_longitude, events$mapcoordinate_clean_latitude))
# hist(events$roads_mapcoordinate)
# summary(events$roads_mapcoordinate)
#
# events$roads_gaz_ensemble <- raster::extract(raster_roads, cbind(events$gaz_ensemble_longitude, events$gaz_ensemble_latitude))
# hist(events$roads_gaz_ensemble)
# summary(events$roads_gaz_ensemble)
#
#
# ```
