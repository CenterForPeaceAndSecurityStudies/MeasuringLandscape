

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
  HexPts <- sp::spsample(
    region_of_interest %>% as("Spatial"),
    type = "hexagonal",
    cellsize = cellsize_km / 111
  )
  HexPols <- sp::HexPoints2SpatialPolygons(HexPts)
  # plot(HexPols)
  HexPols_sf <- st_as_sf(HexPols)
  HexPols_sf$hexid <- 1:length(HexPols)
  HexPols_sf$hex_longitude <- sp::coordinates(HexPols)[, 1]
  HexPols_sf$hex_latitude <- sp::coordinates(HexPols)[, 2]

  return(HexPols_sf)
}


