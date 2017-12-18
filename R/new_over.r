#
# This is a replacement for over for sf features, to extract properties at point from either rasters or pologyons
#

new_over <- function(x,y, variable) {
  if(class(y)[[1]] %in% "sf"){
    indexes <- sapply(st_intersects(x %>% st_transform(crs=32737),
                                    y %>% st_transform(crs=32737) ), function(z) if (length(z)==0) NA_integer_ else z[1])
    
    return(as.vector(as.data.frame(y)[indexes,variable]))
  } 
  
  if(class(y)[[1]] %in% "RasterLayer"){
    return(
      raster::extract(y %>% raster::projectRaster(crs="+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs"),
                      x  %>% sf::st_transform(crs=32737) %>% as("Spatial") %>% as("SpatialPoints")  
      )
    )
  }
  
}