

#
# This function loads (or creates extracts) of a number of geospatial files. It saves them as a list and also returns them as a list
#
prep_covariates <- function(roi, fromscratch=F) {
  # p_load(rasterVis)
  # p_load(ggplot2)
  # p_load(rgdal)
  # p_load(raster)
  # p_load("rgdal") # requires sp, will use proj.4 if installed
  # p_load("maptools")
  # p_load("ggplot2")
  # p_load("plyr")
  # p_load(raster)
  # p_load(glue)

  # Not in the paper
  # District
  # kenya_cadastral_district_sf <- st_read(system.file("extdata", "kenya_cadastral_district.gpkg", package = "MeasuringLandscape"))

  # Not in the paper
  # Cadastral Map
  # kenya_cadastral_sf <- st_read(system.file("extdata", "kenya_cadastral.gpkg", package = "MeasuringLandscape"), crs = 4326)

  # Not in the paper
  # Language
  # LanguagePolygons <- st_read(system.file("extdata", "LanguagePolygons.shp", package = "MeasuringLandscape"), crs = 4326)

  # Not in the paper
  ## Land Use
  # kenya_landuse <- st_read(system.file("extdata", "kenya_landuse.shp", package = "MeasuringLandscape"), crs = 4326)

  # Not in the paper
  # Ethnic Group
  # kenya_tribes <- st_read(system.file("extdata", "kenya_tribes.shp", package = "MeasuringLandscape"), crs = 4326)

  # Rain
  fromscratch2 <- F # Unreachable unless you have the full precipition files and want to create the extract from scratch
  if (fromscratch2) {
    layer27 <- list()
    for (i in 1:12) {
      name <- paste0("prec", i, "_", 27, ".tif")
      layer27[[i]] <- raster(glue("/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar_TooBig/", name))
    }
    prec27 <- layer27[[1]] + layer27[[2]] + layer27[[3]] + layer27[[4]] + layer27[[5]] + layer27[[6]] + layer27[[7]] + layer27[[8]] + layer27[[9]] + layer27[[10]] + layer27[[11]] + layer27[[12]]

    layer37 <- list()
    for (i in 1:12) {
      name <- paste0("prec", i, "_", 37, ".tif")
      layer37[[i]] <- raster(glue("/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar_TooBig/", name))
    }
    prec37 <- layer37[[1]] + layer37[[2]] + layer37[[3]] + layer37[[4]] + layer37[[5]] + layer37[[6]] + layer37[[7]] + layer37[[8]] + layer37[[9]] + layer37[[10]] + layer37[[11]] + layer37[[12]]
    # plot(prec37)

    raster_rain <- raster::merge(prec27, prec37)
    raster_rain <- raster_rain %>% projectRaster(., crs = "+proj=longlat +datum=WGS84 +no_defs")
    rasterOptions(tolerance = 0.1)
    raster_rain <- raster_rain %>% crop(., as(roi, "Spatial"))
    saveRDS(raster_rain, "/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar/inst/extdata/raster_rain.Rds")
  }
  raster_rain <- readRDS(system.file("extdata", "raster_rain.Rds", package = "MeasuringLandscape"))

  # Population
  fromscratch2 <- F # Unreachable unless you have the full  files and want to create the extract from scratch
  if (fromscratch2) {
    pop_raster_roi <- raster(glue("/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar_TooBig/", "afpopd60.gtiff")) %>%
      projectRaster(., crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
      crop(., as(roi, "Spatial"))
    saveRDS(pop_raster_roi, "/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar/inst/extdata/pop_raster_roi.Rdata")
  }
  pop_raster_roi <- readRDS(system.file("extdata", "pop_raster_roi.Rdata", package = "MeasuringLandscape"))

  # Forest
  fromscratch2 <- F # Unreachable unless you have the full  files and want to create the extract from scratch
  if (fromscratch2) {
    forest_raster_roi <- raster(glue("/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar_TooBig/", "kenya_treecover.tif")) %>%
      projectRaster(., crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
      crop(., as(roi, "Spatial"))
    saveRDS(forest_raster_roi, "/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar/inst/extdata/forest_raster_roi.Rdata")
  }
  forest_raster_roi <- readRDS(system.file("extdata", "forest_raster_roi.Rdata", package = "MeasuringLandscape"))

  # Ruggedness
  fromscratch2 <- F # Unreachable unless you have the full  files and want to create the extract from scratch
  if (fromscratch2) {
    ruggedness_raster_roi <- raster(glue("/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar_TooBig/", "ruggedness1K_kenya_wgs84.tif")) %>%
      projectRaster(., crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
      crop(., as(roi, "Spatial"))
    saveRDS(ruggedness_raster_roi, "/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar/inst/extdata/ruggedness_raster_roi.Rdata")
  }
  ruggedness_raster_roi <- readRDS(system.file("extdata", "ruggedness_raster_roi.Rdata", package = "MeasuringLandscape"))


  # Roads
  # devtools::install_github("ecohealthalliance/fasterize")
  # library(fasterize)  #install this breaks SF. Reinstall it again afterword
  # library(devtools)
  # install_github("r-spatial/sf",force = TRUE)
  library(sf)
  fromscratch2 <- F
  if (fromscratch2) {
    r.raster <- raster(
      xmn = 129265, xmx = 409892.2, ymn = -158627.5, ymx = 60385.9,
      crs = "+proj=longlat +datum=WGS84 +no_defs",
      resolution = 1000
    ) # It's in meters do 1km
    r.raster <- setValues(r.raster, 0)

    road_raster_roi <- st_read(glue("/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar_TooBig/", "ModernRoads_TakingPropertiesFromRaster.shp")) %>%
      st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
      as(., "Spatial") %>%
      as(., "SpatialLinesDataFrame") %>%
      crop(., as(roi, "Spatial")) %>%
      rasterize(., r.raster, 1, background = 0)

    writeRaster(
      road_raster_roi,
      filename = "/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar_TooBig/ModernRoads_TakingPropertiesFromRaster.tif",
      overwrite = TRUE
    )

    roads_distance_to <- raster(glue("/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar_TooBig/", "roads_distance_to.tif")) %>%
      projectRaster(., crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
      crop(., as(roi, "Spatial"))

    saveRDS(roads_distance_to, "/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar/inst/extdata/roads_distance_to.Rdata")
  }
  roads_distance_to <- readRDS(system.file("extdata", "roads_distance_to.Rdata", package = "MeasuringLandscape"))


  covariate_list <- list()
  covariate_list[['raster_rain']] <- raster_rain
  covariate_list[['pop_raster_roi']] <- pop_raster_roi
  covariate_list[['forest_raster_roi']] <- forest_raster_roi
  covariate_list[['ruggedness_raster_roi']] <- ruggedness_raster_roi
  covariate_list[['roads_distance_to']] <- roads_distance_to
    
    # kenya_cadastral_district_sf,
    # kenya_cadastral_sf,
    # LanguagePolygons,
    # kenya_tribes,
    # kenya_landuse

  if (fromscratch) {
    saveRDS(
      covariate_list,
      "/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar/inst/extdata/covariate_list.Rds"
    )
  }

  return(covariate_list)
}


# region_of_interest_sf_wg84 <- create_roi(bottom_left_x=35.67,
#                                          bottom_left_y=-1.43285,
#                                          top_right_x=38.19,
#                                          top_right_y=0.54543,
#                                          crs_out = "+proj=longlat +datum=WGS84 +no_defs")
#
# prep_covariates(roi=region_of_interest_sf_wg84)


# Imputed Coordinates are different than reported coordinates

# UNEP/GRID - Sioux Falls Clearinghouse
# Don't Duck Metadata
# Welcome to the United Nations Environment Programme / Global Resource Information Database (UNEP/GRID) Spatial Data Clearinghouse. The North American node of UNEP/GRID, designated as GRID-Sioux Falls, is located at the U.S. Geological Survey's EROS Data Center. UNEP GRID Sioux Falls participates in the Global Spatial Data Infrastructure (GSDI) activities.
# http://na.unep.net/siouxfalls/globalpop/africa/afpopd60.gif
# http://na.unep.net/siouxfalls/globalpop/africa/Africa_index.html
# http://na.unep.net/metadata/unep/GRID/AFPOP60.html
# http://na.unep.net/siouxfalls/unepdownload/form.php?type=africa

# Projection Parameters Geographic Units: DD Datum: WGS84
# Spatial Information Raster: Number of Columns: 2160 Number of Rows: 1800 Pixel Resolution (m): 2.5km (2.5 minutes) Data Type: integer
