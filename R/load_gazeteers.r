
# This file describes functions that load and process gazeteer data
# The design philosphy is create extracts from each source needed for the paper and that are small enough to include with the package
# But to leave as much of the transformations and processing of those extracts to the package side so they can be altered


# install.packages("remotes")
# remotes::install_github("krlmlr/styler")



# This is trickier now because really what we mean is st_intersect with a box
region_of_interest_sf_utm <- create_roi(
  bottom_left_x = 35.67,
  bottom_left_y = -1.43285,
  top_right_x = 38.19,
  top_right_y = 0.54543
)
region_of_interest_sf_utm <- sf::st_sf(a = 1, geom = region_of_interest_sf_utm)
region_of_interest_sf_utm$a <- NULL
# plot(region_of_interest_sf_utm)

subset_roi <- function(shapes_sf, roi) {
  print(dim(shapes_sf))

  newcrs <- sf::st_crs(shapes_sf)$epsg

  condition <- as.vector(
    sf::st_intersects(
      shapes_sf,
      sf::st_transform(roi, newcrs),
      sparse = F
    )
  )

  table(condition) # How many are within the ROI?

  return(shapes_sf[condition, ])
}



# temp <- shapes_sf %>% filter(source_dataset=="livestock_boundaries") #It doesn't like to do mixed
# p_temp <- ggplot() +
#           geom_sf(data=temp[1:100,], alpha = 0.1, size=1)  +
#           geom_sf(data=region_of_interest_sf_utm %>% st_transform(4326), alpha = 0.1, size=1, col="red")
# p_temp


# Do the point files first


load_wikidata <- function(long_min, long_max, lat_min, lat_max, fromscratch=F) {
  if (fromscratch) {

    # This code is unreachable but can be implimented by hand if you have the full hundreds of gb extract
    fromdoublscratch <- F
    if (fromdoublscratch) {
      # This a directory of tabular wikidata files, processed from a raw wikidata JSON dump
      file.names <- dir(path = "/home/rexdouglass/Dropbox (rex)/CDD/historicalborders/othergis/wikidata/processed/", pattern = ".fst", full.names = T)
      wikidata_list <- list()
      for (q in file.names) {
        print(q)
        if (is.null(wikidata_list[[q]])) {
          try({
            # This loads each file and pulls out only those with lat longs
            wikidata_list[[q]] <- subset(
              read.fst(q, as.data.table = T),
              !is.na(mainsnak.datavalue.value.latitude)
            ) # & mainsnak.property_text=="coordinate location"
          })
        }
      }
      wikidata <- rbindlist(wikidata_list, fill = T)[, c(
        "id", "type", "wikidata_name_first", "wikidata_name", "mainsnak.property_text",
        "wikidata_description_first", "wikidata_description", "wikidata_name2_first", "wikidata_name2",
        "mainsnak.datavalue.value.latitude", "mainsnak.datavalue.value.longitude"
      ), with = F]

      # Which we process and extract only those with latitudes and longitudes
      fwrite(wikidata, file = "/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar_TooBig/wikidata_coordinates.csv")
    }

    # An extract I've made available which only has wikidata entries with latitude and longitude coordinates
    wikidata <- fread("/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar_TooBig/wikidata_coordinates.csv")

    # We further reduce this to just points in our region of interest
    wikidata_roi <- subset(wikidata, mainsnak.datavalue.value.latitude > lat_min &
      mainsnak.datavalue.value.latitude < lat_max &
      mainsnak.datavalue.value.longitude > long_min &
      mainsnak.datavalue.value.longitude < long_max)
    dim(wikidata_roi)

    # Save an extract in the package directory, available for publishing
    fwrite(wikidata_roi, glue(dir_package_files, "wikidata_roi.csv"))
  }

  wikidata_roi <- fread(system.file("extdata/", "wikidata_roi.csv", package = "MeasuringLandscape"))

  wikinames <- strsplit(x = paste(wikidata_roi$wikidata_name_first, wikidata_roi$wikidata_name, wikidata_roi$wikidata_name2_first, wikidata_roi$wikidata_name2, sep = ";"), split = ";")
  wikinames_en <- sapply(wikinames, FUN = function(x) unique(x[grepl("en_", x)]))
  wikinames_en <- unlist(sapply(wikinames_en, FUN = function(x) paste(gsub("en_", "", x), collapse = ";")))
  wikidata_roi$name_alternates <- wikinames_en
  wikidata_roi$names <- wikinames_en
  wikidata_sf_roi <- wikidata_roi %>%
    dplyr::select("names", "mainsnak.datavalue.value.latitude", "mainsnak.datavalue.value.longitude", "name_alternates") %>%
    setNames(c("name", "latitude", "longitude", "name_alternates")) %>%
    dplyr::mutate(source_dataset = "wikidata") %>%
    dplyr::mutate(name = strsplit(name, ";| see | SEE | check if same as ")) %>%
   tidyr::unnest(name) %>%
    as.data.frame() %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant", remove = F) %>%
    mutate(timeperiod = "2017-01-01")

  return(wikidata_sf_roi)
}




load_tgn <- function(long_min, long_max, lat_min, lat_max, fromscratch=F) {
  if (fromscratch) {

    # Start with the full global gazeteer
    getty_tgn_wide <- data.table::read.fst("/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar_TooBig/getty_tgn_wide.fst", as.data.table = T)
    dim(getty_tgn_wide) # 2,505,494      71

    names(getty_tgn_wide) <- gsub(" <http://creativecommons.org/", "", names(getty_tgn_wide), fixed = T)
    names(getty_tgn_wide) <- gsub(" <http://purl.org/dc/terms/", "", names(getty_tgn_wide), fixed = T)
    names(getty_tgn_wide) <- gsub(" <http://purl.org/iso25964/", "", names(getty_tgn_wide), fixed = T)
    names(getty_tgn_wide) <- gsub(" <http://vocab.getty.edu/", "", names(getty_tgn_wide), fixed = T)

    names(getty_tgn_wide) <- gsub(" <http://rdfs.org/ns/", "", names(getty_tgn_wide), fixed = T)
    names(getty_tgn_wide) <- gsub("ontology#", "", names(getty_tgn_wide), fixed = T)
    names(getty_tgn_wide) <- gsub(" <http://www.w3.org/1999/02/", "", names(getty_tgn_wide), fixed = T)
    names(getty_tgn_wide) <- gsub(" <http://www.w3.org/2000/01/", "", names(getty_tgn_wide), fixed = T)
    names(getty_tgn_wide) <- gsub(" <http://www.w3.org/2004/02/skos/", "", names(getty_tgn_wide), fixed = T)
    names(getty_tgn_wide) <- gsub(" <http://www.w3.org/ns/", "", names(getty_tgn_wide), fixed = T)

    names(getty_tgn_wide) <- gsub(" <http://xmlns.com/foaf/0.1/focus", "", names(getty_tgn_wide), fixed = T)
    names(getty_tgn_wide) <- gsub("core#", "", names(getty_tgn_wide), fixed = T)

    getty_tgn_wide[, gettyid := gsub("<http://vocab.getty.edu/tgn/", "", V1, fixed = T), ]

    getty_tgn_wide <- getty_tgn_wide[
      , c("gettyid", "prefLabel", "altLabel", "placeType", "parentString", "parentStringAbbrev", "placeTypeNonPreferred", "prefLabelGVP"),
      with = F
    ]

    # Subset to locations with kenya in the name
    getty_tgn_wide_kenya <- getty_tgn_wide[grepl("Kenya", parentString)]
    dim(getty_tgn_wide_kenya)

    # Lat Longs are in a seperate file so go clean that up and merge on it
    getty_geometry_wide <- read.fst("/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar_TooBig/getty_geometry_wide.fst", as.data.table = T)
    dim(getty_geometry_wide) # 2,505,494      71

    names(getty_geometry_wide) <- gsub(" <http://schema.org/", "", names(getty_geometry_wide), fixed = T)
    names(getty_geometry_wide) <- gsub(" <http://www.w3.org/1999/02/", "", names(getty_geometry_wide), fixed = T)
    getty_geometry_wide[, gettyid := gsub("<http://vocab.getty.edu/tgn/", "", V1, fixed = T), ]
    getty_geometry_wide[, gettyid := gsub("-geometry", "", gettyid), ]

    getty_geometry_wide[, latitude := gsub("\"^^<http://www.w3.org/2001/XMLSchema#decimal", "", latitude, fixed = T), ]
    getty_geometry_wide[, longitude := gsub("\"^^<http://www.w3.org/2001/XMLSchema#decimal", "", longitude, fixed = T), ]

    getty_geometry_wide[, latitude := gsub("\"", "", latitude, fixed = T), ]
    getty_geometry_wide[, longitude := gsub("\"", "", longitude, fixed = T), ]
    getty_geometry_wide <- getty_geometry_wide[, c("gettyid", "latitude", "longitude"), with = F]

    table(getty_tgn_wide_kenya$gettyid %in% getty_geometry_wide$gettyid) # 51 of them don't have coords for some reason. They have hierchy but no coords

    tgn_kenya <- merge(getty_tgn_wide_kenya, getty_geometry_wide, by = "gettyid", all.x = T, all.y = F)

    getty_tgn_wide <- NULL
    getty_geometry_wide <- NULL
    gc()

    fwrite(
      tgn_kenya,
      file = glue(dir_package_files, "tgn_kenya.csv")
    )
  }

  tgn_sf <- fread(system.file("extdata", "tgn_kenya.csv", package = "MeasuringLandscape")) %>%
    dplyr::mutate(name_alternates = paste(prefLabel, altLabel, sep = ";")) %>%
    dplyr::mutate(location_text = paste(prefLabel, altLabel, sep = ";")) %>%
    dplyr::select("location_text", "latitude", "longitude", "name_alternates") %>%
    setNames(c("name", "latitude", "longitude", "name_alternates")) %>%
    dplyr::mutate(source_dataset = "tgn") %>%
    dplyr::mutate(name = gsub("\"| \\.|", "", name)) %>%
    dplyr::filter(!is.na(longitude) & !is.na(latitude)) %>%
    dplyr::mutate_at(c("latitude", "longitude"), as.numeric) %>%
    dplyr::mutate(name = strsplit(name, ";| see | SEE | check if same as ")) %>%
    tidyr::unnest(name) %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant", remove = F) %>%
    dplyr::mutate(timeperiod = "2017-01-01")

  return(tgn_sf)
}



load_historical <- function(roi, fromscratch=F) {
  if (fromscratch) {
    historical_sf <- read_csv(glue("/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar_TooBig/", "kenya_streamperfect_REX_Pages_1_to_381_final_clean.csv")) %>%
      common_cleaning() %>%
      dplyr::select(c("locationtype", "locationname", "latitude", "longitude")) %>%
      setNames(c("feature_code", "name", "latitude", "longitude")) %>%
      dplyr::mutate(source_dataset = "historical") %>%
      dplyr::mutate(timeperiod = "1964-01-01") %>%
      dplyr::mutate(name_alternates = sapply(strsplit(name, ";| see | SEE | check if same as "), FUN = function(x) paste(x, collapse = ";"))) %>%
      dplyr::mutate(name = strsplit(name, ";| see | SEE | check if same as ")) %>%
     tidyr::unnest(name) %>%
      sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant", remove = F)

    saveRDS(
      historical_sf %>% subset_roi(roi),
      file = glue(dir_package_files, "historical_sf_roi.Rdata")
    )
  }

  historical_sf_roi <- readRDS(system.file("extdata", "historical_sf_roi.Rdata", package = "MeasuringLandscape"))

  return(historical_sf_roi)
}


# Geonames

# Codebook http://geonames.nga.mil/gns/html/gis_countryfiles.html
# Language Code. A three alphabetic character code (ISO 639-3) uniquely identifying the language assigned to a feature name. For a description of these codes/values, please see the "Look-up Tables..." section on the GNS Offered Services page.
# SHORT_FORM	A part of the full name that could substitute for the full name.	Character
# GENERIC	The descriptive part of the full name such as Cerro (mountain), Arroyo (river), or Golfo (gulf) (generally does not apply to populated place names or English based generics).	Character
# SORT_NAME_RO	Sort name - reading order. A form of the full name that allows for alphabetical sorting of the file into gazetteer sequence. For Roman script names, all character/diacritic combinations and special characters are substituted with QWERTY (visible U.S. English keyboard) characters, all characters are upper-cased, numerals are converted to lower-case characters (0-9 = a-j), spaces and hyphens are stripped out, and commas replaced with a space. Additionally, for non-roman script names in languages using Arabic based letters. , vowel markings/pointers are removed. This field is included for the benefit of the end user of the data to aid in the sorting of names if required.	Character
# FULL_NAME_RO	Full name - reading order. The full name is the complete name that identifies a named feature.  The full name is output in reading order, "Mount Everest", vs. reversed generic, "Everest, Mount".	Character
# FULL_NAME_ND_RO	Full name - reading order with no diacritics. Same as the full name but the character/diacritic combinations and special characters are substituted with QWERTY (visible U.S. English keyboard) characters while still maintaining casing and spaces. This field also includes non-roman script based names which are stripped of vowel markings.
# SORT_NAME_RG	Sort name - reversed generic. A form of the full name that allows for alphabetical sorting of the file into gazetteer sequence. For Roman script names, all character/diacritic combinations and special characters are substituted with QWERTY (visible U.S. English keyboard) characters, all characters are upper-cased, numerals are converted to lower-case characters (0-9 = a-j), spaces and hyphens are stripped out, and commas replaced with a space. Additionally, for non-roman script names in languages using Arabic based letters, vowel markings/pointers are removed. This field is included for the benefit of the end user of the data to aid in the sorting of names if required.	Character
# FULL_NAME_RG	Full name - reversed generic. The full name is the complete name that identifies a named feature. The full name is output in reversed generic, "Everest, Mount" vs. reading order, "Mount Everest."	Character
# FULL_NAME_ND_RG	Full name - reversed generic with no diacritics. Same as the full name but the character/diacritic combinations and special characters are substituted with QWERTY (visible U.S. English keyboard) characters while still maintaining casing and spaces. This field also includes non-roman script based names which are stripped of vowel markings.
# Foreign geographic names data is freely available. A suitable citation note is: "Toponymic information is based on the Geographic Names Database, containing official standard names approved by the United States Board on Geographic Names and maintained by the National Geospatial-Intelligence Agency. More information is available at the Maps and Geodata link at www.nga.mil. The National Geospatial-Intelligence Agency name, initials, and seal are protected by 10 United States Code Section 425."
load_geonames <- function(roi, fromscratch=F) {
  if (fromscratch) {
    geonames_sf <- read_tsv(glue("/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar_TooBig/", "geonames_KE.txt")) %>%
      common_cleaning() %>%
      setNames(c(
        "geonameid", "name", "asciiname", "alternatenames", "latitude", "longitude",
        "feature_class", "feature_code", "country_code", "cc2", "admin1_code", "admin2_code",
        "admin3_code", "admin4_code", "population", "elevation", "dem", "timezone", "modification_date"
      )) %>%
      mutate(alternatenames = gsub(",", ";", alternatenames)) %>%
      mutate(name_alternates = paste(asciiname, alternatenames, sep = ";")) %>%
      mutate(name_alternates = gsub(";NA", "", name_alternates, fixed = T)) %>%
      mutate(name = paste(asciiname, alternatenames, sep = ";")) %>%
      select(c("feature_code", "name", "latitude", "longitude", "modification_date", "name_alternates")) %>%
      setNames(c("feature_code", "name", "latitude", "longitude", "timeperiod", "name_alternates")) %>%
      mutate(source_dataset = "geonames") %>%
      mutate(name = gsub(";NA", "", name, fixed = T)) %>%
      mutate(name = strsplit(name, ";| see | SEE | check if same as ")) %>%
     tidyr::unnest(name) %>%
      sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant", remove = F)

    saveRDS(
      geonames_sf %>% subset_roi(roi),
      file = glue(dir_package_files, "geonames_sf_roi.Rdata")
    )
  }

  geonames_sf_roi <- readRDS(system.file("extdata", "geonames_sf_roi.Rdata", package = "MeasuringLandscape"))

  return(geonames_sf_roi)
}

load_nga <- function(roi, fromscratch=F) {
  if (fromscratch) {
    nga_sf <- read_tsv(glue("/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar_TooBig/", "nga_ke.txt"), col_names = T) %>%
      common_cleaning() %>%
      select(c("dsg", "full_name_ro", "lat", "long", "modify_date")) %>%
      setNames(c("feature_code", "name", "latitude", "longitude", "timeperiod")) %>%
      mutate(source_dataset = "nga") %>%
      mutate(name = strsplit(name, ";| see | SEE | check if same as ")) %>%
     tidyr::unnest(name) %>%
      sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant", remove = F)

    saveRDS(
      nga_sf %>% subset_roi(roi),
      file=glue(dir_package_files, "nga_sf_roi.Rdata")
    )
  }

  nga_sf_roi <- readRDS(system.file("extdata", "nga_sf_roi.Rdata", package = "MeasuringLandscape"))

  return(nga_sf_roi)
}




load_googlemaps <- function(roi, fromscratch=F) {
  if (fromscratch) {
    googlemaps_sf <- readRDS(glue("/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar_TooBig/", "googlemaps_cleaned.Rdata")) %>%
      common_cleaning() %>%
      select(c("searchtext", "types_all", "long_name", "location_lat", "location_lng")) %>%
      setNames(c("searchtext", "feature_code", "name", "latitude", "longitude")) %>%
      mutate(source_dataset = "google") %>%
      sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant", remove = F) %>%
      mutate(timeperiod = "2017-01-01")

    saveRDS(
      googlemaps_sf %>% subset_roi(roi),
      file = glue(dir_package_files, "googlemaps_sf_roi.Rdata")
    )
  }

  googlemaps_sf_roi <- readRDS(system.file("extdata", "googlemaps_sf_roi.Rdata", package = "MeasuringLandscape"))

  return(googlemaps_sf_roi)
}

load_bingmaps <- function(roi, fromscratch=F) {
  if (fromscratch) {
    bingmaps_sf <- readRDS(glue("/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar_TooBig/", "bingmaps_df.Rdata")) %>%
      common_cleaning() %>%
      select(c("searchtext", "suggestion_number", "entitytype", "name", "geocodepointscoordinates", "geocodepointscoordinates_2")) %>%
      setNames(c("searchtext", "suggestion_number", "feature_code", "name", "latitude", "longitude")) %>%
      mutate(source_dataset = "bing") %>%
      mutate(name = gsub(", Kenya$|, kenya$", "", name)) %>% # remove , Kenya from the end of the line
      filter(name != "Kenya") %>% # Remove any that are just "Kenya"
      filter(is.na(suggestion_number) | suggestion_number == 1) %>% # Limit to just the first google/bing suggestion
      sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant", remove = F) %>%
      mutate(timeperiod = "2017-01-01")

    saveRDS(
      bingmaps_sf %>% subset_roi(roi),
      file = glue(dir_package_files, "bingmaps_sf_roi.Rdata")
    )
  }

  bingmaps_sf_roi <- readRDS(system.file("extdata", "bingmaps_sf_roi.Rdata", package = "MeasuringLandscape"))

  return(bingmaps_sf_roi)
}




load_ken_adm <- function(roi, fromscratch=F) {
  if (fromscratch) {
    KEN_adm1_sf <- sf::st_read("/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar_TooBig/KEN_adm1.shp", crs = 4326) %>%
      common_cleaning() %>%
      select(c("engtype_1", "name_1", "geometry")) %>%
      setNames(c("feature_code", "name", "geometry")) %>%
      mutate(source_dataset = "gadm")

    KEN_adm1_sf %>%
      sf::st_write(
        "/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar_TooBig/KEN_adm1.gpkg",
        delete_layer = T
      )

    KEN_adm2_sf <- sf::st_read("/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar_TooBig/KEN_adm2.shp", crs = 4326) %>%
      common_cleaning() %>%
      select(c("engtype_2", "name_2", "geometry")) %>%
      setNames(c("feature_code", "name", "geometry")) %>%
      mutate(source_dataset = "gadm")
    KEN_adm2_sf %>%
      sf::st_write(
        "/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar_TooBig/KEN_adm2.gpkg",
        delete_layer = T
      )

    KEN_adm3_sf <- sf::st_read("/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/Violent Events//data/gis_data/KEN_adm3.shp", crs = 4326) %>%
      common_cleaning() %>%
      select(c("engtype_3", "name_3", "geometry")) %>%
      setNames(c("feature_code", "name", "geometry")) %>%
      mutate(source_dataset = "gadm")
    KEN_adm3_sf %>%
      sf::st_write(
        "/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar_TooBig/KEN_adm3.gpkg",
        delete_layer = T
      )

    KEN_adm4_sf <- sf::st_read("/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/Violent Events//data/gis_data/KEN_adm4.shp", crs = 4326) %>%
      common_cleaning() %>%
      select(c("engtype_4", "name_4", "geometry")) %>%
      setNames(c("feature_code", "name", "geometry")) %>%
      mutate(source_dataset = "gadm")
    KEN_adm4_sf %>%
      sf::st_write(
        "/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar_TooBig/KEN_adm4.gpkg",
        delete_layer = T
      )

    KEN_adm5_sf <- sf::st_read("/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/Violent Events//data/gis_data/KEN_adm5.shp", crs = 4326) %>%
      common_cleaning() %>%
      select(c("engtype_5", "name_5", "geometry")) %>%
      setNames(c("feature_code", "name", "geometry")) %>%
      mutate(source_dataset = "gadm")
    KEN_adm5_sf %>%
      sf::st_write(
        "/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar_TooBig/KEN_adm5.gpkg",
        delete_layer = T
      )
    # KEN_adm5 %>% ggplot() + geom_sf()

    KEN_adm_sf <- list(
      KEN_adm1_sf,
      KEN_adm2_sf,
      KEN_adm3_sf,
      KEN_adm4_sf,
      KEN_adm5_sf
    ) %>%
      reduce(rbind) %>%
      mutate(timeperiod = "2017-01-01")

    saveRDS(
      KEN_adm_sf %>% subset_roi(roi),
      file = glue(dir_package_files, "KEN_adm_sf_roi.Rdata")
    )
  }

  KEN_adm_sf_roi <- readRDS(system.file("extdata", "KEN_adm_sf_roi.Rdata", package = "MeasuringLandscape"))

  return(KEN_adm_sf_roi)
}


load_openstreetmap <- function(roi, fromscratch=F) {
  if (fromscratch) {
    openstreetmap_list <- list()
    filenames <- c(
      "gis.osm_buildings_a_free_1.shp", "gis.osm_landuse_a_free_1.shp", "gis.osm_natural_a_free_1.shp", "gis.osm_natural_free_1.shp",
      "gis.osm_places_a_free_1.shp", "gis.osm_places_free_1.shp", "gis.osm_pofw_a_free_1.shp", "gis.osm_pofw_free_1.shp", "gis.osm_pois_a_free_1.shp",
      "gis.osm_pois_free_1.shp", "gis.osm_railways_free_1.shp", "gis.osm_roads_free_1.shp", "gis.osm_traffic_a_free_1.shp", "gis.osm_traffic_free_1.shp",
      "gis.osm_transport_a_free_1.shp", "gis.osm_transport_free_1.shp", "gis.osm_water_a_free_1.shp", "gis.osm_waterways_free_1.shp"
    )

    for (filename in filenames) {
      openstreetmap_list[[filename]] <- sf::st_read(glue("/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar_TooBig/", filename))
    }

    openstreetmap_sf <- openstreetmap_list %>%
      reduce(rbind_sf) %>%
      mutate(timeperiod = "2017-01-01")

    saveRDS(
      openstreetmap_sf %>% subset_roi(roi),
      file = glue(dir_package_files, "openstreetmap_sf_roi.Rdata")
    )
  }

  openstreetmap_sf_roi <- readRDS(system.file("extdata", "openstreetmap_sf_roi.Rdata", package = "MeasuringLandscape")) %>%
    common_cleaning() %>%
    select(c("fclass", "name", "geometry")) %>%
    setNames(c("feature_code", "name", "geometry")) %>%
    mutate(source_dataset = "openstreetmap") %>%
    filter(!is.na(name))

  return(openstreetmap_sf_roi)
}



load_kenya_cadastral <- function(roi, fromscratch=F) {

  # Boundaries
  # https://stackoverflow.com/questions/42287164/install-udunits2-package-for-r3-3
  # install.packages('udunits2',configure.args='--with-udunits2-include=/usr/include/udunits2') #sudo yum install *units2*
  if (fromscratch) {
    p_load(sf)
    kenya_cadastral_sf <- sf::st_read("/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar_TooBig/kenya_cadastral.shp", crs = 4326) %>%
      filter(type %in% c("forest", "lake", "mountain", "Native Reserve", "reserve")) %>%
      common_cleaning() %>%
      mutate(name = paste(district, type)) %>%
      mutate(name = gsub(" NA| regular", "", name, fixed = F)) %>%
      select(c("name", "geometry")) %>%
      group_by(name) %>%
      dplyr::summarize() %>% # This combines polys with the same name into same poly  #https://github.com/r-spatial/sf/issues/290
      mutate(source_dataset = "kenya_cadastral") %>%
      mutate(timeperiod = "1950")

    kenya_cadastral_sf %>% 
      subset_roi(roi) %>% 
      saveRDS(
        file = glue(dir_package_files, "kenya_cadastral_roi.Rdata")
        )
  }

  kenya_cadastral_roi <- readRDS(system.file("extdata", "kenya_cadastral_roi.Rdata", package = "MeasuringLandscape"))

  return(kenya_cadastral_roi)
}

load_kenya_cadastral_district <- function(roi, fromscratch=F) {
  if (fromscratch) {
    kenya_cadastral_district_sf <-
      sf::st_read("/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar_TooBig/kenya_cadastral_district.shp", crs = 4326) %>%
      common_cleaning() %>%
      filter(district != "Temp") %>%
      mutate(name = paste(district, type)) %>%
      mutate(name = gsub(" NA| regular", "", name, fixed = F)) %>%
      select(c("name", "geometry")) %>%
      mutate(source_dataset = "kenya_cadastral_district") %>%
      mutate(feature_code = "district")
  
    kenya_cadastral_district_sf %>% 
      subset_roi(roi) %>% 
      sf::st_write("/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar_TooBig/kenya_cadastral_district.gpkg", delete_layer = T)
    
    kenya_cadastral_district_sf %>% 
      subset_roi(roi) %>% 
      saveRDS(
        file = glue(dir_package_files, "kenya_cadastral_district_roi.Rdata")
        )  

  }
  kenya_cadastral_district_roi <- readRDS(system.file("extdata", "kenya_cadastral_district_roi.Rdata", package = "MeasuringLandscape"))
  
  # kenya_cadastral_district_centroids_sf <- kenya_cadastral_district_sf  %>% st_centroid

  return(kenya_cadastral_district_roi)
}

load_kenya_districts1962 <- function(roi, fromscratch=F) {
  if (fromscratch) {
    kenya_districts1962_sf <- sf::st_read("/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/Violent Events//data/gis_data/kenya_districts1962.shp", crs = 4326) %>%
      common_cleaning() %>%
      setNames(c("name", "geometry")) %>%
      mutate(source_dataset = "kenya_district1962") %>%
      mutate(feature_code = "district")

    kenya_districts1962_sf %>% 
      subset_roi(roi) %>% 
      saveRDS(
        glue(dir_package_files, "kenya_districts1962_roi.Rdata")
        )
  }

  kenya_districts1962_roi <- readRDS(system.file("extdata", "kenya_districts1962_roi.Rdata", package = "MeasuringLandscape"))

  return(kenya_districts1962_roi)
}



load_livestock <- function(roi, fromscratch=F) {

  # Almanac Characterization Tool (ACT) database
  if (fromscratch) {
    livestock_villages <- sf::st_read("/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar_TooBig/kenya_villages.shp", crs = 4326) %>%
      common_cleaning() %>%
      select(c("theme", "name", "geometry")) %>%
      setNames(c("feature_code", "name", "geometry")) %>%
      mutate(source_dataset = "livestock_points")

    livestock_towns <- sf::st_read("/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar_TooBig/kenya_all_towns.shp", crs = 4326) %>%
      common_cleaning() %>%
      select(c("town_type", "town_name", "geometry")) %>%
      setNames(c("feature_code", "name", "geometry")) %>%
      mutate(source_dataset = "livestock_points")

    livestock_locations <- sf::st_read("/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar_TooBig/kenya_locations.shp", crs = 4326) %>%
      common_cleaning() %>%
      mutate(feature_code = "location") %>%
      select(c("feature_code", "location", "geometry")) %>%
      setNames(c("feature_code", "name", "geometry")) %>%
      mutate(source_dataset = "livestock_boundaries")

    livestock_sublocations <- sf::st_read("/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar_TooBig/kenya_sublocations.shp", crs = 4326) %>%
      common_cleaning() %>%
      mutate(feature_code = "sublocations") %>%
      select(c("feature_code", "subloc", "geometry")) %>%
      setNames(c("feature_code", "name", "geometry")) %>%
      mutate(source_dataset = "livestock_boundaries")

    livestock_sf <- rbind(livestock_villages, livestock_towns, livestock_locations, livestock_sublocations) %>%
      mutate(timeperiod = "2000-01-01")

    saveRDS(
      livestock_sf %>% 
      subset_roi(roi),
      glue(dir_package_files, "livestock_sf_roi.Rdata")
    )
  }

  livestock_sf_roi <- readRDS(system.file("extdata", "livestock_sf_roi.Rdata", package = "MeasuringLandscape"))

  return(livestock_sf_roi)
}
