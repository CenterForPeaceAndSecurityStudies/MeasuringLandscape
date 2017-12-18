# library(pacman)
# p_load(dplyr)
# p_load(tidytext)
# p_load(janeaustenr)
# p_load(data.table)

# geonames_postfixes(fromscratch=T)

# This function generates a list of toponym postfixes that appear globally a certain number of times
geonames_postfixes <- function(fromscratch=F,
                               cutoff=5,
                               postfix_handmade=c(),
                               maxlength=50,
                               whitelist=c("north", "south", "east", "west"),
                               verbose=F) {
  if (fromscratch) {
    print("Generating From Scratch")
    geonames_all <- fread("/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar_TooBig/allCountries.txt", sep = "\t", data.table = T)
    names(geonames_all) <- c(
      "geonameid", "name", "asciiname", "alternatenames", "latitude", "longitude",
      "feature_class", "feature_code", "country_code", "cc2", "admin1_code", "admin2_code",
      "admin3_code", "admin4_code", "population", "elevation", "dem", "timezone", "modification_date"
    )
    geonames_all[, alternatenames := gsub(",", ";", alternatenames), ]
    
    geonames_all_alternatenames <- unlist(strsplit(geonames_all$alternatenames, ";"))
    geonames_all_names_unique <- unique(trimws(tolower(c(geonames_all$name, geonames_all$asciiname, geonames_all_alternatenames))))
    length(geonames_all_names_unique) # 14,885,300
    
    geonames_all_names_unique_spaced <- geonames_all_names_unique[grepl(" ", geonames_all_names_unique)]
    length(geonames_all_names_unique_spaced) # 8,756,449
    
    geonames_all_names_unique_spaced_nofront <- trimws(gsub("^[^ ]+", "", geonames_all_names_unique_spaced, perl = T)) # remove first word
    lengths <- nchar(geonames_all_names_unique_spaced_nofront)
    geonames_all_names_unique_spaced_nofront <- geonames_all_names_unique_spaced_nofront[lengths < maxlength]
    geonames_all_names_unique_spaced_nofront_df <- as.data.table(table(geonames_all_names_unique_spaced_nofront)) # %>% filter(Freq>=2)
    
    setkey(geonames_all_names_unique_spaced_nofront_df, N)
    
    saveRDS(
      geonames_all_names_unique_spaced_nofront_df,
      file = "/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar/inst/extdata/geonames_all_names_unique_spaced_nofront_df.Rdata"
    )
  } else {
    print("Loading Preexisting")
    geonames_all_names_unique_spaced_nofront_df <- readRDS(system.file("extdata", "geonames_all_names_unique_spaced_nofront_df.Rdata", package = "MeasuringLandscape"))
  }
  
  post_fix_combined <- c(as.character(geonames_all_names_unique_spaced_nofront_df$geonames_all_names_unique_spaced_nofront[geonames_all_names_unique_spaced_nofront_df$N >= cutoff]))
  postfix_handmade <- c(
    postfix_handmade, paste("loc", 1:50),
    paste("location", 1:50),
    "loc", "guard", "farm labour", "cattle dip", "children hospital", "education centre", "youth chapter",
    "sublocation", "sub locaticn", "forest block", "sub locat|on", "billiard table", "billiards table",
    "escarpment reserve", "butchery",
    "sub locatian", "demonstration farm", "mkt", "loc 2", "pump house", "river edge", "rv", "sow mills", "loc2",
    "loc 1415", "agriculture farm",
    "squatter", "farm timau", "farm makuyu", "arm mweiga", "forest nyeri", "secondary school", "general hardware", 
    "pub", "church centre",
    "guard post", "thika road", "interschool",
    "timber mills", "settled farms",
    "welfare center", "mky", "escarpment forest", "escarp forest", "home guard post", "timber company",
    "mill no1", "rivr", "eatate", "sublocations", "subsection",
    "provincial administration chiefs camp", "hill heading to location", "demonstration farm", "fort hall",
    "ol doinyo", "oldoinyo", "limited estate", "foresters post",
    "community centre", "mini shop", "thermal power station", "community centre", "lava plateau", "clinic and laboratory services",
    "chool historical mt", "fort hall", "snr", "fh", "rivr", "kikuyu camp", "frm", "river area", "fn", "escarpment forest", 
    "entate", "estata", "fort",
    1:1000 # do numbers last
  ) # Add some final postfixes by hand
  
  postfixesall <- c(post_fix_combined, postfix_handmade)
  postfixesall <- postfixesall[order(nchar(postfixesall), decreasing = T)]
  postfixesall <- setdiff(postfixesall, whitelist)
  
  return(postfixesall)
}

