
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
    geonames_all_names_unique_spaced_nofront_df <- readRDS(system.file("extdata", "geonames_all_names_unique_spaced_nofront_df.Rdata", package = "MeasuringLandscapeCivilWar"))
  }

  post_fix_combined <- c(as.character(geonames_all_names_unique_spaced_nofront_df$geonames_all_names_unique_spaced_nofront[geonames_all_names_unique_spaced_nofront_df$N >= cutoff]))
  postfix_handmade <- c(
    postfix_handmade, paste("loc", 1:50), paste("location", 1:50), "loc", "guard", "farm labour", "cattle dip", "children hospital", "education centre", "youth chapter",
    "sublocation", "sub locaticn", "forest block", "sub locat|on", "billiard table", "billiards table", "escarpment reserve", "butchery",
    "sub locatian", "demonstration farm", "mkt", "loc 2", "pump house", "river edge", "rv", "sow mills", "loc2", "loc 1415", "agriculture farm",
    "squatter", "farm timau", "farm makuyu", "arm mweiga", "forest nyeri", "secondary school", "general hardware", "pub", "church centre",
    "guard post", "thika road", "interschool",
    "timber mills", "settled farms",
    "welfare center", "mky", "escarpment forest", "escarp forest", "home guard post", "timber company",
    "mill no1", "rivr", "eatate", "sublocations", "subsection",
    "provincial administration chiefs camp", "hill heading to location", "demonstration farm", "fort hall",
    "ol doinyo", "oldoinyo", "limited estate", "foresters post",
    "community centre", "mini shop", "thermal power station", "community centre", "lava plateau", "clinic and laboratory services",
    "chool historical mt", "fort hall", "snr", "fh", "rivr", "kikuyu camp", "frm", "river area", "fn", "escarpment forest", "entate", "estata", "fort",
    1:1000 # do numbers last
  ) # Add some final postfixes by hand

  postfixesall <- c(post_fix_combined, postfix_handmade)
  postfixesall <- postfixesall[order(nchar(postfixesall), decreasing = T)]
  postfixesall <- setdiff(postfixesall, whitelist)

  return(postfixesall)
}



strip_postfixes <- function(to_be_striped,
                            postfixes=geonames_postfixes(postfix_handmade = c("snr", "s.n.r", "not clear", "<not clear>")),
                            whitelist=c(
                              "fort hall", "nanyuki", "thara", "churo", "coles", "weru", "mcconnell", "mcintyre", "aberdare",
                              "christopher", "cooks", "allan", "kedong", "chuka", "kedong", "davis", "howard", "yara", "chura", "lari", "matara"
                            ),
                            verbose=F) {
  p_load(re2r)
  # to_be_striped[is.na(to_be_striped)] <- ""

  # length(post_fix_combined) #160,052

  postfix_possible <- na.omit(as.character(postfixes[order(nchar(as.character(postfixes)), decreasing = T)]))
  to_be_strip_unique <- na.omit(tolower(unique(to_be_striped)))

  to_be_strip_unique_small <- to_be_strip_unique[endsWith(to_be_strip_unique, paste0(" ", postfix_possible))]

  p_load(parallel)
  detectCores()
  condition <- mclapply(
    postfix_possible,
    FUN = function(q) sum(endsWith(to_be_strip_unique, suffix = q)),
    mc.cores = detectCores()
  )
  postfix_confirmed <- as.character(postfix_possible[condition > 0])
  length(postfix_confirmed) # 2,473 confirmed

  # We do this on the original flatfile so that we can aggregate by stem if we choose
  to_be_striped_cleaner <- trimws(tolower(str_replace_all(to_be_striped, "[[:punct:]]|`", "")))
  to_be_striped_cleaner_nospace <- gsub(" ", "", to_be_striped_cleaner)

  name_cleaner_stemmed <- gsub("-", " ", to_be_striped_cleaner) # remove dashes
  name_cleaner_suffix <- rep("", length(name_cleaner_stemmed))
  name_cleaner_prefix <- rep("", length(name_cleaner_stemmed))


  # First strip prefixes
  prefixes <- c(
    "northern parts of",
    "north of",
    "south of",
    "west of",
    "east of",
    "3 miles south of",
    "1 mile west of",
    "eight miles west of",
    "1 mile nr of",
    "nr",
    "1 mile east of",
    "1 mile nr of",
    "1 mile wof",
    "2 miles from",
    "2 mils nyeri side",
    "25 miles wloc12",
    "3 miles of",
    "3 miles south of",
    "4 miles from",
    "5 miles from",
    "5 miles r of",
    "5 miles south of",
    "8 miles from",
    "across",
    "between",
    "footpath leading to",
    "forest above"
  )
  prefixes <- paste0("^", prefixes, " ")
  prefixes <- prefixes[order(nchar(prefixes), decreasing = T)]
  regexp <- re2(paste(prefixes, collapse = "|"))
  name_cleaner_prefix <- re2_extract(name_cleaner_stemmed, regexp, parallel = T)
  name_cleaner_stemmed <- re2_replace(name_cleaner_stemmed, regexp, replacement = "", parallel = T)

  # I can skip this loop by ordering the regex from bigger to smaller
  # Don't strip anything that ends with something on the white list
  reg <- paste0(" ", postfix_confirmed, "$")
  regexp <- re2(paste(reg, collapse = "|"))

  name_cleaner_suffix <- re2_extract(name_cleaner_stemmed, regexp, parallel = T)

  condition <- !name_cleaner_stemmed %in% whitelist
  name_cleaner_stemmed[condition] <- re2_replace(name_cleaner_stemmed[condition], regexp, replacement = "", parallel = T)

  condition <- !name_cleaner_stemmed %in% whitelist
  name_cleaner_stemmed[condition] <- re2_replace(name_cleaner_stemmed[condition], regexp, replacement = "", parallel = T) # Run it twice

  # condition <- name_cleaner_stemmed %in% postfixes
  # name_cleaner_suffix[condition] <- name_cleaner_stemmed[condition]
  # name_cleaner_stemmed[condition] <- ""

  return(list(
    name_cleaner_stemmed = trimws(name_cleaner_stemmed),
    name_cleaner_suffix = trimws(name_cleaner_suffix),
    name_cleaner_prefix = trimws(name_cleaner_prefix)
  ))
}
