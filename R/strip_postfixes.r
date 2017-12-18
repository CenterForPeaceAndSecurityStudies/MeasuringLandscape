#
#This function strips common postifxes from toponyms
#It takes in a vector of toponyms
#And returns a list of original toponyms, stripped toponyms, and the postfixes that were removed


strip_postfixes <- function(to_be_striped,
                            postfixes=geonames_postfixes(postfix_handmade = c("snr", "s.n.r", "not clear", "<not clear>")),
                            whitelist=c(
                              "fort hall", "nanyuki", "thara", "churo", "coles", "weru", "mcconnell", "mcintyre", "aberdare",
                              "christopher", "cooks", "allan", "kedong", "chuka", "kedong", "davis", "howard", "yara", "chura",
                              "lari", "matara"
                            ),
                            verbose=F) {
  #p_load(re2r)
  # to_be_striped[is.na(to_be_striped)] <- ""

  # length(post_fix_combined) #160,052

  postfix_possible <- na.omit(as.character(postfixes[order(nchar(as.character(postfixes)), decreasing = T)]))
  to_be_strip_unique <- na.omit(tolower(unique(to_be_striped)))

  to_be_strip_unique_small <- to_be_strip_unique[endsWith(to_be_strip_unique, paste0(" ", postfix_possible))]


  #condition <- sapply(postfix_possible, function(q) sum(endsWith(to_be_strip_unique, suffix = q)))
  
  #p_load(doSNOW)
  check_any_ending <- function(q){
    return(
      sapply(q,
             function(q) sum(endsWith(to_be_strip_unique, suffix = q)))
    )
  }
  

  d=length(postfix_possible)/parallel::detectCores()
  chunks <- split(
                  postfix_possible,
                  ceiling(seq_along(1:length(postfix_possible))/d
              ))

  #library(doParallel)
  #library(foreach)
  #`%dopar%` <- foreach::`%dopar%`
  #`%do%` <- foreach::`%do%`
  
  cl<- parallel::makeCluster(parallel::detectCores()) #change the 2 to your number of CPU cores
  #doParallel::registerDoParallel(cl)
  
  #condition <- foreach::foreach(q=chunks, .combine='c') %dopar%  check_any_ending(q)
  #parallel::clusterExport(cl, c("check_any_ending"))
  condition <-unlist(parallel::parLapply(cl=cl,chunks, fun=function(q) check_any_ending(q)))
  #doParallel::stopCluster(cl)
  parallel::stopCluster(cl)
  
  
  
  #parallel::detectCores()
  #condition <-   mclapply(
  #  postfix_possible,
  #  FUN = function(q) sum(endsWith(to_be_strip_unique, suffix = q)),
  #  mc.cores = parallel::detectCores()
  #)
  postfix_confirmed <- as.character(postfix_possible[condition > 0])
  length(postfix_confirmed) # 2,473 confirmed

  # We do this on the original flatfile so that we can aggregate by stem if we choose
  to_be_striped_cleaner <- trimws(tolower(stringr::str_replace_all(to_be_striped, "[[:punct:]]|`", "")))
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
  regexp <- re2r::re2(paste(prefixes, collapse = "|"))
  name_cleaner_prefix <- re2r::re2_extract(name_cleaner_stemmed, regexp, parallel = T)
  name_cleaner_stemmed <- re2r::re2_replace(name_cleaner_stemmed, regexp, replacement = "", parallel = T)

  # I can skip this loop by ordering the regex from bigger to smaller
  # Don't strip anything that ends with something on the white list
  reg <- paste0(" ", postfix_confirmed, "$")
  regexp <- re2r::re2(paste(reg, collapse = "|"))

  name_cleaner_suffix <- re2r::re2_extract(name_cleaner_stemmed, regexp, parallel = T)

  condition <- !name_cleaner_stemmed %in% whitelist
  name_cleaner_stemmed[condition] <- re2r::re2_replace(name_cleaner_stemmed[condition], regexp, replacement = "", parallel = T)

  condition <- !name_cleaner_stemmed %in% whitelist
  name_cleaner_stemmed[condition] <- re2r::re2_replace(name_cleaner_stemmed[condition], regexp, replacement = "", parallel = T) # Run it twice

  # condition <- name_cleaner_stemmed %in% postfixes
  # name_cleaner_suffix[condition] <- name_cleaner_stemmed[condition]
  # name_cleaner_stemmed[condition] <- ""

  return(list(
    name_cleaner_stemmed = trimws(name_cleaner_stemmed),
    name_cleaner_suffix = trimws(name_cleaner_suffix),
    name_cleaner_prefix = trimws(name_cleaner_prefix)
  ))
}
