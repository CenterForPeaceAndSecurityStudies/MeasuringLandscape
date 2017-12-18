
.datatable.aware=TRUE #You have to declare this at https://github.com/tidyverse/dplyr/issues/548

# This function takes in a dataframe with two strings, a and b, and then calculates string distance features on them.
p_load(stringdist)
toponym_add_distances_dt <- function(dt, fromscratch=F, nthread=parallel::detectCores()) {
  dt <- as.data.table(dt)

  print("1 of 24")
  dt[, First_Mistmatch := firstmismatch(a, b, verbose = F)]
  print("2 of 24")
  dt[, Jaro := stringsim(a, b, "jw", nthread = nthread), ]
  print("3 of 24")
  dt[, Optimal_String_Alignment := stringsim(a, b, "osa", nthread = nthread), ]
  print("4 of 24")
  dt[, Levenshtein := stringsim(a, b, "lv", nthread = nthread), ]
  print("5 of 24")
  dt[, Damerau_Levenshtein := stringsim(a, b, "dl", nthread = nthread), ]
  print("6 of 24")
  dt[, Longest_Common_Substring := stringsim(a, b, "lcs", nthread = nthread), ]
  print("7 of 24")
  dt[, q_gram_1 := stringsim(a, b, "qgram", nthread = nthread, q = 1), ]
  print("8 of 24")
  dt[, q_gram_2 := stringsim(a, b, "qgram", nthread = nthread, q = 2), ]
  print("9 of 24")
  dt[, q_gram_3 := stringsim(a, b, "qgram", nthread = nthread, q = 3), ]
  print("10 of 24")
  dt[, q_gram_4 := stringsim(a, b, "qgram", nthread = nthread, q = 4), ]
  print("11 of 24")
  dt[, q_gram_5 := stringsim(a, b, "qgram", nthread = nthread, q = 5), ]
  print("12 of 24")
  dt[, Cosine_1 := stringsim(a, b, "cosine", nthread = nthread, q = 1), ]
  dt[, Cosine_2 := stringsim(a, b, "cosine", nthread = nthread, q = 2), ]
  dt[, Cosine_3 := stringsim(a, b, "cosine", nthread = nthread, q = 3), ]
  dt[, Cosine_4 := stringsim(a, b, "cosine", nthread = nthread, q = 4), ]
  dt[, Cosine_5 := stringsim(a, b, "cosine", nthread = nthread, q = 5), ]
  print("13 of 24")
  dt[, Jaccard := stringsim(a, b, "jaccard", nthread = nthread), ]
  print("14 of 24")
  dt$a_nchar <- nchar(dt$a)
  print("15 of 24")
  dt$b_nchar <- nchar(dt$b)
  print("16 of 24")
  dt$ab_nchar_diff <- abs(dt$a_nchar - dt$b_nchar)
  print("17 of 24")
  dt[, dJaro := stringdist(a, b, "jw", nthread = nthread), ]
  print("18 of 24")
  dt[, dOptimal_String_Alignment := stringdist(a, b, "osa", nthread = nthread), ]
  print("19 of 24")
  dt[, dLevenshtein := stringdist(a, b, "lv", nthread = nthread), ]
  print("20 of 24")
  dt[, dDamerau_Levenshtein := stringdist(a, b, "dl", nthread = nthread), ]
  print("21 of 24")
  dt[, dLongest_Common_Substring := stringdist(a, b, "lcs", nthread = nthread), ]
  print("22 of 24")
  dt[, dq_gram := stringdist(a, b, "qgram", nthread = nthread), ]
  print("23 of 24")
  dt[, dCosine := stringdist(a, b, "cosine", nthread = nthread), ]
  print("24 of 24")
  dt[, dJaccard := stringdist(a, b, "jaccard", nthread = nthread), ]

  # p_load(TraMineR)
  # http://traminer.unige.ch/doc/seqdef.html

  # labels_unique <- sort(unique(c(dt$a, dt$b )))
  # sequences_a_df <- generate_sequences( labels_unique,
  #                                      maxlength=20,
  #                                      mask=NA,
  #                                      intcutoff=256) #takes about a minute+


  # processed_a <- seqdef( data= sequences_a_df )

  # dt$a_numeric <- as.numeric(factor(dt$a, levels= labels_unique ))
  # dt$b_numeric <- as.numeric(factor(dt$b, levels= labels_unique ))
  #
  #   costs <- seqcost(processed_a, method = "INDELSLOG") #I think we calculate this on the training data, save it, and pass it in at ru ntime
  #
  #   dist_OM <- seqdist(processed_a, method = "OM", indel = costs$indel, sm = costs$sm , norm="auto")
  #   dt[,OM:=dist_OM[ cbind(a_numeric, b_numeric) ],]
  #
  #   dist_OMloc <- seqdist(processed_a, method = "OMloc", indel = costs.tr$indel, sm = costs.tr$sm, with.missing = F) #intentionally no norm
  #   dt[,OMloc:=dist_OMloc[ cbind(a_numeric, b_numeric) ],]
  #
  #   dist_OMslen <- seqdist(processed_a, method = "OMslen", indel = costs.tr$indel, sm = costs.tr$sm, with.missing = F )#intentionally no norm
  #   dt[,OMslen:=dist_OMslen[ cbind(a_numeric, b_numeric) ],]
  #
  #   dist_OMspell <- seqdist(processed_a, method = "OMspell", sm = costs.tr$sm, indel = 1, with.missing = TRUE )#intentionally no norm
  #   dt[,OMspell:=dist_OMspell[ cbind(a_numeric, b_numeric) ],]
  #
  #   #ex1.OMstran <- seqdist(processed_a, method = "OMstran", sm = costs.tr$sm, indel = 1, with.missing = TRUE, otto=.5)#intentionally no norm
  #   dist_TWED <- seqdist(processed_a, method = "TWED", sm = costs.tr$sm, indel = 1, with.missing = TRUE , nu=.5)#intentionally no norm
  #   dt[,TWED:=dist_TWED[ cbind(a_numeric, b_numeric) ],]
  #
  #   dist_LCS <- seqdist(processed_a, method = "LCS", norm = "auto")
  #   dt[,LCS:=dist_LCS[ cbind(a_numeric, b_numeric) ],]
  #
  #   dist_LCP <- seqdist(processed_a, method = "LCP", norm = "auto")
  #   dt[,LCP:=dist_LCP[ cbind(a_numeric, b_numeric) ],]
  #
  #   dist_RLCP <- seqdist(processed_a, method = "RLCP", norm = "auto")
  #   dt[,RLCP:=dist_RLCP[ cbind(a_numeric, b_numeric) ],]
  #
  #   dist_NMS <- seqdist(processed_a, method = "NMS") #intentionally no norm
  #   dt[,NMS:=dist_NMS[ cbind(a_numeric, b_numeric) ],]
  #
  #   dist_NMSMST <- seqdist(processed_a, method = "NMSMST") #intentionally no norm
  #   dt[,NMSMST:=dist_NMSMST[ cbind(a_numeric, b_numeric) ],]
  #
  #   dist_SVRspell <- seqdist(processed_a, method = "SVRspell") #intentionally no norm #this one takes too long to process
  #   dt[,SVRspell:=dist_SVRspell[ cbind(a_numeric, b_numeric) ],]
  #
  #   dist_CHI2 <- seqdist(processed_a, method = "CHI2", step = max(seqlength(processed_a)))
  #   dt[,CHI2:=dist_CHI2[ cbind(a_numeric, b_numeric) ],]

  return(dt)
}

# This is for corpus features that are no longer used in the paper
toponym_add_corpus <- function(data, fromscratch=F) {
  data <- as.data.table(data)

  # Load the ngram corpus and use it to look up things
  # Takes a while to load even with fst
  p_load(fst)

  # Add feature counts from the gazzetter
  if (fromscratch) {
    csv_dt_grams_all <- read.fst("/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar/inst/extdata/csv_dt_grams_all.fst", as.data.table = T) # this is expensive
    head(csv_dt_grams_all)
    setkey(csv_dt_grams_all, bigram) # this is going to take a while, and then we're going to pull every single flatfile and event name_cleaner and save that as a smaller file for next time
    csv_dt_grams_places <- csv_dt_grams_all[bigram %in% unique(c(events_dt$name_cleaner, flatfiles_sf$name_cleaner)), ]
    dim(csv_dt_grams_places) # 20k matches
    fwrite(csv_dt_grams_places, "/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar/inst/extdata/csv_dt_grams_places.csv")
  }
  csv_dt_grams_places <- fread("/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar/inst/extdata/csv_dt_grams_places.csv")

  csv_dt_grams_places_a <- csv_dt_grams_places[, c("bigram", "count", "year_min", "year_median", "year_mean", "year_max")]
  names(csv_dt_grams_places_a) <- c("bigram_a", "corpus_mention_count_a", "corpus_mention_year_min_a", "corpus_mention_year_median_a", "corpus_mention_year_mean_a", "corpus_mention_year_max_a")
  csv_dt_grams_places_b <- csv_dt_grams_places[, c("bigram", "count", "year_min", "year_median", "year_mean", "year_max")]
  names(csv_dt_grams_places_b) <- c("bigram_b", "corpus_mention_count_b", "corpus_mention_year_min_b", "corpus_mention_year_median_b", "corpus_mention_year_mean_b", "corpus_mention_year_max_b")

  data <- merge(data, csv_dt_grams_places_a, by.x = "a", by.y = "bigram_a", all.x = T)
  data <- merge(data, csv_dt_grams_places_b, by.x = "b", by.y = "bigram_b", all.x = T)



  # Count number of mentions across gazeteers
  if (fromscratch) {
    flatfiles_sf <- readRDS(system.file("extdata", "flatfiles_sf.Rdata", package = "MeasuringLandscape"))
    temp <- strip_postfixes(flatfiles_sf$name_cleaner)
    flatfiles_sf$name_cleaner_stem <- temp[[1]]

    fwrite(as.data.frame(flatfiles_sf)[
      !flatfiles_sf$source_dataset %in% c("events", "events_poly"),
      c("name_cleaner", "name_cleaner_stem")
    ], "/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar/inst/extdata/gazeteer_grams_noevents.csv")
  }
  gazeteer_grams_noevents <- fread("/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar/inst/extdata/gazeteer_grams_noevents.csv")

  temp <- gazeteer_grams_noevents[, list(gazeteer_mentions_count = .N), by = name_cleaner]
  temp2 <- gazeteer_grams_noevents[, list(gazeteer_stem_mentions_count = .N), by = name_cleaner_stem]

  data <- merge(
    data,
    setnames(temp, c("name_cleaner", "gazeteer_mentions_count_a")),
    by.x = "a",
    by.y = "name_cleaner",
    all.x = T
  )
  data <- merge(
    data,
    setnames(temp, c("name_cleaner", "gazeteer_mentions_count_b")),
    by.x = "b",
    by.y = "name_cleaner",
    all.x = T
  )

  data <- merge(
    data,
    setnames(temp2, c("name_cleaner_stem", "gazeteer_stem_mentions_count_a")),
    by.x = "a",
    by.y = "name_cleaner_stem",
    all.x = T
  )
  data <- merge(
    data,
    setnames(temp2, c("name_cleaner_stem", "gazeteer_stem_mentions_count_b")),
    by.x = "b",
    by.y = "name_cleaner_stem",
    all.x = T
  )

  # I should fill in NAs
  data$corpus_mention_count_a[is.na(data$corpus_mention_count_a)] <- 0
  data$corpus_mention_count_b[is.na(data$corpus_mention_count_b)] <- 0

  data$gazeteer_mentions_count_a[is.na(data$gazeteer_mentions_count_a)] <- 0
  data$gazeteer_mentions_count_b[is.na(data$gazeteer_mentions_count_b)] <- 0

  data$gazeteer_stem_mentions_count_a[is.na(data$gazeteer_stem_mentions_count_a)] <- 0
  data$gazeteer_stem_mentions_count_b[is.na(data$gazeteer_stem_mentions_count_b)] <- 0


  return(data)
}

toponym_add_features <- function(dt, fromscratch=F) {
  dt <- as.data.table(dt)
  dt <- toponym_add_distances_dt(dt)
  # data <- toponym_add_corpus(data) #Currently excluded from paper

  return(dt)
}
