# These functions involve retrieving string suggestions based on locality sensitive hashing
#
#
#
#
# library(textreuse)
# library(LSHR)
lhs_getpairs <- function(
                         strings,
                         grams,
                         bands_number=400,
                         rows_per_band=5
                         ) {
  
  pairs <- get_similar_pairs(
    grams,
    bands_number = bands_number,   # increase this number, and you'll get more false positives but fewer misses
    rows_per_band = rows_per_band, # basically shrink this number
    distance = "cosine",           # jaccardok cosine is officialy broken just use jaccard
    seed = 1,
    mc.cores = 48,
    verbose = T
  )

  dim(pairs) # 426,604 That's about 9.4 matches per which is almost exactly what we were looking for. 20 bands, 6 rows 344,923,166
  suggestions <- as.data.table(pairs) # pairs[order(-N)]
  dim(suggestions) # 426,604 That's about 9.4 matches per which is almost exactly what we were looking for. 20 bands, 6 rows 344,923,166

  suggestions$a <- strings[suggestions$id1]
  suggestions$b <- strings[suggestions$id2]

  suggestions[, ab := paste(a, b, sep = "_")]
  suggestions[, ba := paste(b, a, sep = "_")]

  return(suggestions)
}



eval_lshr <- function(
                      strings,
                      grams,
                      data,
                      bands_number=400,
                      rows_per_band=5
                      ) {
  
  suggestions <- lhs_getpairs(
                              strings,
                              grams,
                              bands_number,
                              rows_per_band
                              )

  hits <- data$stemmed_ab[data$rex_match == 1] %in% suggestions$ab |
          data$stemmed_ab[data$rex_match == 1] %in% suggestions$ba
  # data$ab[data$rex_match==1][hits]
  misses <- !data$stemmed_ab[data$rex_match == 1] %in% suggestions$ab &
            !data$stemmed_ab[data$rex_match == 1] %in% suggestions$ba
  # data$ab[data$rex_match==1][misses] #Anything that's an exact match is being counted as a miss because suggestions don't count themselves

  results <- data.frame(
    hits = sum(hits),
    misses = sum(misses),
    suggestions = nrow(suggestions),
    qgrams = ncol(grams),
    bands_number = bands_number,
    rows_per_band = rows_per_band,
    suggestions_per = round(nrow(suggestions) / nrow(grams))
  )
  return(results)
}
