# This function calculates the number of characters from the beginning before the first mismatch between two strings
#
#

firstmismatch <- function(string1, string2, verbose=T) {
  dt <- data.table(string1 = string1, string2 = string2)
  dt[, counts := 0, ] # All counts start as zero matches
  dt[, condition := T, ] # All eligibile from the start
  dt[, nchars1 := nchar(string1), by = string1] # Stop counting for each when we pass this
  dt[, nchars2 := nchar(string2), by = string2]


  nchars_max <- max(dt$nchars1)
  for (i in 1:nchars_max) {
    if (verbose) {
      cat("Letter ", i, " ", sum(dt$condition), " left to check;  ")
    }


    dt[
      condition == T,
      string1_substring := substr(string1, 1, i), # cut first string down
      by = string1
    ]

    dt[
      condition == T,
      string1_substring_nchar := nchar(trimws(string1_substring)), #
      by = string1_substring
    ]

    # dt[condition==T,
    #   condition:=condition &
    #              string1_substring_nchar <= nchars2 &
    #              grepl(paste0("^",string1_substring), string2),
    #   by=string2]

    dt[
      condition == T,
      condition := condition & # Was already on
        string1_substring_nchar <= nchars2 & # Stop counting when longer than second string
        nchars1 >= i & # Stop counting when longer than current count
        startsWith(string2, string1_substring[1]), # is this substring part of A at the beginning of B
      # grepl(paste0("^",string1_substring[1]), string2),
      # re2_match(string=string2, pattern=paste0("^",string1_substring[1]), parallel = T),
      by = string1_substring # parallelize over the search pattern, string2 can be whatever
    ]

    dt[
      condition == T,
      counts := counts + 1,
    ]
  }

  return(dt$counts)
}


# Ok this piece of code counts until the first mismatch. It's pretty fast, so can do between each pair if necessary
# temp <- firstmismatch("murinduko sub-location",flatfiles_unique$name_clean)
# tail(flatfiles_unique$name_clean[order(temp)])


# Quick function to count the number of characters of overlap up front only
# string2=c("murinduko hill forest","murinduko hill","forest murinduko settlement scheme")
# p_load(re2r)
# library(devtools)
# install_github("qinwf/re2r", build_vignettes = T)
# library(re2r)
