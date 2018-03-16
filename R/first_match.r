# This function calculates the number of characters from the beginning before the first mismatch between two strings
#
#
.datatable.aware=TRUE #You have to declare this at https://github.com/tidyverse/dplyr/issues/548


firstmismatch <- function(a, b, verbose=T) {
  dt <- data.table(a = a,
                   b = b)
  dt <- as.data.table(dt)
  dt[, counts := 0, ] # All counts start as zero matches
  dt[, condition := T, ] # All eligibile from the start
  dt[, nchars1 := nchar(a), by = a] # Stop counting for each when we pass this
  dt[, nchars2 := nchar(b), by = b]


  nchars_max <- max(dt$nchars1)

  for (i in 1:nchars_max) {
    if (verbose) {
      cat("Letter ", i, " ", sum(dt$condition), " left to check;  ")
    }


    dt[
      condition == T,
      a_substring := substr(a, 1, i), # cut first string down
      by = a
    ]

    dt[
      condition == T,
      a_substring_nchar := nchar(trimws(a_substring)), #
      by = a_substring
    ]

    # dt[condition==T,
    #   condition:=condition &
    #              a_substring_nchar <= nchars2 &
    #              grepl(paste0("^",a_substring), b),
    #   by=b]

    dt[
      condition == T,
      condition := condition & # Was already on
        a_substring_nchar <= nchars2 & # Stop counting when longer than second string
        nchars1 >= i & # Stop counting when longer than current count
        startsWith(b, a_substring[1]), # is this substring part of A at the beginning of B
      # grepl(paste0("^",a_substring[1]), b),
      # re2_match(string=b, pattern=paste0("^",a_substring[1]), parallel = T),
      by = a_substring # parallelize over the search pattern, b can be whatever
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
# b=c("murinduko hill forest","murinduko hill","forest murinduko settlement scheme")
# p_load(re2r)
# library(devtools)
# install_github("qinwf/re2r", build_vignettes = T)
# library(re2r)
