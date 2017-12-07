get_s_curve_new <- function (number_hashfun, n_bands_min = 1, n_rows_per_band_min = 1, 
          plot = interactive()) 
{
  s = seq(0.0, 1, 0.005)
  bands_number <- LSHR:::divisors(number_hashfun)
  rows_per_band <- number_hashfun/bands_number
  i <- bands_number >= n_bands_min & rows_per_band >= n_rows_per_band_min
  bands_number <- bands_number[i]
  rows_per_band <- rows_per_band[i]
  s_curve <- mapply(function(n_band, n_rows_per_band) {
    data.table(probability_become_candidate = 1 - (1 - s^n_rows_per_band)^n_band, 
               similarity = s, n_bands = n_band, n_rows_per_band = n_rows_per_band)
  }, bands_number, rows_per_band, SIMPLIFY = F) %>% rbindlist
  if (plot) {
    g <- ggplot(s_curve) + geom_line(aes(x = similarity, 
                                         y = probability_become_candidate, col = interaction(n_bands, 
                                                                                             n_rows_per_band, sep = " : "))) + scale_color_discrete("bands_number : rows_per_band")
    print(g)
  }
  attr(s_curve, "ggplot") = g
  invisible(s_curve)
}