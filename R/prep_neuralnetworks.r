# I'm not masking zeros right now anyway, so the embedding might be using them as is. Maybe just go ahead and add a column to the fron for zeros.
generate_sequences <- function(text, maxlength=20, mask=0, intcutoff=256) {
  text <- paste0("^", text, "$")
  p_load(parallel)
  text_df <- rbindlist(
    mclapply(
      text,
      FUN = function(x) as.data.frame(t(utf8ToInt(x))),
      mc.cores = parallel::detectCores()
    )
    , fill = T
  )
  text_df[is.na(text_df) | text_df > intcutoff] <- mask
  if (ncol(text_df) < maxlength) {
    text_df <- cbind(
      text_df,
      matrix(
        0, nrow(text_df),
        maxlength - ncol(text_df)
      )
    )
  }
  text_df <- text_df[, 1:maxlength]
  return(text_df)
}
