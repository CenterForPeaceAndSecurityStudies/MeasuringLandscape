

prep_events <- function(fromscratch=F) {
  if (fromscratch) {
    # Careful running this from scratch because the hash IDs are used downstream and all the code would have to be run again
    events <- read_csv(glue("/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar_TooBig/", "Kenya_Events_SollyStreamPerfect_Original_RexMerged_2015_donebyhand.csv")) %>%
      clean_names() %>%
      remove_empty_rows() %>%
      remove_empty_cols() %>%
      mutate_all(funs(stri_enc_toascii)) %>% # converts everything to character and proper UTF8
      mutate_all(funs(trimws)) %>% # remove whitespace
      distinct()
    events$event_hash <- apply(events, 1, digest, "crc32") # create a unique id

    saveRDS(events, "/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar/inst/extdata/MeasuringLandscapeCivilWar_events_cleaned.Rdata")
  }

  events <- readRDS(system.file("extdata", "MeasuringLandscapeCivilWar_events_cleaned.Rdata", package = "MeasuringLandscapeCivilWar"))
  dim(events)

  return(events)
}


labels_write <- function(data, variable, file, overite=F) {
  x <- data[, variable]
  temp <- as.data.frame(sort(table(x), decreasing = T))
  names(temp) <- "count_original"
  temp$label_original <- rownames(temp)
  temp$label_new <- NA
  if (!file.exists(file) | overite) {
    write.csv(x = temp, file = file, row.names = F, na = "")
  } else {
    print("File Exists, no overite")
  }
}

labels_read <- function(data, variable, file) {
  temp <- read.csv(file)
  rownames(temp) <- as.character(temp$label_original)
  data[, paste0(variable, "_clean")] <- temp[as.character(data[, variable]), "label_new"]
  return(data)
}
