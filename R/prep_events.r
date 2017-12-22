

prep_events <- function(fromscratch=F) {
  if (fromscratch) {
    
    #This csv file does not exist on the github
    #We're holding it back so that the hash applied to each observation never changes
    #The rdata MeasuringLandscapeCivilWar_events_cleaned.Rdata contains all the same information
    events <- readr::read_csv(glue::glue("C:/Users/skynetmini/Dropbox (rex)/MSSL/projects/MeasuringLandscape/MeasuringLandscapeCivilWar_TooBig/",
                            "Kenya_Events_SollyStreamPerfect_Original_RexMerged_2015_donebyhand.csv")) %>%
      janitor::clean_names() %>%
      janitor::remove_empty_rows() %>%
      janitor::remove_empty_cols() %>%
      mutate_all(funs(stringi::stri_enc_toascii)) %>% # converts everything to character and proper UTF8
      mutate_all(funs(trimws)) %>% # remove whitespace
      distinct()
    events$event_hash <- apply(events, 1, digest::digest, "crc32") # create a unique id

    saveRDS(events, "C:/rex_github_private/MeasuringLandscape/inst/extdata/MeasuringLandscapeCivilWar_events.Rds")
  }

  events <- readRDS(system.file("extdata", "MeasuringLandscapeCivilWar_events.Rds", package = "MeasuringLandscape"))
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
