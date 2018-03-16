# This function takes in a vector of strings and ids, and returns a ngram matrix with the request number of grams and skipgrams

library(pacman)
p_load(dissUtils)
p_load(dplyr)
p_load(tidytext)
p_load(janeaustenr)

qgram_hash <- function(strings, n=2, k=0) {
  strings <- as.character(strings)

  p_load(quanteda)
  gram_list <- list()
  for (i in 1:n) {
    for (k in 0:k) {
      if (i > 1 | k == 0) {
        index <- paste("dfm", i, k, sep = "_")
        gram_list[[index]] <- dfm(quanteda::tokens(strings, what = "character", ngrams = i, skip = k, concatenator = "_", hash = F))
      }
    }
  }

  dfm_all <- purrr::reduce(gram_list, cbind)
  dtm <- as(dfm_all, "dgCMatrix")
  gram_list <- NULL
  gc()

  return(dtm)
}



# data <- data.frame(string=strings, stringsAsFactors=F )
# data$string_character <- sapply( strsplit(data$string, split=''),  paste, collapse=" ")
#
# grams2 <- data %>%tidyr::unnest_tokens(bigram, string_character, token = "ngrams", n = 2) %>% mutate(ones=1) %>% as.data.table()
# grams2$bigram <- paste(substring(grams2$bigram, 1, 1), "0", substring(grams2$bigram, 3), sep = "")  #number of skips
# grams2 <- dcast(grams2, string ~ bigram, fun=sum, value.var="ones")
#
# grams_21 <- data %>%tidyr::unnest_tokens(bigram, string_character, token = "skip_ngrams", n = 2, k = 1) %>% mutate(ones=1) %>% as.data.table()
# grams_21$bigram <- paste(substring(grams_21$bigram, 1, 1), "1", substring(grams_21$bigram, 3), sep = "")  #number of skips
# grams_21 <- dcast(grams_21, string ~ bigram, fun=sum, value.var="ones")
# grams_wide <- merge(grams2, grams_21, by=c("string"), all=T)
#
# grams_22 <- data %>%tidyr::unnest_tokens(bigram, string_character, token = "skip_ngrams", n = 2, k = 2) %>% mutate(ones=1) %>% as.data.table()
# grams_22$bigram <- paste(substring(grams_22$bigram, 1, 1), "2", substring(grams_22$bigram, 3), sep = "")  #number of skips
# grams_22 <- dcast(grams_22, string ~ bigram, fun=sum, value.var="ones")
# grams_wide <- merge(grams_wide, grams_22, by=c("string"), all=T)
#
# grams_23 <- data %>%tidyr::unnest_tokens(bigram, string_character, token = "skip_ngrams", n = 2, k = 3) %>% mutate(ones=1) %>% as.data.table()
# grams_23$bigram <- paste(substring(grams_23$bigram, 1, 1), "3", substring(grams_23$bigram, 3), sep = "")  #number of skips
# grams_23 <- dcast(grams_23, string ~ bigram, fun=sum, value.var="ones")
# grams_wide <- merge(grams_wide, grams_23, by=c("string"), all=T)
#
# grams_24 <- data %>%tidyr::unnest_tokens(bigram, string_character, token = "skip_ngrams", n = 2, k = 4) %>% mutate(ones=1) %>% as.data.table()
# grams_24$bigram <- paste(substring(grams_24$bigram, 1, 1), "4", substring(grams_24$bigram, 3), sep = "")  #number of skips
# grams_24 <- dcast(grams_24, string ~ bigram, fun=sum, value.var="ones")
# grams_wide <- merge(grams_wide, grams_24, by=c("string"), all=T)
#
# grams3 <- data %>%tidyr::unnest_tokens(bigram, string_character, token = "ngrams", n = 3) %>% mutate(ones=1) %>% as.data.table()
# grams3$bigram <- paste(substring(grams3$bigram, 1, 1), "0", substring(grams3$bigram, 3), sep = "")  #number of skips
# grams3 <- dcast(grams3, string ~ bigram, fun=sum, value.var="ones")
# grams_wide <- merge(grams_wide, grams3, by=c("string"), all=T)
#
# grams31 <- data %>%tidyr::unnest_tokens(bigram, string_character, token = "skip_ngrams", n = 3, k = 1) %>% mutate(ones=1) %>% as.data.table()
# grams31$bigram <- paste(substring(grams31$bigram, 1, 1), "1", substring(grams31$bigram, 3), sep = "")  #number of skips
# grams31 <- dcast(grams31, string ~ bigram, fun=sum, value.var="ones")
# grams_wide <- merge(grams_wide, grams31, by=c("string"), all=T)
#
# grams32 <- data %>%tidyr::unnest_tokens(bigram, string_character, token = "skip_ngrams", n = 3, k = 2) %>% mutate(ones=1) %>% as.data.table()
# grams32$bigram <- paste(substring(grams32$bigram, 1, 1), "2", substring(grams32$bigram, 3), sep = "")  #number of skips
# grams32 <- dcast(grams32, string ~ bigram, fun=sum, value.var="ones")
# grams_wide <- merge(grams_wide, grams32, by=c("string"), all=T)
#
# grams33 <- data %>%tidyr::unnest_tokens(bigram, string_character, token = "skip_ngrams", n = 3, k = 3) %>% mutate(ones=1) %>% as.data.table()
# grams33$bigram <- paste(substring(grams33$bigram, 1, 1), "3", substring(grams32$bigram, 3), sep = "")  #number of skips
# grams33 <- dcast(grams33, string ~ bigram, fun=sum, value.var="ones")
# grams_wide <- merge(grams_wide, grams33, by=c("string"), all=T)
#
# grams34 <- data %>%tidyr::unnest_tokens(bigram, string_character, token = "skip_ngrams", n = 3, k = 4) %>% mutate(ones=1) %>% as.data.table()
# grams34$bigram <- paste(substring(grams34$bigram, 1, 1), "4", substring(grams32$bigram, 3), sep = "")  #number of skips
# grams34 <- dcast(grams34, string ~ bigram, fun=sum, value.var="ones")
# grams_wide <- merge(grams_wide, grams34, by=c("string"), all=T)
#
# grams4 <- data %>%tidyr::unnest_tokens(bigram, string_character, token = "ngrams", n = 4) %>% mutate(ones=1) %>% as.data.table()
# grams4$bigram <- paste(substring(grams4$bigram, 1, 1), "0", substring(grams4$bigram, 3), sep = "")  #number of skips
# grams4 <- dcast(grams4, string ~ bigram, fun=sum, value.var="ones")
# grams_wide <- merge(grams_wide, grams4, by=c("string"), all=T)
#
# grams41 <- data %>%tidyr::unnest_tokens(bigram, string_character, token = "skip_ngrams", n = 3, k = 1) %>% mutate(ones=1) %>% as.data.table()
# grams41$bigram <- paste(substring(grams41$bigram, 1, 1), "1", substring(grams32$bigram, 3), sep = "")  #number of skips
# grams41 <- dcast(grams41, string ~ bigram, fun=sum, value.var="ones")
# grams_wide <- merge(grams_wide, grams41, by=c("string"), all=T)
#
# grams42 <- data %>%tidyr::unnest_tokens(bigram, string_character, token = "skip_ngrams", n = 3, k = 2) %>% mutate(ones=1) %>% as.data.table()
# grams42$bigram <- paste(substring(grams42$bigram, 1, 1), "2", substring(grams32$bigram, 3), sep = "")  #number of skips
# grams42 <- dcast(grams42, string ~ bigram, fun=sum, value.var="ones")
# grams_wide <- merge(grams_wide, grams42, by=c("string"), all=T)
#
# grams43 <- data %>%tidyr::unnest_tokens(bigram, string_character, token = "skip_ngrams", n = 3, k = 3) %>% mutate(ones=1) %>% as.data.table()
# grams43$bigram <- paste(substring(grams43$bigram, 1, 1), "3", substring(grams32$bigram, 3), sep = "")  #number of skips
# grams43 <- dcast(grams43, string ~ bigram, fun=sum, value.var="ones")
# grams_wide <- merge(grams_wide, grams43, by=c("string"), all=T)
#
# grams_wide_m <- as.matrix(grams_wide[,-c(1)])
# rownames(grams_wide_m) <- grams_wide$string


# stemmed_ab_character <- sapply( strsplit(stemmed_ab, split=''),  paste, collapse=" ")
# it <- itoken(stemmed_ab, preprocess_function = tolower, tokenizer = char_tokenizer)
# v = create_vocabulary(it, c(1L, 5L) )
# dtm <- create_dtm(it, vectorizer=vocab_vectorizer(v)) ; dim(dtm)
# dtm_RsparseMatrix = as(dtm, "RsparseMatrix")
# dtm_m <- as.matrix(dtm) > 0
#
# temp <- tokenize_skip_ngrams(stemmed_ab_character[100], n=2,k=1)
# sapply(stemmed_ab_character, tokenize_skip_ngrams, n=2,k=1)
#
# it_2_5 <- itoken(stemmed_ab_character, preprocess_function = tolower, tokenizer = tokenize_skip_ngrams, n=2,k=0)
# v = create_vocabulary(it_2_5)
# dtm <- create_dtm(it, vectorizer=vocab_vectorizer(v)) ; dim(dtm)


#
# #We can calculate jiccard dircetly and then see if there's a threshold that works
# p_load(parallelDist)
# d <- parDist(dtm_m, method = "binary") #calculate the jaccard distance directly
# saveRDS(d,"/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar/inst/extdata/humanlabeled_ngram_dist_1_4_skips.Rds") #.011 is about 300 and 0.015
#
# #saveRDS(d,"/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar/inst/extdata/humanlabeled_ngram_dist_5.Rds") #.011 is about 300 and 0.015
# #saveRDS(d,"/home/rexdouglass/Dropbox (rex)/Kenya Article Drafts/MeasuringLandscapeCivilWar/inst/extdata/humanlabeled_ngram_dist_6.Rds") #.011 is about 200 and 0.02
#
# d_m <- as.matrix(d)
# #make sure the row and col names are right
# #convert to
# hist(d_m, breaks=50)
# colnames(d_m) <- stemmed_ab
# rownames(d_m) <- stemmed_ab
#
# d_m_long = data.table::melt(d_m) ; dim(d_m_long)
# d_m_long$rex_match <- F
# d_m_long <- as.data.table(d_m_long)
# d_m_long[,ab:=paste(Var1, Var2, sep="_"),]
# d_m_long[,ba:=paste(Var2, Var1, sep="_"),]
#
# condition <- d_m_long$ab %in% subset(handlabeled, rex_match==1)$stemmed_ab | d_m_long$ba %in% subset(handlabeled, rex_match==1)$stemmed_ab ; table(condition)
# d_m_long$rex_match[condition] <- T
# table(d_m_long$rex_match)
#
# d_m_long <- subset(d_m_long, value!=0) #exclude any with 0 distance, those aren't interesting

# boxplot( value ~ rex_match, data=d_m_long)
# summary(d_m_long$value[d_m_long$rex_match==0])
# summary(d_m_long$value[d_m_long$rex_match==1])
# quantile(d_m_long$value[d_m_long$rex_match==0], probs=.01)
# quantile(d_m_long$value[d_m_long$rex_match==1], probs=.995) #ok so under jaccard distance of .92 captures 99% of matches
# table(d_m_long$value[d_m_long$rex_match==0]<0.9991918)
# sum(d_m_long$value[d_m_long$rex_match==0]<.92) / nrow(d_m_long)  #and would exclude 95% of false negatives
# s_curve <- get_s_curve(number_hashfun=480, n_bands_min = 1, n_rows_per_band_min = 1)
# s_curve <- s_curve %>% filter(n_rows_per_band<32)

# q <- seq(.8,1,.005)
# temp_list <- list()
# for(i in q){
#   print(i)
#   temp_list[[as.character(i)]]  <-  data.frame(i=i,
#                                                true_negative = sum(d_m_long$value[d_m_long$rex_match==0]<i) / nrow(d_m_long),
#                                                false_negative = 1 - sum(d_m_long$value[d_m_long$rex_match==1]<i) / sum(d_m_long$rex_match==1),
#                                                negative_suggestions_per = sum(d_m_long$value[d_m_long$rex_match==0]<i) / length(stemmed_ab)
#   )
# }
# temp <- rbindlist(temp_list)
# head(temp)
# temp$sim_jaccard <- round(1-temp$i,4)
#
# #ggplot(temp, aes(x=false_negative,y=true_negative, label=sim_jaccard))  + geom_text(size=2)
# ggplot(temp, aes(x=false_negative,y=negative_suggestions_per, label=sim_jaccard))  + geom_text(size=2) +
#   scale_y_continuous(breaks = round(seq(0, max(temp$negative_suggestions_per), by = 100),1)) +
#   scale_x_continuous(breaks = round(seq(0, max(temp$false_negative), by = .01),2))
#
#
# d_m_long$sim_jaccard <- round(1-d_m_long$value,4)
# d_m_long_thresh <- subset(d_m_long, sim_jaccard>0.07)
# table(d_m_long_thresh$rex_match)
#
# d_m_long_thresh <- subset(d_m_long, sim_jaccard>0.07)
# table(d_m_long_thresh$rex_match) #1270 recovers
# table(d_m_long$rex_match==1) #1286, misses 16 which ain't bad
# nrow(d_m_long_thresh) / 2  / length(stemmed_ab) #That's 169.1292 suggestions per, which is still too much
#
# #table(table(d_m_long_thresh$Var1))
# mean(table(d_m_long_thresh$Var1))
#
#
# s_curve <- get_s_curve(240, n_bands_min = 1, n_rows_per_band_min = 1)
#
# s_curve$n_bands_n_rows_per_band <- paste(s_curve$n_bands,s_curve$n_rows_per_band)
# s_curve <- s_curve %>% filter(!duplicated(n_bands_n_rows_per_band))
#
# lsh_threshold(h = 80*1, b = 80)
