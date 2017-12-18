

lhs_textreuse <- function(minhash_count=240,  bands=80, ngram_count=2){
  
  library(textreuse)
  handlabeled$stemmed_ab <- paste(handlabeled$stemmed_a,handlabeled$stemmed_b, sep="_")
  handlabeled$stemmed_a_spaced <- sapply(strsplit(handlabeled$stemmed_a, split="") , paste, collapse=" ")
  handlabeled$stemmed_b_spaced <- sapply(strsplit(handlabeled$stemmed_b, split="") , paste, collapse=" ")
  stemmed_ab <- unique(c(handlabeled$stemmed_a, handlabeled$stemmed_b))
  stemmed_ab_spaced <- sapply(strsplit(stemmed_ab, split="") , paste, collapse=" ")
  
  minhash <- minhash_generator(n = minhash_count, seed = 1)
  options("mc.cores" = parallel::detectCores()) #Much faster when paralized
  #options("mc.cores" = 1) #Much faster when paralized
  
  corpus_ab_spaced <- TextReuseCorpus(text = stemmed_ab_spaced,
                                      tokenizer = tokenize_ngrams,
                                      n = 2, #wow if you pass this as a variable instead of a number it crashes. Wtf?
                                      minhash_func = minhash,
                                      keep_tokens = TRUE,
                                      progress = T)
  #buckets <- lsh(corpus_ab_spaced, bands = 80, progress = T) #Single threaded but should be parallizable

  #p_load(multicore)
  n.cores <- parallel::detectCores()
  cuts <- cut(1:length(corpus_ab_spaced), n.cores)
  #buckets_list <- mclapply(levels(cuts),
  #               function(q) lsh(corpus_ab_spaced[cuts==q], bands = bands, progress = T) ,
  #               mc.cores = n.cores)
  #buckets <- do.call(rbind, buckets_list)
  
  #library(doParallel)
  #library(foreach)
  cl<- parallel::makeCluster(parallel::detectCores()) #change the 2 to your number of CPU cores
  doParallel::registerDoParallel(cl)
  
  `%dopar%` <- foreach::`%dopar%`
  `%do%` <- foreach::`%do%`
  
  buckets <- foreach::foreach(q=levels(cuts), .combine='rbind', .packages=c('textreuse')) %dopar%
                lsh(corpus_ab_spaced[cuts==q], bands = bands, progress =F)
  stopCluster(cl)
  
  
  candidates <- lsh_candidates(buckets)
  candidates$a_numeric <- as.numeric( gsub("doc-","",candidates$a) )
  candidates$b_numeric <- as.numeric( gsub("doc-","",candidates$b) )
  candidates$stemmed_a <- stemmed_ab[candidates$a_numeric]
  candidates$stemmed_b <- stemmed_ab[candidates$b_numeric]
  candidates$stemmed_ab <- paste(candidates$stemmed_a,candidates$stemmed_b, sep="_")
  candidates$stemmed_ba <- paste(candidates$stemmed_b,candidates$stemmed_a, sep="_")
  
  handlabeled$lsh_match <- handlabeled$stemmed_ab %in% candidates$stemmed_ab | #It's flagged as a match
                           handlabeled$stemmed_ab %in% candidates$stemmed_ba |
                           handlabeled$stemmed_a == handlabeled$stemmed_b      #Or has an identical stem
  
  dim(candidates)
  
  d=table(handlabeled$lsh_match, handlabeled$rex_match)
  
  results <- data.frame(
    minhash_count=minhash_count,
    ngram_count=ngram_count,
    bands=bands,
    candidates_nrow= nrow(candidates),
    suggestions_per = nrow(candidates)/length(stemmed_ab),
    true_negative=d[1,1],
    false_negative=d[1,2],
    false_positive=d[2,1],
    true_positive=d[2,2]
  )
  return(results)
}





