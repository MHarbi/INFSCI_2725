train <- read_csv('input/train.csv', locale = locale(encoding = "ISO-8859-1"))
train <- data.frame(search_term=train$search_term)
train$search_term <- as.character(train$search_term)

#cl <- makeCluster(no_cores)
train$search_term <- mcmapply(str_clean, s = train$search_term, x = 1:length(train$search_term), mc.cores = no_cores, USE.NAMES = F)
#stopCluster(cl)

train$search_term <- mapply(str_clean, s = train$search_term, x = 1:length(train$search_term), USE.NAMES = F)

hdfs.init()
from.dfs(mapreduce(input = to.dfs(1:1000), map = function(k, v) keyval(v, v^2)))

finalResult <- local({
  f <- fifo(tempfile(), open="w+b", blocking=T)
  if (inherits(parallel:::mcfork(), "masterProcess")) {
    # Child
    progress <- 0.0
    while (progress < 1 && !isIncomplete(f)) {
      msg <- readBin(f, "double")
      progress <- progress + as.numeric(msg)
      cat(sprintf("Progress: %.2f%%\n", progress * 100))
    } 
    parallel:::mcexit()
  }
  numJobs <- 100
  result <- mclapply(1:numJobs, function(...) {
    # Do something fancy here... For this example, just sleep
    Sys.sleep(0.05)
    # Send progress update
    writeBin(1/numJobs, f)
    # Some arbitrary result
    sample(1000, 1)
  })
  close(f)
  result
})
cat("Done.\n")












terms <- c("accounts", "account", "accounting", "acounting", "acount", "acounts", "accounnt")

library(tm); library(qdap)

fake_text <- unlist(lapply(terms, function(x) {
  paste(sample(c(x, sample(DICTIONARY[[1]], sample(1:5, 1)))), collapse=" ")
}))

fake_text

myCorp <- Corpus(VectorSource(fake_text))
terms2 <- unique(bag_o_words(as.data.frame(myCorp)[[2]]))
misses <- terms2[is.na(match(terms2, DICTIONARY[[1]]))]

chars <- nchar(DICTIONARY[[1]])

replacements <- sapply(misses, function(x, range = 3, max.distance = .2) {
  x <- stemDocument(x)
  wchar <- nchar(x)
  dict <- DICTIONARY[[1]][chars >= (wchar - range) & chars <= (wchar + range)]
  dict <- dict[agrep(x, dict, max.distance=max.distance)]
  names(which.min(sapply(dict, qdap:::Ldist, x)))
})

replacer <- content_transformer(function(x) { 
  mgsub(names(replacements), replacements, x, ignore.case = FALSE, fixed = FALSE)
})

myCorp <- tm_map(myCorp, replacer)
inspect(myCorp <- tm_map(myCorp, stemDocument))