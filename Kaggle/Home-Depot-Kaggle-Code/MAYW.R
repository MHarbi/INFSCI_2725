cat("Initializing the session \n")
rm(list=ls()) # clear the workspace
t <- Sys.time() # start time

cat("Loading some customized functions to the workspace \n")
source("Functions.R")
#source("peter-norvigs-spell-checker.R")

cat("Detaching all previous packages \n")
detachAllPackages()

#source("PorterStemmer.R")
# detach PorterStemmer
#rm(splitstring, h1, m, v, d, cvc, stem) 

set.seed(463)

cat("Loading/Attaching some packages \n")
library(SnowballC) # being used in Function.R
library(gbm)
library(readr)
library(ggplot2)   # Data visualization
library(dplyr)     # A Grammar of Data Manipulation
library(randomForest)
library(party)
library(tm)        # Text Mining
library(hunspell)
library(ranger)

cat("Reading data \n")
train <- read_csv('input/train.csv', locale = locale(encoding = "ISO-8859-1"))
test <- read_csv('input/test.csv', locale = locale(encoding = "ISO-8859-1"))
desc <- read_csv('input/product_descriptions.csv', locale = locale(encoding = "ISO-8859-1"))
attr <- read_csv('input/attributes.csv', locale = locale(encoding = "ISO-8859-1"))

cat("Clean up attributes data form unjoined(unwanted) data (empty product_uid) \n")
attr <- attr[!is.na(attr$product_uid),]

cat("####### Basic data manipulation ######## \n")
relevance <- train$relevance
train <- select(train, -relevance)
end_trn <- nrow(train)

cat("Binding train with test data \n")
all <- rbind(train, test)
end <- nrow(all)

cat("Merging description with all (train and test) data \n")
all <- merge.with.order(all, desc, by.x = "product_uid", by.y = "product_uid", all.x = TRUE, all.y = FALSE, keep_order = 1)

cat("Merging 'brand' attribute with all data \n")
attr$name <- ifelse(attr$name != "", tolower(attr$name))
all <- merge.with.order(all, attr %>% filter(name == "mfg brand name") %>% select(product_uid, value) %>% rename(brand=value), by.x = "product_uid", by.y = "product_uid", all.x = TRUE, all.y = FALSE, keep_order = 1)

cat("Merge 'color' attribute with all data \n")
attr_colors <- filter(attr, grepl("colo*r", attr$name)) %>% group_by(product_uid) %>% select(product_uid, value) %>% rename(colors=value)
attr_colors <- unique(attr_colors)
attr_colors <- aggregate(colors~product_uid, paste, collapse=" | ", data=attr_colors)
attr_colors$colors <- mapply(function(x) gsub("( *)/( *)", " ", x), attr_colors$colors, USE.NAMES = F)
all <- merge.with.order(all, attr_colors, by.x = "product_uid", by.y = "product_uid", all.x = TRUE, all.y = FALSE, keep_order = 1)
rm(attr_colors)
# 
# #attr <- train %>% mutate(hasColorAtt = ifelse(product_uid %in% unique(filter(attr, grepl("colo*r", attr$name))$product_uid), "Yes"))

cat(paste("--- Files Loaded: ", round(Sys.time()-t, digits=2), "minutes --- \n"))



#vectors <- mapply(str_clean, s = all$search_term, x = 1:end, USE.NAMES = F)
# build a corpus
#corpus <- Corpus(VectorSource(vectors))
# remove punctuation 
#corpus <- tm_map(corpus, removePunctuation)
# build a term-document matrix
#tdm <- TermDocumentMatrix(corpus)

#mapply(hunspell_check ,Terms(tdm)[1166:1200], USE.NAMES = F)

##mapply(function(x){ (unlist(strsplit(x, " "))) }, x = bag_of_words[1:10], USE.NAMES = F) 


 
all$search_term         <- mapply(str_spell_check, s = all$search_term, x = 1:end, USE.NAMES = FALSE)
all$product_title       <- mapply(str_spell_check, s = all$product_title, x = 1:end, USE.NAMES = FALSE)
all$product_description <- mapply(str_spell_check, s = all$product_description, x = 1:end, USE.NAMES = FALSE)
all$brand               <- mapply(str_spell_check, s = all$brand, x = 1:end, USE.NAMES = FALSE)
all$colors              <- mapply(str_spell_check, s = all$colors, x = 1:end, USE.NAMES = FALSE)
cat(paste("--- Spelling checker: ", round(Sys.time()-t, digits=2), "minutes --- \n"))


all$search_term         <- mapply(str_normalize, s = all$search_term, x = 1:end, USE.NAMES = FALSE)
all$product_title       <- mapply(str_normalize, s = all$product_title, x = 1:end, USE.NAMES = FALSE)
all$product_description <- mapply(str_normalize, s = all$product_description, x = 1:end, USE.NAMES = FALSE)
all$brand               <- mapply(str_normalize, s = all$brand, x = 1:end, USE.NAMES = FALSE)
all$colors              <- mapply(str_normalize, s = all$colors, x = 1:end, USE.NAMES = FALSE)
cat(paste("--- Normalizing: ", round(Sys.time()-t, digits=2), "minutes --- \n"))

# all$search_term         <- mapply(str_num_normalize, s = all$search_term, x = 1:end, USE.NAMES = FALSE)
# all$product_title       <- mapply(str_num_normalize, s = all$product_title, x = 1:end, USE.NAMES = FALSE)
# all$product_description <- mapply(str_num_normalize, s = all$product_description, x = 1:end, USE.NAMES = FALSE)
# all$brand               <- mapply(str_num_normalize, s = all$brand, x = 1:end, USE.NAMES = FALSE)
# all$colors              <- mapply(str_num_normalize, s = all$colors, x = 1:end, USE.NAMES = FALSE)
# cat(paste("--- Number Normalizing: ", round(Sys.time()-t, digits=2), "minutes --- \n"))

all$search_term         <- mapply(str_spell_check, s = all$search_term, x = 1:end, USE.NAMES = FALSE)
all$product_title       <- mapply(str_spell_check, s = all$product_title, x = 1:end, USE.NAMES = FALSE)
all$product_description <- mapply(str_spell_check, s = all$product_description, x = 1:end, USE.NAMES = FALSE)
all$brand               <- mapply(str_spell_check, s = all$brand, x = 1:end, USE.NAMES = FALSE)
all$colors              <- mapply(str_spell_check, s = all$colors, x = 1:end, USE.NAMES = FALSE)
cat(paste("--- Spelling checker: ", round(Sys.time()-t, digits=2), "minutes --- \n"))

write_csv(all,"output/all.csv")

all$search_term         <- mapply(str_stem, s = all$search_term, x = 1:end, USE.NAMES = FALSE)
all$product_title       <- mapply(str_stem, s = all$product_title, x = 1:end, USE.NAMES = FALSE)
all$product_description <- mapply(str_stem, s = all$product_description, x = 1:end, USE.NAMES = FALSE)
all$brand               <- mapply(str_stem, s = all$brand, x = 1:end, USE.NAMES = FALSE)
all$colors              <- mapply(str_stem, s = all$colors, x = 1:end, USE.NAMES = FALSE)
cat(paste("--- Steeming: ", round(Sys.time()-t, digits=2), "minutes --- \n"))

all$product_info <- paste(all$search_term, all$product_title, all$product_description, sep = "\t")
cat(paste("--- Prod Info: ", round(Sys.time()-t, digits=2), "minutes --- \n"))

all$len_of_query        <- mapply(function(x) length(unlist(strsplit(x, " "))), all$search_term)
all$len_of_title        <- mapply(function(x) length(unlist(strsplit(x, " "))), all$product_title)
all$len_of_description  <- mapply(function(x) length(unlist(strsplit(x, " "))), all$product_description)
all$len_of_brand        <- mapply(function(x) length(unlist(strsplit(x, " "))), all$brand)
all$len_of_color        <- mapply(function(x) length(unlist(strsplit(x, "|"))), all$brand)
cat(paste("--- Len of: ", round(Sys.time()-t, digits=2), "minutes --- \n"))

# agrep("whirlpoolstainless", "whirlpool stainless wwwww", max= 4, value = T)
# regexpr(all$search_term[3369], substr(all$product_title[3369], 1, nchar(all$product_title[3370])), fixed = T)[1]
# aregexec("blk", all$product_title[3369], fixed = T)[[1]][1]
# str_common_word(all$search_term[3369], all$product_title[3369])

#all$search_term <- mapply(seg_words, all$search_term, all$product_title) 
#cat(paste("--- Search Term Segment: ", round(Sys.time()-t, digits=2), "minutes --- \n"))

all$query_in_title        <- mapply(function(x) { x <- unlist(strsplit(x, "\t")); str1=x[1]; str2=x[2];  str_whole_word(str1, str2, 1) }, all$product_info, USE.NAMES = FALSE)
all$query_in_description  <- mapply(function(x) { x <- unlist(strsplit(x, "\t")); str1=x[1]; str2=x[3];  str_whole_word(str1, str2, 1) }, all$product_info, USE.NAMES = FALSE)
cat(paste("--- Query In: ", round(Sys.time()-t, digits=2), "minutes --- \n"))

all$query_last_word_in_title        <- mapply(function(x) { x <- unlist(strsplit(x, "\t")); str1=tail(unlist(strsplit(x[1], " ")), n=1); str2=x[2];  str_common_word(str1, str2) }, all$product_info, USE.NAMES = FALSE)
all$query_last_word_in_description  <- mapply(function(x) { x <- unlist(strsplit(x, "\t")); str1=tail(unlist(strsplit(x[1], " ")), n=1); str2=x[3];  str_common_word(str1, str2) }, all$product_info, USE.NAMES = FALSE)
cat(paste("--- Query Last Word In: ", round(Sys.time()-t, digits=2), "minutes --- \n"))

all$word_in_title       <- mapply(function(x) { x <- unlist(strsplit(x, "\t")); str1=x[1]; str2=x[2];  str_common_word(str1, str2) }, all$product_info, USE.NAMES = FALSE) 
all$word_in_description <- mapply(function(x) { x <- unlist(strsplit(x, "\t")); str1=x[1]; str2=x[3];  str_common_word(str1, str2) }, all$product_info, USE.NAMES = FALSE)
#all$word_in_colors       <- mapply(str_common_word, all$search_term, all$colors, USE.NAMES = FALSE) 

all$ratio_title         <- all$word_in_title/all$len_of_query
all$ratio_description   <- all$word_in_description/all$len_of_query
all$attr                <- paste(all$search_term, all$brand, sep = "\t")
all$word_in_brand       <- mapply(function(x) { x <- unlist(strsplit(x, "\t")); str1=x[1]; str2=x[2];  str_common_word(str1, str2) }, all$attr, USE.NAMES = FALSE)
all$ratio_brand         <- all$word_in_brand/all$len_of_brand
#all$ratio_color         <- all$word_in_colors/all$len_of_color

brand <- unlist(unique(all$brand))
brand <- as.data.frame(brand)

d <- brand; 
for(s in 1:nrow(d)) row.names(d)[s] <- paste("c", d$brand[s])
for(s in 1:nrow(d)) row.names(d)[s] <- paste(d[s,1], sep = "")
d$brand <- seq(1000, (1000 + (length(brand$brand) - 1) * 3), by = 3)
rm(s);

all$brand_feature <- mapply(function(x) d[x, 1], all$brand, USE.NAMES = FALSE)
all$search_term_feature <- mapply(nchar, all$search_term, USE.NAMES = FALSE)

#write_csv(all,"output/all.csv")

#trrn <- all[1:end_trn,]

#train   <- all[1:end_trn,]
#test    <- all[(end_trn+1):end,]
id_test <- test$id
y_train <- relevance
X_train <- select(all[1:end_trn,], -id,-search_term,-product_title,-product_description,-product_info,-attr,-brand,-colors)
#X_train$relevance <- relevance
X_test  <- select(all[(end_trn+1):end,], -id,-search_term,-product_title,-product_description,-product_info,-attr,-brand,-colors)
cat(paste("--- Features Set: ", round(Sys.time()-t, digits=2), "minutes --- \n"))

#rf <- randomForest(X_train, y_train, ntree=60, sampsize=end_trn, mtry=5, importance=T)
#rf <- cforest(as.factor(y_train)~all[1:end_trn,9]+all[1:end_trn,10]+all[1:end_trn,11]+all[1:end_trn,12]+all[1:end_trn,13]+all[1:end_trn,14]+all[1:end_trn,15]+all[1:end_trn,16]+all[1:end_trn,17]+all[1:end_trn,19]+all[1:end_trn,20], controls=cforest_unbiased(ntree=2000, mtry=3))
#TrainPredictions = predict(object = rf, newdata =X_train)
#TestPredictions = predict(object = rf, newdata =X_test)

#rfr = RandomForestRegressor(n_estimators = 500, n_jobs = -1, random_state = 2016, verbose = 1)
#tfidf = TfidfVectorizer(ngram_range=(1, 1), stop_words='english')
#tsvd = TruncatedSVD(n_components=10, random_state = 2016)

traina <- cbind(X_train, y_train)
write_csv(traina,"output/traina.csv")
#rm(traina)

########## The model #############
#GBM: Generalized Boosted Models: an ensemble of classification or regression trees

cat("A simple linear model on number of words and number of words that match\n")
#gbm_model <- gbm.fit(train[,7:9],train$relevance,distribution = "gaussian",interaction.depth = 2,shrinkage=0.04,n.trees=800)
#test_relevance <- predict(gbm_model,test[,6:8],n.trees=800)

#gbm_model1 <- gbm.fit(all[1:end_trn,6:8],relev,distribution = "gaussian",interaction.depth = 2,shrinkage=0.04,n.trees=800)
#rm(gbm_model1)

gbm_model = gbm.fit( 
  x = X_train #dataframe of features
  , y = relevance #dependent variable
  #two ways to fit the model
  #use gbm.fit if you are going to specify x = and y = 
  #instead of using a formula
  #if there are lots of features, I think it's easier to specify 
  #x and y instead of using a formula
  
  , distribution = "gaussian"
  #use bernoulli for binary outcomes
  #other values are "gaussian" for GBM regression 
  #or "adaboost" or "multinomial" or "bernoulli"
  
  , n.trees = 6000
  #Choose this value to be large, then we will prune the
  #tree after running the model
  
  , shrinkage = 0.01 
  #smaller values of shrinkage typically give slightly better performance
  #the cost is that the model takes longer to run for smaller values
  
  , interaction.depth = 3
  #use cross validation to choose interaction depth!!
  
  , n.minobsinnode = 10
  #n.minobsinnode has an important effect on overfitting!
  #decreasing this parameter increases the in-sample fit, 
  #but can result in overfitting
  
  , nTrain = round(end_trn * 0.8)
  #use this so that you can select the number of trees at the end
  
  #, var.monotone = c() 
  # uncomment, can help with overfitting, will smooth bumpy curves
  
  , verbose = TRUE #print the preliminary output
)
#gbm_model = gbm.fit(x = select(X_train, -word_in_brand, -len_of_brand, -word_in_description, -query_in_title, -query_in_description, -word_in_title, -query_last_word_in_description, -ratio_brand), y = relevance, distribution = "gaussian", n.trees = 3000, shrinkage = 0.006, interaction.depth = 7, n.minobsinnode = 5, nTrain = round(end_trn * 0.8), verbose = TRUE)
#gbm_model = gbm.fit(x = X_train, y = relevance, distribution = "gaussian", n.trees = 3000, shrinkage = 0.006, interaction.depth = 7, n.minobsinnode = 5, nTrain = round(end_trn * 0.8), verbose = TRUE)
gbm_model = gbm.fit(x = X_train, y = relevance, distribution = "gaussian", n.trees = 6000, shrinkage = 0.01, interaction.depth = 7, n.minobsinnode = 10, nTrain = round(end_trn * 0.8), verbose = TRUE)

1720-40
#look at the last model built
#Relative influence among the variables can be used in variable selection
summary(gbm_model)
#If you see one variable that's much more important than all of the rest,
#that could be evidence of overfitting.

#optimal number of trees based upon CV
gbm.perf(gbm_model)

#look at the effects of each variable, does it make sense?
for(i in 1:length(gbm_model$var.names)){
  plot(gbm_model, i.var = i
       , ntrees = gbm.perf(gbm_model, plot.it = FALSE) #optimal number of trees
       , type = "response" #to get fitted probabilities
  )
}

################ Make predictions ##################
#test set predictions
TestPredictions = predict(object = gbm_model,newdata =X_test
                          , n.trees = gbm.perf(gbm_model, plot.it = FALSE)
                          , type = "response") #to output a probability
#training set predictions
TrainPredictions = predict(object = gbm_model,newdata =X_train
                           , n.trees = gbm.perf(gbm_model, plot.it = FALSE)
                           , type = "response")

TestPredictions = predict(object = gbm_model,newdata =select(X_test, -word_in_brand, -len_of_brand, -word_in_description, -query_in_title, -query_in_description, -word_in_title, -query_last_word_in_description, -ratio_brand), n.trees = gbm.perf(gbm_model, plot.it = FALSE), type = "response")
TrainPredictions = predict(object = gbm_model,newdata =select(X_train, -word_in_brand, -len_of_brand, -word_in_description, -query_in_title, -query_in_description, -word_in_title, -query_last_word_in_description, -ratio_brand), n.trees = gbm.perf(gbm_model, plot.it = FALSE), type = "response")

TestPredictions <- ifelse(TestPredictions>3,3,TestPredictions)
TestPredictions <- ifelse(TestPredictions<1,1,TestPredictions)
TrainPredictions <- ifelse(TrainPredictions>3,3,TrainPredictions)
TrainPredictions <- ifelse(TrainPredictions<1,1,TrainPredictions)

head(TrainPredictions, n = 20)
head(relevance, n = 20)

# Sample Classification Accuracy
1 - sum(abs(relevance - TrainPredictions)) / length(TrainPredictions) 

# Root Mean Squared Error (RMSE)
sqrt(mean((relevance - TrainPredictions)^2))

#write the submission
submission <- data.frame(id=test$id, relevance=TestPredictions)
write_csv(submission,"output/submission.csv")

print(Sys.time()-t)
rm(TestPredictions, TrainPredictions, gbm_model)
################# Linear Model ####################
#lm_model <- lm(relev ~ all[1:end_trn,8]+all[1:end_trn,9]+all[1:end_trn,10])
#lm_model <- lm(relev ~ all[1:end_trn,8]*all[1:end_trn,9]*all[1:end_trn,10])
#lm_model <- lm(relevance ~ all[1:end_trn,9]+all[1:end_trn,10]+all[1:end_trn,11]+all[1:end_trn,12]+all[1:end_trn,13]+all[1:end_trn,14]+all[1:end_trn,15]+all[1:end_trn,16]+all[1:end_trn,17]+all[1:end_trn,19]+all[1:end_trn,20])
lm_model <- lm(y_train ~ ., data=traina)
  
summary(lm_model)

################ Make predictions for Linear Model ##################
#test set predictions
TestPredictions1 = predict(object = lm_model,newdata =X_test, type = "response") #to output a probability
#training set predictions
TrainPredictions1 = predict(object = lm_model,newdata =X_train, type = "response")

TestPredictions1 <- ifelse(TestPredictions1>3,3,TestPredictions1)
TestPredictions1 <- ifelse(TestPredictions1<1,1,TestPredictions1)
TrainPredictions1 <- ifelse(TrainPredictions1>3,3,TrainPredictions1)
TrainPredictions1 <- ifelse(TrainPredictions1<1,1,TrainPredictions1)

head(TrainPredictions1, n = 20)
head(relevance, n = 20)

# Sample Classification Accuracy
1 - sum(abs(relevance - TrainPredictions1)) / length(TrainPredictions1) 

# Root Mean Squared Error (RMSE)
sqrt(mean((relevance - TrainPredictions1)^2))

#write the submission
submission <- data.frame(id=test$id, relevance=TestPredictions1)
write_csv(submission,"output/submission.csv")

rm(TestPredictions1, TrainPredictions1, lm_model)
##########################################################################

ggplot(data = submission[submission$id %in% train$id,], aes(x = id)) + geom_histogram(binwidth=10)

##########################################################################

#rf <- ranger(y_train ~ ., data = traina, importance = "permutation", write.forest = TRUE, num.trees = 2500)
#rf <- ranger(y_train ~  product_uid+query_in_title+query_in_description+word_in_brand+word_in_description+word_in_title, data = traina, importance = "permutation", write.forest = TRUE, num.trees = 2500)
rf <- ranger(y_train ~ ., data = select(X_train, -word_in_brand, -len_of_brand, -word_in_description, -query_in_title, -query_in_description, -word_in_title, -query_last_word_in_description, -ratio_brand), importance = "permutation", write.forest = TRUE, num.trees = 4000)

importance(rf)
plot(timepoints(rf), predictions(rf)[1,])
importance.ranger(rf)

#traina.idx <- sample(x = 150, size = 100)
#traina.train <- traina[traina.idx, ]
#traina.test <- traina[-traina.idx, ]
#rf <- ranger(y_train ~ ., data = traina.train, write.forest = TRUE)

#test set predictions
TestPredictions2 = predict(object = rf, data =X_test, type = "response")
TestPredictions2 = predict(object = rf, data =select(X_test, -word_in_brand, -len_of_brand, -word_in_description, -query_in_title, -query_in_description, -word_in_title, -query_last_word_in_description, -ratio_brand), n.trees = 4000, type = "response")
#training set predictions
TrainPredictions2 = predict(object = rf, data =X_train, type = "response")
TrainPredictions2 = predict(object = rf, data =select(X_train, -word_in_brand, -len_of_brand, -word_in_description, -query_in_title, -query_in_description, -word_in_title, -query_last_word_in_description, -ratio_brand), n.trees = 4000, type = "response")

TestPredictions2 = TestPredictions2$predictions
TrainPredictions2 = TrainPredictions2$predictions

TestPredictions2 <- ifelse(TestPredictions2>3,3,TestPredictions2)
TestPredictions2 <- ifelse(TestPredictions2<1,1,TestPredictions2)
TrainPredictions2 <- ifelse(TrainPredictions2>3,3,TrainPredictions2)
TrainPredictions2 <- ifelse(TrainPredictions2<1,1,TrainPredictions2)

head(TrainPredictions2, n = 20)
head(relevance, n = 20)

# Sample Classification Accuracy
1 - sum(abs(relevance - TrainPredictions2)) / length(TrainPredictions2) 

# Root Mean Squared Error (RMSE)
sqrt(mean((relevance - TrainPredictions2)^2))

#write the submission
submission <- data.frame(id=test$id, relevance=TestPredictions2)
write_csv(submission,"output/submission.csv")


rm(TestPredictions2, TrainPredictions2, rf, traina.idx, traina.train, traina.test)

#age = data.frame(Age = c(45.0, 16.0, 9.0, 10.0, 8.0, 46.0, 13.0, 39.0, 55.0, 25.0, 46.0, 11.0, 30.0, 66.0, 26.0, 52.0, 5.0, 35.0, 42.0, 72.0, 41.0, 90.0, 57.0, 2.0, 40.0))
#age = data.frame(Age = c("[25:50)","[0:25)","[0:25)","[0:25)","[0:25)","[25:50)","[0:25)","[25:50)","[50:75)","[25:50)","[25:50)","[0:25)","[25:50)","[50:75)","[25:50)","[50:75)","[0:25)","[25:50)","[25:50)","[50:75)","[25:50)","[0:100)","[50:75)","[0:25)","[25:50)"))
age = data.frame(Age = c("[25:50)","[0:25)","[0:25)","[0:25)","[0:25)","[25:50)","[0:25)","[25:50)","[0:100)","[25:50)","[25:50)","[0:25)","[25:50)","[0:100)","[25:50)","[0:100)","[0:25)","[25:50)","[25:50)","[0:100)","[25:50)","[0:100)","[0:100)","[0:25)","[25:50)"))
positions <- c("[0:25)", "[25:50)", "[50:75)", "[0:100)")
#ggplot(age, aes(Age)) + geom_histogram(binwidth = 4, color="black", fill="#56B4E9") + xlim(0, 100) + ggtitle("Histogram for Age (K = 1)")
#ggplot(age, aes(Age)) + geom_bar(color="black", fill="#56B4E9") + xlim(positions) + ggtitle("Histogram for Age (K = 4)")
ggplot(age, aes(Age)) + geom_bar(color="black", fill="#56B4E9") + xlim(positions) + ggtitle("Histogram for Age (K = 6)")

