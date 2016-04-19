detachAllPackages()

library(gbm)
library(readr)
library(ggplot2)   # Data visualization
library(dplyr)     # A Grammar of Data Manipulation

cat("Reading data\n")
train <- read_csv('input/train.csv', locale = locale(encoding = "ISO-8859-1"))
test <- read_csv('input/test.csv', locale = locale(encoding = "ISO-8859-1"))
desc <- read_csv('input/product_descriptions.csv', locale = locale(encoding = "ISO-8859-1"))
attr <- read_csv('input/attributes.csv', locale = locale(encoding = "ISO-8859-1"))

# Clean up attributes data form unjoined(unwanted) data (empty product_uid)
attr <- attr[!is.na(attr$product_uid),]

####### Basic data manipulation ########
relev = train$relevance
train = select(train, -relevance)
end_trn = nrow(train)

#unique(filter(attr, grepl('Brand', name)))

cat("Merge description with train and test data \n")
train <- merge(train,desc, by.x = "product_uid", by.y = "product_uid", all.x = TRUE, all.y = FALSE)
test <- merge(test,desc, by.x = "product_uid", by.y = "product_uid", all.x = TRUE, all.y = FALSE)

cat("Merge train with test data \n")
all = rbind(train,test)
end = nrow(all)

cat("Merge attributes with all data \n")
attr$name <- ifelse(attr$name != "", tolower(attr$name))
all <- merge(all, attr %>% filter(name == "mfg brand name") %>% select(product_uid, value) %>% rename(brand=value), by.x = "product_uid", by.y = "product_uid", all.x = TRUE, all.y = FALSE)

attr_colors <- filter(attr, grepl("colo*r", attr$name)) %>% group_by(product_uid) %>% select(product_uid, value) %>% rename(colors=value)
attr_colors <- aggregate(colors~product_uid, paste, collapse=",", data=attr_colors)
all <- merge(all, attr_colors, by.x = "product_uid", by.y = "product_uid", all.x = TRUE, all.y = FALSE)
rm(attr_colors)

#attr <- train %>% mutate(hasColorAtt = ifelse(product_uid %in% unique(filter(attr, grepl("colo*r", attr$name))$product_uid), "Yes"))

t <- Sys.time()
word_match <- function(words, title, desc, brand, colors){
  n_title <- 0
  n_desc <- 0
  n_brand <- 0
  n_color <- 0
  words <- unlist(strsplit(words, " "))
  nwords <- length(words)
  for(i in 1:length(words)){
    pattern <- paste("(^| )", words[i], "($| )", sep="")
    n_title <- n_title + grepl(pattern, title, perl=TRUE, ignore.case=TRUE)
    n_desc  <- n_desc  + grepl(pattern, desc, perl=TRUE, ignore.case=TRUE)
    n_brand <- n_brand + grepl(pattern, brand, perl=TRUE, ignore.case=TRUE)
    n_color <- n_color + grepl(pattern, colors, perl=TRUE, ignore.case=TRUE)
  }
  return(c(n_title, nwords, n_desc, n_brand, n_color))
}

# cat("Get number of words and word matching title in train\n")
# train_words <- as.data.frame(t(mapply(word_match,train$search_term,train$product_title,train$product_description)))
# train$nmatch_title <- train_words[,1]
# train$nwords <- train_words[,2]
# train$nmatch_desc <- train_words[,3]
# 
# cat("Get number of words and word matching title in test\n")
# test_words <- as.data.frame(t(mapply(word_match,test$search_term,test$product_title,test$product_description)))
# test$nmatch_title <- test_words[,1]
# test$nwords <- test_words[,2]
# test$nmatch_desc <- test_words[,3]

cat("Get number of words and word matching title in all data\n")
all_words <- as.data.frame(t(mapply(word_match, all$search_term, all$product_title, all$product_description, all$brand, all$colors)))
all$nmatch_title <- all_words[,1]
all$nwords <- all_words[,2]
all$nmatch_desc <- all_words[,3]
all$nmatch_brand <- all_words[,4]
all$nmatch_color <- all_words[,5]

# rm(train_words,test_words)
rm(all_words)

########## The model #############
#GBM: Generalized Boosted Models: an ensemble of classification or regression trees
ntrees = 5000

cat("A simple linear model on number of words and number of words that match\n")
#gbm_model <- gbm.fit(train[,7:9],train$relevance,distribution = "gaussian",interaction.depth = 2,shrinkage=0.04,n.trees=800)
#test_relevance <- predict(gbm_model,test[,6:8],n.trees=800)

#gbm_model1 <- gbm.fit(all[1:end_trn,6:8],relev,distribution = "gaussian",interaction.depth = 2,shrinkage=0.04,n.trees=800)
#rm(gbm_model1)

gbm_model = gbm.fit( 
  x = all[1:end_trn,8:12] #dataframe of features
  , y = relev #dependent variable
  #two ways to fit the model
  #use gbm.fit if you are going to specify x = and y = 
  #instead of using a formula
  #if there are lots of features, I think it's easier to specify 
  #x and y instead of using a formula
  
  , distribution = "gaussian"
  #use bernoulli for binary outcomes
  #other values are "gaussian" for GBM regression 
  #or "adaboost"
  
  , n.trees = ntrees
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
TestPredictions = predict(object = gbm_model,newdata =all[(end_trn+1):end,]
                          , n.trees = gbm.perf(gbm_model, plot.it = FALSE)
                          , type = "response") #to output a probability
#training set predictions
TrainPredictions = predict(object = gbm_model,newdata =all[1:end_trn,]
                           , n.trees = gbm.perf(gbm_model, plot.it = FALSE)
                           , type = "response")

TestPredictions <- ifelse(TestPredictions>3,3,TestPredictions)
TestPredictions <- ifelse(TestPredictions<1,1,TestPredictions)
TrainPredictions <- ifelse(TrainPredictions>3,3,TrainPredictions)
TrainPredictions <- ifelse(TrainPredictions<1,1,TrainPredictions)

head(TrainPredictions, n = 20)
head(relev, n = 20)

# Sample Classification Accuracy
1 - sum(abs(relev - TrainPredictions)) / length(TrainPredictions) 

# Root Mean Squared Error (RMSE)
sqrt(mean((relev - TrainPredictions)^2))

#write the submission
submission <- data.frame(id=test$id,relevance=TestPredictions)
write_csv(submission,"output/submission.csv")

print(Sys.time()-t)

################# Linear Model ####################
#lm_model <- lm(relev ~ all[1:end_trn,8]+all[1:end_trn,9]+all[1:end_trn,10])
#lm_model <- lm(relev ~ all[1:end_trn,8]*all[1:end_trn,9]*all[1:end_trn,10])
lm_model <- lm(relev ~ all[1:end_trn,8]+all[1:end_trn,9]+all[1:end_trn,10]+all[1:end_trn,11]+all[1:end_trn,12])

summary(lm_model)
################ Make predictions for Linear Model ##################
#test set predictions
TestPredictions1 = predict(object = lm_model,newdata =all[(end_trn+1):end,]
                          , type = "response") #to output a probability
#training set predictions
TrainPredictions1 = predict(object = lm_model,newdata =all[1:end_trn,]
                           , type = "response")

TestPredictions1 <- ifelse(TestPredictions1>3,3,TestPredictions1)
TestPredictions1 <- ifelse(TestPredictions1<1,1,TestPredictions1)
TrainPredictions1 <- ifelse(TrainPredictions1>3,3,TrainPredictions1)
TrainPredictions1 <- ifelse(TrainPredictions1<1,1,TrainPredictions1)

head(TrainPredictions1, n = 20)
head(relev, n = 20)

# Sample Classification Accuracy
1 - sum(abs(relev - TrainPredictions1)) / length(TrainPredictions1) 

# Root Mean Squared Error (RMSE)
sqrt(mean((relev - TrainPredictions1)^2))
rm(TestPredictions1, TrainPredictions1, lm_model)
##########################################################################

ggplot(data = submission[submission$id %in% train$id,], aes(x = id)) + geom_histogram(binwidth=10)
