library(quanteda)
library(quanteda.textmodels) # some machine learning algorithms from quanteda
library(caret) # ecosystem of machine learning models
library(e1071) # machine learning models
library(randomForest) # random forest algorithm
library(openxlsx) # read xlsx
library(dplyr) # %>% operator

seed <- 123 # set random number generator seed for reproducibility

classified_data <- read.xlsx("Classfied messages (2 = work, 1 = not work).xlsx") # load labeled data

classified_data$work[classified_data$work == 1] <- "not_work" # 1 = not work
classified_data$work[classified_data$work == 2] <- "work" # 2 = work

classified_data <- classified_data[-which(duplicated(classified_data$text)), ] # remove duplicates

length(which(classified_data$work == "work")) # check number of observations about job offer
length(which(classified_data$work == "not_work")) # check number of everything else

set.seed(seed) # set seed for random generator (use this command before using a function that uses random numbers)
sample_index <- sample(1:nrow(classified_data), nrow(classified_data) / 5) # index random slice of data

# 1 of the key concepts for training supervised classification models
# create random samples of training data and test data for model verification
train_data <- classified_data[-sample_index, ] # index everything except random slice of data

test_data <- classified_data[sample_index, ] # index  random slice of data

# train model on train_data
# data preprocessing
corp_train <- corpus(train_data, text_field = "text")
dfm_train <- tokens(corp_train, remove_punct = T) %>%
  tokens_tolower() %>%
  tokens_wordstem(language = "ru") %>% # stemming
  tokens_ngrams(n = 1:2) %>% # we are going to build a model with unigrams and bigrams
  dfm() %>% 
  dfm_trim(min_termfreq = 10) %>% # remove tokens which occur less than 10 times on all documents
  dfm_select(min_nchar = 2) # remove tokens which are shorter than one character

# we are going to use a model with tf-idf
# we do not use the built-in dfm_tfidf function, 
# because we need to save the idf vector from the test data into a separate variable, 
# which we will use to calculate tf-idf for new data

# Our function for calculating term frequency (TF)
term.frequency <- function(row) { 
  row / sum(row)
}

# Our function for calculating inverse document frequency (IDF)
inverse.doc.freq <- function(col) {
  corpus_size <- length(col)
  doc_count <- length(which(col > 0))
  
  log10(corpus_size / doc_count)
}

# Our function for calculating TF-IDF.
tf.idf <- function(tf, idf) {
  tf * idf
}

train_idf_vector <- apply(dfm_train, 2, inverse.doc.freq) # apply function (inverse.doc.freq) to each column vector of dfm_train
dfm_train_tf_idf <- apply(dfm_train, 1, term.frequency) %>% # apply function (term.frequency) to each row vector of dfm_train
  apply(2, tf.idf, idf = train_idf_vector) %>% # apply function (tf.idf) to each row vector of dfm_train_tf. pass idf (train_idf_vector) argument to applied function
  t() # transpose matrix
dfm_train_tf_idf[which(!complete.cases(dfm_train_tf_idf)), ] <- rep(0.0, ncol(dfm_train_tf_idf)) # Fix documents that did not contain tokens at all (or were too few))
dfm_train_tf_idf <- as.dfm(dfm_train_tf_idf) # coerce matrix to dfm object


# applying model to our data


set.seed(seed) # set seed for random generator (use this command before using a function that uses random numbers)
nbmodel <- textmodel_nb(dfm_train_tf_idf, train_data$work) # train naive bayes model with dfm_train_tf_idf and labeled classes - train_data$work
# https://en.wikipedia.org/wiki/Naive_Bayes_classifier
summary(nbmodel)
set.seed(seed)
svm_model <- svm(dfm_train_tf_idf, as.factor(train_data$work), kernel = "linear", probability = T)
# https://en.wikipedia.org/wiki/Support_vector_machine
summary(svm_model)
set.seed(seed)
rand_forest <- convert(dfm_train_tf_idf, "matrix") %>% # convert dfm to matrix, because randomForest function can't deal with dfm object
  randomForest(y = as.factor(train_data$work))
# https://en.wikipedia.org/wiki/Random_forest
summary(rand_forest)
library(xgboost)
set.seed(seed)
boost_rand_forest <- convert(dfm_train_tf_idf, "matrix") %>% # random forest on steroids (very good algorithm)
  xgboost(label = as.factor(train_data$work), nrounds = 50)
# https://en.wikipedia.org/wiki/XGBoost
summary(boost_rand_forest)


# model testing on test data


# prepare new data
corp_test <- corpus(test_data, text_field = "text")
dfm_test <- tokens(corp_test, remove_punct = T) %>%
  tokens_tolower() %>%
  tokens_wordstem(language = "ru") %>%
  tokens_ngrams(n = 1:2) %>%
  dfm() %>%
  dfm_match(features = featnames(dfm_train)) %>% # IMPORTANT function (leaves in dfm only those tokens on which the model was trained)
  apply(1, term.frequency) %>% # # apply function (term.frequency) to each row vector of dfm_test
  apply(2, tf.idf, idf = train_idf_vector) %>% # USING TRAIN_IDF for proper vector space
  t() # transpose
dfm_test[which(!complete.cases(dfm_test)), ] <- rep(0.0, ncol(dfm_test)) # Fix documents that did not contain tokens at all (or were too few))
dfm_test <- as.dfm(dfm_test) # coerce matrix to dfm


# predict classes with new data


predicted_classNB <- predict(nbmodel, dfm_test) # predict classes (work, not work) on test data
tab_classNB <- table(predicted_classNB, test_data$work) # create confusion matrix
confusionMatrix(tab_classNB, mode = "everything") # caret function (calculates a lot of different metrics based on confusion matrix)
# Most important metrics
# Accuracy: (number of correctly classified) / (number of observations)
# Sensitivity: (number of correctly classified observations that actually fall into class 1) / (number of observations that actually fall into class 1)
# Specificity: (number of correctly classified observations that actually fall into class 2) / (number of observations that actually fall into class 2)
# for this example: class 1 - not_work, class 2 - work
# https://en.wikipedia.org/wiki/Sensitivity_and_specificity

predicted_classSVM <- predict(svm_model, newdata = dfm_test)
tab_classSVM <- table(predicted_classSVM, test_data$work)
confusionMatrix(tab_classSVM, mode = "everything")

predicted_classFOREST <- predict(rand_forest, newdata = dfm_test)
tab_classFOREST <- table(predicted_classFOREST, test_data$work)
confusionMatrix(tab_classFOREST, mode = "everything")

predicted_classXGBoost <- predict(boost_rand_forest, newdata = dfm_test)
tab_classXGBoost <- table(predicted_classFOREST, test_data$work)
confusionMatrix(tab_classXGBoost, mode = "everything")


rand_forest <- list(
  "random_forest" = rand_forest,
  "dfm_model" = dfm_train,
  "model_idf_vector" = train_idf_vector
)
saveRDS(rand_forest, "random forest model.RDS")


# load glove word2vec model


word_vectors <- readRDS("word_vectors.RDS")

guess_semantics <- word_vectors["требуется", , drop = FALSE] -
  word_vectors["мужчина", , drop = FALSE] +
  word_vectors["женщина", , drop = FALSE] +
  word_vectors["девушки", , drop = FALSE] -
  word_vectors["семейная", , drop = FALSE] -
  word_vectors["парень", , drop = FALSE]

cos_sim <- textstat_simil(x = as.dfm(word_vectors), y = as.dfm(guess_semantics),
                          method = "cosine")
vec <- sort(cos_sim[, 1], decreasing = TRUE) 
N <- 20
sim_words <- data.frame("cosine" = vec,
                        "token" = names(vec),
                        "token_rank" = 1:length(vec)) %>% 
  slice_head(n = N)

ggplot(sim_words, aes(x = token_rank, y = cosine)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = 1:N ,labels = sim_words$token, trans = "reverse") +
  scale_y_continuous(breaks = seq(0, 1, 0.25)) +
  coord_flip() +
  theme_bw(base_size = 28)



