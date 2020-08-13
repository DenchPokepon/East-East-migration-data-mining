library(quanteda) # text analysis ecosystem of functions
library(openxlsx) # read xlsx
library(dplyr) # %>% operator
library(stringr) # work with strings/characters
library(knitr) # pretty tables
library(lubridate) # save sanity when working with dates
library(ggplot2) # plots
library(ggraph) # graphs
library(igraph) # graphs
library(visNetwork) # html java interactive graphs

quanteda_options("threads" = 6) # set number of threads for efficient computing
quanteda_options("verbose" = TRUE) # return processing information when executing functions

setwd("C:/Users/denis/Desktop/KOREA") # working directory

source("FUN segments connector.R", encoding = "UTF-8") # source function

classified_data <- read.xlsx("train_segmentation 2866.xlsx", detectDates = T) # read data

classified_data <- classified_data[1:2866, ] # index only classified data

segmented_data <- segments.connector(classified_data) # connect segments

segmented_data <- segmented_data[nchar(segmented_data$text) != 0, ] # remove empty messages


corp <- corpus(segmented_data, text_field = "text") # create corpus of texts


# analyzing text summary statistics


toks <- tokens(corp, remove_punct = TRUE, remove_numbers = TRUE) %>% # tokenize corpus, remove punctuation and all numbers
  tokens_tolower() %>% # lowercase tokens
  tokens_wordstem(language = "ru") %>% # stem tokens
  tokens_ngrams(1:4) # create unigrams, bigrams, trigrams, 4-grams

dfm_data <- dfm(toks) # create document-term matrix


# these commands creates a token distribution graph
textstat_frequency(dfm_data) %>% # calculate tokens frequency over all documents
  ggplot(aes(x = rank, y = frequency)) + # create basis of a plot (choose x and y axis data)
  geom_line() + # add geometry to represent specified data in particular way
  labs(x = "Frequency rank", y = "Term frequency") # sign the axes of the chart

# plot the most common tokens
textstat_frequency(dfm_data, n = 10) %>% # calculate tokens frequency over all documents and subset top 10
  ggplot(aes(x = reorder(feature, -rank), y = frequency)) + # create basis of a plot (choose x and y axis data), order observations by descending order
  geom_bar(stat = "identity") + # add geometry to represent specified data in particular way
  coord_flip() + # flip system of coordinates
  labs(x = "", y = "Term Frequency as a Count") # sign the axes of the chart

man_df <- dfm_data %>% # create new dfm which will contain only one token - "мужчин"
  dfm_keep(pattern = "мужчин") %>%
  convert(to = "data.frame") # conver dfm to data.frame (special quanteda transformer function)

man_df$doc_id <- 1:nrow(man_df) # replace document titles with indexes to avoid text overplotting 

# plot the frequency of the token in each document
ggplot(data = man_df, aes(x = doc_id, y = мужчин)) +
  geom_bar(stat = "identity") + # add geometry to represent specified data in particular way
  labs(x = "Message",
       y = "Frequency",
       title = 'Occurrence of "мужчин"')


## TF-IDF


dfm_data <- dfm(toks) %>% 
  dfm_tfidf(scheme_tf = "prop") # tfidf from quanteda (sheme_tf default to "count", but classic tf is "prop")

textstat_frequency(dfm_data, force = TRUE) %>% # take note of the force argument, quanteda automatically recognizes weighted documents. counting the frequency of weighted tokens can be distorted heavily by outliers
  ggplot(aes(x = rank, y = frequency)) +
  geom_line() +
  labs(x = "Frequency rank", y = "Term frequency")

textstat_frequency(dfm_data, n = 10, force = TRUE) %>%
  ggplot(aes(x = reorder(feature, -rank), y = frequency)) +
  geom_bar(stat = "identity") + coord_flip() +
  labs(x = "", y = "Term Frequency as a Count")


# TF-IDF by hands


term.frequency <- function(row) { # TF
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


dfm_data <- dfm(toks)

# basic but very slow approach
data_tf <- numeric()
for (i in 1:nrow(dfm_data)) {
  print(i)
  row <- convert(dfm_data[i, ], "matrix")
  data_tf <- rbind(data_tf, term.frequency(row))
}
data_idf <- numeric()
for (i in 1:ncol(dfm_data)) {
  print(i)
  column <- convert(dfm_data[, i], "matrix")
  data_idf <- rbind(data_idf, inverse.doc.freq(column))
}
data_tf_idf <- numeric()
for (i in 1:ncol(data_tf)) {
  print(i)
  column <- data_tf[, i, drop = FALSE]
  data_tf_idf <- cbind(data_tf_idf, tf.idf(column, data_idf[i]))
}


# good approach
?apply
data_tf <- apply(dfm_data, 1, term.frequency) # Returns a vector or array or list of values obtained by applying a function to margins of an array or matrix
data_idf <- apply(dfm_data, 2, inverse.doc.freq)
data_tf_idf <- apply(data_tf, 2, tf.idf, idf = data_idf) %>% 
  t() %>%  # matrix transpose
  as.dfm()



toks <- tokens(corp, remove_punct = TRUE, remove_numbers = TRUE) %>%
  tokens_tolower() %>%
  tokens_wordstem(language = "ru")
collocations <- textstat_collocations(toks)

toks <- tokens_compound(toks, collocations[1:20, ])

dfm_data <- dfm(toks) %>% 
  dfm_trim(5) %>%
  dfm_tfidf(scheme_tf = "prop")

dual.cor <- function(df) { # duplicating df, but with rearranged token names. Used for easier indexing of tokens
  df_dummy <- df[, c(2, 1, 3)] %>% 
    `names<-` (names(df))
  rbind(df, df_dummy)
}

correlations <- textstat_simil(dfm_data, margin = "features" , method = "correlation") %>% # calculate pearson correlation coef between tokens
  as.data.frame() %>% # coerce textstat_simil object to data.frame
  dual.cor() %>% # rbind additional df with rearranged token names
  filter(str_detect(feature1, "^чонс$|^арбайт$|^петын$|^мотел$|^щикта$|^морск$")) # filter correlations with these tokens

correlations %>% 
  group_by(feature1) %>% # group by tokens
  top_n(20) %>% # subset top 20 correlations
  ungroup() %>% # ungroup by tokens
  mutate(feature2 = reorder(feature2, correlation)) %>% # reorder feature2 by correlation
  ggplot(aes(feature2, correlation)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~ feature1, scales = "free") + # create 6 different plots in one (grouped by feature1)
  coord_flip() + # flip coordinates
  theme_bw(base_size = 28) # set theme and font size

png("correlations pearson.png", 1920, 1080)
correllations %>%
  group_by(feature1) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(feature2 = reorder(feature2, correlation)) %>%
  ggplot(aes(feature2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ feature1, scales = "free") +
  coord_flip() +
  theme_bw(base_size = 28)
dev.off()


# you can cluster documents or words based on different dissimilarity metrics

hclust_tree <- textstat_dist(dfm_data, margin = "documents", method = "euclidean") %>% 
  as.dist() %>% 
  hclust("ward.D2") # hierarchical clustering https://en.wikipedia.org/wiki/Hierarchical_clustering

library(ggdendro)

png("dendogram.png", width = 1920, 1080)
ggdendrogram(hclust_tree, rotate = TRUE, size = 3) # plots dendogram https://ru.wikipedia.org/wiki/%D0%94%D1%80%D0%B5%D0%B2%D0%BE%D0%B2%D0%B8%D0%B4%D0%BD%D0%B0%D1%8F_%D1%81%D1%85%D0%B5%D0%BC%D0%B0
dev.off()

clusters <- textstat_dist(dfm_data, margin = "documents", method = "euclidean") %>% 
  as.dist() %>% # coerce textstat_dist to dist object
  hclust("ward.D2") %>% 
  cutree(5) # create 5 clusters from data


# correlations representation through graphs


correlations %>%
  filter(correlation > .15) %>% 
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

png("work correlation graph.png", 3840, 2160)
correlations %>%
  filter(correlation > 0.15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 12) +
  geom_node_text(aes(label = name), repel = TRUE, size = 12) +
  theme_void()
dev.off()


## you can make very big interactive graphs with visNetwork


correlations <- textstat_simil(dfm_data, margin = "features" , method = "correlation") %>% # calculate pearson correlation coef between tokens
  as.data.frame() # coerce textstat_simil object to data.frame


edges <- correlations %>%
  filter(correlation > 0.30) %>% 
  `names<-` (c("from", "to", "value"))
edges$label <- round(edges$value, 1)

nodes <- data.frame("id" = unique((c(as.character(edges$from), as.character(edges$to)))),
                    "label" = unique(c(as.character(edges$from), as.character(edges$to))))

visNetwork(nodes, edges, width = 1920, height = 1080) %>%
  visIgraphLayout(layout = "layout_with_fr") %>% 
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) 


## dictionary method


dict_raw <- read.xlsx("dictionary.xlsx") # dictionary in xlsx format
dict <- split(dict_raw[, c("standard_token", "token")], dict_raw[, "class"]) %>% # coerce excel table to quanteda dictionary
  lapply(function(class) split(class[, "token", drop = TRUE], class[, "standard_token"])) %>% # coerce excel table to quanteda dictionary
  dictionary(separator = " ") # coerce excel table to quanteda dictionary

toks <- tokens(corp, remove_punct = T)

toks_money <- tokens_lookup(toks, dict$`money_(KRW)`, valuetype = "regex", exclusive = T) 
# exclusive = TRUE - delete all tokens except those in the dictionary (dictionary mode)
# exclusive = FALSE - turn tokens into their dictionary form, if it's specified in the dictionary (thesaurus mode)
