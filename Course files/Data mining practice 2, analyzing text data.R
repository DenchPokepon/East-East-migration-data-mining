library(quanteda) # text analysis ecosystem of functions
library(openxlsx) # read xlsx
library(dplyr) # %>% operator
library(stringr) # work with strings/characters
library(knitr) # pretty tables
library(lubridate) # save sanity when working with dates

quanteda_options("threads" = 6) # set number of threads for efficient computing
quanteda_options("verbose" = TRUE) # return processing information when executing functions


setwd("C:/Users/denis/Desktop/KOREA") # working directory

source("FUN segments connector.R", encoding = "UTF-8") # source function

classified_data <- read.xlsx("train_segmentation 2866.xlsx", detectDates = T) # read data

classified_data <- classified_data[1:2866, ] # index only classified data

segmented_data <- segments.connector(classified_data) # connect segments

segmented_data <- segmented_data[nchar(segmented_data$text) != 0, ] # remove empty messages

corp <- corpus(segmented_data, text_field = "text") # create corpus of texts


## non-informative tokens


good_stopwords <- stopwords(language = "russian") %in% c("не", "все") # determine your vector of stopwords

stopwords(language = "russian") # set non-informative of tokens from snowball project

toks <- tokens(corp, remove_punct = TRUE) %>% 
  tokens_select(stopwords("russian")[-good_stopwords], "remove") # delete all specified  patterns 
# (by default - the pattern is specified as a glob expression) 
# glob: https://ru.wikipedia.org/wiki/%D0%A8%D0%B0%D0%B1%D0%BB%D0%BE%D0%BD_%D0%BF%D0%BE%D0%B8%D1%81%D0%BA%D0%B0
# regex: https://ru.wikipedia.org/wiki/%D0%A0%D0%B5%D0%B3%D1%83%D0%BB%D1%8F%D1%80%D0%BD%D1%8B%D0%B5_%D0%B2%D1%8B%D1%80%D0%B0%D0%B6%D0%B5%D0%BD%D0%B8%D1%8F
# fixed: exact string matching
# valuetype argument in tokens_select: https://quanteda.io/reference/valuetype.html

dfm_data <- dfm(toks) # create document-term matrix. Rows - documents (job offers), columns - tokens/features
# document and token intersection - number of tokens in this document (simple count)

frequency <- textstat_frequency(dfm_data) # the function counts the token/feature frequency in all documents

kable(frequency[1:20, ]) # pretty table


## wordcloud and graphics


textplot_wordcloud(dfm_data) # function creates a graph - word cloud
# the size of words and their proximity to the center reflect their frequency of occurrence in all documents

png("wordcloud") # open graphic device of type PNG, automatically creates a graph in the working directory with this name
textplot_wordcloud(dfm_data) # function creates a graph - word cloud
dev.off() # close current graphic device

png("wordcloud.png", width = 1920, height = 1080) # open graphic device of type PNG, automatically creates a graph in the working directory with this name
textplot_wordcloud(dfm_data) # function creates a graph - word cloud
dev.off() # close current graphic device


## collocations analysis


collocations <- textstat_collocations(toks) # Returns a table with data on possible token collocations
# The table is sorted Wald by z-statistics
# good visualization on z-statistic: 
# https://desktop.arcgis.com/ru/arcmap/10.3/tools/spatial-statistics-toolbox/GUID-CBF63B74-D1B2-44FC-A316-7AC2B1C1D464-web.png

collocations3 <- textstat_collocations(toks, size = 3) # by default size = 2, we can tune it

# connect most significant collocations in tokens object
toks <- tokens_compound(toks, collocations[1:100, ])
# function usage examples (below description): https://quanteda.io/reference/tokens_compound.html

dfm_data <- dfm(toks) # create new dfm with compounded tokens

frequency <- textstat_frequency(dfm_data) # calculate new frequencies

png("wordcloud.png", width = 3840, height = 2160) # 4K ultra HD word cloud
textplot_wordcloud(dfm_data)
dev.off()

sparsity(dfm_data) # return the proportion of sparseness of a document-feature matrix, equal to the proportion of cells that have zero counts

dfm_data <- dfm_trim(dfm_data, 5) 
# Returns a document by feature matrix reduced in size based on document and term frequency, 
#   usually in terms of a minimum frequency, but may also be in terms of maximum frequencies. 
# Setting a combination of minimum and maximum frequencies will select features based on a range.
# Feature selection is implemented by considering features across all documents, 
#   by summing them for term frequency, or counting the documents in which they occur for document frequency.
# more examples: https://quanteda.io/reference/dfm_trim.html?q=dfm%20_%20trim

sparsity(dfm_data) # greatly reduced

dfm_data <- dfm(toks) %>% 
  dfm_trim(min_docfreq  = 0.05,
           max_docfreq = 0.9,
           docfreq_type = "prop")

sparsity(dfm_data)

dfm_data <- dfm(toks) %>% 
  dfm_weight("prop") # https://quanteda.io/reference/dfm_weight.html?q=dfm%20_%20w


## stemming


toks <- tokens(corp, remove_punct = TRUE) %>% 
  tokens_select(stopwords("russian"), "remove") %>% 
  tokens_wordstem("russian") # https://quanteda.io/reference/tokens_wordstem.html?q=tokens%20_%20wordstem

collocations <- textstat_collocations(toks)

toks <- tokens_compound(toks, collocations[1:30, ])

dfm_data <- dfm(toks) %>% 
  dfm_trim(5)

sparsity(dfm_data)

frequency <- textstat_frequency(dfm_data)
kable(frequency[1:20, ])

png("wordcloud.png", width = 1920, height = 1020)
textplot_wordcloud(dfm_data)
dev.off()


## n-grams* https://quanteda.io/reference/tokens_ngrams.html?q=tokens%20_%20ngram
# they are not very often used for text analysis, because collocations are doing better in this area
# they are much more commonly used in machine learning tasks


text <- "Съешь ещё этих мягких французских булок, да выпей же чаю"

text %>% 
  tokens() %>% 
  tokens_ngrams(n = 2)

text %>% 
  tokens() %>% 
  tokens_ngrams(n = 3)

text %>% 
  tokens() %>% 
  tokens_ngrams(n = 4)

text %>% 
  tokens() %>% 
  tokens_ngrams(n = 1:2)

text %>% 
  tokens() %>% 
  tokens_ngrams(n = 2:4)

text %>% 
  tokens() %>% 
  tokens_ngrams(n = 2:4, skip = 2)


