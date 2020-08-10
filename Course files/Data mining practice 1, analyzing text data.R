install.packages("quanteda")
install.packages("openxlsx")
install.packages("dplyr")
install.packages("stringr")
install.packages("knitr")
install.packages("lubridate")

# download libraries

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

corp_summary <- textstat_summary(corp) # summary on corpus


corp_summary$date <- segmented_data$date # add date column to summary data.frame
corp_summary$date <- round_date(corp_summary$date, unit = "month") # round date to month


corp_summary <- corp_summary[order(corp_summary$date), ] # sort by date


tokens_freq <- corp_summary %>% 
  group_by(date) %>% 
  summarise(tokens = sum(tokens)) # calculate tokens frequency in time

plot(tokens_freq$date, tokens_freq$tokens)


kwic(corp, "мужчин", 2, "regex") # key word in context


# tokens

toks <- tokens(corp, remove_punct = TRUE) # tokenizer

# dfm

dfm_data <- dfm(toks) # create document-term matrix

frequency <- textstat_frequency(dfm_data) # cacluclate frequency of each token

kable(frequency[1:20, ])
