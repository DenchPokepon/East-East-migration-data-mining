library(rjson) # parse JSON
library(lubridate) # dates
library(stringr) # strings operations
library(tibble) # add_column function

setwd("C:/Users/denis/Desktop/KOREA")

viber_dir <- "viber messages" # viber messages directory
whatsapp_dir <- "whatsapp messages" # whatsapp messages directory
telegram_dir <- "telegram messages" # telegram messages directory


telegram.to.data.frame <- function(path) {
  
  file <- list.files(path, pattern = "result*", full.names = T) # returns all files that located in a specified path
  data <- fromJSON(file = file) # parse JSON file to R list
  messages <- lapply(data$messages, function(message) { # label service messages
    if (message$type == "message") message else NULL
  })
  messages <- messages[unlist(lapply(messages, function(message) !is.null(message)))] # delete service messages
  df <- lapply(messages, function(message) { # transform JSON list to data frame
    if (!is.list(message$text)) { 
      text <- message$text # if not list than just string
    } else { # if list then process list of characters
      text <- lapply(message$text, function(submessage) {
        if (is.list(submessage)) submessage$text else submessage
      }) %>%
        c(sep = "\n") %>%
        do.call("paste", args = .)
    }
    c(
      "id" = message$id,
      "date" = message$date,
      "from" = if (data$type == "public_supergroup") {
        if (!is.null(message$from)) {
          message$from
        } else {
          NA
        }
      } else {
        data$name
      },
      "from_id" = if (data$type == "public_supergroup") {
        message$from_id
      } else {
        data$id
      },
      "text" = text
    )
  }) %>%
    c(stringsAsFactors = FALSE) %>%
    do.call("rbind.data.frame", args = .)
  names(df) <- c("id", "date", "from", "from_id", "text")
  df$date <- ymd_hms(df$date) # coerce character to date type
  
  # concatenate messages, that has been sent by one person in short interval of time
  text_list <- list() 
  time_limit <- 5 # minutes
  i <- 1
  while (i != nrow(df)) {
    j <- i + 1
    text_list[[paste0(i)]] <- matrix(
      c(as.character(df$id[i]),
        as.character(df$date[i]),
        as.character(df$from[i]),
        as.character(df$from_id[i]),
        as.character(df$text[i])),
      nrow = 1,
      ncol = 5)
    while (df$from_id[i] == df$from_id[j] & j != nrow(df)) {
      if (abs(difftime(df$date[j - 1], df$date[j], units = "mins")) < time_limit) {
        j <- j + 1
      } else {
        break
      }
    }
    if (length(i:(j - 1)) > 1) {
      text_list[[paste0(i)]][1, 5] <- df[i:(j - 1), 5] %>% 
        paste(collapse = "\n")
    }
    i <- j
  }
  
  df <- do.call("rbind", text_list) %>% 
    as.data.frame(stringsAsFactors=FALSE) %>% 
    `names<-` (c("id", "date", "from", "from_id", "text"))
  df$date <- ymd_hms(df$date)
  df$group_name <- data$name
  df$group_type <- data$type
  df$messenger <- "telegram"
  
  return(df)
}


chat.to.data.frame <- function(path, messenger) {
  
  if (tolower(messenger) == "whatsapp") {
    regex <- "[0-3][0-9]\\.[0-1][0-9]\\.(20)?[0-9]{2}, [0-2][0-9]:[0-9]{2} - (.*?)([^:]+|присоединился|покинул|удалил|изменён)"
    date_regex1 <- "[0-3][0-9]\\.[0-1][0-9]\\.(20)?[0-9]{2}, [0-2][0-9]:[0-9]{2} -"
    date_regex2 <- ", [0-2][0-9]:[0-9]{2} -"
  } else if (tolower(messenger) == "viber") {
    regex <- "\\[ ([0-9]|[0-9]{2}) .* (20)?[0-9]{2} .\\. ([0-9]|[0-9][0-9]):[0-9]{2} ] [^:]+"
    date_regex1 <- "\\[ ([0-9]|[0-9]{2}) .* (20)?[0-9]{2} .\\. ([0-9]|[0-9][0-9]):[0-9]{2} ]"
    date_regex2 <- "г\\. ([0-9]|[0-9][0-9]):[0-9]{2} ]"
  } else {
    stop("This function only supports whatsapp, viber")
  }
  
  data <- readLines(path, encoding = "UTF-8", warn = T)
  
  grep_res <- grep(pattern = regex, x = data)
  df <- matrix("", nrow = 0, ncol = 2)
  colnames(df) <- c("message", "text")
  i <- 1
  
  while (i < length(grep_res) + 1) {
    if (i < length(grep_res)) {
      line1 <- grep_res[i]
      line2 <- grep_res[i + 1]
      text_lines <- line1:(line2 - 1)
      text <- paste0(data[text_lines], collapse = "\n")
    } else {
      line1 <- grep_res[i]
      text <- paste0(data[grep_res[i]:length(data)], collapse = "\n")
    }
    message <- c(data[grep_res[i]], text)
    df <- rbind(df, message)
    i <- i + 1
  }
  df[, 1] <- str_extract(df[, 1], regex)
  df[, 2] <- str_remove(df[, 2], regex)
  df <- as.data.frame(df)
  df[1:ncol(df)] <- apply(df, 2, as.character)
  name <- str_remove(df$message, date_regex1) # extract date
  date <- str_extract(df$message, date_regex1) # extract date
  date <- str_remove(date, date_regex2) # extract date
  if (tolower(messenger) == "viber") { # exeptions that dmy can't parse properly
    date <- str_remove(date, "\\[")
    date <- str_replace(date, "марта", "03")
    date <- str_replace(date, "августа", "08")
    date <- str_replace(date, "мая", "05")
  }
  date <- dmy(date, locale = "Russian_Russia.1251", truncated = 0)
  df <- tibble::add_column(df, "date" = date, "from" = name, .before = "message")
  df$message <- NULL # remove source message
  df$group_name <- str_extract(path, "(?<=messages\\/)(.*)(?=\\.)")
  df$group_type <- paste(messenger, "group")
  df$messenger <- messenger
  
  return(df)
}


chat.loader <- function(directory = NULL, files = NULL, messenger = NULL) {
  
  messenger_list <- list()
  if (tolower(messenger) == "telegram") {
    dirs <- list.dirs("telegram messages/")
    ind <- str_which(dirs, "telegram messages\\/\\/.*dir(?!\\/)")
    dirs <- dirs[ind]
    for (dir in dirs) {
      messenger_list[[str_extract(dir, "(?<=//)(.*)(?= dir)")]] <- telegram.to.data.frame(dir)
    }
    return(messenger_list)
  }
  for (file in files) {
    path <- paste(directory, file, sep = "/")
    messenger_list[[str_extract(file, "^.*(?=(\\.txt))")]] <- chat.to.data.frame(path, messenger)
  }
  return(messenger_list)
}


viber_list <- chat.loader(viber_dir, 
                          list.files(path = viber_dir, pattern = "\\.txt"), 
                          "viber")

whatsapp_list <- chat.loader(whatsapp_dir, 
                             list.files(path = whatsapp_dir, pattern = "\\.txt"), 
                             "whatsapp")

telegram_list <- chat.loader(telegram_dir, messenger = "telegram") %>% 
  lapply(function(group) {
    group$id <- NULL # information standardization for unifying telegram with whatsapp and viber 
    group$from_id <- NULL
    group
  })
