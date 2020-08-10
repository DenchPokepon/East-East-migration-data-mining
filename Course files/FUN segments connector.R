library(stringr)


segments.connector <- function(df) {
  df$doc_id2 <- str_extract(df$doc_id, ".*(?=\\.)")
  messages_list <- list()
  for (message in unique(df$doc_id2)) {
    messages_list[[message]] <- list()
    message_index <- which(df$doc_id2 == message)
    segment_start <- message_index[1]
    segment_name <- paste0("segment", segment_number <- 1)
    if (length(message_index) < 2) {
      messages_list[[message]][[segment_name]] <- message_index
      next
    }
    line <- message_index[1]
    mes_len <- length(message_index)
    while (line != message_index[mes_len]) {
      if ((df$start_end[line] != 1 | line == segment_start) & 
          df$text[line + 1] != "between_message_space") {
        line <- line + 1
      } else if (df$start_end[line] == 1 & line != segment_start) {
          messages_list[[message]][[segment_name]] <- segment_start:line
          segment_name <- paste0("segment", segment_number <- segment_number + 1)
          segment_start <- line + 1
          line <- line + 1
      } else {
        messages_list[[message]][[segment_name]] <- segment_start:line
        line <- line + 1
      }
    }
  }
  segmented_messages <- lapply(messages_list, function(message) {
    mapply(message, names(message), FUN = function(segment, segment_name) {
      segment_lines <- df[segment[1], ]
      segment_lines$text <- paste0(df$text[segment], collapse = "\n") %>% 
        str_remove_all("inside_message_space")
      segment_lines$segment_name <- paste0(segment_lines$doc_id, ".", segment_name)
      segment_lines
    }, SIMPLIFY = FALSE) %>% 
      do.call("rbind", args = .)
  }) %>% 
    do.call("rbind", args = .)
  return(segmented_messages)
}
