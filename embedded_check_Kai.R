read_embedded <- function(origtext, newtext, i) {
  # Define the pattern to search for (non-greedy)
  pattern <- "\\$\\{.*?\\}"
  
  # Find all occurrences of the pattern in the original text
  matches <- gregexpr(pattern, origtext, perl = TRUE)
  
  # Extract the substrings based on the pattern matches
  substrings <- regmatches(origtext, matches)[[1]]
  
  # Find all occurrences of the pattern in the new text
  new_matches <- gregexpr(pattern, newtext, perl = TRUE)
  new_substrings <- regmatches(newtext, new_matches)[[1]]
  
  # Check if the number of patterns is the same
  if (length(substrings) != length(new_substrings)) {
    return(paste("Wrong embedded text in translation row", i))
  }
  
  # Count occurrences of each substring in the original and new texts
  orig_counts <- table(substrings)
  new_counts <- table(new_substrings)
  
  # Ensure both tables have the same structure
  all_names <- union(names(orig_counts), names(new_counts))
  orig_counts <- orig_counts[all_names]
  new_counts <- new_counts[all_names]
  orig_counts[is.na(orig_counts)] <- 0
  new_counts[is.na(new_counts)] <- 0
  
  # Compare the counts of each substring in both texts
  if (!all(orig_counts == new_counts)) {
    return(paste("Wrong embedded text in Translation Row", i))
  }
  
  # Check if the order of the patterns is the same
  if (!identical(substrings, new_substrings)) {
    return(paste("Correct embedded texts, wrong order: Row", i))
  }
  
  # If all substrings and their counts and order match, return "correct"
  return("correct")
}



read_bold <- function(origtext, newtext, i) {
  # Define the start and end delimiters
  start_delim <- "<b>"
  end_delim <- "<\\\\/b>"
  
  # Create the pattern to search for based on the provided delimiters
  pattern <- paste0(start_delim, ".*?", end_delim)
  
  # Find all occurrences of the pattern in the original text
  matches <- gregexpr(pattern, origtext, perl = TRUE)
  
  # Extract the substrings based on the pattern matches
  substrings <- regmatches(origtext, matches)[[1]]
  
  # Find all occurrences of the pattern in the new text
  new_matches <- gregexpr(pattern, newtext, perl = TRUE)
  new_substrings <- regmatches(newtext, new_matches)[[1]]
  
  # Check if the number of patterns is the same
  if (length(substrings) != length(new_substrings)) {
    return(paste("Wrong amount of bold in Translation Row", i))
  }
  
  # If the number of patterns matches, return "correct"
  return("correct")
}

read_ul <- function(origtext, newtext, i) {
  # Define the start and end delimiters
  start_delim <- "<u>"
  end_delim <- "<\\\\/u>"
  
  # Create the pattern to search for based on the provided delimiters
  pattern <- paste0(start_delim, ".*?", end_delim)
  
  # Find all occurrences of the pattern in the original text
  matches <- gregexpr(pattern, origtext, perl = TRUE)
  
  # Extract the substrings based on the pattern matches
  substrings <- regmatches(origtext, matches)[[1]]
  
  # Find all occurrences of the pattern in the new text
  new_matches <- gregexpr(pattern, newtext, perl = TRUE)
  new_substrings <- regmatches(newtext, new_matches)[[1]]
  
  # Check if the number of patterns is the same
  if (length(substrings) != length(new_substrings)) {
    return(paste("Wrong underline text in Translation Row", i))
  }
  
  # If the number of patterns matches, return "correct"
  return("correct")
}

embedded_check_all <- function(origtext, newtext, i) {
  # Skip the first 56 rows
  if (i <= 56) {
    return(NULL)
  }
  
  error_messages <- c()
  
  embedded_text <- read_embedded(origtext, newtext, i)
  if (embedded_text != "correct") {
    error_messages <- c(error_messages, embedded_text)
  }
  
  bold_text <- read_bold(origtext, newtext, i)
  if (bold_text != "correct") {
    error_messages <- c(error_messages, bold_text)
  }
  
  ul_text <- read_ul(origtext, newtext, i)
  if (ul_text != "correct") {
    error_messages <- c(error_messages, ul_text)
  }
  
  if (length(error_messages) > 0) {
    return(paste(error_messages, collapse = "; "))
  } else {
    return(NULL)  # Return NULL if no errors
  }
}
