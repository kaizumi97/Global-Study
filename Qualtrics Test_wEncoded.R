setwd(dir = "c://Users/Kaizu/Dropbox/Shared/Design/Full survey/Translations/Main Files/Main Files Used/")

# Load necessary libraries
library(readxl)
library(dplyr)
library(writexl)  # Library to write Excel files

# Source the script containing the checking functions
source("embedded_check_Kai.R")

# Define file paths
excel_file_path <- "Translation Data (Kai).xlsx"
sheet_name <- "Countries"
input_survey_path <- "Global_Study_Master_Version.qsf"
output_directory <- "translated_qsf_files/"  # Directory to save translated files

# Load the translation data from the Excel file
translations <- read_excel(excel_file_path, sheet = sheet_name)

# Ensure the output directory exists
if (!dir.exists(output_directory)) {
  dir.create(output_directory)
}

# Read the Qualtrics survey file
survey_content <- readLines(input_survey_path, warn = FALSE)

# Function to replace text based on translation table and perform checks
replace_text <- function(content, translations, country) {
  error_list <- list()
  
  for (i in 1:nrow(translations)) {
    original_text <- translations[i, "Original"]
    replacement_text <- translations[i, country]
    
    # Skip if the replacement text is 0 or NA or empty
    if (replacement_text == 0 || is.na(replacement_text) || replacement_text == "" || is.na(original_text) || original_text == "") {
      next
    }
    
    # Check for embedded, bold, and underline text errors before replacement
    embedded_check <- embedded_check_all(original_text, replacement_text, i)
    if (!is.null(embedded_check)) {
      error_list <- append(error_list, list(list(Row = i, Error = embedded_check, Original = as.character(original_text), Translation = as.character(replacement_text))))
    }
    
    # Replace text in the content
    content <- gsub(original_text, replacement_text, content, fixed = TRUE)
  }
  
  # Return modified content and errors
  return(list(content = content, errors = error_list))
}

# Initialize a data frame to store errors for all countries
all_errors <- data.frame(Country = character(), Row = integer(), Error = character(), Original = character(), Translation = character(), stringsAsFactors = FALSE)

# Get all country columns (excluding the first 'Original' column)
country_columns <- colnames(translations)[-1]
#country_columns <- c("Poland","UK") # For Specific countries
#country_columns <- "Philippines (English)"  # Replace with your desired country name


# Iterate over each country column and create a translated survey file
for (country in country_columns) {
  # Check if there is any non-NA, non-empty data for the current country
  if (all(is.na(translations[[country]]) | translations[[country]] == "")) {
    cat("Skipping", country, "due to lack of data.\n")
    next
  }
  
  # Perform the text replacement and check for errors for the current country
  result <- replace_text(survey_content, translations, country)
  modified_survey_content <- result$content
  country_errors <- result$errors
  
  # Store errors for the country in a dataframe
  if (length(country_errors) > 0) {
    error_df <- do.call(rbind, lapply(country_errors, function(x) data.frame(
      Country = country, 
      Error = x$Error, 
      Original = x$Original, 
      Translation = x$Translation, 
      stringsAsFactors = FALSE
    )))
    all_errors <- rbind(all_errors, error_df)
  }
  
  # Change the survey name for the current country
  modified_survey_content <- gsub(
    '"SurveyName":"Global Study Master Version"',
    paste0('"SurveyName":"Global Study ', country, '"'),
    modified_survey_content,
    fixed = TRUE
  )
  
  # Define output file path for the current country
  output_survey_path <- paste0(output_directory, "survey_", country, ".qsf")
  
  # Write the modified content to a new file
  writeLines(modified_survey_content, output_survey_path)
  
  cat("Text replacement completed for", country, ". The modified survey has been saved to", output_survey_path, "\n")
}

# Display the summary of errors and write to Excel file
if (nrow(all_errors) > 0) {
  message <- paste("There are:", nrow(all_errors), "errors found.")
  print(message)
  
  # Write the errors to an Excel file
  excel_output_path <- "translation_errors_summary.xlsx"
  write_xlsx(all_errors, excel_output_path)
  cat("Summary of errors has been saved to", excel_output_path, "\n")
} else {
  print("No errors found in any country.")
}
