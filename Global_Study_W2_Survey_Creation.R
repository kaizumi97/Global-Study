setwd(dir = "c://Users/Kaizu/Downloads/")


# Load libraries
library(readxl)
library(writexl)
library(dplyr)

# Source the script containing the checking functions
source("embedded_check.R")

# Load Excel data
excel_file_path <- "Translation Data (New Global Study).xlsx"
translations <- read_excel(excel_file_path, sheet = "Countries")
# 🔐 Escape all quotation marks in all strings

translations$Original <- gsub('"', '\\\\"', translations$Original)

translations[] <- lapply(translations, function(column) {
  if (is.character(column)) {
    column <- gsub("\u2014", "\\u2014", column, fixed = TRUE) # em dash → \u2014
    column
  } else {
    column
  }
})

ppp_data <- read_excel(excel_file_path, sheet = "Language & Currency Data")

# Step 1: Extract dollar values from PPP column headers
# (These are numeric-like columns, e.g. "1", "3", "5", "10", "25", etc.)
dollar_columns <- colnames(ppp_data)
dollar_values <- suppressWarnings(as.numeric(dollar_columns))
dollar_values <- dollar_values[!is.na(dollar_values)]  # Remove non-numeric
dollar_values

# Pre-format PPP values to use safe dot character for dot-separator countries
for (i in 1:nrow(ppp_data)) {
  if (!is.na(ppp_data$`Thousand Seperator`[i]) && ppp_data$`Thousand Seperator`[i] == "Dot") {
    for (col in colnames(ppp_data)) {
      if (grepl("^[0-9]+$", col)) {  # Only numeric columns (like "1", "3", "5", etc.)
        val <- as.character(ppp_data[i, col, drop = TRUE])
        # Replace dot as thousand separator with Unicode dot \u2024
        if (!is.na(val)) {
          new_val <- gsub("(?<=\\d)\\.(?=\\d{3}(\\D|$))", "\u2024", val, perl = TRUE)
          ppp_data[i, col] <- new_val
        }
      }
    }
  }
}

# Step 2: Loop through translation columns
translation_columns <- setdiff(colnames(translations), "Original")

for (col in translation_columns) {
  # If column is e.g., "India (English)", get base country name "India"
  base_country <- sub(" \\(English\\)$", "", col)
  ppp_row <- which(ppp_data$Countries == base_country)
  if (length(ppp_row) == 0) next
  
  rows_to_edit <- 13:nrow(translations)  # ✅ Skip rows 1–12
  
  # === First pass: Replace all $X with placeholders to avoid compounding ===
  for (value in rev(dollar_values)) {
    if (value == 1) next
    
    if (value == 1000) {
      originals_with_1000_milliones <- grepl("\\$1000\\s+milliones", translations$Original, ignore.case = TRUE)
      if (any(originals_with_1000_milliones)) {
        matching_translation <- translations[[col]][originals_with_1000_milliones]
        if (any(grepl("\\$1000\\s+milliones", matching_translation, ignore.case = TRUE))) {
          next
        }
      }
    }
    
    if (value == 10) {
      originals_with_10_yi <- grepl("\\$10\\s+亿", translations$Original, ignore.case = TRUE)
      if (any(originals_with_10_yi)) {
        matching_translation <- translations[[col]][originals_with_10_yi]
        if (any(grepl("\\$10\\s+亿", matching_translation, ignore.case = TRUE))) {
          next
        }
      }
    }
    
    if (base_country == "Pakistan") {
      # Match both $100 and 100$
      pattern <- paste0("(?<!\\d)\\$", value, "(?!\\s*millones)(?=\\D|$)|(?<=\\D|^)", value, "\\$(?!\\d)")
    } else {
      pattern <- paste0("(?<!\\d)\\$", value, "(?!\\s*millones)(?=\\D|$)")
    }    
    placeholder <- paste0("<<REPLACE_", value, ">>")
    translations[[col]][rows_to_edit] <- gsub(pattern, placeholder, translations[[col]][rows_to_edit], perl = TRUE)
  }
  
  # === Second pass: Replace placeholders with actual values ===
  for (value in dollar_values) {
    if (value == 1) next
    ppp_col <- which(colnames(ppp_data) == as.character(value))
    if (length(ppp_col) == 0) next
    
    replacement_value <- as.character(ppp_data[ppp_row, ppp_col, drop = TRUE])
    if (is.na(replacement_value) || replacement_value == "") next
    
    placeholder <- paste0("<<REPLACE_", value, ">>")
    translations[[col]][rows_to_edit] <- gsub(placeholder, replacement_value, translations[[col]][rows_to_edit], fixed = TRUE)
  }
}



# Identify the donation row
donation_row <- which(translations$Original == "Donation to advocacy group ($)")

# Replace "$" with the corresponding currency symbol in each translation
if (length(donation_row) == 1) {
  for (col in translation_columns) {
    currency_raw <- ppp_data$Currency[ppp_data$Countries == col]
    
    if (length(currency_raw) > 0 && !is.na(currency_raw)) {
      currency_symbol <- gsub("[0-9\\s.,]", "", currency_raw)
      current_translation <- as.character(translations[[donation_row, col]])
      
      if (!is.na(current_translation) && grepl("\\$", current_translation)) {
        updated_translation <- gsub("\\$", currency_symbol, current_translation)
        translations[[donation_row, col]] <- updated_translation
      }
    }
  }
}

input_survey_path <- "Global_Study_2_Final_Version.qsf"
output_directory <- "translated_qsf_files/"

# Ensure output directory exists
if (!dir.exists(output_directory)) {
  dir.create(output_directory)
}

# Read master survey
survey_content <- readLines(input_survey_path, warn = FALSE)

# Function to remove only specific Autoscale blocks
remove_specific_autoscale_blocks <- function(lines) {
  lines <- gsub(',\\s*"Autoscale"\\s*:\\s*\\{"XScale"\\s*:\\s*\\{"Name"\\s*:\\s*"agreeDisagree","Type"\\s*:\\s*"likert","Reverse"\\s*:\\s*false\\}\\}', "", lines, perl = TRUE)
  lines <- gsub(',\\s*"Autoscale"\\s*:\\s*\\{"YScale"\\s*:\\s*\\{"Name"\\s*:\\s*"maleFemale","Type"\\s*:\\s*"likert","Reverse"\\s*:\\s*false\\}\\}', "", lines, perl = TRUE)
  return(lines)
}

# Apply autoscale cleaning
survey_content <- remove_specific_autoscale_blocks(survey_content)

# Function to replace survey content for one country
replace_text <- function(content, translations, country) {
  error_list <- list()
  
  for (i in 1:nrow(translations)) {
    original_text <- translations[i, "Original"]
    replacement_text <- translations[i, country]
    
    if (replacement_text == 0 || is.na(replacement_text) || replacement_text == "" ||
        is.na(original_text) || original_text == "") {
      next
    }
    
    # 🔒 Escape double quotes in the replacement string
    replacement_text <- gsub('"', '\\"', replacement_text, fixed = TRUE)
    
    # Run your embedded text check (optional)
    embedded_check <- embedded_check_all(original_text, replacement_text, i)
    if (!is.null(embedded_check)) {
      error_list <- append(error_list, list(list(Row = i, Error = embedded_check,
                                                 Original = as.character(original_text),
                                                 Translation = as.character(replacement_text))))
    }
    
    content <- gsub(original_text, replacement_text, content, fixed = TRUE)
  }
  
  return(list(content = content, errors = error_list))
}

# Function to replace CSSliderMax 101 with just the numeric PPP-adjusted 100 value
replace_csslidermax <- function(content, ppp_data, country) {
  # Handle "Country (English)" columns by extracting base country name
  base_country <- sub(" \\(English\\)$", "", country)
  ppp_row <- which(ppp_data$Countries == base_country)
  if (length(ppp_row) == 0) {
    return(content)
  }
  
  value_100 <- as.character(ppp_data[ppp_row, "100", drop = TRUE])
  
  # Extract only the numeric part
  numeric_value <- gsub("[^0-9.]", "", gsub("[\u00A0\u2009\\s]", "", value_100))
  
  if (is.na(numeric_value) || numeric_value == "") {
    return(content)
  }
  
  # Replace "CSSliderMax":101 with PPP-adjusted numeric value
  replacement_line <- paste0('"CSSliderMax":"', numeric_value, '"')
  content <- gsub('"CSSliderMax":101', replacement_line, content, fixed = TRUE)
  
  return(content)
}


# Generate translated QSFs
all_errors <- data.frame(Country = character(), Row = integer(), Error = character(),
                         Original = character(), Translation = character(), stringsAsFactors = FALSE)

country_columns <- colnames(translations)[-1]

for (country in country_columns) {
  if (all(is.na(translations[[country]]) | translations[[country]] == "")) {
    next
  }
  
  result <- replace_text(survey_content, translations, country)
  modified_survey_content <- result$content
  modified_survey_content <- replace_csslidermax(modified_survey_content, ppp_data, country)
  
  # Adjust survey name
  modified_survey_content <- gsub(
    '"SurveyName":"Global Study 2 Final Version"',
    paste0('"SurveyName":"Final Tester ', country, '"'),
    modified_survey_content,
    fixed = TRUE
  )
  
  # Save translated QSF
  output_survey_path <- paste0(output_directory, "survey_", country, ".qsf")
  writeLines(modified_survey_content, output_survey_path)
  
  cat("✅ Translated survey saved for", country, "→", output_survey_path, "\n")
}

# === STEP 5: Write error log if needed ===
if (nrow(all_errors) > 0) {
  write_xlsx(all_errors, "translation_errors_summary.xlsx")
  cat("⚠️ Translation issues found — saved to: translation_errors_summary.xlsx\n")
} else {
  cat("✅ No translation errors found.\n")
}

# === STEP 6: Run embedded_check_all on all translations ===
embedded_issues <- data.frame(Country = character(), Row = integer(), Error = character(),
                              Original = character(), Translation = character(), stringsAsFactors = FALSE)

for (col in translation_columns) {
  for (i in 1:nrow(translations)) {
    original <- translations[i, "Original"]
    translation <- translations[[col]][i]
    
    if (!is.na(original) && !is.na(translation) && translation != "") {
      result <- embedded_check_all(original, translation, i)
      
      if (!is.null(result)) {
        embedded_issues <- rbind(embedded_issues, data.frame(
          Country = col,
          Row = i - 13,  # Adjust the row number for the log
          Error = result,
          Original = as.character(original),
          Translation = as.character(translation),
          stringsAsFactors = FALSE
        ))
      }
    }
  }
}

if (nrow(embedded_issues) > 0) {
  write_xlsx(embedded_issues, "embedded_issues_summary.xlsx")
  cat("⚠️ Embedded text issues found — saved to: embedded_issues_summary.xlsx\n")
} else {
  cat("✅ No embedded text issues found.\n")
}
