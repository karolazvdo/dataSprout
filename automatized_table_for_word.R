##################################################################
##################################################################
##  Word table for document report                              ##
##  Karoline Azevedo, 12 de Janeiro de 2025                     ##
##################################################################
##################################################################
##
# Install and load packages
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
library(pacman)

p_load(flextable, officer, stringr, dplyr)

# checkin columns name
colnames(pk)
data <- pk[,c("WDPAID", "NAME", "ISO3", "cultInterest", "sppVulnerability", "label_pt", "label_fr", "label_de", "label_es", "label_it", "label_en", "n_languages", "n_species_total", "propCR", "propVU", "propEN")]


# Function to standardize park names
normalize_park_names <- function(NAME) {
  NAME <- str_trim(NAME)  # Remove extra spaces at the beginning and end
  NAME <- str_to_title(NAME)  # Format the name in title case (first letter of each word capitalized)
  
  # Remove unwanted prefixes and suffixes before standardizing
  NAME <- str_replace_all(NAME, "(?i)\\bParc National( De)?\\b", "")  # Remove "Parc National" and "Parc National de"
  NAME <- str_replace_all(NAME, "(?i)\\bParque Nacional( De)?\\b", "")  # Remove "Parque Nacional" and "Parque Nacional de"
  NAME <- str_trim(NAME)  # Remove extra spaces again
  
  # Add "National Park" at the end of the name
  NAME <- str_c(NAME, " National Park")
  
  return(NAME)
}

# Function to export the table to Word
export_table_to_word <- function(data, file_name = "Tabela.docx", font_size = 10, line_spacing = 1) {
  
  # Ensure WDPAID remains an integer
  if ("WDPAID" %in% colnames(data)) {
    data <- data %>%
      mutate(WDPAID = as.integer(WDPAID))  # Convert WDPAID to integer
  }
  
  # Round numeric columns (excluding WDPAID) to 3 decimal places
  data <- data %>%
    mutate(across(where(is.numeric) & !WDPAID, ~ round(., 3)))  # Round numeric columns to 3 decimal places
  
  # Sort the table by WDPAID in ascending order
  data <- data %>%
    arrange(WDPAID)  # Sort by WDPAID in ascending order
  
  # Check if the "NAME" column exists and apply the normalization function
  if ("NAME" %in% colnames(data)) {
    data <- data %>%
      mutate(NAME = sapply(NAME, normalize_park_names))
  }
  
  # Create the table using the flextable package
  tabela <- flextable(data) %>%
    autofit() %>%  # Automatically adjust column widths
    theme_vanilla() %>%  # Apply a simple theme to the table
    fontsize(size = font_size) %>%  # Set the font size
    line_spacing(space = line_spacing)  # Set the line spacing
  
  # Create a Word document in landscape format
  doc <- read_docx() %>%
    body_add_par("National Parks from Subsaharan Africa and respective values for Cultural Interest and Species Vulnerability Score metrics.", style = "heading 1") %>%
    body_add_flextable(tabela) %>%
    body_end_section_landscape()  # End the section in landscape format
  
  # Save the document
  print(doc, target = file_name)
  
  message("Table successfully exported to: ", file_name)
}

# Check if df is a data.frame
if (!is.data.frame(data)) {
  stop("The object 'data' is not a data.frame. Please convert it to a data.frame before proceeding.")
}

# Export the corrected table
export_table_to_word(data, "MinhaTabela.docx", font_size = 10, line_spacing = 1)
