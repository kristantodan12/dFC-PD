# ============================================================================
# Helper Functions for Parkinson's Disease dFC Shiny Dashboard
# ============================================================================

library(dplyr)
library(tidyr)
library(stringr)
library(readr)

# ----------------------------------------------------------------------------
# Data Loading and Cleaning
# ----------------------------------------------------------------------------

#' Load and clean the main dataset
#' 
#' @param file_path Path to the CSV file
#' @return A cleaned tibble
load_and_clean_data <- function(file_path = "Data_script.csv") {
  
  # Read the data
  data <- read_csv(file_path, show_col_types = FALSE)
  
  # Define missing value strings
  missing_strings <- c("Not reported", "Not applicable", "Not measured", 
                       "not reported", "not applicable", "not measured",
                       "Not Reported", "Not Applicable", "Not Measured")
  
  # Replace missing strings with NA (only for character columns)
  data <- data %>%
    mutate(across(where(is.character), ~na_if(., ""))) %>%
    mutate(across(where(is.character), ~ifelse(. %in% missing_strings, NA, .)))
  # Ensure all character columns are valid UTF-8
  data <- data %>%
    mutate(across(where(is.character), ~iconv(., from = "UTF-8", to = "UTF-8", sub = "")))
  
  # Convert numeric columns
  numeric_cols <- c(
    "Year", "N_sample_PD", "Age_PD_Min", "Age_PD_Max", "Age_PD_Mean", "Age_PD_SD",
    "UPDRS_ON_Mean", "UPDRS_ON_SD", "UPDRS_ON_Median", "UPDRS_ON_IQR",
    "UPDRS_OFF_Mean", "UPDRS_OFF_SD", "UPDRS_OFF_Median", "UPDRS_OFF_IQR",
    "HnY_Mean", "HnY_SD", "HnY_Median", "HnY_IQR", "HnY_Min", "HnY_Max",
    "N_HC", "Age_HC_mean", "Age_HC_sd",
    "TR_ms", "TE_ms", "Number_Volumes", "Length_Scan_Minutes", "Field_Strength",
    "Exclusion_Mean_FD_mm", "Exclusion_Max_FD_mm", 
    "Exclusion_Max_Translation_mm", "Exclusion_Max_Rotation_deg",
    "Exclusion_Outlier_Frames_percent",
    "Low_Cutoff_Hz", "High_Cutoff_Hz", "Low_Cutoff_2_Hz", "High_Cutoff_2_Hz",
    "Number_Networks", "Number_Areas", "Window_Size_Seconds", "Window_Shift_Second",
    "Number_States"
  )
  
  data <- data %>%
    mutate(across(all_of(numeric_cols), ~as.numeric(.)))
  
  # Standardize text columns to title case for consistency
  text_cols <- c(
    "Primary_Focus", "Focus_Specification", "Study_Design", "Data_Source",
    "Data_Source_Institution", "Paradigm_Type", "Motion_Params", "Filter_Type",
    "Brain_Mapping", "Parcellation_Methods", "dFC_Methods", "Clustering_Methods",
    "Network_Areas", "State_Features", "Graph_Measures"
  )

  # Add State_Finding_Category
  data <- data %>% mutate(
    State_Finding_Category = case_when(
      str_detect(tolower(State_Pattern_Conclusion), "integration associated with better|segregation with worse") ~ "Strongly-Connected Beneficial / Sparsely-Connected Detrimental",
      str_detect(tolower(State_Pattern_Conclusion), "integration associated with worse|segregation with better") ~ "Sparsely-Connected Beneficial / Strongly-Connected Detrimental",
      TRUE ~ "Other / Not Specified"
    ),
    Transition_Finding_Category = case_when(
      str_detect(tolower(Transition_Pattern_Conclusion), "(more transitions|frequent transitions).*(better|helpful)") ~ "More Transitions are Beneficial",
      str_detect(tolower(Transition_Pattern_Conclusion), "(fewer transitions|less frequent).*(better|helpful)") ~ "Fewer Transitions are Beneficial",
      str_detect(tolower(Transition_Pattern_Conclusion), "(more transitions|frequent transitions).*worse") ~ "More Transitions are Detrimental",
      TRUE ~ "Other / Not Specified"
    )
  )
  
  data <- data %>%
    mutate(across(all_of(intersect(text_cols, names(data))), 
                  ~str_to_title(str_trim(.))))
  
  # Clean up specific columns
  data <- data %>%
    mutate(
      # Trim whitespace
      across(where(is.character), str_trim),
      # Fix N_HC "Not reported" cases
      N_HC = ifelse(is.na(N_HC), 0, N_HC)
    )
  
  return(data)
}

# ----------------------------------------------------------------------------
# Data Transformation Functions
# ----------------------------------------------------------------------------

#' Split semicolon-separated values and count frequencies
#' 
#' @param data The main dataset
#' @param column_name Name of the column to split
#' @return A data frame with frequencies
split_and_count <- function(data, column_name) {
  data %>%
    filter(!is.na(.data[[column_name]])) %>%
    select(all_of(column_name)) %>%
    mutate(value = str_split(.data[[column_name]], ";\\s*")) %>%
    unnest(value) %>%
    mutate(value = str_trim(value)) %>%
    filter(value != "") %>%
    count(value, sort = TRUE, name = "count")
}

#' Get unique values from a semicolon-separated column
#' 
#' @param data The main dataset
#' @param column_name Name of the column
#' @return A sorted character vector of unique values
get_unique_values <- function(data, column_name) {
  data %>%
    filter(!is.na(.data[[column_name]])) %>%
    pull(.data[[column_name]]) %>%
    str_split(";\\s*") %>%
    unlist() %>%
    str_trim() %>%
    unique() %>%
    sort()
}

#' Filter data by a value that might be in a semicolon-separated list
#' 
#' @param data The main dataset
#' @param column_name Name of the column to filter
#' @param value Value to search for
#' @return Filtered dataset
filter_by_list_value <- function(data, column_name, value) {
  if (is.null(value) || value == "") {
    return(data)
  }
  data %>%
    filter(str_detect(.data[[column_name]], fixed(value)))
}

# ----------------------------------------------------------------------------
# Plotting Functions
# ----------------------------------------------------------------------------

#' Create a histogram with plotly
#' 
#' @param data The dataset
#' @param column Name of the column to plot
#' @param title Plot title
#' @param xlab X-axis label
#' @param color Bar color
#' @return A plotly object
plot_histogram <- function(data, column, title, xlab, color = "#4A90E2") {
  library(plotly)
  
  plot_ly(data, x = ~get(column), type = "histogram",
          marker = list(color = color, line = list(color = "white", width = 1))) %>%
    layout(
      title = list(text = title, font = list(size = 16)),
      xaxis = list(title = xlab),
      yaxis = list(title = "Count"),
      plot_bgcolor = "#f8f9fa",
      paper_bgcolor = "#f8f9fa"
    )
}

#' Create a horizontal bar chart with plotly
#' 
#' @param data The dataset (should have 'name' and 'count' columns)
#' @param title Plot title
#' @param xlab X-axis label
#' @param color Bar color
#' @return A plotly object
plot_horizontal_bar <- function(data, title, xlab = "Count", color = "#4A90E2") {
  library(plotly)
  
  # Ensure data is sorted
  data <- data %>% arrange(count)
  
  plot_ly(data, x = ~count, y = ~name, type = "bar", orientation = "h",
          marker = list(color = color)) %>%
    layout(
      title = list(text = title, font = list(size = 16)),
      xaxis = list(title = xlab),
      yaxis = list(title = "", categoryorder = "total ascending"),
      plot_bgcolor = "#f8f9fa",
      paper_bgcolor = "#f8f9fa",
      margin = list(l = 200)
    )
}

#' Create a violin plot with overlaid boxplot
#' 
#' @param data The dataset
#' @param column Column name for the distribution
#' @param title Plot title
#' @param xlab X-axis label
#' @param color Violin and box color
#' @return A plotly object
plot_violin_box <- function(data, column, title, xlab, color = "#4A90E2") {
  library(plotly)
  
  plot_ly(data, y = ~get(column), type = "violin", 
          box = list(visible = TRUE),
          meanline = list(visible = TRUE),
          fillcolor = color,
          opacity = 0.6,
          line = list(color = color),
          name = "") %>%
    layout(
      title = list(text = title, font = list(size = 16)),
      yaxis = list(title = xlab),
      xaxis = list(title = "", showticklabels = FALSE),
      plot_bgcolor = "#f8f9fa",
      paper_bgcolor = "#f8f9fa",
      showlegend = FALSE
    )
}

#' Create a scatter plot with plotly
#' 
#' @param data The dataset
#' @param x_col X-axis column name
#' @param y_col Y-axis column name
#' @param title Plot title
#' @param xlab X-axis label
#' @param ylab Y-axis label
#' @return A plotly object
plot_scatter <- function(data, x_col, y_col, title, xlab, ylab) {
  library(plotly)
  
  plot_ly(data, x = ~get(x_col), y = ~get(y_col), 
          type = "scatter", mode = "markers",
          marker = list(size = 10, color = "#4A90E2", opacity = 0.6,
                       line = list(color = "white", width = 1)),
          text = ~Label, hoverinfo = "text",
          hovertemplate = paste0("<b>%{text}</b><br>",
                                xlab, ": %{x}<br>",
                                ylab, ": %{y}<extra></extra>")) %>%
    layout(
      title = list(text = title, font = list(size = 16)),
      xaxis = list(title = xlab),
      yaxis = list(title = ylab),
      plot_bgcolor = "#f8f9fa",
      paper_bgcolor = "#f8f9fa"
    )
}

#' Prepare word frequency data from a column
#' 
#' @param data The dataset
#' @param column_name Name of the column
#' @return A data frame with 'word' and 'freq' columns
prepare_wordcloud_data <- function(data, column_name) {
  library(tidytext)
  
  # Custom stop words for scientific text
  custom_stopwords <- c("state", "states", "related", "associated", "findings",
                       "reported", "based", "specific", "general", "found")
  
  data %>%
    filter(!is.na(.data[[column_name]])) %>%
    select(all_of(column_name)) %>%
    unnest_tokens(word, .data[[column_name]]) %>%
    anti_join(stop_words, by = "word") %>%
    filter(!word %in% custom_stopwords) %>%
    filter(nchar(word) > 3) %>%
    count(word, sort = TRUE) %>%
    rename(freq = n)
}

# ----------------------------------------------------------------------------
# Summary Statistics Functions
# ----------------------------------------------------------------------------

#' Calculate total number of studies
#' 
#' @param data The dataset
#' @return Integer count
get_total_studies <- function(data) {
  n_distinct(data$Label)
}

#' Calculate total PD participants
#' Sum across ALL rows (not just unique studies) to account for multiple cohorts
#' 
#' @param data The dataset
#' @return Integer sum
get_total_pd <- function(data) {
  data %>%
    pull(N_sample_PD) %>%
    sum(na.rm = TRUE)
}

#' Calculate total HC participants
#' 
#' @param data The dataset
#' @return Integer sum
get_total_hc <- function(data) {
  data %>%
    distinct(Label, .keep_all = TRUE) %>%
    pull(N_HC) %>%
    sum(na.rm = TRUE)
}

# ----------------------------------------------------------------------------
# Data Export Function
# ----------------------------------------------------------------------------

#' Format data for display in DT tables
#' 
#' @param data The dataset
#' @return Formatted dataset
format_for_display <- function(data) {
  data %>%
    mutate(across(where(is.numeric), ~round(., 2)))
}
