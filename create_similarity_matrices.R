# ============================================================================
# Script to Create Similarity Matrices for Network Visualization
# ============================================================================
# This script analyzes Data.csv and generates eight 37x37 adjacency matrices
# that quantify similarity between studies based on different criteria.
# Output: similarity_matrices.RData

# Required libraries
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

# Optional: for citation matrix (may need manual intervention)
# library(rcrossref)
# options(CrossrefEmail = "your.email@example.com")

cat("=== Starting Similarity Matrix Generation ===\n\n")

# ============================================================================
# 1. LOAD AND CLEAN DATA
# ============================================================================

cat("Step 1: Loading and cleaning Data.csv...\n")

# Load the data
data <- read_csv("Data.csv", show_col_types = FALSE)

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

# Standardize text columns to title case for consistency
text_cols <- c(
  "Primary_Focus", "Focus_Specification", "Study_Design", "Data_Source",
  "Data_Source_Institution", "Paradigm_Type", "Motion_Params", "Filter_Type",
  "Brain_Mapping", "Parcellation_Methods", "dFC_Methods", "Clustering_Methods",
  "Network_Areas", "State_Features", "Graph_Measures"
)

data <- data %>%
  mutate(across(all_of(intersect(text_cols, names(data))), 
                ~str_to_title(str_trim(.))))

# Clean up specific columns
data <- data %>%
  mutate(across(where(is.character), str_trim))

# Convert numeric columns
numeric_cols <- c("Number_States")
data <- data %>%
  mutate(across(all_of(intersect(numeric_cols, names(data))), ~as.numeric(.)))

# Get unique studies
study_labels <- unique(data$Label)
n_studies <- length(study_labels)

cat(sprintf("Loaded %d unique studies.\n\n", n_studies))

# ============================================================================
# 2. INITIALIZE MATRIX LIST
# ============================================================================

similarity_matrices <- list()

# Helper: get the first token (split by "; ") for a given label/column
get_first_token_for_label <- function(df, label, column_name) {
  vals <- df %>% filter(.data$Label == label) %>% pull(!!sym(column_name))
  if (length(vals) == 0) return(NA_character_)
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) return(NA_character_)
  x1 <- as.character(vals[1])
  parts <- str_split(x1, ";\\s*")[[1]]
  token <- str_trim(parts[1])
  if (is.na(token) || token == "") return(NA_character_) else token
}

# ============================================================================
# 3. BINARY SIMILARITY MATRICES
# ============================================================================

cat("Step 2: Generating binary similarity matrices...\n")

# Helper function to create binary similarity matrix
create_binary_matrix <- function(df, column_name, study_labels) {
  n <- length(study_labels)
  mat <- matrix(0, nrow = n, ncol = n)
  rownames(mat) <- study_labels
  colnames(mat) <- study_labels
  
  for (i in 1:n) {
    for (j in 1:n) {
      val_i <- get_first_token_for_label(df, study_labels[i], column_name)
      val_j <- get_first_token_for_label(df, study_labels[j], column_name)

      # If both are not NA and their first tokens are equal, set to 1
      if (!is.na(val_i) && !is.na(val_j) && val_i == val_j) {
        mat[i, j] <- 1
      }
    }
  }
  return(mat)
}

# 3.1 Primary Focus Matrix
cat("  - Generating primary_focus_matrix...\n")
similarity_matrices$primary_focus <- create_binary_matrix(data, "Primary_Focus", study_labels)

# 3.2 Data Source Institution Matrix
cat("  - Generating data_source_matrix...\n")
similarity_matrices$data_source <- create_binary_matrix(data, "Data_Source_Institution", study_labels)

# 3.3 Brain Mapping Matrix
cat("  - Generating brain_mapping_matrix...\n")
similarity_matrices$brain_mapping <- create_binary_matrix(data, "Brain_Mapping", study_labels)

# 3.4 dFC Methods Matrix
cat("  - Generating dfc_method_matrix...\n")
similarity_matrices$dfc_method <- create_binary_matrix(data, "dFC_Methods", study_labels)

# 3.5 Number of States Matrix
cat("  - Generating num_states_matrix...\n")
similarity_matrices$num_states <- create_binary_matrix(data, "Number_States", study_labels)

cat("Binary matrices completed.\n\n")

# ============================================================================
# 4. WEIGHTED SIMILARITY MATRICES (SHARED ITEMS)
# ============================================================================

cat("Step 3: Generating weighted similarity matrices...\n")

# Helper function to create weighted similarity matrix based on shared items
create_weighted_matrix <- function(df, column_name, study_labels) {
  n <- length(study_labels)
  mat <- matrix(0, nrow = n, ncol = n)
  rownames(mat) <- study_labels
  colnames(mat) <- study_labels
  
  for (i in 1:n) {
    for (j in 1:n) {
      # Fetch a single scalar string from each cell
      val_i <- df %>% filter(.data$Label == study_labels[i]) %>% pull(!!sym(column_name))
      val_j <- df %>% filter(.data$Label == study_labels[j]) %>% pull(!!sym(column_name))
      # If multiple values come back for any reason, take the first non-NA
      if (length(val_i) > 1) val_i <- val_i[!is.na(val_i)][1]
      if (length(val_j) > 1) val_j <- val_j[!is.na(val_j)][1]
      
      # Skip if either is NA
      if (length(val_i) == 0 || length(val_j) == 0 || is.na(val_i) || is.na(val_j)) {
        next
      }
      
      # Split by semicolon and trim
      items_i <- str_split(as.character(val_i), ";\\s*")[[1]] %>% str_trim() %>% tolower()
      items_j <- str_split(as.character(val_j), ";\\s*")[[1]] %>% str_trim() %>% tolower()
      
      # Remove empty strings
      items_i <- items_i[items_i != ""]
      items_j <- items_j[items_j != ""]
      
      # Count shared items (intersection)
      shared_count <- length(intersect(items_i, items_j))
      mat[i, j] <- shared_count
    }
  }
  return(mat)
}

# 4.1 Network Areas Matrix
cat("  - Generating network_areas_matrix...\n")
similarity_matrices$network_areas <- create_weighted_matrix(data, "Network_Areas", study_labels)

# 4.2 State Features Matrix
cat("  - Generating state_features_matrix...\n")
similarity_matrices$state_features <- create_weighted_matrix(data, "State_Features", study_labels)

cat("Weighted matrices completed.\n\n")

# ============================================================================
# 5. CITATION MATRIX (ADVANCED - OPTIONAL)
# ============================================================================

cat("Step 4: Generating citation_matrix...\n")
cat("Note: This may take a long time and may fail due to API limits.\n")

# Helper: normalize DOI strings for reliable comparisons
normalize_doi <- function(x) {
  x <- as.character(x)
  x <- tolower(str_trim(x))
  x <- str_replace(x, "^https?://(dx\\.)?doi.org/", "")
  x <- str_replace(x, "^doi:", "")
  x
}

# Initialize citation matrix with zeros
n <- length(study_labels)
citation_matrix <- matrix(0, nrow = n, ncol = n)
rownames(citation_matrix) <- study_labels
colnames(citation_matrix) <- study_labels

# Uncomment and modify the following code if you want to attempt API calls:

if (requireNamespace("rcrossref", quietly = TRUE)) {
  cat("Attempting to fetch citation data via CrossRef API...\n")
  # Set email for polite API requests
  crossref_mailto <- "daniel.kristanto@uol.de"

  for (i in 1:n) {
    doi_i <- data %>% filter(.data$Label == study_labels[i]) %>% pull(DOI)
    if (length(doi_i) > 1) doi_i <- doi_i[!is.na(doi_i)][1]
    
    if (length(doi_i) == 0 || is.na(doi_i) || doi_i == "") {
      next
    }
    doi_i_norm <- normalize_doi(doi_i)

    cat(sprintf("  - Processing study %d/%d: %s (DOI: %s)\n", i, n, study_labels[i], doi_i_norm))

    # Try to get papers that cite study i
    citing_dois <- character(0)
    tryCatch({
      # Use cr_works with filter for cited DOI
      res <- rcrossref::cr_works(filter = c(doi = doi_i_norm), mailto = crossref_mailto, limit = 1000)
      # Extract citing DOIs from works that reference this DOI
      if (!is.null(res) && !is.null(res$data) && is.data.frame(res$data)) {
        if ("reference" %in% names(res$data)) {
          # Parse references to find citing papers
          refs <- res$data$reference
          if (length(refs) > 0 && !all(is.na(refs))) {
            # This approach is complex; simpler: check if any of our studies cite this one
            # by looking up each study's references
            # For now, we'll use a simpler heuristic or leave as placeholder
          }
        }
      }
      # Alternative: query each other study to see if it cites doi_i
      # This is more reliable but slower
      for (j in 1:n) {
        if (i == j) next
        doi_j <- data %>% filter(.data$Label == study_labels[j]) %>% pull(DOI)
        if (length(doi_j) > 1) doi_j <- doi_j[!is.na(doi_j)][1]
        if (length(doi_j) == 0 || is.na(doi_j) || doi_j == "") next
        doi_j_norm <- normalize_doi(doi_j)
        
        # Get work metadata for j to check if it references i
        work_j <- tryCatch({
          rcrossref::cr_works(dois = doi_j_norm, mailto = crossref_mailto)
        }, error = function(e) NULL)
        
        if (!is.null(work_j) && !is.null(work_j$data)) {
          refs_j <- work_j$data$reference
          if (length(refs_j) > 0 && !is.null(refs_j[[1]])) {
            # Check if doi_i is in the references
            ref_dois <- sapply(refs_j[[1]], function(ref) {
              if (is.list(ref) && "DOI" %in% names(ref)) return(ref$DOI)
              if (is.list(ref) && "doi" %in% names(ref)) return(ref$doi)
              return(NA)
            })
            ref_dois <- normalize_doi(ref_dois[!is.na(ref_dois)])
            if (doi_i_norm %in% ref_dois) {
              citation_matrix[i, j] <- 1
            }
          }
        }
        Sys.sleep(0.5) # Rate limit per pair check
      }
      citing_dois <- character(0) # Not used in this approach
    }, error = function(e) {
      cat(sprintf("    Warning: Failed for %s: %s\n", study_labels[i], conditionMessage(e)))
      citing_dois <<- character(0)
    })

    # Note: citation checking is now done within the tryCatch above
  }
} else {
  cat("rcrossref package not available. Using placeholder matrix.\n")
}

similarity_matrices$citation <- citation_matrix
cat("Citation matrix completed (placeholder).\n\n")

# ============================================================================
# 6. SAVE OUTPUT
# ============================================================================

cat("Step 5: Saving similarity matrices to similarity_matrices.RData...\n")

save(similarity_matrices, file = "similarity_matrices.RData")

cat("\n=== Similarity Matrix Generation Complete ===\n")
cat(sprintf("Generated %d matrices:\n", length(similarity_matrices)))
for (name in names(similarity_matrices)) {
  cat(sprintf("  - %s\n", name))
}
cat("\nOutput saved to: similarity_matrices.RData\n")
