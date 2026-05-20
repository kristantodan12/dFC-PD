# ============================================================================
# Publication-Quality Figures for Scientific Article
# ============================================================================

# Load required libraries
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(patchwork)  # For combining plots
library(scales)
library(ggalluvial) # For Sankey/Alluvial plots
library(networkD3) # For interactive Sankey plots
library(ggsci)      # For Lancet journal color palette
library(ggdist)     # For raincloud plots

# Set theme for publication-quality figures with Lancet styling
# Using theme_bw as base for clean, professional appearance
theme_set(theme_bw(base_size = 12, base_family = "sans"))

# Define output directory for figures
output_dir <- "C:/Users/danie/Documents/Projects/Dempark/Review/Manuscript/Figures/Revised"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Load and prepare data
data_raw <- read_csv("Data_script.csv", show_col_types = FALSE)

# Deduplicate by Label to get unique studies
data_unique <- data_raw %>%
  distinct(Label, .keep_all = TRUE)

# Preprocess: Convert "Not reported", "not reported", "Not applicable", "not applicable" to NA
# Process each column individually to handle encoding issues
for (col in names(data_unique)) {
  if (is.character(data_unique[[col]])) {
    # For character columns, convert problematic strings to NA
    # Use iconv to fix encoding issues first
    data_unique[[col]] <- iconv(data_unique[[col]], from = "UTF-8", to = "UTF-8", sub = "")
    # Then replace problematic values
    data_unique[[col]][!is.na(data_unique[[col]]) & 
                       data_unique[[col]] %in% c("Not reported", "not reported", 
                                                  "Not applicable", "not applicable", "")] <- NA_character_
  }
}

# ============================================================================
# FIGURE 2: Data Completeness Analysis
# ============================================================================

# Exclude metadata columns
metadata_cols <- c("Year", "Journal", "Title", "Authors", "DOI", "Label")

# Calculate missing data for each column
missing_data <- data_unique %>%
  select(-any_of(metadata_cols)) %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "NA_Count") %>%
  mutate(
    Total_Studies = nrow(data_unique),
    Completeness = (Total_Studies - NA_Count) / Total_Studies * 100,
    NA_Percent = NA_Count / Total_Studies * 100
  ) %>%
  arrange(NA_Count)  # Sort by NA_Count ascending (most complete first)

# Get top 10 most incomplete (highest NA counts)
plot_data <- missing_data %>%
  tail(10) %>%
  mutate(
    Variable = str_replace_all(Variable, "_", " "),
    Variable = factor(Variable, levels = Variable[order(NA_Count)])
  )

# Define palette colors
# Pastel palette for Figure 2
palette_colors <- brewer.pal(8, "Pastel1")

# Create the figure
fig2_completeness <- ggplot(plot_data, aes(x = NA_Percent, y = Variable)) +
  geom_col(width = 0.7, fill = "#00468BFF") +
  geom_text(aes(label = sprintf("%.1f%%", NA_Percent)), 
            hjust = -0.1, size = 5, fontface = "bold") +
  scale_x_continuous(
    limits = c(0, max(plot_data$NA_Percent) * 1.15),
    breaks = seq(0, 100, 20),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = "Top 10 Variables with Most Missing Data",
    subtitle = sprintf("Analysis of %d unique studies (after converting 'Not reported'/'Not applicable' to NA)", nrow(data_unique)),
    x = "Missing Data (%)",
    y = NULL
  ) +
  theme_bw(base_size = 16, base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0),
    plot.subtitle = element_text(size = 14, color = "gray40", hjust = 0),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90"),
    axis.text.y = element_text(size = 18, face = "bold"),
    axis.text.x = element_text(size = 18),
    axis.title.x = element_text(size = 18, face = "bold"),
    plot.margin = margin(15, 25, 15, 15)
  )

# Display the figure
print(fig2_completeness)

# Save the figure
ggsave(file.path(output_dir, "Figure2_Data_Completeness.png"), 
       plot = fig2_completeness,
       width = 10, 
       height = 7, 
       dpi = 300,
       bg = "white")

ggsave(file.path(output_dir, "Figure2_Data_Completeness.pdf"), 
       plot = fig2_completeness,
       width = 10, 
       height = 7,
       device = cairo_pdf)

cat("Figure 2 saved to:", output_dir, "\n")

# =============================================================================
# FIGURE 3: Data Sources Used in Studies (Multi-panel A/B/C)
# =============================================================================

# Helper: safe text cleansing for plotting
fix_text <- function(x) {
  if (!is.character(x)) return(x)
  x <- iconv(x, from = "", to = "UTF-8", sub = "")
  x <- trimws(x)
  x[x %in% c("Not reported", "not reported", "Not applicable", "not applicable", "")] <- NA_character_
  x
}

ds <- data_unique %>%
  mutate(
    Data_Source = fix_text(Data_Source),
    Data_Availability = fix_text(Data_Availability),
    Data_Source_Institution = fix_text(Data_Source_Institution)
  )

# Define palette for Figure 3
# Pastel palette for Figure 3
palette_ds <- brewer.pal(9, "Pastel1")

# A. Data Source Type (create standardized labels + plot)
ds <- ds %>%
  mutate(Data_Source_Std = case_when(
    !is.na(Data_Source) & grepl("public", Data_Source, ignore.case = TRUE) ~ "Public",
    !is.na(Data_Source) & grepl("priv", Data_Source, ignore.case = TRUE) ~ "Private",
    TRUE ~ "Other/Unspecified"
  ))

plotA_df <- ds %>%
  filter(!is.na(Data_Source_Std)) %>%
  count(Data_Source_Std, name = "n") %>%
  arrange(desc(n))

plot_A <- ggplot(plotA_df, aes(x = Data_Source_Std, y = n, fill = Data_Source_Std)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = scales::comma(n)), vjust = -0.3, size = 5, fontface = "bold") +
  scale_fill_manual(values = c(
    "Public" = "#00468BFF",  # Lancet blue
    "Private" = "#808080",   # Grey for negative connotation
    "Other/Unspecified" = "#00468BFF"  # Lancet blue
  )) +
  labs(title = "A. Data Source Type", x = NULL, y = "Number of Studies", fill = NULL) +
  theme_bw(base_size = 16, base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    plot.title.position = "plot",
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 10)
  )

# B. Data Availability (Private only)
private_df <- ds %>%
  filter(Data_Source_Std == "Private") %>%
  mutate(
    Data_Availability_Std = case_when(
      is.na(Data_Availability) ~ "Not reported",
      grepl("public|open|repository|available", Data_Availability, ignore.case = TRUE) ~ "Publicly available",
      grepl("request|upon", Data_Availability, ignore.case = TRUE) ~ "Upon request",
      grepl("not available|no|restricted|cannot", Data_Availability, ignore.case = TRUE) ~ "Will be made available",
      TRUE ~ "Other"
    )
  ) %>%
  count(Data_Availability_Std, name = "n") %>%
  arrange(desc(n))

plot_B <- ggplot(private_df, aes(x = n, y = reorder(Data_Availability_Std, n), fill = Data_Availability_Std)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = scales::comma(n)), hjust = -0.1, size = 5, fontface = "bold") +
  scale_y_discrete(position = "right") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = c(
    "Publicly available" = "#00468BFF",  # Lancet blue for positive
    "Upon request" = "#00468BFF",        # Lancet blue
    "Will be made available" = "#00468BFF",  # Lancet blue
    "Not reported" = "#808080",          # Grey for negative
    "Other" = "#808080"                  # Grey for negative
  )) +
  labs(title = "B. Data Availability (Private)", x = "Number of Studies", y = NULL, fill = NULL) +
  theme_bw(base_size = 16, base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 17),
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 10)
  )
 

# Plot C: Top 10 Data Source Institutions/Databases
inst_df <- ds %>%
  filter(!is.na(Data_Source_Institution), Data_Source_Institution != "") %>%
  mutate(source = str_split(Data_Source_Institution, ";\\s*")) %>%
  unnest(source) %>%
  mutate(source = str_squish(source)) %>%
  filter(source != "") %>%
  count(source, name = "n", sort = TRUE) %>%
  slice_head(n = 10) %>%
  arrange(n)

plot_C <- ggplot(inst_df, aes(x = n, y = reorder(source, n))) +
  geom_col(width = 0.65, fill = "#00468BFF") +
  geom_text(aes(label = scales::comma(n)), hjust = -0.1, size = 5, fontface = "bold") +
  labs(title = "C. Top 10 Data Source Institutions/Databases", x = "Number of Studies", y = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_y_discrete(position = "right") +
  theme_bw(base_size = 16, base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    axis.text.y = element_text(size = 17),
    axis.text.x = element_text(size = 18),
    plot.margin = margin(10, 10, 10, 10)
  )

# Combine and save
combined_ds <- plot_A + (plot_B / plot_C) + plot_layout(widths = c(1.3, 1))

ggsave(file.path(output_dir, "Figure3_Data_Sources.png"), combined_ds,
       width = 11, height = 8.5, dpi = 300, bg = "white")

ggsave(file.path(output_dir, "Figure3_Data_Sources.pdf"), combined_ds,
       width = 11, height = 8.5, device = cairo_pdf)

cat("Figure 3 saved to:", output_dir, "\n")

# =============================================================================
# FIGURE 4: Sankey Plot using networkD3
# =============================================================================

# Prepare data for Sankey plot
sankey_cols <- c("Study_Design", "Primary_Focus", "Focus_Specification", "Paradigm_Type")
sankey_data <- data_unique %>%
  select(all_of(sankey_cols)) %>%
  mutate(across(everything(), ~ifelse(is.na(.), "Not reported", as.character(.)))) %>%
  group_by(across(everything())) %>%
  summarise(n = n(), .groups = "drop")

# Get unique values for each column
nodes_list <- unique(unlist(sankey_data[, sankey_cols]))
nodes <- data.frame(name = nodes_list, stringsAsFactors = FALSE)

# Helper to get index in nodes
get_node_id <- function(x) match(x, nodes$name) - 1  # networkD3 is 0-indexed

# Build links for each pair of adjacent columns
links <- do.call(rbind, lapply(1:(length(sankey_cols)-1), function(i) {
  df <- sankey_data %>%
    group_by(.data[[sankey_cols[i]]], .data[[sankey_cols[i+1]]]) %>%
    summarise(value = sum(n), .groups = "drop")
  data.frame(
    source = get_node_id(df[[1]]),
    target = get_node_id(df[[2]]),
    value = df$value
  )
}))

# Lancet colors for nodes
lancet_colors <- c("#00468BFF", "#ED0000FF", "#42B540FF", "#0099B4FF", 
                   "#925E9FFF", "#FDAF91FF", "#AD002AFF", "#ADB6B6FF", "#1B1919FF")
node_colors <- rep(lancet_colors, length.out = nrow(nodes))

# Custom axis/column titles
axis_titles <- c("Study Design", "Primary Focus", "Specific Focus", "Paradigm")

# Create Sankey with pastel node colors, link values, and axis titles
fig4_sankey <- sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  fontSize = 18,  # Larger font for node labels
  fontFamily = "Arial",  # Arial font
  nodeWidth = 40,
  height = 600,  # Control height to make it less tall
  width = 1000,  # Control width
  colourScale = JS(sprintf('d3.scaleOrdinal().range(["%s"]);', paste(node_colors, collapse = '","'))),
  sinksRight = FALSE,
  margin = list(top = 100, bottom = 40) # Increased top margin for larger title
)

# Add main title, link values, and axis titles using JavaScript overlay
fig4_sankey <- htmlwidgets::onRender(
  fig4_sankey,
  sprintf('
    function(el, x) {
      // Wait for rendering to complete
      setTimeout(function() {
        var svg = d3.select(el).select("svg");
        if (svg.empty()) return;

        var g = svg.select("g");
        
        // Get the bounding box of the g element to center the title properly
        var gBBox = g.node().getBBox();
        var gWidth = gBBox.width;
        var gX = gBBox.x;
        
        // Add main title at the top - centered within the plot area
        g.append("text")
          .attr("class", "main-title")
          .attr("x", gX + gWidth / 2)
          .attr("y", -50)
          .attr("text-anchor", "middle")
          .style("font-size", "28px")  // Larger title font
          .style("font-weight", "bold")
          .style("font-family", "Arial, sans-serif")  // Arial font
          .style("fill", "#222")
          .text("Landscape of Study Designs and Paradigms");

        // Add link values (number of studies on edges) - BLUE color, larger font
        g.selectAll(".link").each(function(d) {
          if (d.value > 0 && d.dy >= 5) {
            var pathEl = this;
            var l = pathEl.getTotalLength();
            var p = pathEl.getPointAtLength(l / 2);

            g.append("text")
              .attr("class", "link-label")
              .attr("x", p.x)
              .attr("y", p.y)
              .attr("text-anchor", "middle")
              .attr("dominant-baseline", "middle")
              .style("font-size", "18px")  // Larger font for values
              .style("font-weight", "bold")
              .style("font-family", "Arial, sans-serif")  // Arial font
              .style("fill", "#0066cc")  // Blue color for values
              .text(d.value);
          }
        });
        
        // Add axis/column titles - BLACK color
        var axisLabels = %s;
        var nodes = g.selectAll(".node");
        var nodesByDepth = {};
        
        nodes.each(function(d) {
          if (!nodesByDepth[d.depth]) {
            nodesByDepth[d.depth] = [];
          }
          nodesByDepth[d.depth].push(d);
        });
        
        for (var depth in nodesByDepth) {
          if (axisLabels[depth]) {
            var nodesAtDepth = nodesByDepth[depth];
            var xPositions = nodesAtDepth.map(function(d) { return (d.x0 + d.x1) / 2; });
            var avgX = xPositions.reduce(function(a, b) { return a + b; }, 0) / xPositions.length;
            
            g.append("text")
              .attr("class", "axis-label")
              .attr("x", avgX)
              .attr("y", -20)  // Adjusted position
              .attr("text-anchor", "middle")
              .style("font-size", "20px")  // Larger font for column titles
              .style("font-weight", "bold")
              .style("font-family", "Arial, sans-serif")  // Arial font
              .style("fill", "#000")  // Black color for labels
              .text(axisLabels[depth]);
          }
        }
      }, 150);
    }
  ', jsonlite::toJSON(axis_titles))
)

# Save HTML
htmlwidgets::saveWidget(fig4_sankey, file.path(output_dir, "Figure4_Sankey_StudyLandscape.html"), selfcontained = TRUE)

cat("Figure 4 (Sankey with colors, link values, and titles) saved to:", output_dir, "\n")

# ============================================================================
# FIGURE 5: Methodological Divergence, Analytic Convergence
# ============================================================================

# Create unique studies dataframe for Figure 5 (data integrity)
unique_studies_df <- data_unique %>%
  distinct(Label, .keep_all = TRUE)

# Use more pastel color palette
palette_pastel <- brewer.pal(8, "Pastel1")

# ---- Panel A: Acquisition Variability ----

# Plot A1: TR Distribution - RAINCLOUD PLOT
plot_tr <- unique_studies_df %>%
  filter(!is.na(TR_ms)) %>%
  ggplot(aes(x = TR_ms, y = 0)) +
  # Half-violin density (right side)
  ggdist::stat_halfeye(
    adjust = 1,
    width = 0.6,
    justification = -0.2,
    .width = 0,
    point_colour = NA,
    fill = "#00468BFF"  # Lancet blue
  ) +
  # Boxplot (narrow, positioned below the violin)
  geom_boxplot(
    width = 0.15,
    position = position_nudge(y = -0.15),
    outlier.shape = NA,
    fill = "#808080",  # Grey
    alpha = 0.7
  ) +
  # Raw data points (jittered dots)
  geom_jitter(
    width = 0,
    height = 0.05,
    alpha = 0.5,
    size = 2,
    color = "#42B540FF"  # Lancet green
  ) +
  scale_fill_lancet() +
  scale_color_lancet() +
  labs(
    title = "A. TR Distribution (Raincloud Plot)",
    x = "Repetition Time (TR) in ms",
    y = NULL
  ) +
  theme_bw(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    axis.title = element_text(size = 20, face = "bold"),
    axis.text.x = element_text(size = 19),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

# Plot A2: Scan Length Distribution - RAINCLOUD PLOT
plot_scan_length <- unique_studies_df %>%
  filter(!is.na(Length_Scan_Minutes)) %>%
  ggplot(aes(x = Length_Scan_Minutes, y = 0)) +
  # Half-violin density (right side)
  ggdist::stat_halfeye(
    adjust = 1,
    width = 0.6,
    justification = -0.2,
    .width = 0,
    point_colour = NA,
    fill = "#00468BFF"  # Lancet blue
  ) +
  # Boxplot (narrow, positioned below the violin)
  geom_boxplot(
    width = 0.15,
    position = position_nudge(y = -0.15),
    outlier.shape = NA,
    fill = "#808080",  # Grey
    alpha = 0.7
  ) +
  # Raw data points (jittered dots)
  geom_jitter(
    width = 0,
    height = 0.05,
    alpha = 0.5,
    size = 2,
    color = "#42B540FF"  # Lancet green
  ) +
  scale_fill_lancet() +
  scale_color_lancet() +
  labs(
    title = "B. Scan Length Distribution (Raincloud Plot)",
    x = "Scan Length (minutes)",
    y = NULL
  ) +
  theme_bw(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    axis.title = element_text(size = 20, face = "bold"),
    axis.text.x = element_text(size = 19),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

# ---- Panel B: Denoising Pipeline "Divergence" ----

# Count motion parameter methods
data_motion_params <- unique_studies_df %>%
  filter(!is.na(Motion_Params)) %>%
  count(Motion_Params) %>%
  rename(Method = Motion_Params, Count = n)

# Count other denoising steps - FIXED: use str_to_lower for case-insensitive matching
data_denoising_binary <- unique_studies_df %>%
  summarise(
    `WM/CSF Reg.` = sum(str_to_lower(Regress_WM_CSF) == "yes", na.rm = TRUE),
    `CompCor` = sum(str_to_lower(Regress_CompCor) == "yes", na.rm = TRUE),
    `ICA-AROMA` = sum(str_to_lower(Regress_ICA_AROMA) == "yes", na.rm = TRUE),
    `Detrending` = sum(str_to_lower(Detrending) == "yes", na.rm = TRUE),
    `Despiking` = sum(str_to_lower(Despiking) == "yes", na.rm = TRUE),
    `Scrubbing` = sum(str_to_lower(Scrubbing) == "yes", na.rm = TRUE)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Method", values_to = "Count")

# Combine all denoising methods
data_denoising_combined <- bind_rows(data_motion_params, data_denoising_binary) %>%
  arrange(desc(Count))  # Order by count descending

plot_denoising_variability <- data_denoising_combined %>%
  mutate(Method = factor(Method, levels = rev(Method))) %>%  # Reverse for top-to-bottom ordering
  ggplot(aes(x = Count, y = Method)) +
  geom_col(width = 0.7, fill = "#00468BFF") +
  geom_text(aes(label = Count), hjust = -0.2, size = 5.5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "C. High Variability in Denoising Steps",
    x = "Count of Studies",
    y = NULL
  ) +
  theme_bw(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    axis.title = element_text(size = 20, face = "bold"),
    axis.text.y = element_text(size = 18),
    axis.text.x = element_text(size = 19),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

# ---- Panel C: Analysis Pipeline "Convergence" ----

# Plot C1: Parcellation Methods with shortened labels using case_when
plot_parcellation <- unique_studies_df %>%
  filter(!is.na(Parcellation_Methods)) %>%
  mutate(
    # Create a helper column for case-insensitive matching
    Parcellation_Lower = str_to_lower(Parcellation_Methods),
    
    Parcellation_Abbr = case_when(
      # --- Multi-atlas combinations (must come first) ---
      str_detect(Parcellation_Lower, "gordon.*harvard.*suit") ~ "Gordon/HO/SUIT",
      str_detect(Parcellation_Lower, "schaefer.*tian") ~ "Schaefer/Tian",
      
      # --- Standalone atlases ---
      str_detect(Parcellation_Lower, "schaefer") ~ "Schaefer", # Catches standalone Schaefer
      str_detect(Parcellation_Lower, "brainnetome") ~ "Brainnetome",
      str_detect(Parcellation_Lower, "dosenbach") ~ "Dosenbach",
      str_detect(Parcellation_Lower, "harvard-oxford") ~ "Harvard-Oxford",
      str_detect(Parcellation_Lower, "freesurfer") ~ "FreeSurfer",
      str_detect(Parcellation_Lower, "talairach") ~ "Talairach",
      str_detect(Parcellation_Lower, "aal") ~ "AAL",
      str_detect(Parcellation_Lower, "^ica$") ~ "ICA", # ^ica$ ensures it's an exact match
      str_detect(Parcellation_Lower, "^power$") ~ "Power",
      str_detect(Parcellation_Lower, "^suit$") ~ "SUIT",
      
      # --- Fallback (should catch very few) ---
      TRUE ~ str_to_title(Parcellation_Methods) 
    )
  ) %>%
  count(Parcellation_Abbr, sort = TRUE) %>%
  # Use reorder() for cleaner sorting
  mutate(Parcellation_Abbr = reorder(Parcellation_Abbr, n)) %>% 
  
  ggplot(aes(x = n, y = Parcellation_Abbr)) +
  geom_col(width = 0.7, fill = "#00468BFF") +
  geom_text(aes(label = n), hjust = -0.2, size = 5.5) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "D. Convergence on Parcellation",
    x = "Count of Studies",
    y = NULL
  ) +
  theme_bw(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    axis.title = element_text(size = 20, face = "bold"),
    axis.text.y = element_text(size = 17),
    axis.text.x = element_text(size = 19),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

# Plot C2: dFC & Clustering Methods Bubble Plot with specific abbreviations
data_for_bubble <- unique_studies_df %>%
  filter(!is.na(dFC_Methods) & !is.na(Clustering_Methods)) %>%
  
  # --- START: Abbreviation Logic ---
  mutate(
    # Create helper columns for case-insensitive matching
    dFC_Lower = str_to_lower(dFC_Methods),
    Clust_Lower = str_to_lower(Clustering_Methods),
    
    dFC_Methods_Abbr = case_when(
      # --- dFC Methods ---
      str_detect(dFC_Lower, "sliding window") ~ "Sliding Window",
      str_detect(dFC_Lower, "hidden markov") ~ "HMM",
      str_detect(dFC_Lower, "bayesian switching") ~ "Bayesian Switching",
      str_detect(dFC_Lower, "dominant-co-activation") ~ "d-CAPs",
      str_detect(dFC_Lower, "temporal derivative") ~ "Temp. Derivative",
      str_detect(dFC_Lower, "structural equation") ~ "SEM/MAR",
      str_detect(dFC_Lower, "sticky weighted") ~ "Sticky WTV Model",
      
      # --- Fallback ---
      TRUE ~ str_to_title(dFC_Methods) 
    ),
    
    Clustering_Methods_Abbr = case_when(
      # --- Clustering Methods ---
      str_detect(Clust_Lower, "k-mean") ~ "K-Means",
      str_detect(Clust_Lower, "hidden markov") ~ "HMM",
      str_detect(Clust_Lower, "factor analysis") ~ "Factor Analysis",
      str_detect(Clust_Lower, "bayesian switching") ~ "Bayesian Switching",
      str_detect(Clust_Lower, "temporal derivative") ~ "Temp. Derivative",
      str_detect(Clust_Lower, "structural equation") ~ "SEM/MAR",
      str_detect(Clust_Lower, "sticky weighted") ~ "Sticky WTV Model",

      # --- FallKback ---
      TRUE ~ str_to_title(Clustering_Methods) 
    )
  ) %>%
  # --- END: Abbreviation Logic ---
  
  # Filter out "Not Reported" *after* abbreviation
  filter(dFC_Methods_Abbr != "Not Reported", 
         Clustering_Methods_Abbr != "Not Reported") %>%
  
  count(dFC_Methods_Abbr, Clustering_Methods_Abbr)

plot_dfc_cluster_bubble <- data_for_bubble %>%
  ggplot(aes(x = dFC_Methods_Abbr, y = Clustering_Methods_Abbr, size = n)) +
  geom_point(alpha = 0.7, color = "#00468BFF") +  # Lancet blue
  scale_size_area(max_size = 20) +
  labs(
    title = "E. Convergence on dFC & Clustering",
    x = "dFC Method",
    y = "Clustering Method",
    size = "Count"
  ) +
  theme_bw(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    axis.title = element_text(size = 20, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 17),
    axis.text.y = element_text(size = 17),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14),
    panel.grid.minor = element_blank()
  )
  
# Plot C3: Number of Networks - fix for numeric ordering
data_for_network_count <- unique_studies_df %>%
  filter(!is.na(Number_Networks)) %>%
  # Convert to numeric to ensure proper ordering
  mutate(Number_Networks_Num = as.numeric(Number_Networks)) %>%
  filter(!is.na(Number_Networks_Num)) %>%
  count(Number_Networks_Num)

plot_network_count <- data_for_network_count %>%
  ggplot(aes(x = Number_Networks_Num, y = n)) +
  geom_col(width = 0.7, fill = "#00468BFF") +
  geom_text(aes(label = n), vjust = -0.5, size = 5.5) +
  scale_x_continuous(breaks = sort(unique(data_for_network_count$Number_Networks_Num))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "F. Common Network Counts",
    x = "Number of Networks",
    y = "Count of Studies"
  ) +
  theme_bw(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    axis.title = element_text(size = 20, face = "bold"),
    axis.text.x = element_text(size = 19),
    axis.text.y = element_text(size = 18),
    panel.grid.minor = element_blank()
  )

# ---- Final Assembly using Patchwork ----

# Combine all plots in a 2-column, multi-row layout
# Row 1: A + B
# Row 2: C (full width)
# Row 3: D + E
# Row 4: F (full width)

row1 <- plot_tr + plot_scan_length
row2 <- plot_denoising_variability
row3 <- plot_parcellation + plot_dfc_cluster_bubble
row4 <- plot_network_count

final_figure <- (row1 / row2 / row3 / row4)

# Save the combined figure
ggsave(
  filename = file.path(output_dir, "Figure5_Methodological_Divergence_Convergence.png"),
  plot = final_figure,
  width = 14,
  height = 22,
  dpi = 400,
  bg = "white"
)

cat("Figure 5 (Methodological Divergence, Analytic Convergence) saved to:", output_dir, "\n")

# ============================================================================
# FIGURE 6: Results Summary - Findings, Variability, and Limitations
# ============================================================================

# Create unique studies dataframe for Figure 6 (data integrity)
unique_studies_fig6 <- data_unique %>%
  distinct(Label, .keep_all = TRUE)

# Use pastel color palette
palette_fig6 <- brewer.pal(8, "Set2")

# ---- Plot 1: Distribution of Identified States ----
plot_num_states <- unique_studies_fig6 %>%
  filter(!is.na(Number_States)) %>%
  mutate(Number_States = as.numeric(Number_States)) %>%
  filter(!is.na(Number_States)) %>%
  count(Number_States) %>%
  ggplot(aes(x = n, y = reorder(as.factor(Number_States), Number_States))) +
  geom_col(width = 0.7, fill = "#00468BFF") +
  geom_text(aes(label = n), hjust = -0.2, size = 5, fontface = "bold") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "A. Number of Identified States",
    x = "Count of Studies",
    y = "Number of States"
  ) +
  theme_bw(base_size = 16, base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 17),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

# ---- Plot 2: Commonly Calculated State Features ----
plot_state_features <- unique_studies_fig6 %>%
  filter(!is.na(State_Features)) %>%
  separate_rows(State_Features, sep = ";") %>%
  mutate(State_Features = str_trim(str_to_lower(State_Features))) %>%
  filter(State_Features != "") %>%
  count(State_Features, sort = TRUE) %>%
  slice_head(n = 8) %>%
  mutate(State_Features = str_to_title(State_Features)) %>%
  ggplot(aes(x = n, y = reorder(State_Features, n))) +
  geom_col(width = 0.7, fill = "#00468BFF") +
  geom_text(aes(label = n), hjust = -0.2, size = 5, fontface = "bold") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "B. Common State Features",
    x = "Count of Studies",
    y = NULL
  ) +
  theme_bw(base_size = 16, base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text.y = element_text(size = 17),
    axis.text.x = element_text(size = 18),
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

# ---- Plot 3: Variability in State Pattern Conclusions ----
plot_integration_findings <- unique_studies_fig6 %>%
  mutate(
    State_Finding_Category = case_when(
      str_detect(str_to_lower(State_Pattern_Conclusion), 
                 "integration associated with better|segregation with worse") ~ 
        "Strongly-Connected Beneficial /\nSparsely-Connected Detrimental",
      str_detect(str_to_lower(State_Pattern_Conclusion), 
                 "integration associated with worse|segregation with better") ~ 
        "Sparsely-Connected Beneficial /\nStrongly-Connected Detrimental",
      str_detect(str_to_lower(State_Pattern_Conclusion), 
                 "mixed|contradictory") ~ 
        "Mixed/Contradictory",
      str_detect(str_to_lower(State_Pattern_Conclusion), 
                 "neutral|descriptive") ~ 
        "Neutral/Descriptive",
      TRUE ~ "Other/Not Specified"
    )
  ) %>%
  count(State_Finding_Category) %>%
  ggplot(aes(x = n, y = reorder(State_Finding_Category, n))) +
  geom_col(width = 0.7, fill = "#00468BFF") +
  geom_text(aes(label = n), hjust = -0.2, size = 5, fontface = "bold") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "C. State Pattern Interpretation",
    x = "Count of Studies",
    y = NULL
  ) +
  theme_bw(base_size = 16, base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text.y = element_text(size = 17, lineheight = 0.9),
    axis.text.x = element_text(size = 18),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 10, 10, 40)
  )

# ---- Plot 4: Variability in State Transition Conclusions ----
plot_transition_findings <- unique_studies_fig6 %>%
  mutate(
    Transition_Finding_Category = case_when(
      str_detect(str_to_lower(Transition_Pattern_Conclusion), 
                 "(more transitions|frequent transitions).*(better|helpful)") ~ 
        "More Transitions\nBeneficial",
      str_detect(str_to_lower(Transition_Pattern_Conclusion), 
                 "(fewer transitions|less frequent).*(better|helpful)") ~ 
        "Fewer Transitions\nBeneficial",
      str_detect(str_to_lower(Transition_Pattern_Conclusion), 
                 "(more transitions|frequent transitions).*worse") ~ 
        "More Transitions\nDetrimental",
      str_detect(str_to_lower(Transition_Pattern_Conclusion), 
                 "neutral|descriptive") ~ 
        "Neutral/Descriptive",
      TRUE ~ "Other/Not Specified"
    )
  ) %>%
  count(Transition_Finding_Category) %>%
  ggplot(aes(x = n, y = reorder(Transition_Finding_Category, n))) +
  geom_col(width = 0.7, fill = "#00468BFF") +
  geom_text(aes(label = n), hjust = -0.2, size = 5, fontface = "bold") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "D. State Transition Interpretation",
    x = "Count of Studies",
    y = NULL
  ) +
  theme_bw(base_size = 16, base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text.y = element_text(size = 17, lineheight = 0.9),
    axis.text.x = element_text(size = 18),
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 10, 10, 40)
  )

# ---- Plot 5: Most Common Limitations ----
plot_limitations <- unique_studies_fig6 %>%
  filter(!is.na(Limitations)) %>%
  separate_rows(Limitations, sep = ";") %>%
  mutate(
    Limitations = str_trim(str_to_lower(Limitations)),
    # Abbreviate common limitations for cleaner display
    Limitations_Abbr = case_when(
      str_detect(Limitations, "small sample") ~ "Small Sample",
      str_detect(Limitations, "head-motion|motion artifact") ~ "Motion Artifacts",
      str_detect(Limitations, "on state only") ~ "ON State Only",
      str_detect(Limitations, "cross-sectional") ~ "Cross-Sectional Design",
      str_detect(Limitations, "disease duration") ~ "Disease Duration Uncontrolled",
      str_detect(Limitations, "short scan|limited temporal") ~ "Short Scan/Limited Resolution",
      str_detect(Limitations, "methodological") ~ "Methodological Issues",
      str_detect(Limitations, "off state only") ~ "OFF State Only",
      str_detect(Limitations, "no.*control|no.*hc") ~ "No Healthy Controls",
      str_detect(Limitations, "heterogeneity|pd heterogeneity") ~ "PD Heterogeneity",
      TRUE ~ str_to_title(Limitations)
    )
  ) %>%
  filter(Limitations_Abbr != "") %>%
  count(Limitations_Abbr, sort = TRUE) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = n, y = reorder(Limitations_Abbr, n))) +
  geom_col(width = 0.7, fill = "#00468BFF") +
  geom_text(aes(label = n), hjust = -0.2, size = 5, fontface = "bold") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  scale_y_discrete(position = "right") +
  labs(
    title = "E. Common Limitations",
    x = "Count of Studies",
    y = NULL
  ) +
  theme_bw(base_size = 16, base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text.y = element_text(size = 17, lineheight = 0.9),
    axis.text.x = element_text(size = 18),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 40, 10, 10)
  )

# ---- Final Assembly using Patchwork ----
# 3 rows, 2 columns layout
# Row 1: plot_num_states + plot_state_features
# Row 2: plot_integration_findings + plot_transition_findings
# Row 3: plot_limitations + plot_spacer()

final_figure_6 <- (plot_num_states + plot_state_features) /
                  (plot_integration_findings + plot_transition_findings) /
                  (plot_limitations + plot_spacer())

# Save the combined figure
ggsave(
  filename = file.path(output_dir, "Figure6_Results_Summary.png"),
  plot = final_figure_6,
  width = 14,
  height = 16,
  dpi = 400,
  bg = "white"
)

ggsave(
  filename = file.path(output_dir, "Figure6_Results_Summary.pdf"),
  plot = final_figure_6,
  width = 14,
  height = 16,
  device = cairo_pdf
)

cat("Figure 6 (Results Summary: Findings, Variability, and Limitations) saved to:", output_dir, "\n")
