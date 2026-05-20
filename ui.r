# ============================================================================
# UI for Parkinson's Disease dFC Shiny Dashboard
# ============================================================================

library(shiny)
library(bslib)
library(plotly)
library(DT)
library(wordcloud2)
library(visNetwork)
library(igraph)
library(shinyjs)

# Load helper functions
source("helper_functions.r")

# Load and clean data
data <- load_and_clean_data("Data_script.csv")

# Define UI
ui <- navbarPage(
  title = "DynaPD Interactive App",
  id = "navBar",
  collapsible = TRUE,
  windowTitle = "DynaPD - Dynamic Functional Connectivity in Parkinson's Disease",
  position = "fixed-top",
  
  # Custom CSS and JavaScript
  theme = bs_theme(
    bootswatch = "cosmo",
    base_font = font_google("Open Sans"),
    heading_font = font_google("Roboto Slab")
  ),
  
  tags$head(
    tags$style(HTML('
      .navbar {
        min-height: 70px;
        background-color: #FFFFFF;
        border-bottom: 4px solid #3498db;
      }
      .navbar-brand {
        height: 70px;
        padding: 10px 15px;
        font-size: 18px;
        font-weight: bold;
        color: #3498db !important;
      }
      .navbar-nav > li > a {
        padding-top: 25px;
        padding-bottom: 25px;
        color: #3498db;
        font-size: 14px;
        font-weight: bold;
      }
      .navbar-nav > li > a:hover {
        color: #2c3e50 !important;
      }
      body {
        padding-top: 100px;
      }
      .navbar-nav { float: right !important; }
      .hidden-tab { display: none !important; }
      .custom-home-icon {
        cursor: pointer;
        padding: 25px;
        display: flex;
        align-items: center;
      }
      .custom-home-icon i {
        margin-right: 5px;
      }
      .custom-home-icon span {
        font-weight: bold;
        color: #3498db;
        font-size: 14px;
      }
      .custom-home-icon:hover {
        color: #2c3e50 !important;
      }
    ')),
    tags$script(HTML('
      $(document).ready(function() {
        $(".navbar-nav > li").addClass("hidden-tab");
        
        // Add custom home icon with text
        $(".navbar-nav").prepend(\'<li class="custom-home-icon"><i class="fa fa-home"></i><span>HOME</span></li>\');
        
        // Click event for custom home icon
        $(".custom-home-icon").click(function() {
          var homeTab = $(".navbar-nav > li > a[data-value=\'home\']");
          homeTab.click();
        });
      });
    '))
  ),
  
  tags$head(tags$script(HTML('
    function fakeClick(tabName) {
      var dropdownList = document.getElementsByTagName("a");
      for (var i = 0; i < dropdownList.length; i++) {
        var link = dropdownList[i];
        if(link.getAttribute("data-value") == tabName) {
          link.click();
        }
      }
    }
    
    var sendToShiny = function(label) {
      Shiny.onInputChange("node_clicked", label);
    };
  '))),
  
  # ============================================================================
  # TAB 0: HOME
  # ============================================================================
  tabPanel("Home", value = "home", icon = icon("home"),
    useShinyjs(),
    fluidRow(
      align = "left",
      column(
        width = 12,
        wellPanel(
          style = "background-color: #f0f0f0; color: #000; padding: 20px; border: 1px solid #ddd;",
          tags$h3("DynaPD Interactive App:", style = "color: #3498db; font-weight: bold;"),
          tags$h4("Exploring Dynamic Functional Connectivity in Parkinson's Disease",
                  style = "color: #2c3e50; font-weight: normal;"),
          tags$h5("This interactive app allows you to explore dynamic functional connectivity (dFC) studies in Parkinson's Disease through a comprehensive systematic review of the literature.",
                  style = "color: #000; font-weight: normal;"),
          tags$h5("Click on the nodes below to explore the different features of this app:", style = "color: #000;"),
          HTML("<br>")
        )
      )
    ),
    
    fluidRow(
      column(8,
        visNetworkOutput("network_home", width = "100%", height = "600px")
      ),
      column(4,
        align = "left",
        uiOutput("node_description")
      )
    )
  ),
  
  # ============================================================================
  # TAB 1: FULL DATASET
  # ============================================================================
  tabPanel("Full Dataset", value = "full_dataset", icon = icon("database"),
    navset_card_tab(
      id = "full_dataset_tabs",
      title = "Complete Data and Information",
      nav_panel(
        title = "Dataset",
        card(
          card_header("PRISMA Flow Diagram"),
          div(
            class = "text-muted",
            style = "font-size: 12px; line-height: 1.35; margin: 4px 0 8px;",
            HTML(
              paste0(
                "<b>Study Selection Process</b><br>",
                "• This diagram shows the systematic review process following PRISMA guidelines.<br>",
                "• It illustrates how studies were identified, screened, and included in the final dataset."
              )
            )
          ),
          div(
            style = "text-align: center; padding: 20px;",
            img(src = "PRISMA_diagram.png", style = "max-width: 40%; height: auto;")
          )
        )
      ),
      nav_panel(
        title = "Data Explorer",
        card(
          full_screen = TRUE,
          style = "min-height: 800px;",
          card_header("Interactive Data Explorer"),
          div(
            class = "text-muted",
            style = "font-size: 12px; line-height: 1.35; margin: 4px 0 8px;",
            HTML(
              paste0(
                "<b>Dynamic Filtering</b><br>",
                "• Use the sliders and dropdowns at the top of each column to filter data dynamically.<br>",
                "• Numeric variables (e.g., Length_Scan_Minutes, TR_ms) have range sliders.<br>",
                "• Categorical variables have dropdown menus for selection.<br>",
                "• Click column headers to sort; combine filters for precise data exploration."
              )
            )
          ),
          DTOutput("table_data_explorer")
        )
      ),
      nav_panel(
        title = "Coded Information",
        card(
          full_screen = TRUE,
          style = "min-height: 800px;",
          card_header("Column Information and Descriptions"),
          div(
            class = "text-muted",
            style = "font-size: 12px; line-height: 1.35; margin: 4px 0 8px;",
            HTML(
              paste0(
                "<b>About this reference</b><br>",
                "• This table describes every column in the dataset (name, meaning, and coding notes).<br>",
                "• Use the search boxes to find variables quickly (e.g., type part of a name or description).<br>",
                "• Columns can be sorted and the table supports horizontal scrolling for long descriptions."
              )
            )
          ),
          DTOutput("table_coded_information")
        )
      )
    )
  ),
  # ============================================================================
  # TAB 2: STUDY OVERVIEW
  # ============================================================================
  tabPanel("Study Overview", value = "study_overview", icon = icon("chart-bar"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Filters",
        width = 300,
        div(
          class = "text-muted",
          style = "font-size: 12px; line-height: 1.35; margin-bottom: 8px;",
          HTML(
            paste0(
              "<b>What you can do here</b><br>",
              "• Subset studies by <i>Publication Year</i>, <i>Sample Size (PD)</i>, <i>Study Design</i>, <i>Primary Focus</i>, and <i>Focus Specification</i>.<br>",
              "• View KPIs (Total Studies, PD, HC) and plots: Studies by Year, Top Journals, Sample Size and Age distributions, Institutions, and Medical Status during scan.<br>",
              "• Inspect the filtered studies table with per-column search and clickable DOI links.<br>",
              "<b>Tips</b><br>",
              "• The Primary Focus list narrows after you choose a Study Design. Focus Specification narrows after you choose a Primary Focus.<br>",
              "• Most plots count unique papers (deduplicated by study label). Tables support in-place filtering at the top of each column."
            )
          )
        ),
        sliderInput(
          "overview_year_range",
          "Publication Year:",
          min = min(data$Year, na.rm = TRUE),
          max = max(data$Year, na.rm = TRUE),
          value = c(min(data$Year, na.rm = TRUE), max(data$Year, na.rm = TRUE)),
          step = 1,
          sep = ""
        ),
        sliderInput(
          "overview_n_sample_pd_range",
          "Sample Size (PD):",
          min = min(data$N_sample_PD, na.rm = TRUE),
          max = max(data$N_sample_PD, na.rm = TRUE),
          value = c(min(data$N_sample_PD, na.rm = TRUE), max(data$N_sample_PD, na.rm = TRUE)),
          step = 1
        ),
        selectInput(
          "overview_study_design_filter",
          "Study Design:",
          choices = c("All", unique(data$Study_Design[!is.na(data$Study_Design)])),
          selected = "All",
          multiple = FALSE
        ),
        uiOutput("overview_primary_focus_ui"),
        uiOutput("overview_focus_specification_ui")
      ),
      div(
        style = "overflow-y: auto; max-height: calc(100vh - 100px);",
        layout_column_wrap(
          width = 1/3,
          value_box(
            title = "Total Studies",
            value = textOutput("total_studies"),
            showcase = icon("book"),
            theme = "primary"
          ),
          value_box(
            title = "Total PD Participants",
            value = textOutput("total_pd"),
            showcase = icon("users"),
            theme = "info"
          ),
          value_box(
            title = "Total HC Participants",
            value = textOutput("total_hc"),
            showcase = icon("user-check"),
            theme = "success"
          )
        ),
        layout_column_wrap(
          width = 1/2,
          card(
            card_header("Studies by Year"),
            plotlyOutput("plot_year", height = "350px")
          ),
          card(
            card_header("Top 10 Journals"),
            plotlyOutput("plot_journals", height = "350px")
          )
        ),
        layout_column_wrap(
          width = 1/2,
          card(
            card_header("Sample Size Distribution"),
            plotlyOutput("plot_sample_size", height = "350px")
          ),
          card(
            card_header("Mean Age Distribution (PD)"),
            plotlyOutput("plot_age_distribution", height = "350px")
          )
        ),
        card(
          card_header("Data Source Institutions"),
          plotlyOutput("plot_institutions", height = "400px")
        ),
        card(
          card_header("Medical Status During Scan"),
          plotlyOutput("plot_med_status_scan", height = "350px")
        ),
        card(
          card_header("Paradigm Type Distribution"),
          plotlyOutput("plot_paradigm_type", height = "350px")
        ),
        card(
          card_header("Sex Distribution (% Male in PD Samples)"),
          plotlyOutput("plot_sex_distribution", height = "350px")
        ),
        card(
          card_header("Filtered Studies"),
          DTOutput("table_overview_studies")
        )
      )
    )
  ),
  
  # ============================================================================
  # TAB 3: METHOD EXPLORER
  # ============================================================================
  tabPanel("Method Explorer", value = "method_explorer", icon = icon("microscope"),
    navset_card_tab(
      id = "method_explorer_tabs",
      title = "Explore Methodological Approaches",
      
      # Sub-tab A: MRI Acquisition & Preprocessing
      nav_panel(
        title = "MRI Acquisition & Preprocessing",
        layout_sidebar(
          sidebar = sidebar(
            title = "Filters",
            width = 300,
            div(
              class = "text-muted",
              style = "font-size: 12px; line-height: 1.35; margin-bottom: 8px;",
              HTML(
                paste0(
                  "<b>What you can do here</b><br>",
                  "• Filter papers by <i>Publication Year</i>, <i>Sample Size (PD)</i>, <i>Study Design</i>, <i>Primary Focus</i>, and <i>Focus Specification</i>.<br>",
                  "• KPIs show totals for studies and participants in the filtered set.<br>",
                  "• Plots summarize key MRI choices: Motion Parameter strategies, TR/TE distributions, Scan Length, Cleaning Steps (WM/CSF, CompCor, ICA-AROMA, Detrending, Despiking, Scrubbing), and Exclusion thresholds (e.g., Mean FD).<br>",
                  "• The table at the bottom lists the filtered studies with comprehensive MRI fields and clickable DOI links."
                )
              )
            ),
            sliderInput(
              "mri_year_range",
              "Publication Year:",
              min = min(data$Year, na.rm = TRUE),
              max = max(data$Year, na.rm = TRUE),
              value = c(min(data$Year, na.rm = TRUE), max(data$Year, na.rm = TRUE)),
              step = 1,
              sep = ""
            ),
            sliderInput(
              "mri_n_sample_pd_range",
              "Sample Size (PD):",
              min = min(data$N_sample_PD, na.rm = TRUE),
              max = max(data$N_sample_PD, na.rm = TRUE),
              value = c(min(data$N_sample_PD, na.rm = TRUE), max(data$N_sample_PD, na.rm = TRUE)),
              step = 1
            ),
            sliderInput(
              "mri_tr_range",
              "Repetition Time (TR):",
              min = 0,
              max = 4000,
              value = c(0, 4000),
              step = 100,
              post = " ms"
            ),
            selectInput(
              "mri_study_design_filter",
              "Study Design:",
              choices = c("All", unique(data$Study_Design[!is.na(data$Study_Design)])),
              selected = "All",
              multiple = FALSE
            ),
            uiOutput("mri_primary_focus_ui"),
            uiOutput("mri_focus_specification_ui")
          ),
          div(
            style = "overflow-y: auto; max-height: calc(100vh - 100px);",
            
            layout_column_wrap(
              width = 1/3,
              value_box(
                title = "Total Studies",
                value = textOutput("mri_total_studies"),
                showcase = icon("book"),
                theme = "primary"
              ),
              value_box(
                title = "Total PD Participants",
                value = textOutput("mri_total_pd"),
                showcase = icon("users"),
                theme = "info"
              ),
              value_box(
                title = "Total HC Participants",
                value = textOutput("mri_total_hc"),
                showcase = icon("user-check"),
                theme = "success"
              )
            ),
            
            layout_column_wrap(
              width = 1/2,
              card(
                card_header("Motion Parameter Strategies"),
                plotlyOutput("plot_motion_params", height = "300px")
              ),
              card(
                card_header("TR Distribution (ms)"),
                plotlyOutput("plot_tr_hist", height = "300px")
              ),
              card(
                card_header("TE Distribution (ms)"),
                plotlyOutput("plot_te_hist", height = "300px")
              ),
              card(
                card_header("Scan Length Distribution (min)"),
                plotlyOutput("plot_scan_length_hist", height = "300px")
              ),
              card(
                card_header("Cleaning Steps Used"),
                plotlyOutput("plot_cleaning_steps", height = "300px")
              ),
              card(
                card_header("Exclusion Criteria Distributions"),
                plotlyOutput("plot_exclusion_criteria", height = "300px")
              )
            ),
            card(
              card_header("Filtered Studies"),
              DTOutput("table_mri_studies")
            )
          )
        )
      ),
      
      # Sub-tab B: dFC Analysis
      nav_panel(
        title = "dFC Analysis",
        layout_sidebar(
          sidebar = sidebar(
            title = "Filters",
            width = 300,
            div(
              class = "text-muted",
              style = "font-size: 12px; line-height: 1.35; margin-bottom: 8px;",
              HTML(
                paste0(
                  "<b>What you can do here</b><br>",
                  "• Filter by year, sample size, study design, primary focus, and focus specification.<br>",
                  "• Explore dFC-related methods: Brain Mapping, Parcellation, dFC Methods, Window Size/Shift, Clustering, and Number of States.<br>",
                  "• KPIs show total studies and participant counts for the filtered set."
                )
              )
            ),
            sliderInput(
              "dfc_year_range",
              "Publication Year:",
              min = min(data$Year, na.rm = TRUE),
              max = max(data$Year, na.rm = TRUE),
              value = c(min(data$Year, na.rm = TRUE), max(data$Year, na.rm = TRUE)),
              step = 1,
              sep = ""
            ),
            sliderInput(
              "dfc_n_sample_pd_range",
              "Sample Size (PD):",
              min = min(data$N_sample_PD, na.rm = TRUE),
              max = max(data$N_sample_PD, na.rm = TRUE),
              value = c(min(data$N_sample_PD, na.rm = TRUE), max(data$N_sample_PD, na.rm = TRUE)),
              step = 1
            ),
            sliderInput(
              "dfc_tr_range",
              "Repetition Time (TR):",
              min = 0,
              max = 4000,
              value = c(0, 4000),
              step = 100,
              post = " ms"
            ),
            selectInput(
              "dfc_study_design_filter",
              "Study Design:",
              choices = c("All", unique(data$Study_Design[!is.na(data$Study_Design)])),
              selected = "All",
              multiple = FALSE
            ),
            uiOutput("dfc_primary_focus_ui"),
            uiOutput("dfc_focus_specification_ui")
          ),
          
          div(
            style = "overflow-y: auto; max-height: calc(100vh - 100px);",
            
            layout_column_wrap(
              width = 1/3,
              value_box(
                title = "Total Studies",
                value = textOutput("dfc_total_studies"),
                showcase = icon("book"),
                theme = "primary"
              ),
              value_box(
                title = "Total PD Participants",
                value = textOutput("dfc_total_pd"),
                showcase = icon("users"),
                theme = "info"
              ),
              value_box(
                title = "Total HC Participants",
                value = textOutput("dfc_total_hc"),
                showcase = icon("user-check"),
                theme = "success"
              )
            ),
            
            layout_column_wrap(
              width = 1/2,
              card(
                card_header("Brain Mapping Methods"),
                plotlyOutput("plot_brain_mapping", height = "300px")
              ),
              card(
                card_header("Number of Networks Distribution"),
                plotlyOutput("plot_num_networks_hist", height = "300px")
              ),
              card(
                card_header("Parcellation Methods"),
                plotlyOutput("plot_parcellation", height = "300px")
              ),
              card(
                card_header("dFC Methods"),
                plotlyOutput("plot_dfc_methods", height = "300px")
              ),
              card(
                card_header("Window Size Distribution (sec)"),
                plotlyOutput("plot_window_size_hist", height = "300px")
              ),
              card(
                card_header("Window Shift Distribution (sec)"),
                plotlyOutput("plot_window_shift_hist", height = "300px")
              ),
              card(
                card_header("Clustering Methods"),
                plotlyOutput("plot_clustering_methods", height = "300px")
              ),
              card(
                card_header("Number of States Distribution"),
                plotlyOutput("plot_num_states_hist", height = "300px")
              )
            ),
            
            card(
              card_header("Filtered Studies"),
              DTOutput("table_dfc_studies")
            )
          )
        )
      ),
      
      # Sub-tab C: Clinical Focus & Brain Mapping
      nav_panel(
        title = "Clinical Focus & Brain Mapping",
        layout_sidebar(
          sidebar = sidebar(
            title = "Filter by Focus Specification",
            width = 300,
            div(
              class = "text-muted",
              style = "font-size: 12px; line-height: 1.35; margin-bottom: 8px;",
              HTML(
                paste0(
                  "<b>What you can do here</b><br>",
                  "• Filter papers by <i>Focus Specification</i> (e.g., motor, cognition).<br>",
                  "• Word cloud summarizes brain regions/networks mentioned; the table lists their frequencies for the filtered set.<br>",
                  "• Use the table's search boxes to drill down; hover the word cloud to focus on items."
                )
              )
            ),
            selectInput(
              "clinical_focus_specification_filter",
              "Focus Specification:",
              choices = c("All", unique(data$Focus_Specification[!is.na(data$Focus_Specification)])),
              selected = "All",
              multiple = FALSE
            )
          ),
          card(
            card_header("Brain Regions/Networks Word Cloud"),
            wordcloud2Output("wordcloud_networks", height = "600px")
          ),
          card(
            card_header("Network/Area Frequency Table"),
            DTOutput("table_network_freq")
          )
        )
      )
    )
  ),
  
  
  # ============================================================================
  # TAB 4: FINDING EXPLORER
  # ============================================================================
  tabPanel("Finding Explorer", value = "finding_explorer", icon = icon("search"),
    navset_card_tab(
      id = "finding_explorer_tabs",
      title = "Explore Study Findings",
      # Subtab: State Features
      nav_panel(
        title = "State Features",
        layout_sidebar(
          sidebar = sidebar(
            title = "Filters",
            width = 300,
            div(
              class = "text-muted",
              style = "font-size: 12px; line-height: 1.35; margin-bottom: 8px;",
              HTML(
                paste0(
                  "<b>What you can do here</b><br>",
                  "• Filter by year, sample size (PD), study design, primary focus, and focus specification.<br>",
                  "• Explore: <i>Number of States</i> distribution, <i>State Features</i> frequency, <i>Graph Measures</i> frequency, and a detailed table with DOI links.<br>",
                  "• The focus dropdowns are dependent: Study Design → Primary Focus → Focus Specification."
                )
              )
            ),
            sliderInput(
              "sf_year_range",
              "Publication Year:",
              min = min(data$Year, na.rm = TRUE),
              max = max(data$Year, na.rm = TRUE),
              value = c(min(data$Year, na.rm = TRUE), max(data$Year, na.rm = TRUE)),
              step = 1,
              sep = ""
            ),
            sliderInput(
              "sf_n_sample_pd_range",
              "Sample Size (PD):",
              min = min(data$N_sample_PD, na.rm = TRUE),
              max = max(data$N_sample_PD, na.rm = TRUE),
              value = c(min(data$N_sample_PD, na.rm = TRUE), max(data$N_sample_PD, na.rm = TRUE)),
              step = 1
            ),
            selectInput(
              "sf_study_design_filter",
              "Study Design:",
              choices = c("All", unique(data$Study_Design[!is.na(data$Study_Design)])),
              selected = "All",
              multiple = FALSE
            ),
            uiOutput("sf_primary_focus_ui"),
            uiOutput("sf_focus_specification_ui")
          ),
          div(
            style = "overflow-y: auto; max-height: calc(100vh - 100px);",
            layout_column_wrap(
              width = 1,
              card(
                card_header("Number of States Distribution"),
                plotlyOutput("plot_num_states_findings", height = "350px")
              ),
              card(
                card_header("State Features Frequency"),
                plotlyOutput("plot_state_features_freq", height = "350px")
              ),
              card(
                card_header("Graph Measures Frequency"),
                plotlyOutput("plot_graph_measures_freq", height = "350px")
              ),
              card(
                card_header("State Features Table"),
                DTOutput("table_state_features")
              )
            )
          )
        )
      ),
      nav_panel(
        title = "State Interpretation",
        layout_sidebar(
          sidebar = sidebar(
            title = "Filters",
            width = 300,
            div(
              class = "text-muted",
              style = "font-size: 12px; line-height: 1.35; margin-bottom: 8px;",
              HTML(
                paste0(
                  "<b>What you can do here</b><br>",
                  "• Filter by year, sample size (PD), study design, primary focus, and focus specification.<br>",
                  "• Use the 'Finding to Explore' dropdown to view either <i>State Pattern Conclusions</i> or <i>State Transition Patterns</i>.<br>",
                  "• Click a bar in the chart to populate the detailed table below with matching papers (DOIs are clickable)."
                )
              )
            ),
            sliderInput(
              "sfind_year_range",
              "Publication Year:",
              min = min(data$Year, na.rm = TRUE),
              max = max(data$Year, na.rm = TRUE),
              value = c(min(data$Year, na.rm = TRUE), max(data$Year, na.rm = TRUE)),
              step = 1,
              sep = ""
            ),
            sliderInput(
              "sfind_n_sample_pd_range",
              "Sample Size (PD):",
              min = min(data$N_sample_PD, na.rm = TRUE),
              max = max(data$N_sample_PD, na.rm = TRUE),
              value = c(min(data$N_sample_PD, na.rm = TRUE), max(data$N_sample_PD, na.rm = TRUE)),
              step = 1
            ),
            selectInput(
              "sfind_study_design_filter",
              "Study Design:",
              choices = c("All", unique(data$Study_Design[!is.na(data$Study_Design)])),
              selected = "All",
              multiple = FALSE
            ),
            uiOutput("sfind_primary_focus_ui"),
            uiOutput("sfind_focus_specification_ui")
          ),
          div(
            style = "overflow-y: auto; max-height: calc(100vh - 100px); padding-right: 12px;",
            card(
              card_header("Exploring Finding Variability"),
              selectInput(
                "findings_variability_type",
                "Finding to Explore:",
                choices = c(
                  "State Pattern Conclusions" = "state",
                  "State Transition Patterns" = "transition"
                ),
                selected = "state"
              ),
              plotlyOutput("findings_variability_plot", height = "400px"),
              DTOutput("findings_detail_table")
            )
          )
        )
      ),
      nav_panel(
        title = "Proposed Biomarkers",
        layout_sidebar(
          sidebar = sidebar(
            title = "Filters",
            width = 300,
            div(
              class = "text-muted",
              style = "font-size: 12px; line-height: 1.35; margin-bottom: 8px;",
              HTML(
                paste0(
                  "<b>What you can do here</b><br>",
                  "• Filter by year, sample size (PD), study design, primary focus, and focus specification.<br>",
                  "• Review a consolidated table of biomarker proposals, network areas, and related state/transition conclusions.<br>",
                  "• Use column filters to search within the table; DOIs open in a new tab."
                )
              )
            ),
            sliderInput(
              "pb_year_range",
              "Publication Year:",
              min = min(data$Year, na.rm = TRUE),
              max = max(data$Year, na.rm = TRUE),
              value = c(min(data$Year, na.rm = TRUE), max(data$Year, na.rm = TRUE)),
              step = 1,
              sep = ""
            ),
            sliderInput(
              "pb_n_sample_pd_range",
              "Sample Size (PD):",
              min = min(data$N_sample_PD, na.rm = TRUE),
              max = max(data$N_sample_PD, na.rm = TRUE),
              value = c(min(data$N_sample_PD, na.rm = TRUE), max(data$N_sample_PD, na.rm = TRUE)),
              step = 1
            ),
            selectInput(
              "pb_study_design_filter",
              "Study Design:",
              choices = c("All", unique(data$Study_Design[!is.na(data$Study_Design)])),
              selected = "All",
              multiple = FALSE
            ),
            uiOutput("pb_primary_focus_ui"),
            uiOutput("pb_focus_specification_ui")
          ),
          card(
            card_header("Biomarker Summary"),
            DTOutput("table_biomarkers")
          )
        )
      ),
      nav_panel(
        title = "Limitations",
        layout_sidebar(
          sidebar = sidebar(
            title = "Filters",
            width = 300,
            div(
              class = "text-muted",
              style = "font-size: 12px; line-height: 1.35; margin-bottom: 8px;",
              HTML(
                paste0(
                  "<b>What you can do here</b><br>",
                  "• Filter by year, sample size (PD), study design, primary focus, and focus specification.<br>",
                  "• View the Top 10 most common limitations in a horizontal bar chart and browse the per-paper table.<br>",
                  "• The table includes all papers, including entries where limitations were not reported; use the search boxes to filter."
                )
              )
            ),
            sliderInput(
              "lim_year_range",
              "Publication Year:",
              min = min(data$Year, na.rm = TRUE),
              max = max(data$Year, na.rm = TRUE),
              value = c(min(data$Year, na.rm = TRUE), max(data$Year, na.rm = TRUE)),
              step = 1,
              sep = ""
            ),
            sliderInput(
              "lim_n_sample_pd_range",
              "Sample Size (PD):",
              min = min(data$N_sample_PD, na.rm = TRUE),
              max = max(data$N_sample_PD, na.rm = TRUE),
              value = c(min(data$N_sample_PD, na.rm = TRUE), max(data$N_sample_PD, na.rm = TRUE)),
              step = 1
            ),
            selectInput(
              "lim_study_design_filter",
              "Study Design:",
              choices = c("All", unique(data$Study_Design[!is.na(data$Study_Design)])),
              selected = "All",
              multiple = FALSE
            ),
            uiOutput("lim_primary_focus_ui"),
            uiOutput("lim_focus_specification_ui")
          ),
          div(
            style = "overflow-y: auto; max-height: calc(100vh - 100px); padding-right: 12px;",
            card(
              card_header("Most Common Limitations Across Studies"),
              plotlyOutput("limitations_freq_plot", height = "400px")
            ),
            card(
              card_header("Limitations Table"),
              DTOutput("limitations_table")
            )
          )
        )
      )
    )
  ),
  
  # ============================================================================
  # TAB 5: NETWORK OF STUDIES
  # ============================================================================
  tabPanel("Network of Studies", value = "network_of_studies", icon = icon("project-diagram"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Network Controls",
        width = 300,
        div(
          class = "text-muted",
          style = "font-size: 12px; line-height: 1.35; margin-bottom: 8px;",
          HTML(
            paste0(
              "<b>What you can do here</b><br>",
              "• Choose a <i>Node Color</i> mapping (Primary Focus, Data Source, Brain Mapping, dFC Method, Number of States).<br>",
              "• Pick the <i>Edge Connections</i> similarity metric. For Network Areas or State Features, adjust the <i>Minimum Edge Weight</i> to declutter.<br>",
              "• Set <i>Node Size</i> to sample-size-driven or fixed.<br>",
              "• Interactions: hover nodes for details and DOI, search by label, use highlight-nearest, zoom/pan. The layout is fixed for stability."
            )
          )
        ),
        selectInput(
          "network_color_selector",
          "Node Color By:",
          choices = c(
            "Primary Focus" = "primary_focus",
            "Data Source" = "data_source",
            "Brain Mapping" = "brain_mapping",
            "dFC Method" = "dfc_method",
            "Number of States" = "num_states"
          ),
          selected = "primary_focus"
        ),
        selectInput(
          "network_edges_selector",
          "Edge Connections (Similarity Metric):",
          choices = c(
            "Primary Focus" = "primary_focus",
            "Data Source" = "data_source",
            "Brain Mapping" = "brain_mapping",
            "Network Areas" = "network_areas",
            "dFC Method" = "dfc_method",
            "Number of States" = "num_states",
            "State Features" = "state_features"
          ),
          selected = "dfc_method"
        ),
        conditionalPanel(
          condition = "input.network_edges_selector == 'network_areas' || input.network_edges_selector == 'state_features'",
          sliderInput(
            "edge_weight_slider",
            "Minimum Edge Weight:",
            min = 1,
            max = 5,
            value = 1,
            step = 1
          )
        ),
        selectInput(
          "node_sizing_selector",
          "Node Size By:",
          choices = c(
            "Sample Size (PD)" = "sample_size",
            "Fixed Size" = "fixed"
          ),
          selected = "sample_size"
        )
      ),
      card(
        card_header("Interactive Network Visualization"),
        visNetwork::visNetworkOutput("study_network_plot", height = "700px")
      )
    )
  ),
  
  # ============================================================================
  # TAB 6: CONTRIBUTE
  # ============================================================================
  tabPanel("Contribute", value = "contribute", icon = icon("upload"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Data Submission",
        width = 350,
        div(
          class = "text-muted",
          style = "font-size: 12px; line-height: 1.35; margin-bottom: 12px;",
          HTML(
            paste0(
              "<b>How to contribute</b><br>",
              "• Download the CSV template below with all required column headers.<br>",
              "• Fill in your study data following the template structure.<br>",
              "• Upload your completed CSV file and submit.<br>",
              "• Our team will manually verify the data before publishing it live."
            )
          )
        ),
        downloadButton(
          "download_template",
          "Download CSV Template",
          class = "btn-info",
          style = "width: 100%; margin-bottom: 15px;"
        ),
        fileInput(
          "upload_contribution",
          "Upload Completed CSV",
          accept = c(".csv"),
          buttonLabel = "Browse...",
          placeholder = "No file selected"
        ),
        actionButton(
          "submit_contribution",
          "Submit Contribution",
          class = "btn-primary",
          style = "width: 100%;",
          icon = icon("paper-plane")
        )
      ),
      div(
        style = "overflow-y: auto; max-height: calc(100vh - 100px);",
        layout_column_wrap(
          width = 1/2,
          card(
            card_header(icon("envelope"), " Contact Us"),
            div(
              style = "padding: 15px; line-height: 1.6;",
              HTML(
                paste0(
                  "<p>For inquiries, questions, or to report issues, please contact:</p>",
                  "<p style='font-size: 16px; color: #3498db;'>",
                  "<i class='fa fa-envelope'></i> <a href='mailto:daniel.kristanto@uol.de' style='text-decoration: none;'>daniel.kristanto@uol.de</a>",
                  "</p>",
                  "<p>We welcome feedback and collaboration opportunities!</p>"
                )
              )
            )
          ),
          card(
            card_header(icon("book"), " Cite Our Work"),
            div(
              style = "padding: 15px; line-height: 1.6;",
              HTML(
                paste0(
                  "<p><strong>Preprint Citation:</strong></p>",
                  "<p style='font-size: 13px; background-color: #f8f9fa; padding: 12px; border-left: 4px solid #3498db;'>",
                  "Kristanto, D., et al. (2025). Dynamic Functional Connectivity in Parkinson's Disease: A Systematic Review. <em>bioRxiv</em>.",
                  "</p>",
                  "<p style='margin-top: 15px;'>",
                  "<a href='https://www.biorxiv.org/content/10.64898/2025.12.08.692999v1' target='_blank' style='color: #3498db; font-size: 14px;'>",
                  "<i class='fa fa-external-link-alt'></i> View Preprint on bioRxiv",
                  "</a>",
                  "</p>"
                )
              )
            )
          )
        )
      )
    )
  )
)
