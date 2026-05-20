# ============================================================================
# Server Logic for Parkinson's Disease dFC Shiny Dashboard
# ============================================================================

library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(plotly)
library(DT)
library(wordcloud2)
library(scales)
library(visNetwork)
library(igraph)
library(later)
library(httr)
library(shinyjs)


# Suppress dplyr/tidyr NSE variable binding warnings
utils::globalVariables(c(
  "Year", "N_sample_PD", "Focus_Specification", "Primary_Focus", "Study_Design", "Label", "Journal",
  "Network_Areas", "area", "n", "Age_PD_Mean", "Med_Status_Scan"
))

# Load helper functions
source("helper_functions.r")

# Load and clean data
data <- load_and_clean_data("Data_script.csv")

# ============================================================================
# ONE-TIME SETUP FOR NETWORK VISUALIZATION
# ============================================================================

# Load pre-computed similarity matrices
if (file.exists("similarity_matrices.RData")) {
  load("similarity_matrices.RData")
  
  # Create master adjacency matrix for stable layout
  # Combine primary_focus and network_areas matrices
  master_matrix <- similarity_matrices$primary_focus + similarity_matrices$network_areas
  master_matrix[master_matrix > 0] <- 1  # Binarize
  
  # Create igraph object and calculate fixed layout
  master_graph <- graph_from_adjacency_matrix(master_matrix, mode = "undirected", diag = FALSE)
  
  # Calculate force-directed layout coordinates (this is the key for stability)
  set.seed(42)  # For reproducibility
  fixed_layout_coords <- layout_with_fr(master_graph)
  rownames(fixed_layout_coords) <- rownames(master_matrix)
  colnames(fixed_layout_coords) <- c("x", "y")
  
  cat("Network layout calculated successfully.\n")
} else {
  warning("similarity_matrices.RData not found. Network visualization will not be available.")
  similarity_matrices <- NULL
  fixed_layout_coords <- NULL
}

# Define server logic
server <- function(input, output, session) {
  
  # ============================================================================
  # HOME TAB: NETWORK VISUALIZATION AND NAVIGATION
  # ============================================================================
  
  # Define main navigation nodes
  main_nodes <- data.frame(
    id = 1:7, 
    label = c("Home", "Full Dataset", "Study Overview", "Method Explorer", "Finding Explorer", "Network of Studies", "Contribute"), 
    value = c(60, 60, 60, 60, 60, 60, 60), 
    title = "Click to see information", 
    shape = "dot",
    color = "#3498db"
  )
  
  # Define edges between nodes
  main_edges <- data.frame(
    from = c(1, 1, 1, 1, 1, 1), 
    to = c(2, 3, 4, 5, 6, 7)
  )
  
  # Render the home network visualization
  output$network_home <- renderVisNetwork({
    visNetwork(main_nodes, main_edges, width = "100%") %>%
      visEvents(click = "function(properties) {
        var nodeId = properties.nodes[0];
        if(nodeId) {
          var label = this.body.data.nodes.get(nodeId).label;
          Shiny.onInputChange('node_clicked', label);
        }
      }")
  })
  
  # Handle node clicks to show descriptions and child nodes
  observeEvent(input$node_clicked, {
    # Show child nodes for tabs with subtabs
    if (input$node_clicked == "Full Dataset") {
      subnodes <- data.frame(
        id = 8:10, 
        label = c("Dataset", "Data Explorer", "Coded Information"), 
        value = c(30, 30, 30), 
        title = "Click to see information", 
        shape = "dot", 
        color = "#85c1e9"
      )
      subedges <- data.frame(from = rep(2, 3), to = 8:10)
      
      visNetworkProxy("network_home") %>%
        visUpdateNodes(nodes = subnodes) %>%
        visUpdateEdges(edges = subedges)
        
    } else if (input$node_clicked == "Method Explorer") {
      subnodes <- data.frame(
        id = 11:13, 
        label = c("MRI Acquisition & Preprocessing", "dFC Analysis", "Clinical Focus & Brain Mapping"), 
        value = c(30, 30, 30), 
        title = "Click to see information", 
        shape = "dot", 
        color = "#85c1e9"
      )
      subedges <- data.frame(from = rep(4, 3), to = 11:13)
      
      visNetworkProxy("network_home") %>%
        visUpdateNodes(nodes = subnodes) %>%
        visUpdateEdges(edges = subedges)
        
    } else if (input$node_clicked == "Finding Explorer") {
      subnodes <- data.frame(
        id = 14:17, 
        label = c("State Features", "State Interpretation", "Proposed Biomarkers", "Limitations"), 
        value = c(30, 30, 30, 30), 
        title = "Click to see information", 
        shape = "dot", 
        color = "#85c1e9"
      )
      subedges <- data.frame(from = rep(5, 4), to = 14:17)
      
      visNetworkProxy("network_home") %>%
        visUpdateNodes(nodes = subnodes) %>%
        visUpdateEdges(edges = subedges)
    }
    
    # Show description for each node
    info <- switch(input$node_clicked,
      "Home" = "Welcome to explore dynamic functional connectivity studies in Parkinson's Disease!",
      "Full Dataset" = "Check out the complete dataset of dFC studies in Parkinson's Disease extracted from systematic review.",
      "Study Overview" = "Explore comprehensive statistics and trends across all included studies.",
      "Method Explorer" = "Dive deep into the methodological approaches used in dFC studies.",
      "Finding Explorer" = "Discover key findings and patterns across studies.",
      "Network of Studies" = "Visualize connections between studies based on various similarity metrics.",
      "Contribute" = "Submit your data to be included in future updates of the DynaPD database.",
      "Dataset" = "Browse the complete dataset with searchable and filterable tables.",
      "Coded Information" = "View column information and descriptions for all variables.",
      "MRI Acquisition & Preprocessing" = "Explore MRI scanner parameters, motion correction, and preprocessing steps.",
      "dFC Analysis" = "Examine brain mapping methods, parcellation schemes, windowing parameters, and clustering approaches.",
      "Clinical Focus & Brain Mapping" = "Investigate network regions and areas of interest.",
      "State Features" = "Explore the number of states, graph measures, and state characteristics.",
      "State Interpretation" = "View common state descriptions and finding variability.",
      "Proposed Biomarkers" = "Review consolidated biomarker summaries from dFC studies.",
      "Limitations" = "Examine the most common study limitations across the field.",
      "Click on a node to see more information."
    )
    
    output$node_description <- renderUI({
      wellPanel(
        style = "background-color: #f0f0f0; padding: 10px; border: 1px solid #ddd;",
        div(style = "font-size: 18px; font-weight: bold;", input$node_clicked),
        div(style = "font-size: 14px;", info),
        div(style = "margin-top: 20px;", actionButton("go_to_tab", "Go to Tab"))
      )
    })
  })
  
  # Handle "Go to Tab" button clicks
  observeEvent(input$go_to_tab, {
    tab_value <- switch(input$node_clicked,
      "Full Dataset" = "full_dataset",
      "Study Overview" = "study_overview",
      "Method Explorer" = "method_explorer",
      "Finding Explorer" = "finding_explorer",
      "Network of Studies" = "network_of_studies",
      "Contribute" = "contribute",
      "Dataset" = "Dataset",
      "Data Explorer" = "Data Explorer",
      "Coded Information" = "Coded Information",
      "MRI Acquisition & Preprocessing" = "MRI Acquisition & Preprocessing",
      "dFC Analysis" = "dFC Analysis",
      "Clinical Focus & Brain Mapping" = "Clinical Focus & Brain Mapping",
      "State Features" = "State Features",
      "State Interpretation" = "State Interpretation",
      "Proposed Biomarkers" = "Proposed Biomarkers",
      "Limitations" = "Limitations",
      "home"
    )
    
    main_tab <- NULL
    nested_tab <- NULL
    
    # Determine the main tab and nested tab based on tab_value
    if (tab_value %in% c("full_dataset", "Dataset", "Data Explorer", "Coded Information")) {
      main_tab <- "full_dataset"
      nested_tab <- tab_value
    } else if (tab_value %in% c("method_explorer", "MRI Acquisition & Preprocessing", "dFC Analysis", "Clinical Focus & Brain Mapping")) {
      main_tab <- "method_explorer"
      nested_tab <- tab_value
    } else if (tab_value %in% c("finding_explorer", "State Features", "State Interpretation", "Proposed Biomarkers", "Limitations")) {
      main_tab <- "finding_explorer"
      nested_tab <- tab_value
    } else {
      main_tab <- tab_value
    }
    
    # Update the main tabset panel first
    updateTabsetPanel(session, "navBar", selected = main_tab)
    
    # If there's a nested tab to update, do it after the main tab is selected
    if (!is.null(nested_tab) && nested_tab != main_tab) {
      # Use a small delay to ensure the main tab is updated before the nested tab
      later::later(function() {
        if (nested_tab %in% c("Dataset", "Coded Information")) {
          # For Full Dataset nested tabs - need to find the actual tab ID
          # The navset_card_tab uses nav_panel with title, not value
          updateTabsetPanel(session, inputId = "full_dataset_tabs", selected = nested_tab)
        } else if (nested_tab %in% c("MRI Acquisition & Preprocessing", "dFC Analysis", "Clinical Focus & Brain Mapping")) {
          # For Method Explorer nested tabs
          updateTabsetPanel(session, inputId = "method_explorer_tabs", selected = nested_tab)
        } else if (nested_tab %in% c("State Features", "State Interpretation", "Proposed Biomarkers", "Limitations")) {
          # For Finding Explorer nested tabs
          updateTabsetPanel(session, inputId = "finding_explorer_tabs", selected = nested_tab)
        }
      }, delay = 0.1)
    }
  })
  
  # ============================================================================
  # EXISTING SERVER LOGIC CONTINUES BELOW
  # ============================================================================
  
  # Findings Explorer: State Features Plots
  output$plot_num_states_findings <- renderPlotly({
    df <- filtered_data_sf() %>% distinct(Label, .keep_all = TRUE)
    if (nrow(df) == 0) return(NULL)
    plot_ly(
      data = df,
      x = ~Number_States,
      type = 'histogram',
      marker = list(color = '#3498db')
    ) %>% layout(
      xaxis = list(title = 'Number of States'),
      yaxis = list(title = 'Frequency'),
      title = 'Distribution of Number of States'
    )
  })

  output$plot_state_features_freq <- renderPlotly({
    df <- filtered_data_sf() %>% distinct(Label, .keep_all = TRUE)
    if (nrow(df) == 0) return(NULL)
    # Split semicolon-separated values and count
    freq_df <- df %>%
      filter(!is.na(State_Features)) %>%
      mutate(feature = strsplit(as.character(State_Features), ";\\s*")) %>%
      tidyr::unnest(feature) %>%
      mutate(feature = trimws(feature)) %>%
      filter(feature != "") %>%
      count(feature, sort = TRUE)
    plot_ly(
      data = freq_df,
      x = ~n,
      y = ~reorder(feature, n),
      type = 'bar',
      orientation = 'h',
      marker = list(color = '#2ecc71')
    ) %>% layout(
      yaxis = list(title = 'State Feature'),
      xaxis = list(title = 'Count'),
      title = 'Frequency of State Features'
    )
  })

  output$plot_graph_measures_freq <- renderPlotly({
    df <- filtered_data_sf() %>% distinct(Label, .keep_all = TRUE)
    if (nrow(df) == 0) return(NULL)
    # Split semicolon-separated values and count
    freq_df <- df %>%
      filter(!is.na(Graph_Measures)) %>%
      mutate(measure = strsplit(as.character(Graph_Measures), ";\\s*")) %>%
      tidyr::unnest(measure) %>%
      mutate(measure = trimws(measure)) %>%
      filter(measure != "") %>%
      count(measure, sort = TRUE)
    plot_ly(
      data = freq_df,
      x = ~n,
      y = ~reorder(measure, n),
      type = 'bar',
      orientation = 'h',
      marker = list(color = '#e67e22')
    ) %>% layout(
      yaxis = list(title = 'Graph Measure'),
      xaxis = list(title = 'Count'),
      title = 'Frequency of Graph Measures'
    )
  })
  # ==================== FINDINGS EXPLORER (independent per subtab) ====================
  # -- State Features subtab UI and reactive --
  output$sf_primary_focus_ui <- renderUI({
    req(input$sf_study_design_filter)
    if (input$sf_study_design_filter == "All") {
      choices <- unique(data$Primary_Focus[!is.na(data$Primary_Focus)])
    } else {
      filtered_by_design <- data %>% filter(Study_Design %in% input$sf_study_design_filter)
      choices <- unique(filtered_by_design$Primary_Focus[!is.na(filtered_by_design$Primary_Focus)])
    }
    selectInput(
      "sf_primary_focus_filter",
      "Primary Focus:",
      choices = c("All", choices),
      selected = "All",
      multiple = FALSE
    )
  })
  output$sf_focus_specification_ui <- renderUI({
    req(input$sf_primary_focus_filter)
    if (input$sf_primary_focus_filter == "All") {
      if (input$sf_study_design_filter == "All") {
        choices <- unique(data$Focus_Specification[!is.na(data$Focus_Specification)])
      } else {
        filtered_by_design <- data %>% filter(Study_Design %in% input$sf_study_design_filter)
        choices <- unique(filtered_by_design$Focus_Specification[!is.na(filtered_by_design$Focus_Specification)])
      }
    } else {
      filtered_by_design <- data %>% filter(Study_Design %in% input$sf_study_design_filter)
      filtered_by_focus <- filtered_by_design %>% filter(Primary_Focus %in% input$sf_primary_focus_filter)
      choices <- unique(filtered_by_focus$Focus_Specification[!is.na(filtered_by_focus$Focus_Specification)])
    }
    selectInput(
      "sf_focus_specification_filter",
      "Focus Specification:",
      choices = c("All", choices),
      selected = "All",
      multiple = FALSE
    )
  })
  filtered_data_sf <- reactive({
    df <- data
    df <- df %>% filter(Year >= input$sf_year_range[1] & Year <= input$sf_year_range[2])
    df <- df %>% filter(N_sample_PD >= input$sf_n_sample_pd_range[1] & N_sample_PD <= input$sf_n_sample_pd_range[2])
    if (!is.null(input$sf_study_design_filter) && input$sf_study_design_filter != "All") {
      df <- df %>% filter(Study_Design %in% input$sf_study_design_filter)
    }
    if (!is.null(input$sf_primary_focus_filter) && input$sf_primary_focus_filter != "All") {
      df <- df %>% filter(Primary_Focus %in% input$sf_primary_focus_filter)
    }
    if (!is.null(input$sf_focus_specification_filter) && input$sf_focus_specification_filter != "All") {
      df <- df %>% filter(Focus_Specification %in% input$sf_focus_specification_filter)
    }
    df
  })

  # State Features Plot (Findings Explorer)
  output$plot_state_features <- renderPlotly({
    df <- filtered_data_sf() %>%
      distinct(Label, .keep_all = TRUE)
    if (nrow(df) == 0) return(NULL)
    plot_ly(
      data = df,
      x = ~Number_States,
      y = ~State_Features,
      type = 'scatter',
      mode = 'markers',
      marker = list(size = 12, color = ~Graph_Measures, colorscale = 'Viridis'),
      text = ~paste('Label:', Label, '<br>Graph Measures:', Graph_Measures)
    ) %>%
      layout(
        xaxis = list(title = 'Number of States'),
        yaxis = list(title = 'State Features'),
        title = 'State Features & Graph Measures'
      )
  })

  # State Features Table (Findings Explorer)
  output$table_state_features <- renderDT({
    df <- filtered_data_sf() %>%
      distinct(Label, .keep_all = TRUE) %>%
      select(Label, DOI, Authors, Year, Journal, N_sample_PD, N_HC, Age_PD_Mean,
             Study_Design, Primary_Focus, Focus_Specification,
             Number_States, State_Features, Graph_Measures)
    df <- df %>% mutate(DOI = ifelse(!is.na(DOI) & DOI != "", paste0('<a href="https://doi.org/', DOI, '" target="_blank">', DOI, '</a>'), NA))
    datatable(df,
              options = list(pageLength = 10, scrollX = TRUE),
              filter = "top",
              rownames = FALSE,
              escape = FALSE,
              caption = "State Features & Graph Measures Table")
  })
  # ==================== CLINICAL FOCUS TAB ====================
  # UI for Focus Specification (Clinical tab) - ONLY filter by Focus Specification
  output$clinical_focus_specification_ui <- renderUI({
    # Only Focus Specification is used for filtering
    choices <- unique(data$Focus_Specification[!is.na(data$Focus_Specification)])
    selectInput(
      "clinical_focus_specification_filter",
      "Focus Specification:",
      choices = c("All", choices),
      selected = "All",
      multiple = FALSE
    )
  })
  # Filtered data for Clinical Focus tab (independent, only Focus Specification)
  filtered_data_clinical <- reactive({
    df <- data
    # Filter by focus specification ONLY
    if (!is.null(input$clinical_focus_specification_filter) && input$clinical_focus_specification_filter != "All") {
      df <- df %>% filter(Focus_Specification %in% input$clinical_focus_specification_filter)
    }
    return(df)
  })
  
  # ==========================================================================
  # REACTIVE DATA FILTERING (Independent for each tab)
  # ==========================================================================
  
  # ==================== OVERVIEW TAB ====================
  # UI for Primary Focus (Overview tab)
  output$overview_primary_focus_ui <- renderUI({
    req(input$overview_study_design_filter)
    if (input$overview_study_design_filter == "All") {
      choices <- unique(data$Primary_Focus[!is.na(data$Primary_Focus)])
    } else {
      filtered_by_design <- data %>% filter(Study_Design %in% input$overview_study_design_filter)
      choices <- unique(filtered_by_design$Primary_Focus[!is.na(filtered_by_design$Primary_Focus)])
    }
    selectInput(
      "overview_primary_focus_filter",
      "Primary Focus:",
      choices = c("All", choices),
      selected = "All",
      multiple = FALSE
    )
  })
  
  # UI for Focus Specification (Overview tab)
  output$overview_focus_specification_ui <- renderUI({
    req(input$overview_primary_focus_filter)
    if (input$overview_primary_focus_filter == "All") {
      if (input$overview_study_design_filter == "All") {
        choices <- unique(data$Focus_Specification[!is.na(data$Focus_Specification)])
      } else {
        filtered_by_design <- data %>% filter(Study_Design %in% input$overview_study_design_filter)
        choices <- unique(filtered_by_design$Focus_Specification[!is.na(filtered_by_design$Focus_Specification)])
      }
    } else {
      filtered_by_design <- data %>% filter(Study_Design %in% input$overview_study_design_filter)
      filtered_by_focus <- filtered_by_design %>% filter(Primary_Focus %in% input$overview_primary_focus_filter)
      choices <- unique(filtered_by_focus$Focus_Specification[!is.na(filtered_by_focus$Focus_Specification)])
    }
    selectInput(
      "overview_focus_specification_filter",
      "Focus Specification:",
      choices = c("All", choices),
      selected = "All",
      multiple = FALSE
    )
  })
  
  # Filtered data for Overview tab
  filtered_data_overview <- reactive({
    df <- data
    
    # Filter by year
    df <- df %>% filter(Year >= input$overview_year_range[1] & Year <= input$overview_year_range[2])
    
    # Filter by sample size
    df <- df %>% filter(N_sample_PD >= input$overview_n_sample_pd_range[1] & 
                         N_sample_PD <= input$overview_n_sample_pd_range[2])
    
    # Filter by study design
    if (!is.null(input$overview_study_design_filter) && input$overview_study_design_filter != "All") {
      df <- df %>% filter(Study_Design %in% input$overview_study_design_filter)
    }
    # Filter by primary focus
    if (!is.null(input$overview_primary_focus_filter) && input$overview_primary_focus_filter != "All") {
      df <- df %>% filter(Primary_Focus %in% input$overview_primary_focus_filter)
    }
    # Filter by focus specification
    if (!is.null(input$overview_focus_specification_filter) && input$overview_focus_specification_filter != "All") {
      df <- df %>% filter(Focus_Specification %in% input$overview_focus_specification_filter)
    }
    
    return(df)
  })
  
  # Unique studies from filtered overview data (for accurate statistics)
  unique_studies_overview <- reactive({
    filtered_data_overview() %>%
      distinct(Label, .keep_all = TRUE)
  })
  
  # ==================== MRI TAB ====================
  # UI for Primary Focus (MRI tab)
  output$mri_primary_focus_ui <- renderUI({
    req(input$mri_study_design_filter)
    if (input$mri_study_design_filter == "All") {
      choices <- unique(data$Primary_Focus[!is.na(data$Primary_Focus)])
    } else {
      filtered_by_design <- data %>% filter(Study_Design %in% input$mri_study_design_filter)
      choices <- unique(filtered_by_design$Primary_Focus[!is.na(filtered_by_design$Primary_Focus)])
    }
    selectInput(
      "mri_primary_focus_filter",
      "Primary Focus:",
      choices = c("All", choices),
      selected = "All",
      multiple = FALSE
    )
  })
  
  # UI for Focus Specification (MRI tab)
  output$mri_focus_specification_ui <- renderUI({
    req(input$mri_primary_focus_filter)
    if (input$mri_primary_focus_filter == "All") {
      if (input$mri_study_design_filter == "All") {
        choices <- unique(data$Focus_Specification[!is.na(data$Focus_Specification)])
      } else {
        filtered_by_design <- data %>% filter(Study_Design %in% input$mri_study_design_filter)
        choices <- unique(filtered_by_design$Focus_Specification[!is.na(filtered_by_design$Focus_Specification)])
      }
    } else {
      filtered_by_design <- data %>% filter(Study_Design %in% input$mri_study_design_filter)
      filtered_by_focus <- filtered_by_design %>% filter(Primary_Focus %in% input$mri_primary_focus_filter)
      choices <- unique(filtered_by_focus$Focus_Specification[!is.na(filtered_by_focus$Focus_Specification)])
    }
    selectInput(
      "mri_focus_specification_filter",
      "Focus Specification:",
      choices = c("All", choices),
      selected = "All",
      multiple = FALSE
    )
  })
  
  # Filtered data for MRI tab (independent from Overview)
  filtered_data_mri <- reactive({
    df <- data
    
    # Filter by year
    df <- df %>% filter(Year >= input$mri_year_range[1] & Year <= input$mri_year_range[2])
    
    # Filter by sample size
    df <- df %>% filter(N_sample_PD >= input$mri_n_sample_pd_range[1] & 
                         N_sample_PD <= input$mri_n_sample_pd_range[2])
    
    # Filter by TR (Repetition Time)
    if (!is.null(input$mri_tr_range)) {
      df <- df %>% filter(is.na(TR_ms) | (TR_ms >= input$mri_tr_range[1] & TR_ms <= input$mri_tr_range[2]))
    }
    
    # Filter by study design
    if (!is.null(input$mri_study_design_filter) && input$mri_study_design_filter != "All") {
      df <- df %>% filter(Study_Design %in% input$mri_study_design_filter)
    }
    # Filter by primary focus
    if (!is.null(input$mri_primary_focus_filter) && input$mri_primary_focus_filter != "All") {
      df <- df %>% filter(Primary_Focus %in% input$mri_primary_focus_filter)
    }
    # Filter by focus specification
    if (!is.null(input$mri_focus_specification_filter) && input$mri_focus_specification_filter != "All") {
      df <- df %>% filter(Focus_Specification %in% input$mri_focus_specification_filter)
    }
    
    return(df)
  })
  
  # Unique studies from filtered MRI data
  unique_studies_mri <- reactive({
    filtered_data_mri() %>%
      distinct(Label, .keep_all = TRUE)
  })
  
  # ==================== dFC TAB ====================
  # UI for Primary Focus (dFC tab)
  output$dfc_primary_focus_ui <- renderUI({
    req(input$dfc_study_design_filter)
    if (input$dfc_study_design_filter == "All") {
      choices <- unique(data$Primary_Focus[!is.na(data$Primary_Focus)])
    } else {
      filtered_by_design <- data %>% filter(Study_Design %in% input$dfc_study_design_filter)
      choices <- unique(filtered_by_design$Primary_Focus[!is.na(filtered_by_design$Primary_Focus)])
    }
    selectInput(
      "dfc_primary_focus_filter",
      "Primary Focus:",
      choices = c("All", choices),
      selected = "All",
      multiple = FALSE
    )
  })
  
  # UI for Focus Specification (dFC tab)
  output$dfc_focus_specification_ui <- renderUI({
    req(input$dfc_primary_focus_filter)
    if (input$dfc_primary_focus_filter == "All") {
      if (input$dfc_study_design_filter == "All") {
        choices <- unique(data$Focus_Specification[!is.na(data$Focus_Specification)])
      } else {
        filtered_by_design <- data %>% filter(Study_Design %in% input$dfc_study_design_filter)
        choices <- unique(filtered_by_design$Focus_Specification[!is.na(filtered_by_design$Focus_Specification)])
      }
    } else {
      filtered_by_design <- data %>% filter(Study_Design %in% input$dfc_study_design_filter)
      filtered_by_focus <- filtered_by_design %>% filter(Primary_Focus %in% input$dfc_primary_focus_filter)
      choices <- unique(filtered_by_focus$Focus_Specification[!is.na(filtered_by_focus$Focus_Specification)])
    }
    selectInput(
      "dfc_focus_specification_filter",
      "Focus Specification:",
      choices = c("All", choices),
      selected = "All",
      multiple = FALSE
    )
  })
  
  # Filtered data for dFC Analysis tab (independent)
  filtered_data_dfc <- reactive({
    df <- data
    
    # Filter by year
    df <- df %>% filter(Year >= input$dfc_year_range[1] & Year <= input$dfc_year_range[2])
    
    # Filter by sample size
    df <- df %>% filter(N_sample_PD >= input$dfc_n_sample_pd_range[1] & 
                         N_sample_PD <= input$dfc_n_sample_pd_range[2])
    
    # Filter by TR
    df <- df %>% filter(is.na(TR_ms) | (TR_ms >= input$dfc_tr_range[1] & TR_ms <= input$dfc_tr_range[2]))
    
    # Filter by study design
    if (!is.null(input$dfc_study_design_filter) && input$dfc_study_design_filter != "All") {
      df <- df %>% filter(Study_Design %in% input$dfc_study_design_filter)
    }
    # Filter by primary focus
    if (!is.null(input$dfc_primary_focus_filter) && input$dfc_primary_focus_filter != "All") {
      df <- df %>% filter(Primary_Focus %in% input$dfc_primary_focus_filter)
    }
    # Filter by focus specification
    if (!is.null(input$dfc_focus_specification_filter) && input$dfc_focus_specification_filter != "All") {
      df <- df %>% filter(Focus_Specification %in% input$dfc_focus_specification_filter)
    }
    
    return(df)
  })
  
  # Unique studies from filtered dFC data
  unique_studies_dfc <- reactive({
    filtered_data_dfc() %>%
      distinct(Label, .keep_all = TRUE)
  })
  
  # Unique studies from filtered clinical data
  unique_studies_clinical <- reactive({
    filtered_data_clinical() %>%
      distinct(Label, .keep_all = TRUE)
  })
  
  # ==========================================================================
  # TAB 1: OVERVIEW - VALUE BOXES
  # ==========================================================================
  
  output$total_studies <- renderText({
    format(get_total_studies(unique_studies_overview()), big.mark = ",")
  })
  
  output$total_pd <- renderText({
    # Use filtered_data_overview (not unique) to sum ALL rows including multiple cohorts
    format(get_total_pd(filtered_data_overview()), big.mark = ",")
  })
  
  output$total_hc <- renderText({
    format(get_total_hc(unique_studies_overview()), big.mark = ",")
  })
  
  # ==========================================================================
  # TAB 1: OVERVIEW - PLOTS
  # ==========================================================================
  
  output$plot_year <- renderPlotly({
    # Use unique studies for accurate year distribution
    df <- unique_studies_overview() %>% 
      filter(!is.na(Year)) %>%
      count(Year)
    
    # Create line + marker plot for clearer year-by-year visualization
    plot_ly(df, x = ~Year, y = ~n, type = "scatter", mode = "lines+markers",
            line = list(color = "#3498db", width = 3),
            marker = list(size = 8, color = "#3498db", 
                         line = list(color = "white", width = 2))) %>%
      layout(
        title = list(text = "Studies by Publication Year", font = list(size = 16)),
        xaxis = list(title = "Year", dtick = 1),  # Force integer-only breaks
        yaxis = list(title = "Number of Studies"),
        plot_bgcolor = "#f8f9fa",
        paper_bgcolor = "#f8f9fa",
        hovermode = "x unified"
      )
  })
  
  output$plot_journals <- renderPlotly({
    # Deduplicate by Label and Journal to count unique papers per journal
    df <- filtered_data_overview() %>%
      filter(!is.na(Journal)) %>%
      distinct(Label, Journal) %>%
      count(Journal, sort = TRUE) %>%
      head(10) %>%
      rename(name = Journal, count = n)
    
    plot_horizontal_bar(df, "Top 10 Journals", "Number of Studies", "#e74c3c")
  })
  
  output$plot_sample_size <- renderPlotly({
    # Use unique studies for sample size distribution
    df <- unique_studies_overview() %>% filter(!is.na(N_sample_PD))
    plot_violin_box(df, "N_sample_PD", "PD Sample Size Distribution", "Sample Size (PD)", "#2ecc71")
  })
  
  output$plot_age_distribution <- renderPlotly({
    # Use unique studies for age distribution
    df <- unique_studies_overview() %>% filter(!is.na(Age_PD_Mean))
    plot_violin_box(df, "Age_PD_Mean", "Mean Age Distribution (PD)", "Mean Age (years)", "#9b59b6")
  })
  
  output$plot_institutions <- renderPlotly({
  output$plot_med_status_scan <- renderPlotly({
    # Deduplicate by Label before counting
    df <- filtered_data_overview() %>%
      distinct(Label, Med_Status_Scan) %>%
      filter(!is.na(Med_Status_Scan)) %>%
      count(Med_Status_Scan, sort = TRUE) %>%
      rename(name = Med_Status_Scan, count = n)
    plot_horizontal_bar(df, "Medical Status During Scan", "Number of Studies", "#f39c12")
  })
  
  output$plot_paradigm_type <- renderPlotly({
    # Count Paradigm_Type distribution
    df <- filtered_data_overview() %>%
      distinct(Label, Paradigm_Type) %>%
      filter(!is.na(Paradigm_Type)) %>%
      count(Paradigm_Type, sort = TRUE) %>%
      rename(name = Paradigm_Type, count = n)
    plot_horizontal_bar(df, "Paradigm Type Distribution", "Number of Studies", "#9b59b6")
  })

  output$plot_sex_distribution <- renderPlotly({
    # Calculate percentage of males in PD samples
    # Convert N_Male_PD to numeric, removing "Not Reported" values
    df <- filtered_data_overview() %>%
      distinct(Label, .keep_all = TRUE) %>%
      mutate(
        N_Male_PD_numeric = suppressWarnings(as.numeric(as.character(N_Male_PD))),
        N_sample_PD_numeric = suppressWarnings(as.numeric(as.character(N_sample_PD)))
      ) %>%
      filter(!is.na(N_Male_PD_numeric) & !is.na(N_sample_PD_numeric) & N_sample_PD_numeric > 0) %>%
      mutate(Percent_Male = (N_Male_PD_numeric / N_sample_PD_numeric) * 100)
    
    if (nrow(df) == 0) return(NULL)
    
    # Use the custom helper function to render a violin + boxplot
    # Keeping the original red color ("#e74c3c") used in your histogram
    plot_violin_box(
      df, 
      "Percent_Male", 
      "Distribution of % Male in PD Samples", 
      "% Male", 
      "#e74c3c"
    )
  })
    # Deduplicate by Label before splitting institutions
    df <- filtered_data_overview() %>%
      filter(!is.na(Data_Source_Institution)) %>%
      distinct(Label, Data_Source_Institution) %>%
      # Split institutions if multiple per study
      mutate(Institution = str_split(Data_Source_Institution, ";\\s*")) %>%
      unnest(Institution) %>%
      count(Institution, sort = TRUE) %>%
      head(10) %>%
      rename(name = Institution, count = n)
    
    plot_horizontal_bar(df, "Top 10 Data Source Institutions", "Number of Studies", "#1abc9c")
  })
  
  output$table_overview_studies <- renderDT({
    # Show unique studies with key information
    df <- unique_studies_overview() %>%
      select(Label, DOI, Authors, Year, Journal, N_sample_PD, N_HC, Age_PD_Mean, 
             Primary_Focus, Focus_Specification, Study_Design) %>%
      mutate(DOI = ifelse(!is.na(DOI) & DOI != "", 
        paste0('<a href="https://doi.org/', DOI, '" target="_blank">', DOI, '</a>'), 
        NA)) %>%
      format_for_display()
    
    datatable(df, 
              options = list(pageLength = 10, scrollX = TRUE),
              filter = "top",
              rownames = FALSE,
              escape = FALSE,
              caption = "Filtered studies based on current filter selections")
  })
  
  # ==========================================================================
  # TAB 2: METHODS EXPLORER - MRI ACQUISITION - VALUE BOXES
  # ==========================================================================
  
  output$mri_total_studies <- renderText({
    format(get_total_studies(unique_studies_mri()), big.mark = ",")
  })
  
  output$mri_total_pd <- renderText({
    # Use filtered_data_mri (not unique) to sum ALL rows including multiple cohorts
    format(get_total_pd(filtered_data_mri()), big.mark = ",")
  })
  
  output$mri_total_hc <- renderText({
    format(get_total_hc(unique_studies_mri()), big.mark = ",")
  })
  
  # ==========================================================================
  # TAB 2: METHODS EXPLORER - MRI ACQUISITION - PLOTS
  # ==========================================================================
  
  output$plot_motion_params <- renderPlotly({
    # Deduplicate before counting to avoid double-counting studies
    df <- filtered_data_mri() %>%
      distinct(Label, Motion_Params) %>%
      filter(!is.na(Motion_Params))
    
    freq_df <- split_and_count(df, "Motion_Params") %>%
      rename(name = value)
    
    plot_horizontal_bar(freq_df, "Motion Parameter Strategies", "Frequency", "#f39c12")
  })
  
  output$plot_tr_hist <- renderPlotly({
    # Use unique studies for TR distribution
    df <- unique_studies_mri() %>% filter(!is.na(TR_ms))
    plot_violin_box(df, "TR_ms", "TR Distribution", "TR (ms)", "#3498db")
  })
  
  output$plot_te_hist <- renderPlotly({
    # Use unique studies for TE distribution
    df <- unique_studies_mri() %>% filter(!is.na(TE_ms))
    plot_violin_box(df, "TE_ms", "TE Distribution", "TE (ms)", "#e74c3c")
  })
  
  output$plot_age_hc_distribution <- renderPlotly({
    # Use unique studies for HC age distribution
    df <- unique_studies_overview() %>% filter(!is.na(Age_HC_mean))
    plot_violin_box(df, "Age_HC_mean", "Mean Age Distribution (HC)", "Mean Age (years)", "#27ae60")
  })
  
  output$plot_n_hc_distribution <- renderPlotly({
    # Use unique studies for HC sample size distribution
    df <- unique_studies_overview() %>% filter(!is.na(N_HC))
    plot_violin_box(df, "N_HC", "HC Sample Size Distribution", "Sample Size (HC)", "#16a085")
  })
  
  output$plot_volumes_distribution <- renderPlotly({
    # Use unique studies for number of volumes distribution
    df <- unique_studies_mri() %>% filter(!is.na(Number_Volumes))
    plot_violin_box(df, "Number_Volumes", "Number of Volumes Distribution", "Number of Volumes", "#d35400")
  })
  
  output$plot_scan_length_hist <- renderPlotly({
    # Use unique studies for scan length distribution
    df <- unique_studies_mri() %>% filter(!is.na(Length_Scan_Minutes))
    plot_violin_box(df, "Length_Scan_Minutes", "Scan Length Distribution", "Scan Length (minutes)", "#2ecc71")
  })
  
  output$plot_cleaning_steps <- renderPlotly({
    # Count studies using each cleaning step (deduplicate by Label first)
    df <- unique_studies_mri()
    
    # Count each cleaning step
    cleaning_steps <- tibble(
      Step = c("Regress WM/CSF", "Regress CompCor", "Regress ICA-AROMA", 
               "Detrending", "Despiking", "Scrubbing"),
      Count = c(
        sum(!is.na(df$Regress_WM_CSF) & df$Regress_WM_CSF == "Yes", na.rm = TRUE),
        sum(!is.na(df$Regress_CompCor) & df$Regress_CompCor == "Yes", na.rm = TRUE),
        sum(!is.na(df$Regress_ICA_AROMA) & df$Regress_ICA_AROMA == "Yes", na.rm = TRUE),
        sum(!is.na(df$Detrending) & df$Detrending == "Yes", na.rm = TRUE),
        sum(!is.na(df$Despiking) & df$Despiking == "Yes", na.rm = TRUE),
        sum(!is.na(df$Scrubbing) & df$Scrubbing == "Yes", na.rm = TRUE)
      )
    ) %>%
      rename(name = Step, count = Count)
    
    plot_horizontal_bar(cleaning_steps, "Preprocessing Cleaning Steps", "Number of Studies", "#9b59b6")
  })
  
  output$plot_exclusion_criteria <- renderPlotly({
    # Plot exclusion criteria as multiple histograms or combined view
    # For simplicity, let's show Mean FD exclusion threshold distribution
    df <- unique_studies_mri() %>% filter(!is.na(Exclusion_Mean_FD_mm))
    
    if (nrow(df) > 0) {
      plot_histogram(df, "Exclusion_Mean_FD_mm", "Exclusion Threshold: Mean FD", "Mean FD Threshold (mm)", "#e67e22")
    } else {
      # Return empty plot if no data
      plot_ly() %>% 
        layout(title = "No Exclusion Data Available",
               xaxis = list(title = ""),
               yaxis = list(title = ""))
    }
  })
  
  output$table_mri_studies <- renderDT({
    # Show unique studies with comprehensive MRI information
    df <- unique_studies_mri() %>%
      select(Label, DOI, Authors, Year, Journal, N_sample_PD, N_HC, Age_PD_Mean,
             Study_Design, Primary_Focus, Focus_Specification,
             TR_ms, TE_ms, Field_Strength, Length_Scan_Minutes, Number_Volumes,
             Motion_Params, Filter_Type, Spatial_Smoothing,
             Regress_WM_CSF, Regress_CompCor, Regress_ICA_AROMA, 
             Detrending, Despiking, Scrubbing,
             Exclusion_Mean_FD_mm, Exclusion_Max_FD_mm, 
             Exclusion_Max_Translation_mm, Exclusion_Max_Rotation_deg,
             Exclusion_Outlier_Frames_percent, Exclusion_Other) %>%
      mutate(DOI = ifelse(!is.na(DOI) & DOI != "", 
        paste0('<a href="https://doi.org/', DOI, '" target="_blank">', DOI, '</a>'), 
        NA)) %>%
      format_for_display()
    
    datatable(df, 
              options = list(pageLength = 10, scrollX = TRUE),
              filter = "top",
              rownames = FALSE,
              escape = FALSE,
              caption = "MRI Acquisition & Preprocessing Details")
  })
  
  # ==========================================================================
  # TAB 2: METHODS EXPLORER - dFC ANALYSIS - VALUE BOXES
  # ==========================================================================
  
  output$dfc_total_studies <- renderText({
    format(get_total_studies(unique_studies_dfc()), big.mark = ",")
  })
  
  output$dfc_total_pd <- renderText({
    # Use filtered_data_dfc (not unique) to sum ALL rows including multiple cohorts
    format(get_total_pd(filtered_data_dfc()), big.mark = ",")
  })
  
  output$dfc_total_hc <- renderText({
    format(get_total_hc(unique_studies_dfc()), big.mark = ",")
  })
  
  # ==========================================================================
  # TAB 2: METHODS EXPLORER - dFC ANALYSIS - PLOTS
  # ==========================================================================
  
  output$plot_brain_mapping <- renderPlotly({
    # Deduplicate before counting
    df <- filtered_data_dfc() %>%
      distinct(Label, Brain_Mapping) %>%
      filter(!is.na(Brain_Mapping))
    
    freq_df <- split_and_count(df, "Brain_Mapping") %>%
      rename(name = value)
    
    plot_horizontal_bar(freq_df, "Brain Mapping Methods", "Frequency", "#3498db")
  })
  
  output$plot_num_networks_hist <- renderPlotly({
    # Use unique studies for number of networks distribution
    df <- unique_studies_dfc() %>% filter(!is.na(Number_Networks))
    plot_histogram(df, "Number_Networks", "Number of Networks Distribution", "Number of Networks", "#e74c3c")
  })
  
  output$plot_parcellation <- renderPlotly({
    # Deduplicate before counting
    df <- filtered_data_dfc() %>%
      distinct(Label, Parcellation_Methods) %>%
      filter(!is.na(Parcellation_Methods))
    
    freq_df <- split_and_count(df, "Parcellation_Methods") %>%
      rename(name = value)
    
    plot_horizontal_bar(freq_df, "Parcellation Methods", "Frequency", "#2ecc71")
  })
  
  output$plot_dfc_methods <- renderPlotly({
    # Deduplicate before counting to avoid double-counting studies
    df <- filtered_data_dfc() %>%
      distinct(Label, dFC_Methods) %>%
      filter(!is.na(dFC_Methods))
    
    freq_df <- split_and_count(df, "dFC_Methods") %>%
      rename(name = value)
    
    plot_horizontal_bar(freq_df, "dFC Methods", "Frequency", "#e67e22")
  })
  
  output$plot_window_size_hist <- renderPlotly({
    # Use unique studies for window size distribution
    df <- unique_studies_dfc() %>% filter(!is.na(Window_Size_Seconds))
    plot_histogram(df, "Window_Size_Seconds", "Window Size Distribution", "Window Size (seconds)", "#9b59b6")
  })
  
  output$plot_window_shift_hist <- renderPlotly({
    # Use unique studies for window shift distribution
    df <- unique_studies_dfc() %>% filter(!is.na(Window_Shift_Second))
    plot_histogram(df, "Window_Shift_Second", "Window Shift Distribution", "Window Shift (seconds)", "#1abc9c")
  })
  
  output$plot_clustering_methods <- renderPlotly({
    # Deduplicate before counting to avoid double-counting studies
    df <- filtered_data_dfc() %>%
      distinct(Label, Clustering_Methods) %>%
      filter(!is.na(Clustering_Methods))
    
    freq_df <- split_and_count(df, "Clustering_Methods") %>%
      rename(name = value)
    
    plot_horizontal_bar(freq_df, "Clustering Methods", "Frequency", "#16a085")
  })
  
  output$plot_num_states_hist <- renderPlotly({
    # Use unique studies for number of states distribution
    df <- unique_studies_dfc() %>% filter(!is.na(Number_States))
    plot_histogram(df, "Number_States", "Number of States Distribution", "Number of States", "#f39c12")
  })
  
  output$table_dfc_studies <- renderDT({
    # Show unique studies with comprehensive dFC information
    df <- unique_studies_dfc() %>%
      select(Label, DOI, Authors, Year, Journal, N_sample_PD, N_HC, Age_PD_Mean,
             Study_Design, Primary_Focus, Focus_Specification,
             Brain_Mapping, Number_Networks, Parcellation_Methods,
             dFC_Methods, Window_Size_Seconds, Window_Shift_Second,
             Clustering_Methods, Number_States) %>%
      mutate(DOI = ifelse(!is.na(DOI) & DOI != "", 
        paste0('<a href="https://doi.org/', DOI, '" target="_blank">', DOI, '</a>'), 
        NA)) %>%
      format_for_display()
    
    datatable(df, 
              options = list(pageLength = 10, scrollX = TRUE),
              filter = "top",
              rownames = FALSE,
              escape = FALSE,
              caption = "dFC Analysis Methods Details")
  })
  
  # ==========================================================================
  # TAB 3: CLINICAL FOCUS & BRAIN MAPPING - PLOTS
  # ==========================================================================
  
  output$wordcloud_networks <- renderWordcloud2({
    # Deduplicate by Label before creating word cloud
    df <- filtered_data_clinical() %>%
      filter(!is.na(Network_Areas)) %>%
      distinct(Label, Network_Areas)
    if (nrow(df) == 0) {
      return(NULL)
    }
    # Split and count network areas
    word_freq <- df %>%
      select(Network_Areas) %>%
      mutate(area = str_split(Network_Areas, ";\\s*")) %>%
      unnest(area) %>%
      mutate(area = str_trim(area)) %>%
      filter(area != "") %>%
      count(area, sort = TRUE) %>%
      rename(word = area, freq = n)
    if (nrow(word_freq) == 0) {
      return(NULL)
    }
    
    # Limit to top 15 most frequent terms
    word_freq <- word_freq %>% head(15)
    
    # Adjust size based on frequency range to ensure visibility
    max_freq <- max(word_freq$freq)
    min_freq <- min(word_freq$freq)
    
    # Scale frequencies for better visualization
    # If all frequencies are the same, use a default size
    if (max_freq == min_freq) {
      size_param <- 0.5
    } else {
      # Normalize frequencies to a reasonable range
      word_freq <- word_freq %>%
        mutate(freq = scales::rescale(freq, to = c(10, 100)))
      size_param <- 0.3
    }
    
    wordcloud2(word_freq, 
               size = size_param, 
               color = "random-dark", 
               backgroundColor = "white",
               minRotation = -pi/6, 
               maxRotation = pi/6,
               rotateRatio = 0.3)
  })
  
  output$table_network_freq <- renderDT({
    # Deduplicate by Label before creating frequency table
    df <- filtered_data_clinical() %>%
      filter(!is.na(Network_Areas)) %>%
      distinct(Label, Network_Areas) %>%
      select(Network_Areas) %>%
      mutate(area = str_split(Network_Areas, ";\\s*")) %>%
      unnest(area) %>%
      mutate(area = str_trim(area)) %>%
      filter(area != "") %>%
      count(area, sort = TRUE) %>%
      rename(`Network/Area` = area, Frequency = n)
    datatable(df, 
              options = list(pageLength = 15, scrollX = TRUE),
              rownames = FALSE)
  })
  
  # ==========================================================================
  # TAB 4: FINDINGS EXPLORER - STATE INTERPRETATIONS
  # ==========================================================================
  
  output$wordcloud_states <- renderWordcloud2({
    # Deduplicate by Label before creating word cloud
    df_unique <- data %>%
      distinct(Label, Identified_States_Description)
    
    word_freq <- prepare_wordcloud_data(df_unique, "Identified_States_Description")
    
    if (nrow(word_freq) == 0) {
      return(NULL)
    }
    
    wordcloud2(word_freq, size = 0.6, color = "random-dark", backgroundColor = "white")
  })
  
  output$table_state_conclusions <- renderDT({
    # Show unique studies only
    df <- data %>%
      distinct(Label, .keep_all = TRUE) %>%
      select(Label, DOI, Authors, Year, Primary_Focus,
             State_Pattern_Conclusion, Transition_Pattern_Conclusion,
             Integration_State_Findings, Segregation_State_Findings) %>%
      filter(!is.na(State_Pattern_Conclusion) | !is.na(Transition_Pattern_Conclusion))
    df <- df %>% mutate(DOI = ifelse(!is.na(DOI) & DOI != "", paste0('<a href="https://doi.org/', DOI, '" target="_blank">', DOI, '</a>'), NA))
    datatable(df, 
              options = list(pageLength = 10, scrollX = TRUE),
              filter = "top",
              rownames = FALSE,
              escape = FALSE)
  })
  
  # ==========================================================================
  # TAB 4: FINDINGS EXPLORER - PROPOSED BIOMARKERS
  # ==========================================================================
  # UI for Biomarkers primary focus/specification
  output$pb_primary_focus_ui <- renderUI({
    req(input$pb_study_design_filter)
    if (input$pb_study_design_filter == "All") {
      choices <- unique(data$Primary_Focus[!is.na(data$Primary_Focus)])
    } else {
      filtered_by_design <- data %>% filter(Study_Design %in% input$pb_study_design_filter)
      choices <- unique(filtered_by_design$Primary_Focus[!is.na(filtered_by_design$Primary_Focus)])
    }
    selectInput(
      "pb_primary_focus_filter",
      "Primary Focus:",
      choices = c("All", choices),
      selected = "All",
      multiple = FALSE
    )
  })
  output$pb_focus_specification_ui <- renderUI({
    req(input$pb_primary_focus_filter)
    if (input$pb_primary_focus_filter == "All") {
      if (input$pb_study_design_filter == "All") {
        choices <- unique(data$Focus_Specification[!is.na(data$Focus_Specification)])
      } else {
        filtered_by_design <- data %>% filter(Study_Design %in% input$pb_study_design_filter)
        choices <- unique(filtered_by_design$Focus_Specification[!is.na(filtered_by_design$Focus_Specification)])
      }
    } else {
      filtered_by_design <- data %>% filter(Study_Design %in% input$pb_study_design_filter)
      filtered_by_focus <- filtered_by_design %>% filter(Primary_Focus %in% input$pb_primary_focus_filter)
      choices <- unique(filtered_by_focus$Focus_Specification[!is.na(filtered_by_focus$Focus_Specification)])
    }
    selectInput(
      "pb_focus_specification_filter",
      "Focus Specification:",
      choices = c("All", choices),
      selected = "All",
      multiple = FALSE
    )
  })
  filtered_data_pb <- reactive({
    df <- data
    df <- df %>% filter(Year >= input$pb_year_range[1] & Year <= input$pb_year_range[2])
    df <- df %>% filter(N_sample_PD >= input$pb_n_sample_pd_range[1] & N_sample_PD <= input$pb_n_sample_pd_range[2])
    if (!is.null(input$pb_study_design_filter) && input$pb_study_design_filter != "All") {
      df <- df %>% filter(Study_Design %in% input$pb_study_design_filter)
    }
    if (!is.null(input$pb_primary_focus_filter) && input$pb_primary_focus_filter != "All") {
      df <- df %>% filter(Primary_Focus %in% input$pb_primary_focus_filter)
    }
    if (!is.null(input$pb_focus_specification_filter) && input$pb_focus_specification_filter != "All") {
      df <- df %>% filter(Focus_Specification %in% input$pb_focus_specification_filter)
    }
    df
  })
  output$table_biomarkers <- renderDT({
    # Show unique studies only
    df <- filtered_data_pb() %>%
      distinct(Label, .keep_all = TRUE) %>%
      select(Label, DOI, Authors, Year, Primary_Focus, Focus_Specification,
             Proposed_Biomarker_Summary_Cleaned, Network_Areas,
             State_Pattern_Conclusion, Transition_Pattern_Conclusion) %>%
      filter(!is.na(Proposed_Biomarker_Summary_Cleaned))
    df <- df %>% mutate(DOI = ifelse(!is.na(DOI) & DOI != "", paste0('<a href="https://doi.org/', DOI, '" target="_blank">', DOI, '</a>'), NA))
    datatable(df, 
              options = list(pageLength = 10, scrollX = TRUE),
              filter = "top",
              rownames = FALSE,
              escape = FALSE)
  })
  
  # ==========================================================================
  # TAB 5: FULL DATASET
  # ==========================================================================
  
  output$table_full_dataset <- renderDT({
    df <- data %>% format_for_display()
    
    datatable(df, 
              options = list(
                pageLength = 25, 
                scrollX = TRUE,
                scrollY = "600px",
                fixedColumns = list(leftColumns = 2)
              ),
              filter = "top",
              rownames = FALSE)
  })
  
  output$table_coded_information <- renderDT({
    info_data <- read_csv("Information.csv", show_col_types = FALSE)
    
    datatable(info_data, 
              options = list(
                pageLength = 25, 
                scrollX = TRUE
                #scrollY = "600px"
              ),
              filter = "top",
              rownames = FALSE)
  })
  
  output$table_data_explorer <- renderDT({
    # Create a copy of data for Data Explorer with numeric conversions
    df_explorer <- data %>%
      mutate(
        N_Male_PD = suppressWarnings(as.numeric(as.character(N_Male_PD))),
        N_Male_HC = suppressWarnings(as.numeric(as.character(N_Male_HC)))
      ) %>%
      format_for_display()
    
    # Convert character columns with reasonable unique counts to factors for dropdown filters
    categorical_cols <- c("Study_Design", "Primary_Focus", "Focus_Specification", 
                         "Data_Source", "Data_Source_Institution", "Paradigm_Type", 
                         "Motion_Params", "Filter_Type", "Brain_Mapping", 
                         "Parcellation_Methods", "dFC_Methods", "Clustering_Methods",
                         "PD_Medication_Status", "PD_Cognitive_Status", "PD_Motor_State")
    
    df_explorer <- df_explorer %>%
      mutate(across(all_of(intersect(categorical_cols, names(.))), as.factor))
    
    datatable(df_explorer, 
              extensions = c('FixedHeader', 'FixedColumns'),
              options = list(
                pageLength = 25, 
                scrollX = TRUE,
                scrollY = "600px",
                fixedColumns = list(leftColumns = 2),
                fixedHeader = TRUE,
                dom = 'frtip',  # f = filter, r = processing, t = table, i = info, p = pagination
                initComplete = JS(
                  "function(settings, json) {",
                  "  var api = this.api();",
                  "  var header = $(api.table().header()).clone();",
                  "  var scrollBody = $(api.table().container()).find('.dataTables_scrollBody');",
                  "}"
                )
              ),
              filter = "top",
              rownames = FALSE) %>%
      formatStyle(columns = names(df_explorer), fontSize = '12px')
  })
  
  # ==================== FINDINGS EXPLORER: STATE FEATURES SUBTAB ====================
  # UI for State Features primary focus/specification
  output$sfind_primary_focus_ui <- renderUI({
    req(input$sfind_study_design_filter)
    if (input$sfind_study_design_filter == "All") {
      choices <- unique(data$Primary_Focus[!is.na(data$Primary_Focus)])
    } else {
      filtered_by_design <- data %>% filter(Study_Design %in% input$sfind_study_design_filter)
      choices <- unique(filtered_by_design$Primary_Focus[!is.na(filtered_by_design$Primary_Focus)])
    }
    selectInput(
      "sfind_primary_focus_filter",
      "Primary Focus:",
      choices = c("All", choices),
      selected = "All",
      multiple = FALSE
    )
  })
  output$sfind_focus_specification_ui <- renderUI({
    req(input$sfind_primary_focus_filter)
    if (input$sfind_primary_focus_filter == "All") {
      if (input$sfind_study_design_filter == "All") {
        choices <- unique(data$Focus_Specification[!is.na(data$Focus_Specification)])
      } else {
        filtered_by_design <- data %>% filter(Study_Design %in% input$sfind_study_design_filter)
        choices <- unique(filtered_by_design$Focus_Specification[!is.na(filtered_by_design$Focus_Specification)])
      }
    } else {
      filtered_by_design <- data %>% filter(Study_Design %in% input$sfind_study_design_filter)
      filtered_by_focus <- filtered_by_design %>% filter(Primary_Focus %in% input$sfind_primary_focus_filter)
      choices <- unique(filtered_by_focus$Focus_Specification[!is.na(filtered_by_focus$Focus_Specification)])
    }
    selectInput(
      "sfind_focus_specification_filter",
      "Focus Specification:",
      choices = c("All", choices),
      selected = "All",
      multiple = FALSE
    )
  })
  # Ensure dynamic UI renders even when tab loads lazily
  outputOptions(output, "sfind_primary_focus_ui", suspendWhenHidden = FALSE)
  outputOptions(output, "sfind_focus_specification_ui", suspendWhenHidden = FALSE)
  filtered_data_sfind <- reactive({
    df <- data
    df <- df %>% filter(Year >= input$sfind_year_range[1] & Year <= input$sfind_year_range[2])
    df <- df %>% filter(N_sample_PD >= input$sfind_n_sample_pd_range[1] & N_sample_PD <= input$sfind_n_sample_pd_range[2])
    if (!is.null(input$sfind_study_design_filter) && input$sfind_study_design_filter != "All") {
      df <- df %>% filter(Study_Design %in% input$sfind_study_design_filter)
    }
    if (!is.null(input$sfind_primary_focus_filter) && input$sfind_primary_focus_filter != "All") {
      df <- df %>% filter(Primary_Focus %in% input$sfind_primary_focus_filter)
    }
    if (!is.null(input$sfind_focus_specification_filter) && input$sfind_focus_specification_filter != "All") {
      df <- df %>% filter(Focus_Specification %in% input$sfind_focus_specification_filter)
    }
    df
  })
  # Word Cloud for State Descriptions
  # Reactive for findings variability summary
  findings_variability_summary <- reactive({
    df <- filtered_data_sfind() %>% distinct(Label, .keep_all = TRUE)
    if (input$findings_variability_type == "state") {
      # Use State_Pattern_Conclusion instead of State_Finding_Category
      df %>% 
        filter(!is.na(State_Pattern_Conclusion) & State_Pattern_Conclusion != "") %>%
        group_by(State_Pattern_Conclusion) %>% 
        summarise(n = n()) %>% 
        arrange(desc(n))
    } else {
      df %>% group_by(Transition_Finding_Category) %>% summarise(n = n()) %>% arrange(desc(n))
    }
  })

  # Interactive plot for findings variability
  output$findings_variability_plot <- renderPlotly({
    plot_df <- findings_variability_summary()
    if (nrow(plot_df) == 0) return(NULL)
    # Choose correct y variable
    if (input$findings_variability_type == "state") {
      yvar <- plot_df$State_Pattern_Conclusion
      ylab <- "State Pattern Conclusion"
      plot_title <- "State Pattern Conclusions"
    } else {
      yvar <- plot_df$Transition_Finding_Category
      ylab <- "Transition Finding Category"
      plot_title <- "Finding Variability by Category"
    }
    p <- plot_ly(
      data = plot_df,
      x = ~n,
      y = ~reorder(yvar, n),
      type = 'bar',
      orientation = 'h',
      marker = list(color = '#3498db'),
      source = "findings_var_plot"
    ) %>% layout(
      yaxis = list(title = ylab),
      xaxis = list(title = 'Number of Studies'),
      title = list(text = plot_title, font = list(size = 16)),
      plot_bgcolor = "#f8f9fa",
      paper_bgcolor = "#f8f9fa",
      margin = list(l = 200)
    )
    plotly::event_register(p, 'plotly_click')
    p
  })

  # Interactive detail table for findings
  output$findings_detail_table <- renderDT({
    df <- filtered_data_sfind() %>% distinct(Label, .keep_all = TRUE)
    click <- plotly::event_data("plotly_click", source = "findings_var_plot")
    if (input$findings_variability_type == "state") {
      col_cat <- "State_Pattern_Conclusion"
      col_text <- "State_Pattern_Conclusion"
    } else {
      col_cat <- "Transition_Finding_Category"
      col_text <- "Transition_Pattern_Conclusion"
    }
    if (!is.null(click) && !is.null(click$y)) {
      selected_cat <- click$y
      df <- df %>% filter(.data[[col_cat]] == selected_cat)
    }
    df <- df %>% select(Label, DOI, Authors, Year, !!col_cat, !!col_text)
    df <- df %>% mutate(DOI = ifelse(!is.na(DOI) & DOI != "", paste0('<a href="https://doi.org/', DOI, '" target="_blank">', DOI, '</a>'), NA))
    datatable(df, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE, escape = FALSE)
  })
  
  # ==================== FINDINGS EXPLORER: LIMITATIONS SUBTAB ====================
  # UI for Limitations primary focus/specification
  output$lim_primary_focus_ui <- renderUI({
    req(input$lim_study_design_filter)
    if (input$lim_study_design_filter == "All") {
      choices <- unique(data$Primary_Focus[!is.na(data$Primary_Focus)])
    } else {
      filtered_by_design <- data %>% filter(Study_Design %in% input$lim_study_design_filter)
      choices <- unique(filtered_by_design$Primary_Focus[!is.na(filtered_by_design$Primary_Focus)])
    }
    selectInput(
      "lim_primary_focus_filter",
      "Primary Focus:",
      choices = c("All", choices),
      selected = "All",
      multiple = FALSE
    )
  })
  output$lim_focus_specification_ui <- renderUI({
    req(input$lim_primary_focus_filter)
    if (input$lim_primary_focus_filter == "All") {
      if (input$lim_study_design_filter == "All") {
        choices <- unique(data$Focus_Specification[!is.na(data$Focus_Specification)])
      } else {
        filtered_by_design <- data %>% filter(Study_Design %in% input$lim_study_design_filter)
        choices <- unique(filtered_by_design$Focus_Specification[!is.na(filtered_by_design$Focus_Specification)])
      }
    } else {
      filtered_by_design <- data %>% filter(Study_Design %in% input$lim_study_design_filter)
      filtered_by_focus <- filtered_by_design %>% filter(Primary_Focus %in% input$lim_primary_focus_filter)
      choices <- unique(filtered_by_focus$Focus_Specification[!is.na(filtered_by_focus$Focus_Specification)])
    }
    selectInput(
      "lim_focus_specification_filter",
      "Focus Specification:",
      choices = c("All", choices),
      selected = "All",
      multiple = FALSE
    )
  })
  filtered_data_lim <- reactive({
    df <- data
    df <- df %>% filter(Year >= input$lim_year_range[1] & Year <= input$lim_year_range[2])
    df <- df %>% filter(N_sample_PD >= input$lim_n_sample_pd_range[1] & N_sample_PD <= input$lim_n_sample_pd_range[2])
    if (!is.null(input$lim_study_design_filter) && input$lim_study_design_filter != "All") {
      df <- df %>% filter(Study_Design %in% input$lim_study_design_filter)
    }
    if (!is.null(input$lim_primary_focus_filter) && input$lim_primary_focus_filter != "All") {
      df <- df %>% filter(Primary_Focus %in% input$lim_primary_focus_filter)
    }
    if (!is.null(input$lim_focus_specification_filter) && input$lim_focus_specification_filter != "All") {
      df <- df %>% filter(Focus_Specification %in% input$lim_focus_specification_filter)
    }
    df
  })
  limitations_long <- reactive({
    df <- filtered_data_lim() %>% distinct(Label, .keep_all = TRUE)
    if (!"Limitations" %in% names(df)) return(data.frame())
    df %>%
      filter(!is.na(Limitations)) %>%
      mutate(Limitation = strsplit(as.character(Limitations), "; ")) %>%
      tidyr::unnest(Limitation) %>%
      mutate(Limitation = str_to_title(str_trim(Limitation))) %>%
      filter(Limitation != "")
  })

  output$limitations_freq_plot <- renderPlotly({
    long_df <- limitations_long()
    if (nrow(long_df) == 0) return(NULL)
    freq_df <- long_df %>% count(Limitation, sort = TRUE) %>% head(10)
    plot_ly(
      data = freq_df,
      x = ~n,
      y = ~reorder(Limitation, n),
      type = 'bar',
      orientation = 'h',
      marker = list(color = '#e67e22')
    ) %>% layout(
      yaxis = list(title = 'Limitation'),
      xaxis = list(title = 'Count'),
      title = 'Top 10 Most Common Limitations'
    )
  })

  output$limitations_table <- renderDT({
    df <- filtered_data_lim() %>% distinct(Label, .keep_all = TRUE)
    if (nrow(df) == 0) return(NULL)
    # Show original table with Limitations column - include all papers (NA, Not reported, etc.)
    table_df <- df %>% 
      select(Label, DOI, Authors, Year, Journal, Limitations)
    table_df <- table_df %>% mutate(DOI = ifelse(!is.na(DOI) & DOI != "", paste0('<a href="https://doi.org/', DOI, '" target="_blank">', DOI, '</a>'), NA))
    datatable(table_df, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE, escape = FALSE)
  })
  
  # ==========================================================================
  # TAB: NETWORK OF STUDIES
  # ==========================================================================
  
  # Reactive: Prepare network data based on user selections
  network_data <- reactive({
    # Check if similarity matrices are loaded
    if (is.null(similarity_matrices) || is.null(fixed_layout_coords)) {
      return(NULL)
    }
    
    # Select the appropriate similarity matrix for edges
    selected_matrix <- similarity_matrices[[input$network_edges_selector]]
    
    if (is.null(selected_matrix)) {
      return(NULL)
    }
    
    # Apply edge weight threshold for weighted networks
    if (input$network_edges_selector %in% c("network_areas", "state_features")) {
      threshold <- input$edge_weight_slider
      selected_matrix[selected_matrix < threshold] <- 0
    }
    
    # Create igraph object
    g <- graph_from_adjacency_matrix(selected_matrix, mode = "undirected", 
                                     diag = FALSE, weighted = TRUE)
    
    # Get nodes and edges data frames
    edges_df <- igraph::as_data_frame(g, what = "edges")
    nodes_df <- data.frame(
      id = V(g)$name,
      label = V(g)$name,
      stringsAsFactors = FALSE
    )
    
    # Return list with both data frames
    list(nodes = nodes_df, edges = edges_df, graph = g)
  })
  
  # Render the network visualization
  output$study_network_plot <- visNetwork::renderVisNetwork({
    net_data <- network_data()
    
    if (is.null(net_data)) {
      return(NULL)
    }
    
    nodes_df <- net_data$nodes
    edges_df <- net_data$edges
    
    # Merge with study metadata - include all relevant columns
    nodes_df <- nodes_df %>%
      left_join(data %>% select(Label, Authors, Year, DOI, Primary_Focus, N_sample_PD,
                                 Data_Source, Brain_Mapping, Network_Areas, 
                                 dFC_Methods, Number_States, State_Features), 
                by = c("id" = "Label")) %>%
      distinct(id, .keep_all = TRUE)  # Ensure unique IDs
    
    # Add fixed layout coordinates
    coord_df <- as.data.frame(fixed_layout_coords) %>% 
      tibble::rownames_to_column("id")
    # Coordinates already named "x" and "y" from setup
    # Scale coordinates to a reasonable range for visNetwork (e.g., 0-1000)
    coord_df$x <- scales::rescale(coord_df$x, to = c(0, 1000))
    coord_df$y <- scales::rescale(coord_df$y, to = c(0, 1000))
    
    nodes_df <- nodes_df %>%
      left_join(coord_df, by = "id") %>%
      distinct(id, .keep_all = TRUE)  # Ensure unique IDs after coordinate join
    
    # Apply node sizing
    if (input$node_sizing_selector == "sample_size") {
      # Scale sample size to reasonable node sizes (10-40)
      nodes_df$size <- scales::rescale(nodes_df$N_sample_PD, to = c(10, 40), 
                                       from = range(nodes_df$N_sample_PD, na.rm = TRUE))
      nodes_df$size[is.na(nodes_df$size)] <- 15  # Default size for NA
    } else {
      nodes_df$size <- 20  # Fixed size
    }
    
    # Apply node coloring based on user selection
    color_metric <- input$network_color_selector
    edge_metric <- input$network_edges_selector
    
    # Map color metrics to data columns
    color_column <- switch(color_metric,
      "primary_focus" = "Primary_Focus",
      "data_source" = "Data_Source",
      "brain_mapping" = "Brain_Mapping",
      "dfc_method" = "dFC_Methods",
      "num_states" = "Number_States"
    )
    
    # Set group for coloring and legend
    if (color_column %in% names(nodes_df)) {
      nodes_df$group <- nodes_df[[color_column]]
    }
    
    # Map edge metrics to data columns for tooltip
    edge_column <- switch(edge_metric,
      "primary_focus" = "Primary_Focus",
      "data_source" = "Data_Source",
      "brain_mapping" = "Brain_Mapping",
      "dfc_method" = "dFC_Methods",
      "num_states" = "Number_States",
      "network_areas" = "Network_Areas",
      "state_features" = "State_Features",
      "citation" = NULL
    )
    
    # Create dynamic tooltips based on edge metric
    if (!is.null(edge_column) && edge_column %in% names(nodes_df)) {
      # Get the edge metric label
      edge_label <- switch(edge_metric,
        "primary_focus" = "Primary Focus",
        "data_source" = "Data Source",
        "brain_mapping" = "Brain Mapping",
        "dfc_method" = "dFC Method",
        "num_states" = "Number of States",
        "network_areas" = "Network Areas",
        "state_features" = "State Features"
      )
      
      nodes_df$title <- paste0(
        "<b>", nodes_df$label, "</b><br>",
        nodes_df$Authors, " (", nodes_df$Year, ")<br>",
        "Sample Size (PD): ", nodes_df$N_sample_PD, "<br>",
        edge_label, ": ", nodes_df[[edge_column]], "<br>",
        "DOI: <a href='https://doi.org/", nodes_df$DOI, "' target='_blank'>", 
        nodes_df$DOI, "</a>"
      )
    } else {
      # For citation (no specific column)
      nodes_df$title <- paste0(
        "<b>", nodes_df$label, "</b><br>",
        nodes_df$Authors, " (", nodes_df$Year, ")<br>",
        "Sample Size (PD): ", nodes_df$N_sample_PD, "<br>",
        "DOI: <a href='https://doi.org/", nodes_df$DOI, "' target='_blank'>", 
        nodes_df$DOI, "</a>"
      )
    }
    
    # Prepare edges
    if (nrow(edges_df) > 0) {
      # Add edge width based on weight
      if ("weight" %in% names(edges_df)) {
        edges_df$width <- scales::rescale(edges_df$weight, to = c(1, 5))
      } else {
        edges_df$width <- 1
      }
      edges_df$color <- "#848484"
    }
    
    # Create the network with manual layout (x and y columns already set)
    vis_net <- visNetwork(nodes_df, edges_df, height = "700px", width = "100%")
    
    # Use manual layout (coordinates are already in nodes_df)
    vis_net <- vis_net %>%
      visNodes(physics = FALSE) %>%  # Disable physics to use fixed coordinates
      visEdges(smooth = FALSE, color = list(color = "#848484", opacity = 0.5))
    
    # Add interactive options
    vis_net <- vis_net %>%
      visOptions(
        highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
        nodesIdSelection = TRUE
      ) %>%
      visInteraction(navigationButtons = TRUE, hover = TRUE) %>%
      visPhysics(enabled = FALSE)  # Ensure physics stays off
    
    # Add legend based on node color selection (always show with bigger font)
    vis_net <- vis_net %>% 
      visLegend(
        width = 0.2, 
        position = "right",
        useGroups = TRUE,
        zoom = FALSE,
        ncol = 1,
        stepY = 60,  # Increase spacing between legend items
        addNodes = list(
          shape = "dot",
          size = 20,
          font = list(size = 18, color = "black")  # Bigger font
        )
      ) %>%
      # Automatically fit the network to the viewport (center it)
      visEvents(type = "once", afterDrawing = "function() {
        this.fit();
      }")
    
    vis_net
  })
  
  # ============================================================================
  # CONTRIBUTE TAB: CSV TEMPLATE DOWNLOAD AND FILE UPLOAD
  # ============================================================================
  
  # Download handler for CSV template
  output$download_template <- downloadHandler(
    filename = function() {
      "DynaPD_Submission_Template.csv"
    },
    content = function(file) {
      # Create empty dataframe with same columns as the main dataset
      template <- data.frame(matrix(ncol = ncol(data), nrow = 0))
      colnames(template) <- colnames(data)
      
      # Write to CSV
      write.csv(template, file, row.names = FALSE)
    }
  )
  
  # File upload and submission to WebDAV cloud storage
  observeEvent(input$submit_contribution, {
    # Require that a file has been uploaded
    req(input$upload_contribution)
    
    # Load credentials from file
    if (file.exists("credentials.R")) {
      source("credentials.R")
      nc_user <- NEXTCLOUD_USER
      nc_pass <- NEXTCLOUD_PASS
    } else {
      # Fallback to environment variables for backward compatibility
      nc_user <- Sys.getenv("NEXTCLOUD_USER")
      nc_pass <- Sys.getenv("NEXTCLOUD_PASS")
    }
    
    if (nc_user == "" || nc_pass == "") {
      showNotification(
        "Error: Nextcloud credentials not found. Please ensure credentials.R file exists or environment variables are set.",
        type = "error",
        duration = 10
      )
      return()
    }
    
    # Show processing notification
    showNotification("Processing your submission...", type = "message", duration = 3)
    
    # Get the uploaded file info
    upload_info <- input$upload_contribution
    
    # Generate unique filename with timestamp
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    file_ext <- tools::file_ext(upload_info$name)
    unique_filename <- paste0("contribution_", timestamp, ".", file_ext)
    
    # WebDAV endpoint
    webdav_url <- paste0(
      "https://cloud.uol.de/remote.php/dav/files/wowi8711/DynaPD/Contribution/",
      unique_filename
    )
    
    # Try to upload the file
    tryCatch({
      # Read the file content
      file_content <- readBin(upload_info$datapath, "raw", file.info(upload_info$datapath)$size)
      
      # Upload using httr::PUT with basic authentication
      response <- httr::PUT(
        url = webdav_url,
        body = file_content,
        httr::authenticate(user = nc_user, password = nc_pass, type = "basic"),
        httr::content_type("text/csv"),
        httr::add_headers(
          "OCS-APIRequest" = "true"
        )
      )
      
      # Check response status
      if (httr::status_code(response) %in% c(200, 201, 204)) {
        # Success - clear the file input
        shinyjs::reset("upload_contribution")
        
        # Show success notification
        showNotification(
          "Thank you for your contribution! Our team will manually verify the data before publishing it live.",
          type = "message",
          duration = 8
        )
      } else {
        # Upload failed - get more details
        status <- httr::status_code(response)
        error_msg <- paste0("Upload failed with status code: ", status)
        
        # Add specific guidance for common errors
        if (status == 401) {
          error_msg <- paste0(error_msg, 
            "\n\nAuthentication failed. Please check:\n",
            "1. Your credentials in .Renviron file\n",
            "2. You may need to generate an 'App Password' in Nextcloud settings\n",
            "3. Restart R session after updating .Renviron")
        } else if (status == 404) {
          error_msg <- paste0(error_msg, " - Folder path not found")
        } else if (status == 403) {
          error_msg <- paste0(error_msg, " - Permission denied")
        }
        
        showNotification(
          paste0(error_msg, "\n\nContact daniel.kristanto@uol.de for assistance."),
          type = "error",
          duration = 15
        )
      }
    }, error = function(e) {
      # Handle errors
      showNotification(
        paste0("Error during upload: ", e$message, 
               ". Please check your internet connection and try again."),
        type = "error",
        duration = 10
      )
    })
  })
}
