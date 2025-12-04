# DynaPD Interactive App

An interactive web-based dashboard for exploring dynamic functional connectivity (dFC) studies in Parkinson's Disease.

## 🌐 Live Application

**Access the interactive web application here:** [https://individualbrainproject.shinyapps.io/dfc-pd/](https://individualbrainproject.shinyapps.io/dfc-pd/)

## 📋 Overview

DynaPD Interactive App provides a comprehensive, user-friendly interface for exploring and analyzing research on dynamic functional connectivity in Parkinson's Disease. The dashboard enables researchers, clinicians, and students to:

- **Browse** a curated dataset of dFC studies in PD
- **Filter** studies by multiple criteria (year, sample size, study design, focus areas)
- **Visualize** methodological trends and key findings
- **Explore** brain networks and connectivity patterns
- **Discover** proposed biomarkers and study limitations
- **Interact** with a network visualization of research connections

## 🎯 Key Features

### 1. Full Dataset Browser
- Searchable and filterable complete dataset
- Clickable DOI links for direct access to papers
- Comprehensive metadata and coded information

### 2. Study Overview
- Summary statistics (total studies, participants)
- Publication trends over time
- Sample size and age distributions
- Top journals and institutions
- Medical status during scanning

### 3. Method Explorer
- **MRI Acquisition & Preprocessing**: Scanner parameters, motion correction, cleaning steps
- **dFC Analysis**: Brain mapping methods, parcellation schemes, windowing parameters, clustering approaches
- **Clinical Focus & Brain Mapping**: Network regions and areas of interest

### 4. Finding Explorer
- **State Features**: Number of states, graph measures, state characteristics
- **State Interpretation**: Common descriptions, finding variability (strongly-connected vs. sparsely-connected states)
- **Proposed Biomarkers**: Consolidated biomarker summaries
- **Limitations**: Most common study limitations across the field

### 5. Network of Studies
- Interactive network visualization showing study relationships
- Multiple similarity metrics (methodology, focus areas, networks)
- Customizable node coloring and sizing
- Fixed layout for stable visualization

## 🚀 Getting Started

### Online Access
Simply visit: [https://individualbrainproject.shinyapps.io/dfc-pd/](https://individualbrainproject.shinyapps.io/dfc-pd/)

No installation required!

### Local Installation

If you want to run the app locally:

1. **Clone the repository**
   ```bash
   git clone <repository-url>
   cd dfc-pd
   ```

2. **Install required R packages**
   ```r
   install.packages(c(
     "shiny",
     "bslib",
     "dplyr",
     "tidyr",
     "stringr",
     "plotly",
     "DT",
     "wordcloud2",
     "scales",
     "visNetwork",
     "igraph"
   ))
   ```

3. **Run the app**
   ```r
   shiny::runApp()
   ```

## 📁 Repository Structure

```
dfc-pd/
├── app.R                      # Main application file
├── ui.r                       # User interface definition
├── server.r                   # Server logic
├── helper_functions.r         # Helper functions for data processing
├── Data_script.csv           # Main dataset
├── Information.csv           # Column information and descriptions
├── similarity_matrices.RData # Pre-computed similarity matrices for network
└── README.md                 # This file
```

## 💡 Usage Tips

- **Filtering**: Use the sidebar filters to narrow down studies. Filters are hierarchical (Study Design → Primary Focus → Focus Specification)
- **Tables**: Click column headers to sort; use search boxes at the top of each column for fine-grained filtering
- **Plots**: Hover over plots for detailed information; most plots are interactive
- **Network**: In the Network of Studies tab, adjust edge metrics and node properties to explore different aspects of study relationships
- **DOI Links**: Throughout the app, DOI values are clickable and open papers in a new tab

## 📊 Data Sources

The dataset includes peer-reviewed studies examining dynamic functional connectivity in Parkinson's Disease, with comprehensive coding of:
- Study design and participant characteristics
- MRI acquisition parameters
- Preprocessing pipelines
- dFC analysis methods
- Brain networks and findings
- Proposed biomarkers and limitations

## 🛠️ Technical Details

### Built With
- **R Shiny** - Web application framework
- **bslib** - Modern UI theming
- **plotly** - Interactive visualizations
- **DT** - Interactive data tables
- **visNetwork** - Network visualizations
- **igraph** - Network analysis

### System Requirements
- R version 4.0 or higher
- Modern web browser (Chrome, Firefox, Safari, Edge)
- Internet connection for online access


## 📚 Citation

If you use this tool or dataset in your research, please cite:

```
Will be updated soon.
```

