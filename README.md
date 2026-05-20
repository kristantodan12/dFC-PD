# DynaPD Interactive App

An interactive web-based dashboard for exploring dynamic functional connectivity (dFC) studies in Parkinson's Disease through a comprehensive systematic review.

## 🌐 Live Application

**Access the interactive web application here:** [https://individualbrainproject.shinyapps.io/dfc-pd/](https://individualbrainproject.shinyapps.io/dfc-pd/)

## 📋 Overview

DynaPD Interactive App provides a comprehensive, user-friendly interface for exploring and analyzing research on dynamic functional connectivity in Parkinson's Disease. Built upon a systematic review of the literature, this dashboard enables researchers, clinicians, and students to:

- **Navigate** through an interactive home page with clickable nodes representing different app features
- **Browse** a curated dataset of dFC studies in PD with PRISMA flow diagram
- **Filter** studies by multiple criteria (year, sample size, TR, study design, focus areas)
- **Visualize** methodological trends and key findings across the field
- **Explore** brain networks and connectivity patterns through word clouds and frequency tables
- **Discover** proposed biomarkers, finding variability, and study limitations
- **Interact** with a network visualization showing relationships between studies
- **Contribute** your own study data to future updates (online version only)

## 🎯 Key Features

### 0. Interactive Home Page
- **Network visualization** of all app sections with clickable nodes
- **Expandable sub-nodes** for tabs with multiple subtabs (Full Dataset, Method Explorer, Finding Explorer)
- **Dynamic descriptions** that update based on selected node
- **Direct navigation** to any section by clicking the corresponding node

### 1. Full Dataset
- **Dataset Tab**: PRISMA flow diagram showing systematic review process
- **Data Explorer**: Interactive table with advanced filtering
  - Range sliders for numeric variables (Year, Sample Size, TR, Scan Length, etc.)
  - Dropdown menus for categorical variables (Study Design, Paradigm Type, etc.)
  - Fixed headers and horizontal scrolling for large datasets
  - Clickable DOI links
- **Coded Information**: Comprehensive data dictionary describing all variables

### 2. Study Overview
- Summary statistics (total studies, PD and HC participants)
- Publication trends over time
- Top 10 journals
- Sample size and age distributions
- Data source institutions (top 10)
- Medical status during scanning
- Paradigm type distribution
- Sex distribution (% male in PD samples)
- Hierarchical filtering (Study Design → Primary Focus → Focus Specification)

### 3. Method Explorer
- **MRI Acquisition & Preprocessing**: 
  - Motion parameter strategies
  - TR and TE distributions with filtering capability
  - Scan length analysis
  - Cleaning steps frequency (WM/CSF regression, CompCor, ICA-AROMA, detrending, despiking, scrubbing)
  - Exclusion criteria distributions
- **dFC Analysis**: 
  - Brain mapping methods
  - Number of networks and parcellation schemes
  - dFC methods (sliding window, time-varying connectivity, etc.)
  - Window size and shift distributions
  - Clustering methods
  - Number of states distribution
  - TR filtering for precise method exploration
- **Clinical Focus & Brain Mapping**: 
  - Interactive word cloud of brain regions/networks
  - Frequency table of network areas
  - Filterable by focus specification

### 4. Finding Explorer
- **State Features**: 
  - Number of states distribution across studies
  - State features frequency analysis
  - Graph measures frequency
  - Detailed table with all state-related variables
- **State Interpretation**: 
  - Finding variability exploration (State Pattern Conclusions vs. State Transition Patterns)
  - Interactive bar charts with clickable bars
  - Detailed paper listings for each finding category
- **Proposed Biomarkers**: 
  - Consolidated table of biomarker proposals
  - Network areas associated with biomarkers
  - State and transition conclusions
- **Limitations**: 
  - Top 10 most common limitations across the field
  - Per-paper limitations table with full details

### 5. Network of Studies
- Interactive network visualization showing study relationships
- **Customizable node coloring** by: Primary Focus, Data Source, Brain Mapping, dFC Method, Number of States
- **Flexible edge connections** based on similarity metrics: Primary Focus, Data Source, Brain Mapping, Network Areas, dFC Method, Number of States, State Features
- **Adjustable edge weight threshold** for Network Areas and State Features
- **Node sizing options**: Sample Size (PD) or Fixed Size
- Hover for study details and clickable DOI links
- Fixed layout for stable visualization across sessions

### 6. Contribute
- **Data submission portal** for contributing new studies to the database
- Download CSV template with all required columns
- File upload functionality with validation
- Manual verification process before publication
- Contact information and citation details

**Note:** The Contribute feature is only available in the [live online version](https://individualbrainproject.shinyapps.io/dfc-pd/) due to server-side authentication requirements. Local installations will not support file uploads.

## 🚀 Getting Started

### Online Access (Recommended)
Simply visit: [https://individualbrainproject.shinyapps.io/dfc-pd/](https://individualbrainproject.shinyapps.io/dfc-pd/)

**No installation required!** Full functionality including data contribution is available online.

### Local Installation

If you want to run the app locally for development or offline use:

**⚠️ Important Note:** The local version will NOT support the **Contribute** feature (file upload functionality) as it requires server-side authentication credentials. To contribute data, please use the [live online version](https://individualbrainproject.shinyapps.io/dfc-pd/).

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
     "readr",
     "plotly",
     "DT",
     "wordcloud2",
     "scales",
     "visNetwork",
     "igraph",
     "shinyjs",
     "httr"
   ))
   ```

3. **Run the app**
   ```r
   shiny::runApp()
   ```
   
   Or open `app.R` in RStudio and click "Run App"

## 📁 Repository Structure

```
dfc-pd/
├── app.R                      # Main application entry point
├── ui.r                       # User interface definition with interactive home page
├── server.r                   # Server logic and reactive components
├── helper_functions.r         # Helper functions for data processing and visualization
├── Data_script.csv            # Main systematic review dataset
├── Information.csv            # Data dictionary with column descriptions
├── similarity_matrices.RData  # Pre-computed similarity matrices for network visualization
├── Figures_for_paper.r        # Static figure generation for manuscript
├── .gitignore                 # Git ignore rules
├── LICENSE.md                 # License information
├── README.md                  # This file
├── Data/                      # Additional data files
├── www/                       # Web assets (images, CSS, etc.)
    └── PRISMA_diagram.png    # PRISMA flow diagram

```

## 💡 Usage Tips

- **Home Page Navigation**: Click on any node in the interactive network to navigate to that section. Main tabs with subtabs will expand to show additional clickable options.
- **Hierarchical Filtering**: Filters are dependent - Study Design selection affects Primary Focus options, which in turn affects Focus Specification choices.
- **Advanced Table Filtering**: 
  - Use range sliders at the top of numeric columns (Year, Sample Size, TR, etc.)
  - Use dropdown menus for categorical columns (Study Design, Paradigm Type, etc.)
  - Click column headers to sort
  - Combine multiple filters for precise data exploration
- **Interactive Plots**: Hover over plots for detailed information; most visualizations are interactive with zoom and pan capabilities
- **Network Visualization**: 
  - Adjust edge weight thresholds when using Network Areas or State Features similarity to declutter the visualization
  - Choose different node coloring schemes to highlight different aspects of the research
  - Hover over nodes to see study details and click DOI links
- **DOI Links**: Throughout the app, DOI values are clickable and open papers in a new tab for easy access
- **TR Filtering**: Use the TR (Repetition Time) slider in MRI Acquisition & Preprocessing and dFC Analysis tabs to focus on studies using specific scanner parameters
- **Finding Variability**: In the State Interpretation tab, click bars in the chart to filter the detailed table below to papers with that specific finding

## 📊 Data Sources

The dataset is derived from a comprehensive systematic review following PRISMA guidelines and includes peer-reviewed studies examining dynamic functional connectivity in Parkinson's Disease. Each study has been meticulously coded for:

**Study Characteristics:**
- Study design, publication year, journal
- Sample demographics (PD and HC participants, age, sex distribution)
- Data source institutions
- Primary clinical focus and specifications

**MRI Acquisition:**
- Scanner parameters (TR, TE, field strength)
- Scan length and paradigm type
- Medical status during scanning

**Preprocessing Pipeline:**
- Motion parameter strategies
- Cleaning steps (WM/CSF regression, CompCor, ICA-AROMA, detrending, despiking, scrubbing)
- Exclusion criteria and thresholds

**dFC Analysis Methods:**
- Brain mapping approaches and parcellation schemes
- Number of networks/ROIs
- dFC computation methods
- Window size and shift parameters
- Clustering algorithms
- Number of states identified

**Findings:**
- Brain networks and regions implicated
- State features and graph measures
- State pattern conclusions and transition patterns
- Proposed biomarkers
- Study limitations

All data is presented with direct links to original publications via DOI for verification and deeper exploration.

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

If you use this tool or dataset in your research, please cite our preprint:

```
Kristanto, D., et al. (2025). Dynamic Functional Connectivity in Parkinson's Disease: 
A Systematic Review. bioRxiv. https://doi.org/10.64898/2025.12.08.692999
```

**BibTeX:**
```bibtex
@article{kristanto2025dynamic,
  title={Dynamic Functional Connectivity in Parkinson's Disease: A Systematic Review},
  author={Kristanto, Daniel and [other authors]},
  journal={bioRxiv},
  year={2025},
  doi={10.64898/2025.12.08.692999},
  url={https://www.biorxiv.org/content/10.64898/2025.12.08.692999v1}
}
```

**View the preprint:** [bioRxiv](https://www.biorxiv.org/content/10.64898/2025.12.08.692999v1)

## 📧 Contact

For questions, feedback, or collaboration inquiries:
- **Email:** daniel.kristanto@uol.de
- **Online App:** [https://individualbrainproject.shinyapps.io/dfc-pd/](https://individualbrainproject.shinyapps.io/dfc-pd/)

We welcome contributions and suggestions to improve the app and expand the dataset!

## 📄 License

See [LICENSE.md](LICENSE.md) for details.

---

**Last Updated:** May 2026

