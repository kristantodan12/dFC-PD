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

