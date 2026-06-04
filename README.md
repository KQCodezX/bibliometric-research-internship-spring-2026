# Demographic Representation in Brain-Computer Interface Studies for Motor Recovery and Pain-Related Outcomes
### 🧠 ThinkNeuro Bibliometric Research Internship (Spring 2026)

🎨 **Project Status:** Complete / Publication Pipeline  
💻 **Technologies:** `R`, `ggplot2`, `tidyverse`, `bibliometrix`

---

## 📌 Project Overview
This repository contains the data, analysis scripts, and data visualizations for our bibliometric study investigating global demographic representation within Brain-Computer Interface (BCI) literature. Specifically, this research evaluates how trends in participant demographics intersect across various neurological conditions and motor impairment populations, targeting gaps in clinical trial diversity for motor recovery and pain management.

### 🔬 Key Research Questions
* How has demographic diversity (gender, geographic region, ethnicity) evolved in BCI clinical trials over time?
* To what extent do clinical sample populations match the global epidemiology of the targeted neurological/motor impairments?
* What are the dominant keyword networks and thematic clusters driving current BCI research for pain and motor rehabilitation?

---

## 📂 Repository Structure

```text
├── data/
│   ├── raw_bibliometric_data.csv   # Scopus/Web of Science export data
│   └── processed_demographics.csv  # Cleaned demographic metrics for analysis
├── scripts/
│   ├── 01_data_cleaning.R          # Data parsing and filtering script
│   ├── 02_network_analysis.R       # Keyword co-occurrence and thematic mapping
│   └── 03_visualization_plots.R    # Code generating the publication figures
├── figures/
│   ├── keyword_cooccurrence.png    # Network visualization of BCI research trends
│   ├── demographic_heatmap.png     # Heatmap mapping representation vs. global burden
│   └── population_distribution.png # Stacked bar charts of study cohorts
└── README.md
