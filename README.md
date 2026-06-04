# Demographic Representation in Brain-Computer Interface Studies for Motor Recovery and Pain-Related Outcomes
### 🧠 ThinkNeuro Bibliometric Research Internship (Spring 2026)

🎨 **Project Status:** In progress for more publications/Presented at The National Premier Undergraduate Research Conference at Columbia University.  

---

## 📌 Project Overview
This repository contains the complete replication data, analysis pipelines, and publication-ready visualizations for our bibliometric study investigating participant demographic representation within Brain-Computer Interface (BCI) literature. 

This research maps out historical trends in clinical sample populations (spanning age, race, and sex/gender metrics) and analyzes how they align with global burdens of neurological and motor impairments. Additionally, it evaluates the structural landscape of the field via keyword co-occurrence and co-authorship networks.

---

## 📂 Repository Structure

The project is structured modularly by metric. Each subdirectory within `Data Figures/` contains the raw/processed extraction data (`.csv`), the respective production script (`.R`), and the final high-resolution visual output (`.png`).

```text
.
├── Data Figures/
│   ├── Age/
│   │   ├── figure6b_age_bins.csv             # Age distribution binning data
│   │   ├── figure6b_age_stacked_bar.png      # Stacked bar plot of age demographics
│   │   └── figure6b_age_stacked_bar.R        # R script for age visualizations
│   ├── Co-authorship/
│   │   ├── Dataextraction.csv                # Collaboration and institutional data
│   │   ├── Co-authorship Network.R           # Network analysis processing script
│   │   └── Co-authorship_Network_clusters.png# Final collaboration network graph
│   ├── Heatmap/
│   │   ├── Heatmap_Updated.csv               # Cleaned matrix for clinical mapping
│   │   ├── Heatmap.R                         # Heatmap matrix processing script
│   │   └── BCI_Heatmap_Final_V2.png          # Production heatmap graphic
│   ├── Keyword Co-occurrence/
│   │   ├── 100articles.csv                   # Filtered core article dataset
│   │   ├── Clean_Keyword_Matrix.csv          # Keyword parsing matrix
│   │   ├── CSV.R / Keyword Co-occurrence.R   # Tokenization and network scripts
│   │   └── Keyword_Network_Improved.png      # Conceptual theme map graphic
│   ├── Race/
│   │   ├── figure6c_race_data.csv            # Diversity extraction metrics
│   │   ├── figure6c_race_reporting.png       # Reporting compliance trend graphic
│   │   └── figure6c_race_stacked_bar.R       # R script for race demographics
│   └── Sex:Gender/
│       ├── Figure6DataExtraction.csv         # Gender-focused sample extractions
│       ├── figure6_sex_barchart.R            # Cross-sectional plot script
│       ├── figure6_sex_demographics_yearly.R # Longitudinal trend script
│       ├── figure6_sex_demographics.png      # Consolidated sex distribution plot
│       └── Figure6_Sex_StackedBarChart.png   # Longitudinal stacked bar chart
└── README.md
