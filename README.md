AIRESS Behavioral Science Journal Analysis
================


**Author:** Zachary H. Garfield  
**Institution:** UM6P – AIRESS  
**Repository:** <https://github.com/zhgarfield/AIRESSJournalList>

## Overview

This repository contains a fully reproducible workflow for constructing
a transparent, data-driven classification of academic journals relevant
to the interdisciplinary Behavioral Sciences at AIRESS (UM6P).

The project integrates bibliometric data from Scopus (2024), applies
dimensionality-reduction to extract a unified “impact” measure, and uses
clustering to produce a defensible tiering system:

- **A: Excellent**  
- **B: Preferred**  
- **C: Acceptable**  

The resulting resource supports publication decisions, research
evaluation, student mentoring, and institutional planning.

## Repository Structure

## Directory Structure

```text
AIRESSJournalList/
├── README.Rmd                # Project documentation (source)
├── README.md                 # Rendered GitHub README
├── analysis.R                # Main PCA + clustering + tiering script
├── processing.R              # Helper functions for data import & cleaning
├── output/
│   ├── df_final.csv          # Final tiered journal list (output)
│   ├── figures/              # All generated plots
│   │   ├── impact_hist.png
│   │   ├── pca_loadings.png
│   │   ├── tier_distribution.png
│   │   └── cluster_ecdf.png
├── files/
│   └── behavioral_science/   # Raw Scopus subject-area Excel files
│       ├── source-resultsDecisionSciencesMiscellaneous.xlsx
│       ├── source-resultsGeneralDecisionSciences.xlsx
│       ├── ...
└── .gitignore                # File ignore rules
```

## Data Sources

We use Scopus 2024 source listings from subject areas most relevant to
behavioral science, including:

- Anthropology  
- Psychology (General, Applied, Cognitive, Developmental, Social)  
- Decision Sciences  
- Cultural Studies  
- Sociology and Political Science  
- Demography  
- Health (Social Science)  
- Multidisciplinary

Each Scopus file includes:

- **CiteScore**  
- **SNIP (Source Normalized Impact per Paper)**  
- **SJR (SCImago Journal Rank)**  
- **Publisher and metadata**

Two epidemiological policy outlets (MMWR Surveillance Summaries and MMWR
Recommendations and Reports) were removed, since they fall far outside
the domain of behavioral science research.

## Methodology

### 1. Data Import and Cleaning

All Scopus files are read, tagged with a subject-area, merged, and
deduplicated. Core citation metrics are converted to numeric values, and
journals with missing values are dropped.

### 2. Principal Component Analysis (PCA)

PCA is applied to the three standardized metrics:

- CiteScore  
- SNIP  
- SJR

PC1 captures ~88% of total variance and represents a unified **impact
dimension**.  
The axis is oriented so that higher PC1 corresponds to higher impact.

### 3. Construction of the Impact Index

PC1 is rescaled to an intuitive **0–100 Impact Index**, where:

- **0 = extremely low-impact**
- **100 = highest-impact journal in the dataset**

The lowest quartile of journals (very low impact) is removed prior to
tiering.

### 4. K-means Clustering and Tier Assignment

A k-means model with **k = 3** identifies natural journal groupings
within the Impact Index.

Clusters are ordered by mean impact and assigned:

1.  **A: Excellent**  
2.  **B: Preferred**  
3.  **C: Acceptable**

Journals removed earlier are assigned to **Avoid**.

## Reproducing the Analysis

To run the full analysis:

``` r
source("processing.R")
```

To regenerate the full report:

``` r
rmarkdown::render("AIRESS_Journal_Analysis.Rmd")
```
