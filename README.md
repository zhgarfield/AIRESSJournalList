AIRESS Behavioral Science Journal Analysis
================


**Author:** Pr. Zachary H. Garfield  
**Institution:** UM6P – AIRESS  
**Repository:** <https://github.com/zhgarfield/AIRESSJournalList>

## Overview

This repository contains a fully reproducible workflow for constructing a transparent, data-driven classification of academic journals relevant to the interdisciplinary Behavioral Sciences at AIRESS (UM6P).

The project integrates bibliometric data from Scopus (2024), applies dimensionality reduction to derive a discipline-aware journal impact measure, and uses unsupervised clustering to generate a defensible tiering system within disciplinary subcategories. The resulting framework supports publication strategy, research evaluation, student mentoring, and institutional planning, while respecting differences in citation practices across fields.

Journals are classified into three tiers within each subcategory:

- **A: Excellent**  
- **B: Preferred**  
- **C: Acceptable**  

Because journals may belong to multiple disciplinary subcategories, the same journal may appear more than once in the final output, with different scores or tier assignments depending on context.

## Repository Structure

## Directory Structure

```text
AIRESSJournalList/
├── README.Rmd                  # Project documentation (source)
├── README.md                   # Rendered GitHub README
├── analysis.R                  # Main PCA + clustering + tiering workflow
├── processing.R                # Data import, cleaning, and helper functions
├── output/
│   ├── AIRESS_Journal_Tiers_by_Subcategory.csv   # Final tiered journal list
│   ├── figures/                # Generated figures for report and appendix
│   │   ├── impact_hist_by_subcategory.png
│   │   ├── pca_loadings_by_subcategory.png
│   │   ├── cluster_centers_by_subcategory.png
│   │   ├── cluster_ecdf_by_subcategory.png
│   │   └── density_by_subcategory.png
├── files/
│   └── behavioral_science/     # Raw Scopus subject-area Excel files
│       ├── source-resultsDecisionSciencesMiscellaneous.xlsx
│       ├── source-resultsGeneralDecisionSciences.xlsx
│       ├── source-resultsGeneralPsychology.xlsx
│       ├── ...
└── .gitignore                  # File ignore rules
```

## Data Sources

The analysis uses Scopus 2024 Source Lists drawn from subject areas most relevant to behavioral science research and training at AIRESS, including:

* Psychology (General, Applied, Cognitive, Developmental, Social)
* Anthropology and Cultural Studies
* Decision Sciences
* Demography
* Sociology and Political Science
* History and Philosophy of Science
* Health (Social Science)
* Multidisciplinary and related social science domains

Each Scopus source file includes:

* CiteScore
* SNIP (Source Normalized Impact per Paper)
* SJR (SCImago Journal Rank)
* Publisher information and metadata

Two epidemiological policy outlets (MMWR Surveillance Summaries and MMWR Recommendations and Reports) were removed because they fall outside the scope of behavioral science publishing and would disproportionately influence citation-based analyses.

## Methodology

### 1. Data Import and Preparation

All Scopus source files are read, tagged with subject-area identifiers extracted from filenames, and merged into a single dataset. Journals indexed under multiple Scopus categories are consolidated while preserving all subject-area memberships via binary indicator columns. Core bibliometric variables are coerced to numeric format, and journals with missing values are excluded.

### 2. Subcategory Mapping

Scopus subject areas are grouped into a smaller set of analytically meaningful AIRESS subcategories (e.g., Psychology, Anthropology, Decision Sciences). These subcategories reflect how behavioral science is organized in practice at AIRESS and serve as the unit of evaluation for all subsequent analyses.

### 3. Principal Component Analysis (PCA)

Within each subcategory, PCA is applied to three correlated citation metrics (CiteScore, SNIP, SJR). The first principal component (PC1) captures the dominant shared signal among these measures and serves as a latent, discipline-specific impact dimension. PC1 is oriented so that higher values correspond to higher citation performance.

### 4. Filtering and Impact Index Construction

Within each subcategory, journals in the lowest quartile of PC1 scores are removed to avoid distortions from extremely low-impact outlets. Remaining PC1 scores are then rescaled to a 0–100 Impact Index, where values are interpretable only relative to journals in the same subcategory.

### 5. K-means Clustering and Tier Assignment

K-means clustering (k = 3) is applied within each subcategory to the Impact Index to identify natural groupings of journals. Clusters are ordered post hoc by their mean Impact Index and mapped to qualitative tiers:

* A: Excellent (highest-impact cluster)
* B: Preferred (intermediate-impact cluster)
* C: Acceptable (lowest-impact cluster among retained journals)

Cluster labels are arbitrary; tier assignments always reflect relative impact within subcategories.

### 6. Diagnostics and Visualization

The workflow generates diagnostic plots to assess tier separation and robustness, including:

PC1 loading plots by subcategory
Impact Index histograms by subcategory
Cluster center plots
ECDF and density plots by cluster and subcategory

These figures are used to evaluate how sharply tiers separate and to document field-specific differences in citation structure.

## Outputs

The primary output is:

`AIRESS_Journal_Tiers_by_Subcategory.csv`

A long-format table containing journal titles, subcategory membership, PCA scores, Impact Index values, percentile ranks within subcategories, and tier assignments.

An interactive, sortable version of this table is also embedded directly in the HTML report.

## Reproducing the Analysis

To run the full analysis:

``` r
source("processing.R")
```

To regenerate the full report:

``` r
rmarkdown::render("AIRESS_Journal_Analysis.Rmd")
```
All results are fully reproducible given the included Scopus source files.
