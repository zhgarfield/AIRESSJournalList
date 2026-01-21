AIRESS Behavioral Science Journal Analysis
================


**Author:** Pr. Zachary H. Garfield  
**Institution:** UM6P – AIRESS  
**Repository:** <https://github.com/zhgarfield/AIRESSJournalList>

## Overview

This repository contains a fully reproducible workflow for constructing a transparent, data-driven classification of academic journals relevant to all research domains represented at AIRESS (UM6P), including behavioral science, political science, economics, law, history, geography and development, and global affairs.

The project integrates bibliometric data from Scopus (2024), applies dimensionality reduction to derive discipline-aware measures of journal impact, and uses unsupervised clustering to generate a defensible tiering system within disciplinary subcategories. The resulting framework is intended to support publication strategy, research evaluation, student mentoring, and institutional planning, while respecting substantial differences in citation practices, scale, and publication norms across fields.

Rather than imposing a single global ranking, journals are evaluated relative to appropriate disciplinary peers. Because journals may belong to multiple subcategories, the same journal may appear more than once in the final output, with different impact scores or tier assignments depending on context.

## Journal Tiers

Within each AIRESS subcategory, journals are classified into four data-driven tiers:

* *Elite* – exceptionally influential journals forming the upper tail of the impact distribution
* *Excellent* – top-tier journals with consistently high impact and visibility
* *Preferred* – solid, reputable journals appropriate for strong publication outcomes
* *Acceptable* – legitimate journals with modest impact, suitable for specialized or early-stage work

In addition, the framework explicitly allows for:

*Off-list* / *hors catégorie* journals: faculty-nominated outlets that are relevant for particular research areas, languages, regions, or scholarly traditions but are not evaluated using the quantitative tiering system.

Off-list journals are acknowledged as legitimate venues but are not directly comparable to journals appearing in the Elite–Acceptable tiers.

## Repository Structure

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
│   └── scopus_sources/         # Raw Scopus subject-area Excel files
│       ├── source-resultsDecisionSciencesMiscellaneous.xlsx
│       ├── source-resultsGeneralDecisionSciences.xlsx
│       ├── source-resultsPoliticalScienceandInternationalRelations.xlsx
│       ├── source-resultsEconomicsandEconometrics.xlsx
│       ├── ...
└── .gitignore
```

## Data Sources

The analysis uses Scopus 2024 Source Lists drawn from subject areas relevant to the full scope of AIRESS research, including:

* Anthropology
* Applied Social Sciences	
* Computational & Methods	
* Decision SciencesSciences
* Demography
* Economics
* Geography, Planning & Development
* History	
* Law
* Multidisciplinary	
* Philosophy of Science
* Political Science and IR	
* Psychology	
* Sociology
* Other	

Each Scopus source file includes:

* CiteScore
* SNIP (Source Normalized Impact per Paper)
* SJR (SCImago Journal Rank)
* Publisher information and metadata

Two epidemiological policy outlets (**MMWR Surveillance Summaries** and **MMWR Recommendations and Reports**) were removed at the outset, as they function primarily as surveillance reports rather than scholarly journals and would disproportionately distort citation-based analyses.

## Methodology

### 1. Data Import and Preparation

All Scopus source files are read, tagged with subject-area identifiers extracted from filenames, and merged into a single dataset. Journals indexed under multiple Scopus categories are consolidated while preserving all subject-area memberships via binary indicator columns. Core bibliometric variables are coerced to numeric format, and journals with missing values are excluded.

### 2. Subcategory Mapping

Scopus subject areas are grouped into a set of AIRESS subcategories designed to reflect the research domains represented across AIRESS rather than mirroring Scopus classifications one-to-one. These subcategories form the unit of evaluation for all subsequent analyses and allow journals to be assessed relative to appropriate disciplinary peers.

### 3. Principal Component Analysis (PCA)

Within each subcategory, PCA is applied to CiteScore, SNIP, and SJR. The first principal component (PC1) captures the dominant shared signal among these metrics and serves as a latent, discipline-specific measure of journal impact. PC1 is oriented so that higher values correspond to higher citation performance.

### 4. Filtering and Impact Index Construction

Within each subcategory, journals in the lowest quartile of PC1 scores are removed to avoid distortions from extremely low-impact outlets. Remaining PC1 scores are rescaled to a 0–100 Impact Index, interpretable only within the subcategory.

### 5. Clustering and Tier Assignment

Unsupervised clustering is applied within each subcategory to identify natural groupings in the Impact Index. Clusters are ordered post hoc by mean impact and mapped to the four qualitative tiers: *Elite*, *Excellent*, *Preferred*, and *Acceptable*. Tier labels always reflect relative standing within subcategories, not absolute impact across fields.

### 6. Diagnostics and Visualization

The workflow produces a full set of diagnostic figures, including:

* PC1 loading plots by subcategory
* Impact Index histograms by subcategory
* Cluster center plots
* ECDF and density plots by cluster and subcategory

These visualizations document the structure of citation distributions and allow inspection of how sharply tiers separate within each field.

## Outputs

The primary output is:

`AIRESS_Journal_Tiers_by_Subcategory.csv`

This long-format table contains journal titles, subcategory membership, PCA scores, Impact Index values, percentile ranks within subcategories, and tier assignments. Journals may appear multiple times if they belong to multiple subcategories.

An interactive, sortable table is also embedded directly in the HTML report to facilitate exploration and filtering by faculty and administrators.

### Off-list (hors catégorie) Journals

Faculty may propose journals that do not appear in the Scopus-based lists or that do not meet the quantitative criteria used in this framework. Such journals may be designated as off-list (**hors catégorie**) to acknowledge their relevance for particular research areas, languages, regional scholarship, or intellectual traditions not well captured by citation metrics.

Off-list journals are recognized as legitimate publication venues but are not evaluated or tiered using the PCA- and clustering-based system. Their inclusion does not imply equivalence with journals appearing in the Elite–Acceptable tiers and should be interpreted separately in evaluation and reporting contexts.

## Manual Tier Overrides (Faculty Review)

In a small number of cases, faculty identified journals whose scholarly importance is not adequately captured by citation-based metrics alone. These journals were manually reassigned to a different tier within a specific AIRESS subcategory only, based on disciplinary judgment. Such overrides are documented transparently below.

| **AIRESS Subcategory** | **Journal**                                   | **Original Tier** | **Revised Tier** | **Faculty Rationale / Reviewer**                                                     |
| ---------------------- | --------------------------------------------- | ----------------- | ---------------- | ------------------------------------------------------------------------------------ |
| Philosophy of Science  | *Philosophy of Science*                       | C: Acceptable     | A: Excellent     | Core flagship journal in philosophy of science (Mathieu Charbonneau)                 |
| Philosophy of Science  | *History and Philosophy of the Life Sciences* | C: Acceptable     | B: Preferred     | High disciplinary relevance; under-cited relative to influence (Mathieu Charbonneau) |
| Philosophy of Science  | *HOPOS*                                       | C: Acceptable     | B: Preferred     | Central venue for history & philosophy of science (Mathieu Charbonneau)              |
| Philosophy of Science  | *Journal of the History of Biology*           | C: Acceptable     | B: Preferred     | Foundational journal in history of biology (Mathieu Charbonneau)                     |
| Psychology             | *Evolution and Human Behavior*                | C: Acceptable     | B: Preferred     | Flagship journal in evolutionary behavioral science (Zachary Garfield)               |

## Notes

* Overrides apply only within the specified subcategory and do not affect the journal’s tier in other subcategories.
* All overrides are explicitly documented to preserve transparency and reproducibility.
* Additional manual adjustments may be proposed by AIRESS faculty and reviewed periodically.

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

#
