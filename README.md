AIRESS Research Journal Analysis and List
================


**Author:** Pr. Zachary H. Garfield  
**Institution:** UM6P – AIRESS  
**Repository:** <https://github.com/zhgarfield/AIRESSJournalList>

## Overview

This repository contains a fully reproducible workflow for constructing a transparent, data-driven classification of academic journals relevant to all research domains represented at AIRESS (UM6P), including behavioral science, political science, economics, law, history, geography and development, and global affairs.

The project integrates bibliometric data from Scopus (2024) and uses SJR (SCImago Journal Rank) as a prestige-weighted measure of journal impact, computed and rescaled within disciplinary subcategories. Unsupervised clustering is then applied to generate a defensible tiering system within each subcategory. The resulting framework is intended to support publication strategy, research evaluation, student mentoring, and institutional planning, while respecting substantial differences in citation practices, scale, and publication norms across fields.

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
├── processing.R                # Main data import, cleaning, impact index, clustering, and tiering workflow
├── AIRESS_Journal_Analysis.Rmd # R Markdown report
├── output/
│   ├── AIRESS_Journal_Tiers_by_Subcategory.csv   # Final tiered journal list
│   └── figures/                # Generated figures for report and appendix
│       ├── impact_hist_by_subcategory.png
│       ├── cluster_centers_by_subcategory.png
│       ├── cluster_ecdf_by_subcategory.png
│       └── density_by_subcategory.png
├── files/
│   ├── scopus_sources/         # Raw Scopus subject-area Excel files
│   │   ├── source-resultsDecisionSciencesMiscellaneous.xlsx
│   │   ├── source-resultsGeneralDecisionSciences.xlsx
│   │   ├── source-resultsPoliticalScienceandInternationalRelations.xlsx
│   │   ├── source-resultsEconomicsandEconometrics.xlsx
│   │   └── ...
│   └── revisions/              # Faculty-submitted tier revision files
│       ├── AIRESS_Journal_Tiers_by_Subcategory_2026-02-09_AMIR.xlsx
│       └── Econ_list_0_MEHDI.xlsx
└── .gitignore
```

## Data Sources

The analysis uses Scopus 2024 Source Lists drawn from subject areas relevant to the full scope of AIRESS research, including:

* Anthropology
* Applied Social Sciences
* Computational & Methods
* Decision Sciences
* Demography
* Economics
* General Social Science
* Geography, Planning & Development
* History
* Law
* Multidisciplinary
* Philosophy of Science
* Political Science and IR
* Psychology
* Sociology
* Other

Each Scopus source file includes CiteScore, SNIP (Source Normalized Impact per Paper), SJR (SCImago Journal Rank), publisher information, and metadata. Only the SJR metric is used in the impact index construction; CiteScore and SNIP are retained in the dataset for reference.

Two epidemiological policy outlets (**MMWR Surveillance Summaries** and **MMWR Recommendations and Reports**) were removed at the outset, as they function primarily as surveillance reports rather than scholarly journals and would disproportionately distort citation-based analyses.

## Methodology

### 1. Data Import and Preparation

All Scopus source files are read, tagged with subject-area identifiers extracted from filenames, and merged into a single dataset. Journals indexed under multiple Scopus categories are consolidated while preserving all subject-area memberships via binary indicator columns. SJR is coerced to numeric format and journals with missing SJR values are excluded.

### 2. Subcategory Mapping

Scopus subject areas are grouped into a set of AIRESS subcategories designed to reflect the research domains represented across AIRESS rather than mirroring Scopus classifications one-to-one. These subcategories form the unit of evaluation for all subsequent analyses and allow journals to be assessed relative to appropriate disciplinary peers.

### 3. Impact Index Construction

Within each subcategory, journals in the bottom 25% of the SJR distribution are removed before constructing the Impact Index. Four considerations motivate this step. First, the source data are already restricted to Q1–Q2 journals, so the bottom quartile of any subcategory consists of the weakest outlets within an already-filtered pool — venues unlikely to be primary publication targets for AIRESS faculty or students. Second, SJR distributions remain heavily right-skewed even within Q1–Q2 lists; retaining the lower tail compresses the 0–100 rescaling into a range where meaningful differences in journal quality are difficult to distinguish. Third, k-means is sensitive to extreme low-end values, which tend to form an isolated cluster and effectively waste one tier on marginal outlets. Fourth, retaining these journals pulls cluster centers downward and weakens tier separation in the upper portion of the distribution. Remaining SJR scores are rescaled to a 0–100 Impact Index, interpretable only within the subcategory. SJR was chosen as the sole metric because it is a prestige-weighted measure that values citations by the influence of the citing journal, making it well-suited for within-discipline comparisons where absolute citation volumes differ substantially across fields.

### 4. Clustering and Tier Assignment

K-means clustering (k = 4) is applied within each subcategory to identify natural groupings in the Impact Index. Clusters are ordered post hoc by mean impact and mapped to the four qualitative tiers: *Elite*, *Excellent*, *Preferred*, and *Acceptable*. Tier labels always reflect relative standing within subcategories, not absolute impact across fields.

### 5. Diagnostics and Visualization

The workflow produces a full set of diagnostic figures, including:

* Impact Index histograms by subcategory
* Cluster center plots
* ECDF and density plots by cluster and subcategory

These visualizations document the structure of SJR distributions and allow inspection of how sharply tiers separate within each field.

## Outputs

The primary output is:

`AIRESS_Journal_Tiers_by_Subcategory.csv`

This long-format table contains journal titles, subcategory membership, SJR values, Impact Index scores, percentile ranks within subcategories, and tier assignments. Journals may appear multiple times if they belong to multiple subcategories.

An interactive, sortable table is also embedded directly in the HTML report to facilitate exploration and filtering by faculty and administrators.

### Off-list (hors catégorie) Journals

Faculty may propose journals that do not appear in the Scopus-based lists or that do not meet the quantitative criteria used in this framework. Such journals may be designated as off-list (**hors catégorie**) to acknowledge their relevance for particular research areas, languages, regional scholarship, or intellectual traditions not well captured by citation metrics.

Off-list journals are recognized as legitimate publication venues but are not evaluated or tiered using the SJR-based clustering system. Their inclusion does not imply equivalence with journals appearing in the Elite–Acceptable tiers and should be interpreted separately in evaluation and reporting contexts.

## Manual Tier Overrides (Faculty Review)

In a small number of cases, faculty identified journals whose scholarly importance is not adequately captured by citation-based metrics alone. These journals were manually reassigned to a different tier within a specific AIRESS subcategory, based on disciplinary judgment. Such overrides are documented transparently below.

Overrides are compiled from three sources: hardcoded entries from named reviewers (below), and revision files submitted by Amir and Mehdi. All overrides apply only within the specified subcategory and do not affect a journal's tier in other subcategories.

| **AIRESS Subcategory** | **Journal**                                   | **Revised Tier** | **Reviewer / Rationale**                                                             |
| ---------------------- | --------------------------------------------- | ---------------- | ------------------------------------------------------------------------------------ |
| Philosophy of Science  | *Philosophy of Science*                       | B: Excellent     | Core flagship journal in philosophy of science (Mathieu Charbonneau)                 |
| Philosophy of Science  | *History and Philosophy of the Life Sciences* | C: Preferred     | High disciplinary relevance; under-cited relative to influence (Mathieu Charbonneau) |
| Philosophy of Science  | *HOPOS*                                       | C: Preferred     | Central venue for history & philosophy of science (Mathieu Charbonneau)              |
| Philosophy of Science  | *Journal of the History of Biology*           | C: Preferred     | Foundational journal in history of biology (Mathieu Charbonneau)                     |
| Psychology             | *Evolution and Human Behavior*                | C: Preferred     | Flagship journal in evolutionary behavioral science (Zachary Garfield)               |
| Multidisciplinary      | *Nature Communications*                       | B: Excellent     | Highly competitive interdisciplinary journal crossing many fields (Zachary Garfield)               |
| Political Science & IR | *(multiple entries)*                          | *(see file)*     | Amir Abul Red — see `files/revisions/AIRESS_Journal_Tiers_by_Subcategory_2026-02-09_AMIR.xlsx` |
| Economics              | *(multiple entries)*                          | *(see file)*     | Mehdi Bartal — see `files/revisions/Econ_list_0_MEHDI.xlsx`                                |
| Decision Sciences      | *Management Science*                          | B: Excellent     | Mehdi Bartal                                                                                |

## Notes

* Overrides apply only within the specified subcategory and do not affect the journal's tier in other subcategories.
* All overrides are explicitly documented to preserve transparency and reproducibility.
* Ambiguous proposed rankings (e.g. "B voir A, ask around") are skipped automatically and printed to the console during processing for manual follow-up.
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
