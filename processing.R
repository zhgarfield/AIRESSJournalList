
# Introduction and background ---------------------------------------------
# Pr. Zachary Garfield, zachary.garfield@um6p.ma
# October 2025

# This script compiles and analyzes Scopus journal data across a 
# selected set of behavioral-science–related subject areas. The goal 
# is to create a transparent, data-driven system for evaluating the 
# overall research impact of journals relevant to AIRESS and the 
# behavioral science community.
#
# The workflow proceeds in several steps:
#
# (1) LOAD & MERGE
#     We import all Scopus journal lists from the specified directory. 
#     Each file corresponds to one Scopus subject area (e.g., 
#     Applied Psychology, Anthropology, Social Psychology), and each 
#     journal is tagged with the subject area extracted from the 
#     filename.
#
# (2) CLEAN & STANDARDIZE
#     We convert the core bibliometric variables (CiteScore, SNIP, SJR) 
#     to numeric form and remove journals missing any of these values. 
#     We also drop a small number of outlier journals that distort 
#     the analysis.
#
# (3) SUBJECT MEMBERSHIP MATRIX
#     Journals can appear in multiple Scopus subject areas. We create 
#     a wide-format matrix with 0/1 indicators showing which journals 
#     belong to which subject categories. This preserves the full 
#     disciplinary profile of each journal.
#
# (4) PCA (PRINCIPAL COMPONENT ANALYSIS)
#     CiteScore, SNIP, and SJR are correlated indicators of journal 
#     reputation and citation performance. PCA reduces these three 
#     measures to a single latent “impact” dimension (PC1). We orient 
#     PC1 so that higher bibliometric values consistently produce 
#     higher PC1 scores.
#
# (5) IMPACT INDEX
#     The standardized PC1 scores are rescaled to a 0–100 
#     “impact_index”, which allows journals to be compared on an 
#     intuitive, interpretable scale. Higher scores indicate higher 
#     impact.
#
# (6) CLUSTERING & TIERING
#     We apply k-means clustering to the impact_index to identify 
#     natural groupings of journals. These clusters are then labeled 
#     as:
#         - A: Excellent
#         - B: Preferred
#         - C: Acceptable
#     This provides a simple tiering system for evaluation, selection, 
#     and strategic planning.
#
# RESULT:
#     The final dataframe (df_wide or df_final) contains:
#         • Original bibliometric data
#         • Subject-area memberships
#         • PCA scores and impact_index
#         • A/B/C tier assignments
#
# This analysis provides a principled, reproducible, and transparent 
# basis for assessing journal quality within the behavioral sciences.


# Load libraries ----------------------------------------------------------
library(readxl)
library(tidyverse)
library(stringr)
library(purrr)
library(ggplot2)
library(scales)
library(hagenutils)
library(viridisLite)
library(forcats)

# Read files --------------------------------------------------------------

data_dir <- "./files/behavioral_science/"
file_list <- list.files(data_dir, pattern = "\\.xlsx$", full.names = TRUE)

read_and_tag <- function(filepath) {
  fname <- basename(filepath)
  subject_area <- str_extract(fname, "(?<=source-results)[A-Za-z0-9]+")
  
  read_excel(filepath, col_types = "text") %>%
    mutate(subject_area = subject_area)
}

data <- map_df(file_list, read_and_tag)


# Data processing ---------------------------------------------------------

# Remove outliers
data <- data %>%
  filter(!(`Source title` %in% c(
    "MMWR Surveillance Summaries",
    "MMWR Recommendations and Reports"
  ))) %>%
  distinct()


# Prepare PCA variables
pca_vars <- c("CiteScore", "SNIP", "SJR")

data <- data %>%
  mutate(across(all_of(pca_vars), ~ suppressWarnings(as.numeric(.)))) %>%
  filter(if_all(all_of(pca_vars), ~ !is.na(.)))

# Build subject-area membership matrix
df_flagged <- data %>% mutate(flag = 1)

subject_matrix <- df_flagged %>%
  select(`Source title`, subject_area, flag) %>%
  distinct() %>%
  pivot_wider(
    names_from  = subject_area,
    values_from = flag,
    values_fill = list(flag = 0)
  )

journal_metadata <- df_flagged %>%
  select(-subject_area, -flag) %>%
  distinct(`Source title`, .keep_all = TRUE)

df_wide <- journal_metadata %>%
  left_join(subject_matrix, by = "Source title") %>%
  distinct()


# Subcategory mapping ------------------------------------------------------

subcategory_map <- list(
  DecisionSciences = c("DecisionSciencesMiscellaneous", "GeneralDecisionSciences"),
  Psychology = c(
    "PsychologyMiscellaneous", "GeneralPsychology", "AppliedPsychology",
    "ExperimentalandCognitivePsychology", "SocialPsychology",
    "DevelopmentalandEducationalPsychology"
  ),
  Anthropology = c("Anthropology", "CulturalStudies"),
  Demography = c("Demography"),
  Sociology = c("SociologyandPoliticalScience"),
  PhilosophyOfScience = c("HistoryandPhilosophyofScience"),
  AppliedSocialSciences = c("HealthSocialScience"),
  Multidisciplinary = c("Multidisciplinary"),
  Other = c("SocialSciencesMiscellaneous", "GeneralSocialSciences", "GeneralAgricultureandBiologicalSciences", "GeneralMedicine")
)

# Create 0/1 membership columns for each subcategory (no changes to existing subject-area columns)
for (sc in names(subcategory_map)) {
  areas <- subcategory_map[[sc]]
  areas_present <- intersect(areas, names(df_wide))
  df_wide[[sc]] <- if (length(areas_present) == 0) 0L else as.integer(rowSums(df_wide[, areas_present, drop = FALSE], na.rm = TRUE) > 0)
}

# Subcategory workflow: PCA + Impact Index + K-means WITHIN each subcategory ----

# Map Scopus subject areas (from filenames) to AIRESS subcategories
subcategory_map <- list(
  DecisionSciences = c("DecisionSciencesMiscellaneous", "GeneralDecisionSciences"),
  Psychology = c(
    "PsychologyMiscellaneous", "GeneralPsychology", "AppliedPsychology",
    "ExperimentalandCognitivePsychology", "SocialPsychology",
    "DevelopmentalandEducationalPsychology"
  ),
  Anthropology = c("Anthropology", "CulturalStudies"),
  Demography = c("Demography"),
  Sociology = c("SociologyandPoliticalScience"),
  PhilosophyOfScience = c("HistoryandPhilosophyofScience"),
  AppliedSocialSciences = c("HealthSocialScience"),
  Multidisciplinary = c("Multidisciplinary"),
  Other = c("SocialSciencesMiscellaneous", "GeneralSocialSciences", "GeneralAgricultureandBiologicalSciences", "GeneralMedicine")
)

# Create 0/1 membership columns for each subcategory (based on subject-area columns already in df_wide)
for (sc in names(subcategory_map)) {
  areas <- subcategory_map[[sc]]
  areas_present <- intersect(areas, names(df_wide))
  df_wide[[sc]] <- if (length(areas_present) == 0) 0L else as.integer(rowSums(df_wide[, areas_present, drop = FALSE], na.rm = TRUE) > 0)
}

# Function to run PCA + cutoff + impact index + kmeans within one subcategory
run_subcat <- function(sc_name, df_all, pca_vars, cutoff_q = 0.25) {
  
  df_sc <- df_all %>%
    filter(.data[[sc_name]] == 1) %>%
    select(`Source title`, Publisher, all_of(pca_vars))
  
  # Keep only rows with complete PCA inputs
  df_sc <- df_sc %>% drop_na(all_of(pca_vars))
  
  # If too small to support PCA/kmeans robustly, return empty
  if (nrow(df_sc) < 10) {
    return(tibble())
  }
  
  # PCA
  pca_model <- prcomp(df_sc[, pca_vars], center = TRUE, scale. = TRUE)
  pc1_raw <- pca_model$x[, 1]
  
  # Orient PC1 so higher metrics -> higher PC1
  pc1_dir <- ifelse(cor(df_sc$CiteScore, pc1_raw) < 0, -1, 1)
  df_sc$PC1_score <- pc1_raw * pc1_dir
  
  # Apply within-subcategory cutoff (same logic as your global script)
  pc1_cutoff <- quantile(df_sc$PC1_score, cutoff_q)
  df_sc <- df_sc %>% filter(PC1_score >= pc1_cutoff)
  
  if (nrow(df_sc) < 10) {
    # still compute within-subcategory impact index, but skip kmeans if too small
    df_sc <- df_sc %>%
      mutate(
        subcategory = sc_name,
        impact_index = scales::rescale(PC1_score, to = c(0, 100)),
        cluster_raw = NA_integer_,
        tier = NA_character_
      )
    return(df_sc %>% select(`Source title`, Publisher, subcategory, all_of(pca_vars), PC1_score, impact_index, cluster_raw, tier))
  }
  
  # Impact index (0–100) WITHIN subcategory
  df_sc <- df_sc %>%
    mutate(
      subcategory = sc_name,
      impact_index = scales::rescale(PC1_score, to = c(0, 100))
    )
  
  # K-means within subcategory
  set.seed(123)
  km <- kmeans(df_sc$impact_index, centers = 3)
  df_sc$cluster_raw <- km$cluster
  
  # Label tiers within subcategory by cluster mean
  cluster_order <- df_sc %>%
    group_by(cluster_raw) %>%
    summarise(mean_index = mean(impact_index), .groups = "drop") %>%
    arrange(mean_index)
  
  rank_map <- cluster_order %>%
    mutate(tier = c("C: Acceptable", "B: Preferred", "A: Excellent"))
  
  df_sc <- df_sc %>%
    left_join(rank_map, by = "cluster_raw")
  
  df_sc %>%
    select(`Source title`, Publisher, subcategory, all_of(pca_vars), PC1_score, impact_index, cluster_raw, tier)
}

# Run across all subcategories and KEEP duplicates (journal can appear in multiple subcategories)
df_subcat <- purrr::map_dfr(
  names(subcategory_map),
  run_subcat,
  df_all = df_wide,
  pca_vars = pca_vars,
  cutoff_q = 0.25
)

# Optional: compute within-subcategory ranks/percentiles for reporting (keeps duplicates)
df_subcat <- df_subcat %>%
  group_by(subcategory) %>%
  arrange(desc(impact_index), .by_group = TRUE) %>%
  mutate(
    rank_in_subcategory = dense_rank(desc(impact_index)),
    n_in_subcategory = n(),
    percentile_in_subcategory = 100 * (1 - (rank_in_subcategory - 1) / pmax(n_in_subcategory - 1, 1))
  ) %>%
  ungroup()


# PC1 loadings by subcategory (all in one plot) ----------------------------

get_subcat_loadings <- function(sc_name, df_all, pca_vars) {
  
  df_sc <- df_all %>%
    filter(.data[[sc_name]] == 1) %>%
    select(all_of(pca_vars)) %>%
    drop_na(all_of(pca_vars))
  
  if (nrow(df_sc) < 10) return(tibble())
  
  pca_model <- prcomp(df_sc[, pca_vars], center = TRUE, scale. = TRUE)
  
  pc1_raw <- pca_model$x[, 1]
  pc1_dir <- ifelse(cor(df_sc$CiteScore, pc1_raw) < 0, -1, 1)
  
  tibble(
    subcategory = sc_name,
    metric = rownames(pca_model$rotation),
    loading = as.numeric(pca_model$rotation[, 1]) * pc1_dir
  )
}

loadings_all <- purrr::map_dfr(
  names(subcategory_map),
  get_subcat_loadings,
  df_all = df_wide,
  pca_vars = pca_vars
)

pc1_loadings_by_subcat_plot <- ggplot(loadings_all, aes(x = metric, y = loading)) +
  geom_col() +
  facet_wrap(~ subcategory, scales = "free_y") +
  coord_flip() +
  labs(
    title = "PC1 Loadings by Subcategory (PCA within subcategory)",
    x = NULL,
    y = "PC1 loading (oriented so higher metrics → higher PC1)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.y = element_text(size = 12)
  )

pc1_loadings_by_subcat_plot


# K-means descriptive plots by subcategory (all in one plot set) ------------

# Keep only rows where clustering ran (some small subcats may have NA cluster_raw)
df_km <- df_subcat %>%
  filter(!is.na(cluster_raw))

# 1) Cluster centers per subcategory
cluster_centers_by_subcat <- df_km %>%
  group_by(subcategory, cluster_raw) %>%
  summarise(center = mean(impact_index, na.rm = TRUE), .groups = "drop")

cluster_centers_plot_all <- ggplot(cluster_centers_by_subcat,
                                   aes(x = factor(cluster_raw), y = center, group = 1)) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 2.5) +
  facet_wrap(~ subcategory, scales = "free_y") +
  labs(
    title = "ECDF of Impact Index by K-means Cluster (within subcategory)",
    x = "Impact Index",
    y = "Cumulative Proportion",
    color = "Cluster",
    caption = "Note: K-means cluster labels are arbitrary. Clusters are ordered post hoc by\nmean impact within each subcategory, and tier labels (Excellent, Preferred,\nAcceptable) are assigned accordingly."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11)
  )

cluster_centers_plot_all


# 2) ECDF separation plot by subcategory
ecdf_plot_all <- ggplot(df_km, aes(x = impact_index, color = factor(cluster_raw))) +
  stat_ecdf(linewidth = 1.0) +
  facet_wrap(~ subcategory, scales = "free_x") +
  labs(
    title = "ECDF of Impact Index by K-means Cluster (within subcategory)",
    x = "Impact Index",
    y = "Cumulative Proportion",
    color = "Cluster"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11)
  )

ecdf_plot_all


# 3) Density plot by subcategory
density_plot_all <- ggplot(df_km, aes(x = impact_index, fill = factor(cluster_raw))) +
  geom_density(alpha = 0.35) +
  facet_wrap(~ subcategory, scales = "free_x") +
  labs(
    title = "Impact Index Density by K-means Cluster (within subcategory)",
    x = "Impact Index",
    y = "Density",
    fill = "Cluster"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11)
  )

density_plot_all


# Final output (LONG format; journals repeated across subcategories) --------

df_final <- df_subcat %>%
  arrange(subcategory, desc(impact_index)) %>%
  select(
    subcategory,
    rank_in_subcategory,
    percentile_in_subcategory,
    `Source title`,
    tier,
    Publisher,
    CiteScore, SNIP, SJR,
    PC1_score,
    impact_index
  )

head(df_final)

# Tier distribution plot

tier_plot <- ggplot(df_final, aes(x = impact_index, fill = tier)) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 50) +
  labs(
    title = "Impact Index Distribution by Tier",
    x = "Impact Index (0–100)",
    y = "Count"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    axis.text  = element_text(size = 14)
  )
tier_plot

# Impact Index histogram by subcategory ------------------------------------

index_hist_by_subcat <- ggplot(df_final, aes(x = impact_index)) +
  geom_histogram(bins = 40, color = "black", fill = "steelblue", alpha = 0.8) +
  facet_wrap(~ subcategory, scales = "free_x") +
  coord_cartesian(ylim = c(0, 400)) +
  labs(
    title = "Distribution of Impact Index by Subcategory",
    subtitle = "Impact Index (0–100) computed within each disciplinary subcategory",
    x = "Impact Index",
    y = "Number of Journals"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title    = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 13, margin = margin(b = 10)),
    strip.text    = element_text(size = 12, face = "bold"),
    axis.title.x  = element_text(size = 13),
    axis.title.y  = element_text(size = 13),
    axis.text     = element_text(size = 11)
  )

index_hist_by_subcat



# Save subcategory-specific tier list as CSV --------------------------------

write_csv(df_final, "AIRESS_Journal_Tiers_by_Subcategory.csv")
message("Saved: AIRESS_Journal_Tiers_by_Subcategory.csv")






