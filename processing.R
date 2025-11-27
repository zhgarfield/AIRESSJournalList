
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

# Conduct PCA -------------------------------------------------------------
# Run PCA on the three standardized metrics
pca_model <- prcomp(
  df_wide[, pca_vars],
  center = TRUE,
  scale. = TRUE
)

# View variance explained and loadings
print(summary(pca_model))
print(pca_model$rotation)

# PCA Loadings Plot
pc1_loadings_plot <- hagenutils::pca_loadings_plot(pca_model)

# Extract the raw PC1 scores
pc1_raw <- pca_model$x[, 1]

# Flip PC1 direction so that higher metric values → higher PC1
# (If the correlation between PC1 and CiteScore is negative, multiply PC1 by -1)
pc1_dir <- ifelse(cor(df_wide$CiteScore, pc1_raw) < 0, -1, 1)

# Apply the corrected direction
df_wide$PC1_score <- pc1_raw * pc1_dir

# Determine low-impact cutoff and filter df_wide
# Choose the cutoff. Here bottom 25%
pc1_cutoff <- quantile(df_wide$PC1_score, 0.25)

cat("Using PC1 cutoff:", pc1_cutoff, "\n")

# Keep only journals above cutoff
df_wide <- df_wide %>%
  filter(PC1_score >= pc1_cutoff)

cat("Remaining journals after cutoff:", nrow(df_wide), "\n")

# Compute Impact Index (0–100) AFTER filtering
df_wide <- df_wide %>%
  mutate(
    impact_index = scales::rescale(PC1_score, to = c(0, 100))
  )

summary(df_wide$impact_index)

# Histogram of impact index
index_hist <- ggplot(df_wide, aes(x = impact_index)) +
  geom_histogram(bins = 40, color = "black", fill = "steelblue", alpha = 0.85) +
  labs(
    title = "Distribution of Impact Index Scores",
    subtitle = "Impact Index (0–100) derived from PCA on CiteScore, SNIP, and SJR",
    x = "Impact Index",
    y = "Number of Journals"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title    = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 14, margin = margin(b = 10)),
    axis.title.x  = element_text(size = 16),
    axis.title.y  = element_text(size = 16),
    axis.text.x   = element_text(size = 14),
    axis.text.y   = element_text(size = 14)
  )
index_hist

# K-means clustering on impact_index --------------------------------------

set.seed(123)

km <- kmeans(df_wide$impact_index, centers = 3)

df_wide$cluster_raw <- km$cluster

# Rank clusters from lowest to highest impact

cluster_order <- df_wide %>%
  group_by(cluster_raw) %>%
  summarise(mean_index = mean(impact_index)) %>%
  arrange(mean_index)

rank_map <- cluster_order %>%
  mutate(
    tier = c("C: Acceptable", "B: Preferred", "A: Excellent")
  )

df_wide <- df_wide %>%
  left_join(rank_map, by = "cluster_raw")

# Cluster centers plot 
cluster_centers <- df_wide %>%
  group_by(cluster_raw) %>%
  summarise(center = mean(impact_index), .groups = "drop")

ggplot(cluster_centers, aes(x = factor(cluster_raw), y = center, group = 1)) +
  geom_line(linewidth = 1.2, color = "steelblue") +
  geom_point(size = 4, color = "darkred") +
  labs(
    title = "K-means Cluster Centers (Impact Index)",
    x = "Cluster",
    y = "Mean Impact Index"
  ) +
  theme_minimal(base_size = 14)

# ECDF separation plot 
ggplot(df_wide, aes(x = impact_index, color = factor(cluster_raw))) +
  stat_ecdf(linewidth = 1.1) +
  labs(
    title = "ECDF of Impact Index by K-means Cluster",
    x = "Impact Index",
    y = "Cumulative Proportion",
    color = "Cluster"
  ) +
  theme_minimal(base_size = 14)

# Density plot by cluster
ggplot(df_wide, aes(x = impact_index, fill = factor(cluster_raw))) +
  geom_density(alpha = 0.4) +
  labs(
    title = "Impact Index Density by Cluster",
    x = "Impact Index",
    y = "Density",
    fill = "Cluster"
  ) +
  theme_minimal(base_size = 14)




# Final output
df_final <- df_wide %>%
  arrange(desc(impact_index)) %>%
  select(
    `Source title`,
    CiteScore, SNIP, SJR,
    PC1_score,
    impact_index,
    tier,
    everything()
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


# Save final journal tier list as CSV


final_export <- df_wide %>%
  select(
    `Source title`,
    CiteScore,
    SNIP,
    SJR,
    PC1_score,
    impact_index,
    tier
  ) %>%
  arrange(desc(impact_index))

write_csv(final_export, "AIRESS_Journal_Tiers.csv")

message("Saved: AIRESS_Journal_Tiers.csv")


