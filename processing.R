# ============================================================
# UPDATE: Use SJR ONLY (no PCA), drop bottom 25%, build Impact Index
# ALSO: add a new AIRESS subcategory "GeneralSocialScience" that draws
# from Scopus: SocialSciencesMiscellaneous + GeneralSocialSciences
# ============================================================

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

data_dir <- "./files/scopus_sources"
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

# We now only require SJR (not CiteScore/SNIP)
impact_var <- "SJR"

data <- data %>%
  mutate(across(all_of(impact_var), ~ suppressWarnings(as.numeric(.)))) %>%
  filter(!is.na(.data[[impact_var]]))

# Build subject-area membership matrix -----------------------------------

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
  Anthropology = c(
    "Anthropology",
    "CulturalStudies"
  ),
  
  AppliedSocialSciences = c(
    "HealthSocialScience",
    "Development"
  ),
  
  ComputationalAndMethods = c(
    "ArtificialIntelligence",
    "ModelingandSimulation",
    "StatisticsandProbability",
    "StatisticsProbabilityandUncertainty"
  ),
  
  DecisionSciences = c(
    "DecisionSciencesMiscellaneous",
    "GeneralDecisionSciences",
    "ManagementScienceandOperationsResearch"
  ),
  
  Demography = c(
    "Demography"
  ),
  
  Economics = c(
    "EconomicsandEconometrics",
    "GeneralEconomicsEconometricsandFinance",
    "EconomicsEconometricsandFinanceMiscellaneous"
  ),
  
  GeographyPlanningAndDevelopment = c(
    "GeographyPlanningandDevelopment",
    "Transportation"
  ),
  
  GeneralSocialScience = c(
    "SocialSciencesMiscellaneous",
    "GeneralSocialSciences"
  ),
  
  History = c(
    "History"
  ),
  
  Law = c(
    "ManagementMonitoringPolicyandLaw",
    "Law"
  ),
  
  Multidisciplinary = c(
    "Multidisciplinary"
  ),
  
  PhilosophyOfScience = c(
    "HistoryandPhilosophyofScience"
  ),
  
  PoliticalScienceAndIR = c(
    "PoliticalScienceandInternationalRelations",
    "SociologyandPoliticalScience",
    "PublicAdministration",
    "ReligiousStudies"
  ),
  
  Psychology = c(
    "PsychologyMiscellaneous",
    "GeneralPsychology",
    "AppliedPsychology",
    "ExperimentalandCognitivePsychology",
    "SocialPsychology",
    "DevelopmentalandEducationalPsychology"
  ),
  
  Sociology = c(
    "SociologyandPoliticalScience"
  ),
  
  Other = c(
    "GeneralAgricultureandBiologicalSciences",
    "GeneralMedicine",
    "Communication",
    "Philosophy"
  )
)

# Nice names for plotting -------------------------------------------------

subcategory_labels <- c(
  Anthropology                   = "Anthropology",
  AppliedSocialSciences          = "Applied\nSocial Sciences",
  ComputationalAndMethods        = "Computational\n& Methods",
  DecisionSciences               = "Decision Sciences",
  Demography                     = "Demography",
  Economics                      = "Economics",
  GeneralSocialScience           = "General\nSocial Science",
  GeographyPlanningAndDevelopment = "Geography,\nPlanning & Development",
  History                        = "History",
  Law                            = "Law",
  Multidisciplinary              = "Multidisciplinary",
  Other                          = "Other",
  PhilosophyOfScience            = "Philosophy\nof Science",
  PoliticalScienceAndIR          = "Political Science\n& International Relations",
  Psychology                     = "Psychology",
  Sociology                      = "Sociology"
)

# Create 0/1 membership columns for each subcategory ----------------------

for (sc in names(subcategory_map)) {
  areas <- subcategory_map[[sc]]
  areas_present <- intersect(areas, names(df_wide))
  df_wide[[sc]] <- if (length(areas_present) == 0) 0L else as.integer(
    rowSums(df_wide[, areas_present, drop = FALSE], na.rm = TRUE) > 0
  )
}

# SJR workflow: cutoff + impact index + kmeans WITHIN each subcategory ----

run_subcat_sjr <- function(sc_name, df_all, impact_var = "SJR", cutoff_q = 0.25, k = 4) {
  
  df_sc <- df_all %>%
    filter(.data[[sc_name]] == 1) %>%
    select(`Source title`, Publisher, all_of(impact_var)) %>%
    drop_na(all_of(impact_var))
  
  # too small to cluster robustly
  if (nrow(df_sc) < 10) return(tibble())
  
  # Drop bottom 25% WITHIN subcategory using SJR directly
  sjr_cutoff <- quantile(df_sc[[impact_var]], cutoff_q, na.rm = TRUE, names = FALSE)
  df_sc <- df_sc %>% filter(.data[[impact_var]] >= sjr_cutoff)
  
  # if after filtering we have too few, still compute impact index but skip kmeans
  if (nrow(df_sc) < 10) {
    df_sc <- df_sc %>%
      mutate(
        subcategory  = sc_name,
        impact_index = scales::rescale(.data[[impact_var]], to = c(0, 100)),
        cluster_raw  = NA_integer_,
        tier         = NA_character_
      )
    return(df_sc %>% select(`Source title`, Publisher, subcategory, all_of(impact_var), impact_index, cluster_raw, tier))
  }
  
  # Impact index 0–100 WITHIN subcategory based on SJR
  df_sc <- df_sc %>%
    mutate(
      subcategory  = sc_name,
      impact_index = scales::rescale(.data[[impact_var]], to = c(0, 100))
    )
  
  # K-means within subcategory on the impact index
  set.seed(123)
  km <- kmeans(df_sc$impact_index, centers = k)
  df_sc$cluster_raw <- km$cluster
  
  # Order clusters by mean impact and label tiers
  cluster_order <- df_sc %>%
    group_by(cluster_raw) %>%
    summarise(mean_index = mean(impact_index), .groups = "drop") %>%
    arrange(mean_index)
  
  # lowest -> highest
  rank_map <- cluster_order %>%
    mutate(tier = c("D: Acceptable", "C: Preferred", "B: Excellent", "A: Elite"))
  
  df_sc <- df_sc %>%
    left_join(rank_map, by = "cluster_raw")
  
  df_sc %>%
    select(`Source title`, Publisher, subcategory, all_of(impact_var), impact_index, cluster_raw, tier)
}

df_subcat <- purrr::map_dfr(
  names(subcategory_map),
  run_subcat_sjr,
  df_all = df_wide,
  impact_var = impact_var,
  cutoff_q = 0.25,
  k = 4
)

# Ranks/percentiles within each subcategory -------------------------------

df_subcat <- df_subcat %>%
  group_by(subcategory) %>%
  arrange(desc(impact_index), .by_group = TRUE) %>%
  mutate(
    rank_in_subcategory = dense_rank(desc(impact_index)),
    n_in_subcategory = n(),
    percentile_in_subcategory = 100 * (1 - (rank_in_subcategory - 1) / pmax(n_in_subcategory - 1, 1))
  ) %>%
  ungroup()

# NOTE: PCA loadings plot no longer applies (no PCA).
# If you keep that section in the report/script, remove it or replace with SJR diagnostics.

# K-means descriptive plots by subcategory --------------------------------

df_km <- df_subcat %>% filter(!is.na(cluster_raw))

cluster_centers_by_subcat <- df_km %>%
  group_by(subcategory, cluster_raw) %>%
  summarise(center = mean(impact_index, na.rm = TRUE), .groups = "drop")

cluster_centers_plot_all <- ggplot(
  cluster_centers_by_subcat,
  aes(x = factor(cluster_raw), y = center, group = 1)
) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 2.5) +
  facet_wrap(
    ~ subcategory,
    scales = "free_y",
    labeller = labeller(subcategory = subcategory_labels)
  ) +
  labs(
    title = "K-means Cluster Centers by Subcategory (SJR-based Impact Index)",
    x = "Cluster (raw k-means label)",
    y = "Mean Impact Index (0–100)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11)
  )

cluster_centers_plot_all

ecdf_plot_all <- ggplot(df_km, aes(x = impact_index, color = factor(cluster_raw))) +
  stat_ecdf(linewidth = 1.0) +
  facet_wrap(
    ~ subcategory,
    scales = "free_x",
    labeller = labeller(subcategory = subcategory_labels)
  ) +
  labs(
    title = "ECDF of Impact Index by Cluster (within subcategory; SJR-based)",
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

density_plot_all <- ggplot(df_km, aes(x = impact_index, fill = factor(cluster_raw))) +
  geom_density(alpha = 0.35) +
  facet_wrap(
    ~ subcategory,
    scales = "free_x",
    labeller = labeller(subcategory = subcategory_labels)
  ) +
  labs(
    title = "Impact Index Density by Cluster (within subcategory; SJR-based)",
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

# Final output ------------------------------------------------------------

df_final <- df_subcat %>%
  arrange(subcategory, desc(impact_index)) %>%
  select(
    subcategory,
    rank_in_subcategory,
    percentile_in_subcategory,
    `Source title`,
    tier,
    Publisher,
    SJR,               # keep only the metric driving the analysis
    impact_index,
    cluster_raw
  )

head(df_final)

# Tier distribution plot --------------------------------------------------

tier_plot <- ggplot(df_final, aes(x = impact_index, fill = tier)) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 50) +
  labs(
    title = "Impact Index Distribution by Tier (SJR-based)",
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

# Impact Index histogram by subcategory -----------------------------------

index_hist_by_subcat <- ggplot(df_final, aes(x = impact_index)) +
  geom_histogram(
    bins = 40,
    color = "black",
    fill = "steelblue",
    alpha = 0.8
  ) +
  facet_wrap(
    ~ subcategory,
    scales = "free_y",
    labeller = labeller(subcategory = subcategory_labels)
  ) +
  labs(
    title = "Distribution of Impact Index by Subcategory (SJR-based)",
    subtitle = "Impact Index (0–100) computed within each disciplinary subcategory from SJR",
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

# Faculty proposed alterations --------------------------------------------
#
# Overrides are compiled from three sources:
#   1. Hardcoded entries from Behavioral Science (named faculty,)
#   2. Pr. AMIR's revision file  (PoliticalScienceAndIR, Sociology,
#                             AppliedSocialSciences, ComputationalAndMethods)
#   3. Pr. MEHDI's revision file (Economics, Decision Sciences)
#
# All proposed rankings use single letters (A/B/C/D).
# These are mapped to the full tier strings used in df_final.
#
# Where the same journal appears in multiple sources, the file-based
# overrides take precedence over the hardcoded ones (files are more recent).
# Ambiguous entries (e.g. "B voir A, ask around", "C ou B") are skipped
# and printed to the console for manual follow-up.

# Map single letters to full tier strings
tier_label_map <- c(
  "A" = "A: Elite",
  "B" = "B: Excellent",
  "C" = "C: Preferred",
  "D" = "D: Acceptable"
)

# --- 1. Hardcoded overrides from BehSci --------------------------------

hardcoded_overrides <- tibble::tribble(
  ~subcategory,          ~`Source title`,                               ~tier_override,
  "PhilosophyOfScience", "Philosophy of Science",                        "B: Excellent", # Mathieu Charbonneau
  "PhilosophyOfScience", "History and Philosophy of the Life Sciences",  "C: Preferred", # Mathieu Charbonneau
  "PhilosophyOfScience", "HOPOS",                                        "C: Preferred", # Mathieu Charbonneau
  "PhilosophyOfScience", "Journal of the History of Biology",            "C: Preferred", # Mathieu Charbonneau
  "Psychology",          "Evolution and Human Behavior",                 "C: Preferred"  # Zachary Garfield
)

# --- 2. AMIR revisions ----------------------------------------------------

amir_raw <- readxl::read_excel("files/revisions/AIRESS_Journal_Tiers_by_Subcategory_2026-02-09_AMIR.xlsx") %>%
  rename(proposed_raw = `Proposed Ranking`) %>% 
  filter(!is.na(proposed_raw)) %>%
  mutate(
    proposed_clean = stringr::str_trim(toupper(proposed_raw)),
    valid          = proposed_clean %in% names(tier_label_map)
  )

# Print ambiguous entries for manual review
amir_skipped <- amir_raw %>%
  filter(!valid) %>%
  select(subcategory, `Source title`, tier, proposed_raw)

if (nrow(amir_skipped) > 0) {
  message("AMIR: ", nrow(amir_skipped), " ambiguous entries skipped — manual review needed:")
  print(amir_skipped)
}

amir_overrides <- amir_raw %>%
  filter(valid) %>%
  transmute(
    subcategory,
    `Source title`,
    tier_override = tier_label_map[proposed_clean]
  )

# --- 3. MEHDI revisions ---------------------------------------------------

mehdi_raw <- readxl::read_excel("files/revisions/Econ_list_0_MEHDI.xlsx",
                                sheet = "Econ_list") %>%
  filter(!is.na(`Proposed ranking`)) %>%
  mutate(
    proposed_clean = stringr::str_trim(toupper(`Proposed ranking`)),
    valid          = proposed_clean %in% names(tier_label_map)
  )

mehdi_skipped <- mehdi_raw %>%
  filter(!valid) %>%
  select(subcategory, `Source title`, tier, `Proposed ranking`)

if (nrow(mehdi_skipped) > 0) {
  message("MEHDI: ", nrow(mehdi_skipped), " unrecognized entries skipped:")
  print(mehdi_skipped)
}

mehdi_overrides <- mehdi_raw %>%
  filter(valid) %>%
  transmute(
    subcategory,
    `Source title`,
    tier_override = tier_label_map[proposed_clean]
  )

# --- 4. Combine and apply -------------------------------------------------
# Bind hardcoded first, then file-based. slice_tail keeps the last entry
# per journal, so file-based overrides win if there is a conflict.

all_overrides <- bind_rows(hardcoded_overrides, amir_overrides, mehdi_overrides) %>%
  group_by(subcategory, `Source title`) %>%
  slice_tail(n = 1) %>%
  ungroup()

message("Total overrides to apply: ", nrow(all_overrides))

df_final <- df_final %>%
  left_join(all_overrides, by = c("subcategory", "Source title")) %>%
  mutate(tier = dplyr::if_else(!is.na(tier_override), tier_override, tier)) %>%
  select(-tier_override)

# Verify: print all overridden rows
df_final %>%
  semi_join(all_overrides, by = c("subcategory", "Source title")) %>%
  select(subcategory, `Source title`, tier, impact_index, rank_in_subcategory) %>%
  arrange(subcategory, `Source title`)

# Save output -------------------------------------------------------------

write_csv(df_final, "AIRESS_Journal_Tiers_by_Subcategory.csv")
message("Saved: AIRESS_Journal_Tiers_by_Subcategory.csv")
