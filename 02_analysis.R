# --- 02_analysis.R ---
# Goal: summarise, visualise, and test interspecific differences
# Source 01_processing.R to get clean `web_metrics`.

# --- Clear workspace ---
rm(list = ls())

# --- Load libraries ---
library(tidyverse)
library(patchwork)
library(ggfortify)
library(vegan)

# --- Source processing script ---
source("01_processing.R")

# --- Quick summaries ---
eda_summary <- web_metrics |>
  group_by(species, data_type) |>
  summarise(
    across(
      c(area_mm2, perimeter_mm, aspect_ratio, convexity, circularity,
        entrance_width_mm, area_ratio_sc),
      list(mean = ~mean(.x, na.rm = TRUE),
           sd   = ~sd(.x, na.rm = TRUE),
           n    = ~sum(!is.na(.x))),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )
print(eda_summary)

# --- Intraspecific variation (CV = sd/mean) ---
cv_summary <- web_metrics |>
  group_by(species, data_type) |>
  summarise(
    across(
      c(area_mm2, perimeter_mm, aspect_ratio, convexity, circularity,
        entrance_width_mm, area_ratio_sc),
      list(cv = ~sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )
print(cv_summary)

# --- Species label map for axis ticks ---
species_labels <- c(
  "a_robustus" = "A. robustus",
  "h_cerberea" = "H. cerberea",
  "h_versuta"  = "H. versuta"
)

# --- Plots (single shared legend from p1) ---

# 1) Web area (log y) WITH legend
(p1 <- ggplot(web_metrics |> drop_na(area_mm2, data_type),
              aes(x = species, y = area_mm2, fill = data_type)) +
    geom_boxplot(outlier.shape = NA, width = 0.7,
                 position = position_dodge(width = 0.8)) +
    geom_jitter(position = position_jitterdodge(jitter.width = 0.15,
                                                dodge.width = 0.8),
                alpha = 0.35, size = 1.8, show.legend = FALSE) +
    scale_y_log10() +
    scale_x_discrete(labels = species_labels) +
    scale_fill_discrete(name = "Zone",
                        labels = c("capture_area" = "Capture area",
                                   "sensory_area" = "Sensory area")) +
    labs(x = NULL, y = "Web area (mm², log scale)") +
    theme_classic())

# 2) Web circularity (no legend)
(p2 <- ggplot(web_metrics |> drop_na(circularity, data_type),
              aes(x = species, y = circularity, fill = data_type)) +
    geom_boxplot(outlier.shape = NA, width = 0.7,
                 position = position_dodge(width = 0.8)) +
    geom_jitter(position = position_jitterdodge(jitter.width = 0.15,
                                                dodge.width = 0.8),
                alpha = 0.35, size = 1.8, show.legend = FALSE) +
    scale_x_discrete(labels = species_labels) +
    labs(x = NULL, y = "Web circularity (4πA / P²)") +
    guides(fill = "none") +
    theme_classic())

# 3) Web perimeter (log y; no legend)
(p4 <- ggplot(web_metrics |> drop_na(perimeter_mm, data_type),
              aes(x = species, y = perimeter_mm, fill = data_type)) +
    geom_boxplot(outlier.shape = NA, width = 0.7,
                 position = position_dodge(width = 0.8)) +
    geom_jitter(position = position_jitterdodge(jitter.width = 0.15,
                                                dodge.width = 0.8),
                alpha = 0.35, size = 1.8, show.legend = FALSE) +
    scale_y_log10() +
    scale_x_discrete(labels = species_labels) +
    labs(x = NULL, y = "Web perimeter (mm, log scale)") +
    guides(fill = "none") +
    theme_classic())

# 4) Web aspect ratio (filter finite; no legend)
(p5 <- ggplot(web_metrics |> filter(is.finite(aspect_ratio)) |> drop_na(aspect_ratio, data_type),
              aes(x = species, y = aspect_ratio, fill = data_type)) +
    geom_boxplot(outlier.shape = NA, width = 0.7,
                 position = position_dodge(width = 0.8)) +
    geom_jitter(position = position_jitterdodge(jitter.width = 0.15,
                                                dodge.width = 0.8),
                alpha = 0.35, size = 1.8, show.legend = FALSE) +
    scale_x_discrete(labels = species_labels) +
    labs(x = NULL, y = "Web aspect ratio (width / height)") +
    guides(fill = "none") +
    theme_classic())

# 5) Web convexity (no legend)
(p6 <- ggplot(web_metrics |> drop_na(convexity, data_type),
              aes(x = species, y = convexity, fill = data_type)) +
    geom_boxplot(outlier.shape = NA, width = 0.7,
                 position = position_dodge(width = 0.8)) +
    geom_jitter(position = position_jitterdodge(jitter.width = 0.15,
                                                dodge.width = 0.8),
                alpha = 0.35, size = 1.8, show.legend = FALSE) +
    scale_x_discrete(labels = species_labels) +
    labs(x = NULL, y = "Web convexity (area / hull area)") +
    guides(fill = "none") +
    theme_classic())

# 6) Sensory : Capture area ratio (log y; no legend)
(p3 <- ggplot(web_metrics |> distinct(file_name, individual, species, area_ratio_sc) |> drop_na(area_ratio_sc),
              aes(x = species, y = area_ratio_sc)) +
    geom_boxplot(outlier.shape = NA, width = 0.7) +
    geom_jitter(width = 0.15, alpha = 0.35, size = 1.8) +
    scale_y_log10() +
    scale_x_discrete(labels = species_labels) +
    labs(x = NULL, y = "Web S/C area ratio (log scale)") +
    theme_classic())

# --- Figure 5 (published): 2 x 2, panels a-d, single legend from p1 ---
# Panel order must match the caption: (a) area, (b) circularity,
# (c) aspect ratio, (d) sensory-to-capture zone ratio.
# At two panels across 180 mm the default axis text is too wide and the
# species names run together ("A. robustusH. cerberea"), so the x-axis text
# is set down to 7 pt. Italic is correct style for species names anyway.
(fig5 <-
    (p1 + p2) /
    (p5 + p3) +
    plot_layout(guides = "collect") +
    plot_annotation(tag_levels = "a") &
    theme(legend.position = "right",
          plot.tag = element_text(face = "bold", size = 12),
          axis.text.x = element_text(face = "italic", size = 7)))

# TIFF at 300 dpi: Ecology and Evolution accepts jpeg, tiff or eps (not png).
# 180 mm wide is full page width; 2126 px is the 300 dpi minimum at that size.
ggsave("Figure5.tif", fig5,
       width = 180, height = 190, units = "mm", dpi = 300,
       device = "tiff", compression = "lzw")

# --- Full descriptor panel (exploratory; not a manuscript figure) ---
(summary_plots <-
    (p1 + p2) /
    (p4 + p5) /
    (p6 + p3) +
    plot_layout(guides = "collect") &
    theme(legend.position = "right"))

ggsave("web_metrics_summary.png", summary_plots,
       width = 8, height = 10, units = "in", dpi = 300)

# ====================================================================
# --- ANALYSES ---
# ====================================================================

# Prepare variables for modelling
web_metrics <- web_metrics |>
  mutate(
    log_area     = log(area_mm2),
    log_perim    = log(perimeter_mm),
    log_ratio_sc = log(area_ratio_sc)
  )

# --- PCA with reduced descriptor set (zones separated) ---
desc_vars <- web_metrics |>
  select(species, data_type, 
         log_area, aspect_ratio, circularity, log_ratio_sc) |>
  drop_na()

# Scale descriptors
desc_scaled <- desc_vars |>
  select(-species, -data_type) |>
  scale()

# Rename columns to human-readable labels for PCA biplot axis labels
colnames(desc_scaled) <- c("Log area (mm\u00b2)", "Aspect ratio", "Circularity", "Log SZ:CZ ratio")

pca_reduced <- prcomp(desc_scaled, center = TRUE, scale. = TRUE)
summary(pca_reduced)

# --- PCA biplot ---
# ggfortify centres each loading label on its arrow tip, so the vector runs
# straight through the text. loadings.label.repel doesn't fix this: ggrepel
# avoids other labels and data points, but knows nothing about the arrows.
# Instead we suppress ggfortify's labels and place our own, anchored just
# beyond each arrow tip and justified away from the origin, so a label can
# never sit on its own shaft.

p_base <- autoplot(pca_reduced, data = desc_vars,
                   colour = 'species', shape = 'data_type',
                   loadings = TRUE, loadings.label = FALSE)

# Recover the arrow coordinates ggfortify actually computed, rather than
# trying to reproduce its internal scaling.
seg_idx <- which(vapply(p_base$layers,
                        function(l) inherits(l$geom, "GeomSegment"),
                        logical(1)))
seg     <- layer_data(p_base, seg_idx[1])
rot     <- pca_reduced$rotation[, 1:2, drop = FALSE]
stopifnot(nrow(seg) == nrow(rot))

# Derive ggfortify's scaling factor and rebuild the tips from the rotation
# matrix, so labels are keyed to variable *names* rather than to row order.
scaler <- median(c(seg$xend / rot[, 1], seg$yend / rot[, 2]))
stopifnot(all(abs(seg$xend - rot[, 1] * scaler) < 1e-8),
          all(abs(seg$yend - rot[, 2] * scaler) < 1e-8))

tip_x <- rot[, 1] * scaler
tip_y <- rot[, 2] * scaler
pad   <- 0.015   # gap between arrowhead and text, in data units
r     <- sqrt(tip_x^2 + tip_y^2)

loading_labels <- data.frame(
  x     = tip_x + pad * tip_x / r,
  y     = tip_y + pad * tip_y / r,
  label = rownames(rot),
  hjust = ifelse(tip_x >= 0, 0, 1),   # text runs away from the origin
  vjust = ifelse(tip_y >= 0, 0, 1)
)

(p_pca_reduced <-
    p_base +
    stat_ellipse(aes(color = species, fill = species),
                 type = "norm", level = 0.68,
                 geom = "polygon", alpha = 0.12, linewidth = 0.6) +
    geom_text(data = loading_labels,
              aes(x = x, y = y, label = label),
              hjust = loading_labels$hjust,
              vjust = loading_labels$vjust,
              size = 3, colour = "black", inherit.aes = FALSE) +
    scale_color_discrete(labels = species_labels, name = "Species") +
    scale_fill_discrete(labels = species_labels, guide = "none") +
    scale_shape_discrete(name = "Zone",
                         labels = c("capture_area" = "Capture area",
                                    "sensory_area" = "Sensory area")) +
    xlim(-0.45, 0.60) +
    ylim(-0.45, 0.32) +
    theme_classic()
)

# Save figure (Figure 6). TIFF at 300 dpi, 180 mm wide, as above.
ggsave("Figure6.tif", p_pca_reduced,
       width = 180, height = 155, units = "mm", dpi = 300,
       device = "tiff", compression = "lzw")

# Inspect PCA loadings
loadings <- pca_reduced$rotation
print(loadings)

# --- PERMANOVA: zone + species model ---
set.seed(123)
dist_mat <- dist(desc_scaled, method = "euclidean")

# Check dispersion for both factors (good practice)
bd_species <- betadisper(dist_mat, group = desc_vars$species)
print(permutest(bd_species, permutations = 9999))

bd_zone <- betadisper(dist_mat, group = desc_vars$data_type)
print(permutest(bd_zone, permutations = 9999))

# Marginal (adjusted) effects of zone and species
perm_species_adj <- adonis2(
  dist_mat ~ data_type + species,
  data = desc_vars,
  permutations = 9999,
  by = "margin"
)
print(perm_species_adj)

# --- Pairwise PERMANOVA among species (unadjusted; descriptive) ---
pairwise_adonis <- function(desc_scaled, groups, perms = 9999, method = "euclidean") {
  levs <- levels(factor(groups))
  purrr::map_dfr(combn(levs, 2, simplify = FALSE), function(pair) {
    idx <- groups %in% pair
    d   <- dist(desc_scaled[idx, , drop = FALSE], method = method)
    df  <- data.frame(group = factor(groups[idx], levels = pair))
    res <- vegan::adonis2(d ~ group, data = df, permutations = perms)
    tibble::tibble(
      group1 = pair[1],
      group2 = pair[2],
      F      = unname(res$F[1]),
      R2     = unname(res$R2[1]),
      p      = unname(res$`Pr(>F)`[1])
    )
  })
}

pairwise_results <- pairwise_adonis(desc_scaled, desc_vars$species, perms = 9999)

# Clean up labels and adjust p-values
pairwise_tidy <- pairwise_results |>
  mutate(
    group1_lab = species_labels[group1],
    group2_lab = species_labels[group2],
    p_adj_holm = p.adjust(p, method = "holm"),
    p_adj_fdr  = p.adjust(p, method = "BH")
  ) |>
  select(group1 = group1_lab, group2 = group2_lab, F, R2, p_raw = p, p_adj_holm, p_adj_fdr) |>
  arrange(desc(R2))

print(pairwise_tidy, n = Inf)
write.csv(pairwise_tidy, "permanova_pairwise_results.csv", row.names = FALSE)