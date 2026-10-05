# --- 01_processing.R ---
# Goal: clean ImageJ exports, compute per-individual web geometry metrics,
#       attach sensory:capture ratios. No file output — source this in 02_analysis.R.

# --- Clear workspace ---
rm(list = ls())

# --- Load libraries ---
library(tidyverse)  # wrangling, pivots, joins
library(sf)         # geometry ops
library(janitor)    # clean_names()
library(stringr)    # filename cleaning

# --- Load + clean raw data ---
dat <- read.csv("data_imageJ_NB.csv") |>
  clean_names() |>
  mutate(
    # Normalise filename artefacts
    file_name = str_replace_all(file_name, "\u00A0", " "), # non-breaking spaces -> space
    file_name = str_squish(file_name),                     # trim + collapse internal spaces
    file_name = str_remove(file_name, "\\.+$"),            # drop trailing dots
    file_name = na_if(file_name, "")                       # blanks -> NA
  ) |>
  filter(!is.na(file_name)) |>
  mutate(
    data_type = factor(data_type, levels = c("sensory_area", "capture_area", "entrance_width")),
    species   = factor(species)
  )

# --- Helper to close polygons (repeat first vertex); NULL if <3 points ---
close_coords <- function(df) {
  coords <- df |> select(scale_x, scale_y) |> as.matrix()
  if (nrow(coords) < 3) return(NULL)
  if (!all(coords[1, ] == coords[nrow(coords), ])) coords <- rbind(coords, coords[1, ])
  coords
}

# --- Polygon metric functions (numeric or NA) ---
get_area <- function(df) {
  coords <- close_coords(df); if (is.null(coords)) return(NA_real_)
  as.numeric(st_area(st_polygon(list(coords))))
}
get_perimeter <- function(df) {
  coords <- close_coords(df); if (is.null(coords)) return(NA_real_)
  as.numeric(st_length(st_linestring(coords)))
}
# Bounding-box aspect ratio = width/height
get_aspect_ratio <- function(df) {
  x_range <- range(df$scale_x, na.rm = TRUE)
  y_range <- range(df$scale_y, na.rm = TRUE)
  width <- diff(x_range); height <- diff(y_range)
  if (is.na(width) || is.na(height)) return(NA_real_)
  if (height == 0 && width == 0) return(NA_real_)  # single point
  if (height == 0) return(Inf)                     # horizontal line
  if (width == 0) return(0)                        # vertical line
  width / height
}
# Convexity = area / convex hull area
get_convexity <- function(df) {
  coords <- close_coords(df); if (is.null(coords)) return(NA_real_)
  poly <- st_polygon(list(coords))
  hull <- st_convex_hull(st_union(st_sfc(poly)))
  as.numeric(st_area(poly) / st_area(hull))
}
# Circularity = 4πA / P²
get_circularity <- function(area, perimeter) {
  out <- 4 * pi * area / (perimeter^2)
  out[is.na(area) | is.na(perimeter) | perimeter == 0] <- NA_real_
  out
}

# --- Per-individual polygon metrics (sensory_area, capture_area) ---
# --- Per-individual polygon metrics (sensory_area, capture_area) ---
metrics <- dat |>
  filter(data_type %in% c("sensory_area", "capture_area")) |>
  group_by(file_name, individual, species, data_type) |>
  summarise(
    n_points_polygon = n(),
    area_mm2         = get_area(pick(scale_x, scale_y)),
    perimeter_mm     = get_perimeter(pick(scale_x, scale_y)),
    aspect_ratio     = get_aspect_ratio(pick(scale_x, scale_y)),
    convexity        = get_convexity(pick(scale_x, scale_y)),
    .groups = "drop"
  ) |>
  mutate(
    circularity = get_circularity(area_mm2, perimeter_mm)
  )

# --- Entrance width per-individual (2 points; else NA) ---
entrance <- dat |>
  filter(data_type == "entrance_width") |>
  group_by(file_name, individual, species) |>
  summarise(
    n_points_entrance = n(),
    entrance_width_mm = if (n() == 2) {
      coords <- cbind(scale_x, scale_y)
      sqrt(sum((coords[2, ] - coords[1, ])^2))
    } else {
      NA_real_
    },
    .groups = "drop"
  )

# --- Merge polygon metrics + entrance widths ---
web_metrics <- left_join(
  metrics,
  entrance,
  by = c("file_name", "individual", "species")
)

# --- Sensory : Capture area ratio (per individual) ---
ratio_df <- metrics |>
  select(file_name, individual, species, data_type, area_mm2) |>
  filter(!is.na(area_mm2)) |>
  pivot_wider(names_from = data_type, values_from = area_mm2) |>
  mutate(
    area_ratio_sc       = sensory_area / capture_area,
    log10_area_ratio_sc = log10(area_ratio_sc)
  ) |>
  select(file_name, individual, species, area_ratio_sc, log10_area_ratio_sc)

web_metrics <- web_metrics |>
  left_join(ratio_df, by = c("file_name", "individual", "species"))

