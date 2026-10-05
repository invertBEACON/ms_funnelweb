# Web architecture in Australian funnel-web spiders (Atracidae)

Data and code underlying:

> Brown, N., & White, T. E. (2026). Inter- and intraspecific variation in web architecture and its significance in Australian funnel-web spiders (Atracidae). *Ecology and Evolution*. [DOI to follow]

Image-based morphometrics of webs built by *Hadronyche cerberea*, *H. versuta* and the *Atrax robustus* complex, with the analyses and figures reported in the paper.

## Contents

| File | What it is |
|---|---|
| `data_imageJ_NB.csv` | Raw ImageJ coordinate exports — the only input |
| `01_processing.R` | Cleans the exports and computes per-web geometry. Produces no files; sourced by the next script |
| `02_analysis.R` | Summaries, PCA, PERMANOVA, and the manuscript figures |

## Reproducing

```r
# from this directory
source("02_analysis.R")   # sources 01_processing.R itself
```

Both scripts use paths relative to the working directory, so run them from the repository root.

Outputs written to the working directory:

- `Figure5.tif` — boxplots of web metrics by species and zone (Fig. 5)
- `Figure6.tif` — PCA biplot (Fig. 6)
- `web_metrics_summary.png` — six-panel exploratory version of Fig. 5, not in the paper
- `permanova_pairwise_results.csv` — pairwise PERMANOVA table

Figures 1–4 are schematic diagrams and photographs, not generated here.

### Environment

R 4.5.1. Packages: **tidyverse**, **sf**, **janitor** (processing); **vegan** 2.7-3, **ggfortify**, **patchwork** (analysis and figures).

```r
install.packages(c("tidyverse", "sf", "janitor", "vegan", "ggfortify", "patchwork"))
```

## Data dictionary

One row per digitised coordinate, 1967 rows covering 37 webs.

| Column | Description |
|---|---|
| `file_name` | Photograph identifier; one web per file |
| `individual` | Individual number within the photograph. Note the trailing space in the header, handled by `janitor::clean_names()` |
| `species` | `a_robustus`, `h_cerberea`, `h_versuta` |
| `data_type` | `sensory_area`, `capture_area` or `entrance_width` |
| `scale_factor (pxl/mm)` | Calibration from the in-frame scale reference |
| `x_coord`, `y_coord` | Raw pixel coordinates from ImageJ |
| `scale_x`, `scale_y` | Calibrated coordinates in mm (`x_coord / scale_factor`) |
| `entrance_#` | Which burrow entrance the trace belongs to |
| `notes` | Free text; populated on 7 rows, recording ambiguous entrances |

Sensory and capture zones are digitised as polygon vertex sequences, from which area, perimeter, aspect ratio, convexity and circularity are derived. Entrance width is two points, from which a distance is computed.

## Licence

- **Code** (`01_processing.R`, `02_analysis.R`) — [MIT](LICENSE)
- **Data** (`data_imageJ_NB.csv`) — [CC BY 4.0](LICENSE-DATA)

