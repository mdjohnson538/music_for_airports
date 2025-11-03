# Music for Airports — Audio Feature Analysis

This repository contains a personal project analyzing **Brian Eno’s _Music for Airports_** through programmatic audio feature extraction and interactive data visualization. The goal is to study structural and timbral patterns using modern audio-signal analysis.

> **Note:** No audio files are included. Only extracted, non-copyrighted feature data generated locally.

---

## Repository Contents

| File / Folder | Description |
|--------------|-------------|
| `01_extract_features.R` | Script to extract audio features from local files |
| `music_for_airports.qmd` | Quarto notebook with analysis & visuals |
| `app.R` | Shiny app for interactive audio-feature exploration |
| `mfa_features.csv` / `mfa_features.parquet` | Machine-listening feature dataset |
| `Music For Airports.Rproj` | RStudio project file |

---

## Objectives

- Load album audio locally
- Extract audio-signal features (timbre, spectral, chroma, dynamics)
- Visualize patterns across time
- Provide interactive inspection via Shiny

---

## Methods

### Features Analyzed
- MFCCs (timbre)
- Spectral centroid / bandwidth / roll-off
- Chroma & tonal features
- Energy & amplitude envelopes

### Tools Used
- **R** (`tidyverse`, `tuneR`, `seewave`, etc.)
- **Quarto** for reproducible reporting
- **Shiny** for interactivity
- **Parquet/CSV** for data storage

---

## Reproducibility & Setup

### Requirements
- R ≥ 4.1
- RStudio or Quarto CLI installed
- Audio files stored locally (not included in repo)

### Example Directory Structure

