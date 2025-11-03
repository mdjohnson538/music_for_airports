# 01_extract_features.R
# Precompute ambient-audio features from MP3 files into a tidy table.

# ---- packages ----
req <- c("av", "tuneR", "seewave", "dplyr", "purrr", "stringr", "arrow", "readr")
new <- setdiff(req, rownames(installed.packages()))
if (length(new)) install.packages(new, repos = "https://cloud.r-project.org")
invisible(lapply(req, library, character.only = TRUE))

# ---- config ----
input_dir  <- "."                 # folder containing your MP3s (the project root works)
output_parquet <- "mfa_features.parquet"
output_csv     <- "mfa_features.csv"

# Windowing (ambient-friendly): analyze in 10-second chunks
win_sec  <- 10
step_sec <- 10   # non-overlapping; make smaller (e.g., 5) if you want overlap

# ---- helpers ----

mixdown_mono <- function(w) {
  # Return a numeric mono vector in [-1, 1]
  if (w@stereo) {
    as.numeric(rowMeans(cbind(w@left, w@right))) / (2^(w@bit - 1))
  } else {
    as.numeric(w@left) / (2^(w@bit - 1))
  }
}

rms_vec <- function(x) sqrt(mean(x^2))

track_features <- function(w, track_label) {
  sr <- w@samp.rate
  mono <- mixdown_mono(w)
  
  n <- length(mono)
  win_n  <- win_sec  * sr
  step_n <- step_sec * sr
  if (n < win_n) return(dplyr::tibble())  # too short
  
  starts <- seq(1, n - win_n + 1, by = step_n)
  
  # If stereo, keep originals for stereo metrics
  L <- if (w@stereo) as.numeric(w@left)  else mono * (2^(w@bit - 1))
  R <- if (w@stereo) as.numeric(w@right) else NA
  
  purrr::map_dfr(starts, function(s0) {
    s1 <- s0 + win_n - 1
    seg_mono <- mono[s0:s1]
    
    # Core spectral shape (no plotting)
    sp   <- seewave::spec(seg_mono, f = sr, plot = FALSE)      # spectrum
    prop <- seewave::specprop(sp, f = sr)                      # centroid, spread, skew, kurt
    
    # Loudness proxy
    seg_rms <- rms_vec(seg_mono)
    
    # Stereo width (1 - |cor(L, R)|); NA if mono
    stereo_width <- NA_real_
    if (w@stereo) {
      seg_L <- L[s0:s1]; seg_R <- R[s0:s1]
      cLR <- suppressWarnings(stats::cor(seg_L, seg_R))
      if (is.finite(cLR)) stereo_width <- 1 - abs(cLR)
    }
    
    dplyr::tibble(
      track        = track_label,
      start_sec    = (s0 - 1) / sr,
      end_sec      = (s1 - 1) / sr,
      centroid_hz  = prop$cent,     # spectral centroid
      spread_hz    = prop$sd,       # spectral spread (std dev)
      skewness     = prop$skew,
      kurtosis     = prop$kurt,
      rms          = seg_rms,       # loudness proxy
      stereo_width = stereo_width   # 0=fully correlated, 1=uncorrelated; NA for mono
    )
  })
}

read_mp3_as_wave <- function(path_mp3) {
  # Convert MP3 -> WAV (PCM) to ensure consistent decoding, then read as Wave
  tmp_wav <- tempfile(fileext = ".wav")
  av::av_audio_convert(path_mp3, tmp_wav, format = "wav")
  on.exit(unlink(tmp_wav), add = TRUE)
  tuneR::readWave(tmp_wav)
}

# ---- ingest all MP3s and compute features ----
mp3s <- list.files(input_dir, pattern = "\\.mp3$", full.names = TRUE)
stopifnot(length(mp3s) > 0)

message("Found ", length(mp3s), " MP3 file(s). Extracting features...")

features <- purrr::map_dfr(mp3s, function(p) {
  label <- basename(p) |>
    stringr::str_remove("\\.mp3$") |>
    stringr::str_trim()
  
  w <- read_mp3_as_wave(p)
  out <- track_features(w, track_label = label)
  
  if (nrow(out) == 0) {
    message("Skipped (too short): ", basename(p))
  } else {
    message("Processed: ", basename(p), " (", nrow(out), " windows)")
  }
  out
})

# ---- write outputs ----
if (nrow(features) == 0) stop("No features extracted. Check files and settings.")

arrow::write_parquet(features, output_parquet)
readr::write_csv(features, output_csv)

message("Done.\n- Parquet: ", normalizePath(output_parquet),
        "\n- CSV:     ", normalizePath(output_csv))