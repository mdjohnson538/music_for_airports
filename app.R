# app.R  ----
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(readr)
library(scales)

# ---- Data/loading (your tryCatch parquet/CSV block) ----
feat <- tryCatch({
  if (file.exists("mfa_features.parquet") && requireNamespace("arrow", quietly = TRUE)) {
    arrow::read_parquet("mfa_features.parquet")
  } else {
    readr::read_csv("mfa_features.csv", show_col_types = FALSE)
  }
}, error = function(e) readr::read_csv("mfa_features.csv", show_col_types = FALSE))

feat <- feat %>%
  mutate(
    midpoint = (start_sec + end_sec) / 2,
    track = factor(track, levels = unique(track))
  )

# ---- Static assets (resource path + mp3 detect) ----
if (dir.exists("www")) addResourcePath("mfa", normalizePath("www"))
bg_src <- if (file.exists("www/album_cover.jpg")) "mfa/album_cover.jpg" else NULL
mp3s <- list.files("www", pattern = "\\.mp3$", full.names = TRUE)
mp3_src <- if (length(mp3s)) paste0("mfa/", basename(mp3s[order(basename(mp3s))][1])) else NA_character_

# ---- Feature labels, palette, theme (your objects) ----
feature_labels <- c(
  centroid_hz  = "Spectral centroid: a proxy for perceived brightness. Higher = more energy in higher frequencies (brighter timbre).",
  spread_hz    = "Spectral spread: how widely energy is distributed around the centroid. Higher = broader, noisier spectrum; lower = tighter, purer tone.",
  skewness     = "Spectral skewness: asymmetry of the spectrum around its center. Positive = more high-frequency tail; negative = more low-frequency tail.",
  kurtosis     = "Spectral kurtosis: ‘peakedness’ of the spectrum. High = sharp, narrow peaks; low = flatter, more even spectrum.",
  rms          = "Loudness (RMS): average signal power over a window. Higher = louder passage; lower = quieter passage.",
  stereo_width = "Stereo width: correlation/energy difference between left and right. Higher = wider image; lower = more mono/centered."
)

track_levels <- levels(feat$track)
pal_tracks <- setNames(
  c("#2D728F", "#5C8D57", "#8A7BBD", "#D96B52")[seq_along(track_levels)],
  track_levels
)

mfa_theme <- theme_minimal(base_size = 13) +
  theme(
    panel.background = element_rect(fill = alpha("white", 0.78), color = NA),
    plot.background  = element_rect(fill = NA, color = NA),
    panel.grid.major = element_line(color = alpha("grey20", 0.18), linewidth = 0.25),
    panel.grid.minor = element_blank(),
    axis.title       = element_text(color = "#0f0f0f"),
    axis.text        = element_text(color = "#222"),
    legend.position  = "right",
    legend.background= element_blank(),
    plot.title       = element_text(face = "bold")
  )

# ---- UI (paste your tags$head/hero/sidebars/etc.) ----
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .hero{max-width:900px;margin:18px auto 12px;text-align:center;
             background:rgba(255,255,255,0.78);backdrop-filter:blur(6px);
             border:1px solid rgba(255,255,255,0.35);border-radius:18px;
             box-shadow:0 10px 30px rgba(0,0,0,0.12);padding:18px 24px;}
      .hero .lead{font-size:1.08rem;line-height:1.6;font-weight:500;
                  color:#111;text-shadow:0 1px 2px rgba(255,255,255,0.6),
                                   0 0 2px rgba(255,255,255,0.3);}
      .meta{max-width:900px;margin:4px auto 18px;text-align:center;font-size:0.9rem;color:#555;}
      .meta a{text-decoration:none;border-bottom:1px dotted #888;}
      .meta .label{font-weight:700;color:#222;}
    ")),
    tags$style(HTML("
      body{
        background: url('mfa/album_cover.jpg') center/cover no-repeat fixed !important;
        background-color: rgba(255,255,255,0.35) !important;
        background-blend-mode: overlay !important;
      }
    ")),
    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function(){
        const a = document.getElementById('mfa_player');
        if (!a) return;
        const p = a.play(); if (p) p.catch(()=>{});
        const unmute = () => { a.muted = false; a.play().catch(()=>{}); 
          document.removeEventListener('click', unmute);
          document.removeEventListener('keydown', unmute); };
        document.addEventListener('click', unmute, { once:true });
        document.addEventListener('keydown', unmute, { once:true });
      });
    "))
  ),
  
  if (!is.na(mp3_src)) tags$audio(
    id = "mfa_player", src = mp3_src, autoplay = NA, muted = NA, controls = NA,
    preload = "auto", `playsinline` = NA,
    style = "width:100%; max-width:480px; display:block; margin:8px 0;"
  ),
  
  tags$div(
    class = "hero",
    tags$h1("Music for Airports — A Data Visualization"),
    
    tags$p(
      class = "lead",
      HTML('Released in <strong>1978</strong>, <em>Music for Airports</em> marked a turning point in how listeners understood sound and attention.
    Rather than a sequence of songs, it offered a spatial experience—music designed to shape the environment, not dominate it.
    Its minimalist structure and generative logic became foundational to what we now describe as ambient music.')
    ),
    
    tags$p(
      class = "lead",
      HTML('This dashboard approaches the album from a computational perspective. It extracts and visualizes audio features to show how the compositions evolve across time and in relation to one another.')
    ),
    
    tags$p(
      class = "lead",
      HTML('Inspired by <a href="https://committedtotape.shinyapps.io/sixtyninelovesongs/" target="_blank">Sixty-Nine Love Songs: The Data Visualization</a>,
    this project applies a similar framework to instrumental material, where patterns emerge not through words but through sound.
    It invites the viewer to consider <em>Music for Airports</em> as both data and design—an early algorithmic experiment.')
    ),
    
    tags$blockquote("Ambient music must be as ignorable as it is interesting. - Eno")
  ),
  tags$div(class="meta",
           HTML('<span class="label">Built by</span> Created by Mark Johnson — with RStudio, Shiny, and Quarto.')
  ),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("track","Track:", c("All", levels(feat$track))),
      selectInput("feature","Feature:", c(
        "Centroid (Hz)"="centroid_hz","Spread (Hz)"="spread_hz",
        "Skewness"="skewness","Kurtosis"="kurtosis",
        "Loudness (RMS)"="rms","Stereo width"="stereo_width")),
      sliderInput("range","Time range (sec):",
                  min=floor(min(feat$start_sec, na.rm=TRUE)),
                  max=ceiling(max(feat$end_sec,  na.rm=TRUE)),
                  value=c(floor(min(feat$start_sec)), ceiling(max(feat$end_sec))),
                  step=10),
      checkboxInput("smooth","Smooth curve", TRUE)
    ),
    mainPanel(
      uiOutput("feat_desc"),
      tabsetPanel(
        tabPanel("Timeline",     plotlyOutput("timeline", height="400px")),
        tabPanel("Distribution", plotlyOutput("hist",     height="400px")),
        tabPanel("Comparison",   plotlyOutput("box",      height="400px"))
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session){
  
  output$feat_desc <- renderUI({
    key <- input$feature
    txt <- feature_labels[[key]]
    if (is.null(txt)) return(NULL)
    tags$p(
      style = "margin:12px auto 8px; max-width:800px; text-align:center;
               font-size:1.2rem; color:#222; background:rgba(255,255,255,0.75);
               border-radius:10px; padding:10px 14px; border:1px solid rgba(0,0,0,0.05);
               box-shadow:0 3px 8px rgba(0,0,0,0.05);",
      txt
    )
  })
  
  filt <- reactive({
    df <- feat
    if (input$track != "All") df <- dplyr::filter(df, track == input$track)
    dplyr::filter(df, midpoint >= input$range[1], midpoint <= input$range[2])
  })
  
  output$timeline <- renderPlotly({
    df <- filt()
    p <- ggplot(df, aes(x = midpoint, y = .data[[input$feature]], color = track)) +
      geom_line(alpha = 0.8, linewidth = 0.7) +
      { if (input$smooth) geom_smooth(se = FALSE, span = 0.2, linewidth = 0.6, color = "#333333") } +
      scale_color_manual(values = pal_tracks, guide = guide_legend(title = "track")) +
      labs(x = "Time (s)", y = input$feature, title = "Feature trajectory over time") +
      mfa_theme
    ggplotly(p) %>% layout(paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(255,255,255,0.78)",
                           legend=list(bgcolor="rgba(0,0,0,0)"))
  })
  
  output$hist <- renderPlotly({
    df <- filt()
    p <- ggplot(df, aes(x = .data[[input$feature]], fill = track)) +
      geom_histogram(bins = 40, alpha = 0.55, color = "white", linewidth = 0.2, position = "identity") +
      scale_fill_manual(values = pal_tracks, guide = guide_legend(title = "track")) +
      labs(x = input$feature, y = "Count", title = "Distribution of values") +
      mfa_theme
    ggplotly(p) %>% layout(paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(255,255,255,0.78)",
                           legend=list(bgcolor="rgba(0,0,0,0)"))
  })
  
  output$box <- renderPlotly({
    p <- ggplot(feat, aes(x = track, y = .data[[input$feature]], fill = track)) +
      geom_boxplot(alpha = 0.7, outlier.size = 0.8, color = scales::alpha("black", 0.25)) +
      scale_fill_manual(values = pal_tracks, guide = "none") +
      labs(x = NULL, y = input$feature, title = "Feature comparison across tracks") +
      mfa_theme + theme(axis.text.x = element_text(angle = 30, hjust = 1))
    ggplotly(p) %>% layout(paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(255,255,255,0.78)")
  })
}

# >>> THIS LINE IS REQUIRED in app.R <<<
shinyApp(ui, server)