# ============================================================
# Premier League Team Similarity App
# Requires in R environment:
#   whitened_matrix     — 500 x 14 whitened data
#   dist_matrix         — 500 x 500 whitened Euclidean distances
#   updated_final_numeric — 500 x 14 pre-whitening numeric data
#   df_final_clean      — full data frame with Season & team name column
# Run: shiny::runApp("pl_similarity_app.R")
# ============================================================

library(shiny)
library(dplyr)
library(ggplot2)

# ── LOAD DATA ────────────────────────────────────────────────

whitened_matrix <- readRDS("data/whitened_matrix.rds")
dist_matrix <- readRDS("data/dist_matrix.rds")
updated_final_numeric <- readRDS("data/updated_final_numeric.rds")
df_final_clean <- readRDS("data/df_final_clean.rds")

# ── DATA SETUP ───────────────────────────────────────────────

team_labels <- paste(df_final_clean$Season, df_final_clean$`Team...78`)
n_teams     <- nrow(whitened_matrix)

# ── RAW DISTANCE MATRIX (z-score standardised, pre-whitening) ──
raw_dist_matrix <- as.matrix(dist(scale(updated_final_numeric)))

# ── HELPER FUNCTIONS ─────────────────────────────────────────
make_pairs_df <- function(dm) {
  idx <- which(upper.tri(dm), arr.ind = TRUE)
  data.frame(
    label_i  = team_labels[idx[, 1]],
    label_j  = team_labels[idx[, 2]],
    distance = dm[idx],
    stringsAsFactors = FALSE
  )
}

make_profile_df <- function(dm) {
  tmp <- dm; diag(tmp) <- NA
  md  <- rowMeans(tmp, na.rm = TRUE)
  data.frame(label = team_labels, mean_dist = round(md, 4),
             stringsAsFactors = FALSE) |>
    dplyr::arrange(dplyr::desc(mean_dist))
}

# ── PRE-COMPUTE BOTH MODES (Tab 3) ───────────────────────────
whitened_pairs_df   <- make_pairs_df(dist_matrix)
raw_pairs_df        <- make_pairs_df(raw_dist_matrix)
whitened_profile_df <- make_profile_df(dist_matrix)
raw_profile_df      <- make_profile_df(raw_dist_matrix)

whitened_vec <- dist_matrix[upper.tri(dist_matrix)]
raw_vec      <- raw_dist_matrix[upper.tri(raw_dist_matrix)]

# ── PRE-COMPUTE TAB 4 DATA ────────────────────────────────────
extracted_team_name <- sub("^[0-9]{4}-[0-9]{4} ", "", team_labels)
extracted_year_num  <- as.numeric(
  sub("^[0-9]{4}-([0-9]{4}) .*", "\\1", team_labels))
unique_team_names   <- sort(unique(extracted_team_name))

avg_consec_for_dm <- function(dm) {
  round(mean(unlist(lapply(unique_team_names, function(tm) {
    idx <- which(extracted_team_name == tm)
    if (length(idx) < 2) return(NULL)
    idx_s <- idx[order(extracted_year_num[idx])]
    sapply(seq_len(length(idx_s) - 1),
           function(k) dm[idx_s[k], idx_s[k + 1]])
  })), na.rm = TRUE), 3)
}
whitened_avg_consec <- avg_consec_for_dm(dist_matrix)
raw_avg_consec      <- avg_consec_for_dm(raw_dist_matrix)

# ── UI ───────────────────────────────────────────────────────
ui <- fluidPage(
  
  tags$head(tags$style(HTML("

    @import url('https://fonts.googleapis.com/css2?family=DM+Sans:wght@400;500;600&family=DM+Mono:wght@400;500&display=swap');

    * { box-sizing: border-box; }
    body {
      font-family: 'DM Sans', sans-serif;
      background: #0d1117; color: #e6edf3; margin: 0; padding: 0;
    }

    /* ── Header ── */
    .app-header {
      background: #0d1117; border-bottom: 1px solid #21262d;
      padding: 20px 36px; display: flex; align-items: baseline; gap: 14px;
    }
    .app-title  { font-size:20px; font-weight:600; color:#e6edf3;
                  letter-spacing:-0.3px; margin:0; }
    .app-subtitle { font-size:13px; color:#7d8590; margin:0; }
    .pl-dot { width:10px; height:10px; background:#3fb950; border-radius:50%;
              display:inline-block; margin-right:8px; }

    /* ── Mode bar ── */
    .mode-bar {
      background: #0d1117; border-bottom: 1px solid #21262d;
      padding: 8px 36px; display: flex; align-items: center;
      gap: 28px; flex-wrap: wrap;
    }
    .mode-group { display:flex; align-items:center; gap:10px; }
    .mode-group-label { font-size:12px; color:#7d8590; font-weight:500;
                        white-space:nowrap; }
    .mode-bar .shiny-input-radiogroup { margin:0; }
    .mode-bar .shiny-options-group { display:flex; gap:0; margin:0; }
    .mode-bar .radio-inline {
      background: #161b22; border: 1px solid #30363d;
      padding: 4px 14px; font-size:12px; font-weight:500;
      color:#7d8590; cursor:pointer; margin:0; transition:all 0.15s;
    }
    .mode-bar .radio-inline:first-child {
      border-radius: 6px 0 0 6px; border-right: none;
    }
    .mode-bar .radio-inline:last-child {
      border-radius: 0 6px 6px 0;
    }
    .mode-bar .radio-inline input[type=radio] { display:none; }
    .mode-bar .radio-inline.active-pill {
      background:#238636; border-color:#2ea043; color:#fff;
    }
    .mode-divider { width:1px; height:28px; background:#30363d; }

    /* ── Tabs ── */
    .nav-tabs {
      border-bottom: 1px solid #21262d !important;
      background: #0d1117; padding: 0 36px; margin-bottom:0 !important;
    }
    .nav-tabs > li > a {
      font-family:'DM Sans',sans-serif; font-size:13px; font-weight:500;
      color:#7d8590 !important; border:none !important;
      border-bottom:2px solid transparent !important;
      background:transparent !important; padding:12px 16px !important;
      border-radius:0 !important; margin:0 !important;
    }
    .nav-tabs > li.active > a, .nav-tabs > li > a:hover {
      color:#e6edf3 !important;
      border-bottom:2px solid #3fb950 !important;
      background:transparent !important;
    }
    .tab-content { padding:32px 36px; background:#0d1117; }

    /* ── Cards ── */
    .card {
      background:#161b22; border:1px solid #21262d;
      border-radius:8px; padding:24px; margin-bottom:20px;
    }
    .card-label {
      font-size:11px; font-weight:600; letter-spacing:0.8px;
      text-transform:uppercase; color:#7d8590; margin-bottom:10px;
    }

    /* ── Inputs ── */
    .selectize-input, .selectize-dropdown {
      font-family:'DM Sans',sans-serif !important; font-size:14px !important;
      background:#0d1117 !important; border:1px solid #30363d !important;
      color:#e6edf3 !important; border-radius:6px !important;
    }
    .selectize-dropdown { border-top:none !important; }
    .selectize-dropdown .option { color:#e6edf3 !important; }
    .selectize-dropdown .option:hover,
    .selectize-dropdown .active { background:#21262d !important; }
    .form-control {
      background:#0d1117 !important; border:1px solid #30363d !important;
      color:#e6edf3 !important; border-radius:6px !important;
      font-family:'DM Sans',sans-serif !important;
    }
    label { font-size:13px; font-weight:500; color:#7d8590; }

    /* ── Buttons ── */
    .btn-primary {
      background:#238636 !important; border:1px solid #2ea043 !important;
      color:#fff !important; font-family:'DM Sans',sans-serif !important;
      font-size:13px !important; font-weight:500 !important;
      border-radius:6px !important; padding:7px 16px !important;
    }
    .btn-primary:hover { background:#2ea043 !important; }

    /* ── Results table ── */
    .results-table { width:100%; border-collapse:collapse; font-size:13px; }
    .results-table th {
      text-align:left; padding:10px 14px; border-bottom:1px solid #21262d;
      font-size:11px; font-weight:600; letter-spacing:0.6px;
      text-transform:uppercase; color:#7d8590;
    }
    .results-table td {
      padding:11px 14px; border-bottom:1px solid #161b22;
      color:#e6edf3; vertical-align:middle;
    }
    .results-table tr:last-child td { border-bottom:none; }
    .results-table tr:hover td { background:#1c2128; }
    .rank-badge {
      font-family:'DM Mono',monospace; font-size:11px;
      color:#7d8590; width:28px; display:inline-block;
    }
    .dist-bar-wrap { display:flex; align-items:center; gap:10px; }
    .dist-bar-bg {
      flex:1; height:4px; background:#21262d;
      border-radius:2px; overflow:hidden;
    }
    .dist-bar-fill { height:100%; background:#3fb950; border-radius:2px; }
    .dist-value {
      font-family:'DM Mono',monospace; font-size:12px;
      color:#3fb950; white-space:nowrap;
    }

    /* ── Score card ── */
    .score-display { text-align:center; padding:32px 0; }
    .score-number {
      font-family:'DM Mono',monospace; font-size:56px; font-weight:500;
      line-height:1; margin-bottom:8px;
    }
    .score-label { font-size:13px; color:#7d8590; margin-bottom:24px; }
    .similarity-band {
      display:inline-block; padding:4px 14px; border-radius:99px;
      font-size:12px; font-weight:600;
    }
    .band-very-close { background:#1a4731; color:#3fb950; }
    .band-close      { background:#1a3a1a; color:#7ee787; }
    .band-moderate   { background:#3b2f05; color:#e3b341; }
    .band-distant    { background:#3b1010; color:#f85149; }

    /* ── Plot area ── */
    .plot-card {
      background:#161b22; border:1px solid #21262d;
      border-radius:8px; padding:20px;
    }

    /* ── Two-col layout ── */
    .two-col { display:grid; grid-template-columns:1fr 1fr; gap:20px; }
    @media (max-width:700px) { .two-col { grid-template-columns:1fr; } }

    /* ── Section header ── */
    .section-header {
      font-size:15px; font-weight:600; color:#e6edf3; margin:0 0 16px 0;
    }
  "))),
  
  # JS to keep pill buttons visually active
  tags$script(HTML("
    $(document).on('shiny:inputchanged', function(e) {
      if (e.name === 'dist_mode' || e.name === 'score_mode') {
        setTimeout(function() {
          ['dist_mode','score_mode'].forEach(function(id) {
            var grp = $('#' + id);
            grp.find('.radio-inline').each(function() {
              var inp = $(this).find('input[type=radio]');
              if (inp.is(':checked')) $(this).addClass('active-pill');
              else $(this).removeClass('active-pill');
            });
          });
        }, 10);
      }
    });
    $(document).ready(function() {
      setTimeout(function() {
        ['dist_mode','score_mode'].forEach(function(id) {
          $('#' + id + ' .radio-inline').each(function() {\r\n            var inp = $(this).find('input[type=radio]');\r\n            if (inp.is(':checked')) $(this).addClass('active-pill');\r\n            else $(this).removeClass('active-pill');\r\n          });
        });
      }, 100);
    });
  ")),
  
  # ── Header ────────────────────────────────────────────────
  div(class = "app-header",
      tags$span(class = "pl-dot"),
      tags$h1(class = "app-title", "PL Team Similarity"),
      tags$p(class = "app-subtitle",
             "500 teams \u00b7 25 seasons \u00b7 14 features")
  ),
  
  # ── Mode bar ──────────────────────────────────────────────
  div(class = "mode-bar",
      
      div(class = "mode-group",
          tags$span(class = "mode-group-label", "Distance mode:"),
          radioButtons("dist_mode", label = NULL, inline = TRUE,
                       choices  = c("Euclidean" = "raw", "Whitened" = "whitened"),
                       selected = "whitened")
      ),
      
      div(class = "mode-divider"),
      
      div(class = "mode-group",
          tags$span(class = "mode-group-label", "Score display:"),
          radioButtons("score_mode", label = NULL, inline = TRUE,
                       choices  = c("Raw distance" = "dist", "Percentile score" = "scaled"),
                       selected = "dist")
      )
  ),
  
  # ── Tabs ──────────────────────────────────────────────────
  tabsetPanel(id = "tabs",
              
              # ── TAB 1 ─────────────────────────────────────────────
              tabPanel("Find Similar Teams",
                       div(class = "tab-content",
                           div(class = "card",
                               div(class = "card-label", "Select reference team"),
                               fluidRow(
                                 column(6,
                                        selectizeInput("ref_team", label = NULL,
                                                       choices = setNames(seq_along(team_labels), team_labels),
                                                       options = list(
                                                         placeholder = "Search for a team \u2014 e.g. 2024-2025 Arsenal",
                                                         maxOptions  = 500))
                                 ),
                                 column(3,
                                        numericInput("n_results", "Number of results",
                                                     value = 10, min = 5, max = 50, step = 5)
                                 ),
                                 column(3,
                                        div(style = "margin-top:24px;",
                                            actionButton("search_btn", "Find Similar Teams",
                                                         class = "btn btn-primary", width = "100%"))
                                 )
                               )
                           ),
                           uiOutput("similarity_results"),
                           uiOutput("distance_plot_ui")
                       )
              ),
              
              # ── TAB 2 ─────────────────────────────────────────────
              tabPanel("Compare Two Teams",
                       div(class = "tab-content",
                           div(class = "card",
                               div(class = "card-label", "Select two teams to compare"),
                               div(class = "two-col",
                                   div(
                                     tags$label("Team A"),
                                     selectizeInput("team_a", label = NULL,
                                                    choices = setNames(seq_along(team_labels), team_labels),
                                                    options = list(placeholder = "Search team A...", maxOptions = 500))
                                   ),
                                   div(
                                     tags$label("Team B"),
                                     selectizeInput("team_b", label = NULL,
                                                    choices = setNames(seq_along(team_labels), team_labels),
                                                    options = list(placeholder = "Search team B...", maxOptions = 500))
                                   )
                               ),
                               div(style = "text-align:right;",
                                   actionButton("compare_btn", "Compare Teams",
                                                class = "btn btn-primary"))
                           ),
                           uiOutput("comparison_result")
                       )
              ),
              
              # ── TAB 3 ─────────────────────────────────────────────
              tabPanel("Dataset Overview",
                       div(class = "tab-content",
                           
                           div(class = "card",
                               div(class = "card-label", "Dataset summary"),
                               div(style = "display:flex;justify-content:space-around;text-align:center;flex-wrap:wrap;gap:16px;",
                                   div(
                                     div(class = "card-label", "Total pairs"),
                                     tags$p(style = "font-size:22px;font-weight:600;margin:0;color:#e6edf3;",
                                            "124,750"),
                                     tags$p(style = "font-size:12px;color:#7d8590;margin:4px 0 0;",
                                            "unique comparisons")
                                   ),
                                   div(
                                     div(class = "card-label", "Mean score"),
                                     tags$p(style = "font-size:22px;font-weight:600;margin:0;color:#e6edf3;",
                                            uiOutput("overview_mean", inline = TRUE)),
                                     tags$p(style = "font-size:12px;color:#7d8590;margin:4px 0 0;",
                                            "all pairs")
                                   ),
                                   div(
                                     div(class = "card-label", "Median score"),
                                     tags$p(style = "font-size:22px;font-weight:600;margin:0;color:#e6edf3;",
                                            uiOutput("overview_median", inline = TRUE)),
                                     tags$p(style = "font-size:12px;color:#7d8590;margin:4px 0 0;",
                                            "50th percentile")
                                   ),
                                   div(
                                     div(class = "card-label", "Best similarity"),
                                     tags$p(style = "font-size:22px;font-weight:600;margin:0;color:#3fb950;",
                                            uiOutput("overview_best", inline = TRUE)),
                                     tags$p(style = "font-size:12px;color:#7d8590;margin:4px 0 0;",
                                            "closest pair")
                                   ),
                                   div(
                                     div(class = "card-label", "Worst similarity"),
                                     tags$p(style = "font-size:22px;font-weight:600;margin:0;color:#f85149;",
                                            uiOutput("overview_worst", inline = TRUE)),
                                     tags$p(style = "font-size:12px;color:#7d8590;margin:4px 0 0;",
                                            "most distant pair")
                                   )
                               )
                           ),
                           
                           div(class = "plot-card", style = "margin-bottom:20px;",
                               tags$p(class = "section-header",
                                      "Distribution of all pairwise scores"),
                               plotOutput("dist_histogram", height = "200px")
                           ),
                           
                           div(class = "card",
                               div(class = "card-label", "Number of pairs to show"),
                               sliderInput("n_extremes", label = NULL,
                                           min = 5, max = 25, value = 10, step = 5, width = "320px")
                           ),
                           
                           div(class = "two-col",
                               div(class = "card",
                                   tags$p(class = "section-header", "Most similar pairs globally"),
                                   tags$table(class = "results-table",
                                              tags$thead(tags$tr(
                                                tags$th("#"), tags$th("Team A"),
                                                tags$th("Team B"), tags$th("Score")
                                              )),
                                              tags$tbody(uiOutput("top_similar_rows"))
                                   )
                               ),
                               div(class = "card",
                                   tags$p(class = "section-header", "Most dissimilar pairs globally"),
                                   tags$table(class = "results-table",
                                              tags$thead(tags$tr(
                                                tags$th("#"), tags$th("Team A"),
                                                tags$th("Team B"), tags$th("Score")
                                              )),
                                              tags$tbody(uiOutput("top_dissimilar_rows"))
                                   )
                               )
                           ),
                           
                           div(class = "two-col",
                               div(class = "card",
                                   tags$p(class = "section-header", "Most statistically unique teams"),
                                   tags$p(style = "font-size:12px;color:#7d8590;margin:-8px 0 14px;",
                                          "Highest average distance to all other teams"),
                                   tags$table(class = "results-table",
                                              tags$thead(tags$tr(
                                                tags$th("#"), tags$th("Team"), tags$th("Avg score")
                                              )),
                                              tags$tbody(uiOutput("most_unique_rows"))
                                   )
                               ),
                               div(class = "card",
                                   tags$p(class = "section-header", "Most statistically typical teams"),
                                   tags$p(style = "font-size:12px;color:#7d8590;margin:-8px 0 14px;",
                                          "Closest to the PL average statistical profile"),
                                   tags$table(class = "results-table",
                                              tags$thead(tags$tr(
                                                tags$th("#"), tags$th("Team"), tags$th("Avg score")
                                              )),
                                              tags$tbody(uiOutput("most_typical_rows"))
                                   )
                               )
                           )
                       )
              ),
              
              # ── TAB 4 ─────────────────────────────────────────────
              tabPanel("Season Journey",
                       div(class = "tab-content",
                           div(class = "card",
                               div(class = "card-label", "Trace a team through their PL seasons"),
                               fluidRow(
                                 column(7,
                                        selectizeInput("journey_team", label = NULL,
                                                       choices  = c("Select a team..." = "", unique_team_names),
                                                       options  = list(placeholder = "e.g. Chelsea, Arsenal, Sunderland",
                                                                       maxOptions  = 100))
                                 ),
                                 column(3,
                                        div(style = "margin-top:24px;",
                                            actionButton("journey_btn", "Trace Journey",
                                                         class = "btn btn-primary", width = "100%"))
                                 )
                               ),
                               tags$p(style = "font-size:12px;color:#7d8590;margin:8px 0 0;",
                                      "Relegated teams appear as dashed yellow segments on return.
                  Teams that never came back have their line end at their last season.")
                           ),
                           uiOutput("journey_stats"),
                           uiOutput("journey_plot_ui"),
                           uiOutput("journey_table")
                       )
              )
  )
)

# ── SERVER ───────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # ── Global reactive helpers ─────────────────────────────────
  
  active_dm <- reactive({
    if (input$dist_mode == "whitened") dist_matrix else raw_dist_matrix
  })
  
  active_vec <- reactive({
    if (input$dist_mode == "whitened") whitened_vec else raw_vec
  })
  
  active_pairs_df <- reactive({
    if (input$dist_mode == "whitened") whitened_pairs_df else raw_pairs_df
  })
  
  active_profile_df <- reactive({
    if (input$dist_mode == "whitened") whitened_profile_df else raw_profile_df
  })
  
  active_avg_consec <- reactive({
    if (input$dist_mode == "whitened") whitened_avg_consec else raw_avg_consec
  })
  
  d_min  <- reactive({ min(active_vec()) })
  d_max  <- reactive({ max(active_vec()) })
  
  # Precompute ECDF for the active distance vector — O(n log n) once,
  # then every score lookup is O(log n)
  active_ecdf <- reactive({ ecdf(active_vec()) })
  
  # Distance → percentile similarity score (full precision float, 0–100)
  # score = fraction of all pairs MORE distant than d, × 100
  # → closest pair ≈ 100, median ≈ 50, most distant ≈ 0
  # No rounding here — rounding happens only at display time in fmt()
  to_score <- function(d, ecdf_fn) {
    100 * (1 - ecdf_fn(d))
  }
  
  # Format a distance for display (raw 4dp or percentile score to 2dp)
  fmt <- function(d) {
    if (input$score_mode == "scaled")
      sprintf("%.2f", to_score(d, active_ecdf()))
    else
      sprintf("%.4f", d)
  }
  
  # Colour for the big score number — mirrors get_band percentile thresholds
  score_color <- function(d) {
    v   <- active_vec()
    q10 <- quantile(v, 0.10)
    q30 <- quantile(v, 0.30)
    q60 <- quantile(v, 0.60)
    if      (d <= q10) "#3fb950"
    else if (d <= q30) "#7ee787"
    else if (d <= q60) "#e3b341"
    else               "#f85149"
  }
  
  # Band label — always percentile-based on the active distance vector
  # so switching display mode never changes the label for the same pair
  get_band <- function(d) {
    v   <- active_vec()
    q10 <- quantile(v, 0.10)
    q30 <- quantile(v, 0.30)
    q60 <- quantile(v, 0.60)
    if      (d <= q10) list(label = "Very similar",         cls = "band-very-close")
    else if (d <= q30) list(label = "Similar",              cls = "band-close")
    else if (d <= q60) list(label = "Moderately different", cls = "band-moderate")
    else               list(label = "Quite different",      cls = "band-distant")
  }
  
  # Bar fill %: always "longer = more similar"
  bar_pct <- function(d, ref_max) {
    if (input$score_mode == "scaled")
      to_score(d, active_ecdf()) # percentile score 0–100
    else
      round(100 * (ref_max - d) / ref_max)   # invert distance
  }
  
  # ── TAB 1 ──────────────────────────────────────────────────
  similar_results <- eventReactive(input$search_btn, {
    req(input$ref_team)
    ref_idx <- as.integer(input$ref_team)
    n       <- as.integer(input$n_results)
    dm      <- active_dm()
    dists   <- dm[ref_idx, ]
    dists[ref_idx] <- Inf
    top_idx <- order(dists)[1:n]
    data.frame(rank = seq_len(n), label = team_labels[top_idx],
               distance = round(dists[top_idx], 6), stringsAsFactors = FALSE)
  })
  
  output$similarity_results <- renderUI({
    req(similar_results())
    res     <- similar_results()
    ref_lbl <- team_labels[as.integer(input$ref_team)]
    max_d   <- max(res$distance)
    
    rows <- lapply(seq_len(nrow(res)), function(i) {
      d    <- res$distance[i]
      band <- get_band(d)
      pct  <- bar_pct(d, max_d)
      tags$tr(
        tags$td(tags$span(class = "rank-badge", sprintf("#%02d", res$rank[i]))),
        tags$td(res$label[i]),
        tags$td(
          div(class = "dist-bar-wrap",
              div(class = "dist-bar-bg",
                  div(class = "dist-bar-fill", style = sprintf("width:%.0f%%", pct))),
              span(class = "dist-value", fmt(d)))
        ),
        tags$td(tags$span(class = paste("similarity-band", band$cls), band$label))
      )
    })
    
    div(class = "card",
        tags$p(class = "section-header",
               sprintf("Teams most similar to %s", ref_lbl)),
        tags$table(class = "results-table",
                   tags$thead(tags$tr(
                     tags$th("#"), tags$th("Team"), tags$th("Score"), tags$th("Similarity")
                   )),
                   tags$tbody(rows)
        )
    )
  })
  
  output$distance_plot_ui <- renderUI({
    req(similar_results())
    div(class = "plot-card",
        tags$p(class = "section-header", "Score profile"),
        plotOutput("distance_plot", height = "220px"))
  })
  
  output$distance_plot <- renderPlot({
    req(similar_results())
    res <- similar_results()
    res$score <- sapply(res$distance, fmt)
    res$val   <- if (input$score_mode == "scaled")
      sapply(res$distance, function(d) to_score(d, active_ecdf()))
    else
      res$distance
    res$label <- factor(res$label, levels = rev(res$label))
    
    ggplot(res, aes(x = val, y = label)) +
      geom_segment(aes(xend = if(input$score_mode=="scaled") 0 else max(val),
                       yend = label),
                   color = "#21262d", linewidth = 0.8) +
      geom_point(color = "#3fb950", size = 3.5) +
      labs(x = if (input$score_mode == "scaled")
        "Percentile similarity score (0\u2013100)" else
          paste0("Distance (", ifelse(input$dist_mode == "raw", "Euclidean", "Whitened"), ")"),
        y = NULL) +
      theme_minimal(base_family = "sans") +
      theme(
        plot.background  = element_rect(fill = "#161b22", color = NA),
        panel.background = element_rect(fill = "#161b22", color = NA),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "#21262d"),
        panel.grid.minor   = element_blank(),
        axis.text  = element_text(color = "#7d8590", size = 11),
        axis.title = element_text(color = "#7d8590", size = 11)
      )
  }, bg = "#161b22")
  
  # ── TAB 2 ──────────────────────────────────────────────────
  comparison <- eventReactive(input$compare_btn, {
    req(input$team_a, input$team_b)
    i <- as.integer(input$team_a)
    j <- as.integer(input$team_b)
    validate(need(i != j, "Please select two different teams."))
    dm <- active_dm()
    list(label_a = team_labels[i], label_b = team_labels[j],
         distance = round(dm[i, j], 6))
  })
  
  output$comparison_result <- renderUI({
    req(comparison())
    comp <- comparison()
    band <- get_band(comp$distance)
    v    <- active_vec()
    pct_rank    <- round(100 * mean(v > comp$distance), 1)
    display_val <- fmt(comp$distance)
    avg_display <- if (input$score_mode == "scaled")
      sprintf("%.2f", to_score(mean(v), active_ecdf()))
    else sprintf("%.2f", mean(v))
    
    div(
      div(class = "card",
          div(class = "two-col",
              div(div(class = "card-label", "Team A"),
                  tags$p(style = "font-size:16px;font-weight:600;margin:0;",
                         comp$label_a)),
              div(div(class = "card-label", "Team B"),
                  tags$p(style = "font-size:16px;font-weight:600;margin:0;",
                         comp$label_b))
          )
      ),
      div(class = "card",
          div(class = "score-display",
              div(class = "score-number",
                  style = paste0("color:", score_color(comp$distance), ";"),
                  display_val),
              div(class = "score-label",
                  if (input$score_mode == "scaled")
                    "Percentile similarity score (100 = most similar)"
                  else paste0(ifelse(input$dist_mode == "raw", "Euclidean", "Whitened"), " distance")),
              tags$span(class = paste("similarity-band", band$cls), band$label)
          ),
          tags$hr(style = "border-color:#21262d;margin:20px 0;"),
          div(style = "display:flex;justify-content:space-around;text-align:center;",
              div(
                div(class = "card-label",
                    if (input$score_mode == "scaled") "Better than" else "More similar than"),
                tags$p(style = "font-size:22px;font-weight:600;margin:0;color:#e6edf3;",
                       paste0(pct_rank, "%")),
                tags$p(style = "font-size:12px;color:#7d8590;margin:4px 0 0;",
                       "of all 124,750 pairs")
              ),
              div(
                div(class = "card-label", "Year gap"),
                tags$p(style = "font-size:22px;font-weight:600;margin:0;color:#e6edf3;",
                       paste0(abs(
                         as.numeric(sub(".*-(\\d{4}).*","\\1", comp$label_a)) -
                           as.numeric(sub(".*-(\\d{4}).*","\\1", comp$label_b))),
                         " yrs")),
                tags$p(style = "font-size:12px;color:#7d8590;margin:4px 0 0;",
                       "between seasons")
              ),
              div(
                div(class = "card-label", "Dataset average"),
                tags$p(style = "font-size:22px;font-weight:600;margin:0;color:#e6edf3;",
                       avg_display),
                tags$p(style = "font-size:12px;color:#7d8590;margin:4px 0 0;",
                       if (input$score_mode == "scaled") "avg similarity score"
                       else "avg distance")
              )
          )
      )
    )
  })
  
  # ── TAB 3 ──────────────────────────────────────────────────
  overview_fmt <- function(d) {
    if (input$score_mode == "scaled")
      sprintf("%.2f", to_score(d, active_ecdf()))
    else sprintf("%.3f", d)
  }
  
  output$overview_mean   <- renderUI({ tags$span(overview_fmt(mean(active_vec()))) })
  output$overview_median <- renderUI({ tags$span(overview_fmt(median(active_vec()))) })
  output$overview_best   <- renderUI({ tags$span(overview_fmt(min(active_vec()))) })
  output$overview_worst  <- renderUI({ tags$span(overview_fmt(max(active_vec()))) })
  
  output$dist_histogram <- renderPlot({
    v <- active_vec()
    plot_v <- if (input$score_mode == "scaled") {
      ef <- active_ecdf()
      100 * (1 - ef(v))   # full precision — no rounding, gives uniform histogram
    } else v
    mu  <- mean(plot_v); med <- median(plot_v)
    x_lab <- if (input$score_mode == "scaled")
      "Percentile similarity score (0\u2013100)" else
        paste0("Distance (", ifelse(input$dist_mode == "raw", "Euclidean", "Whitened"), ")")
    
    ggplot(data.frame(x = plot_v), aes(x = x)) +
      geom_histogram(bins = 80, fill = "#238636", color = NA, alpha = 0.8) +
      geom_vline(xintercept = mu,  color = "#e6edf3", linewidth = 0.8,
                 linetype = "dashed") +
      geom_vline(xintercept = med, color = "#e3b341", linewidth = 0.8,
                 linetype = "dashed") +
      annotate("text", x = mu + diff(range(plot_v))*0.02, y = Inf, vjust = 1.8,
               label = paste("mean", round(mu, 1)), color = "#e6edf3",
               size = 3.2, hjust = 0) +
      annotate("text", x = med - diff(range(plot_v))*0.02, y = Inf, vjust = 1.8,
               label = paste("median", round(med, 1)), color = "#e3b341",
               size = 3.2, hjust = 1) +
      labs(x = x_lab, y = "Number of pairs") +
      theme_minimal(base_family = "sans") +
      theme(
        plot.background  = element_rect(fill = "#161b22", color = NA),
        panel.background = element_rect(fill = "#161b22", color = NA),
        panel.grid.major = element_line(color = "#21262d"),
        panel.grid.minor = element_blank(),
        axis.text  = element_text(color = "#7d8590", size = 11),
        axis.title = element_text(color = "#7d8590", size = 11)
      )
  }, bg = "#161b22")
  
  make_pair_rows <- function(df, color) {
    lapply(seq_len(nrow(df)), function(i) {
      tags$tr(
        tags$td(tags$span(class = "rank-badge", sprintf("#%02d", i))),
        tags$td(df$label_i[i]), tags$td(df$label_j[i]),
        tags$td(tags$span(class = "dist-value",
                          style = paste0("color:", color, ";"),
                          fmt(df$distance[i])))
      )
    })
  }
  
  output$top_similar_rows <- renderUI({
    n   <- input$n_extremes
    top <- active_pairs_df()[order(active_pairs_df()$distance), ][1:n, ]
    make_pair_rows(top, "#3fb950")
  })
  
  output$top_dissimilar_rows <- renderUI({
    n   <- input$n_extremes
    bot <- active_pairs_df()[order(-active_pairs_df()$distance), ][1:n, ]
    make_pair_rows(bot, "#f85149")
  })
  
  make_team_rows <- function(df, color) {
    lapply(seq_len(nrow(df)), function(i) {
      tags$tr(
        tags$td(tags$span(class = "rank-badge", sprintf("#%02d", i))),
        tags$td(df$label[i]),
        tags$td(tags$span(class = "dist-value",
                          style = paste0("color:", color, ";"),
                          fmt(df$mean_dist[i])))
      )
    })
  }
  
  output$most_unique_rows <- renderUI({
    n   <- input$n_extremes
    top <- head(active_profile_df(), n)
    make_team_rows(top, "#f85149")
  })
  
  output$most_typical_rows <- renderUI({
    n   <- input$n_extremes
    bot <- tail(active_profile_df(), n) |> dplyr::arrange(mean_dist)
    make_team_rows(bot, "#3fb950")
  })
  
  # ── TAB 4 ──────────────────────────────────────────────────
  journey_data <- eventReactive(input$journey_btn, {
    req(input$journey_team)
    tm <- input$journey_team
    validate(need(nchar(tm) > 0, "Please select a team."))
    idx <- which(extracted_team_name == tm)
    validate(need(length(idx) >= 2,
                  "This team has fewer than 2 seasons in the dataset."))
    dm    <- active_dm()
    yrs   <- extracted_year_num[idx]
    ord   <- order(yrs)
    idx_s <- idx[ord]; yrs_s <- yrs[ord]; lbl_s <- team_labels[idx_s]
    n     <- length(idx_s)
    dists     <- numeric(n - 1)
    year_gaps <- integer(n - 1)
    for (k in seq_len(n - 1)) {
      dists[k]     <- dm[idx_s[k], idx_s[k + 1]]
      year_gaps[k] <- yrs_s[k + 1] - yrs_s[k]
    }
    list(team = tm, years = yrs_s, labels = lbl_s,
         dists = round(dists, 6), year_gaps = year_gaps,
         n_seasons = n, n_gaps = sum(year_gaps > 1))
  })
  
  output$journey_stats <- renderUI({
    req(journey_data())
    jd <- journey_data()
    mk  <- which.max(jd$dists); sk <- which.min(jd$dists)
    
    div(class = "card",
        div(style = "display:flex;justify-content:space-around;text-align:center;
                   flex-wrap:wrap;gap:16px;",
            div(div(class = "card-label", "Seasons in PL"),
                tags$p(style = "font-size:22px;font-weight:600;margin:0;color:#e6edf3;",
                       jd$n_seasons),
                tags$p(style = "font-size:12px;color:#7d8590;margin:4px 0 0;",
                       paste0(min(jd$years), " \u2013 ", max(jd$years)))),
            div(div(class = "card-label", "Avg year-to-year score"),
                tags$p(style = "font-size:22px;font-weight:600;margin:0;color:#e6edf3;",
                       fmt(mean(jd$dists))),
                tags$p(style = "font-size:12px;color:#7d8590;margin:4px 0 0;",
                       paste0("PL avg: ", fmt(active_avg_consec())))),
            div(div(class = "card-label", "Biggest change"),
                tags$p(style = "font-size:22px;font-weight:600;margin:0;color:#f85149;",
                       fmt(jd$dists[mk])),
                tags$p(style = "font-size:12px;color:#7d8590;margin:4px 0 0;",
                       paste0(jd$years[mk], " \u2192 ", jd$years[mk + 1]))),
            div(div(class = "card-label", "Most stable transition"),
                tags$p(style = "font-size:22px;font-weight:600;margin:0;color:#3fb950;",
                       fmt(jd$dists[sk])),
                tags$p(style = "font-size:12px;color:#7d8590;margin:4px 0 0;",
                       paste0(jd$years[sk], " \u2192 ", jd$years[sk + 1]))),
            div(div(class = "card-label", "Relegation gaps"),
                tags$p(style = "font-size:22px;font-weight:600;margin:0;color:#e3b341;",
                       jd$n_gaps),
                tags$p(style = "font-size:12px;color:#7d8590;margin:4px 0 0;",
                       if (jd$n_gaps == 0) "Never relegated" else "gap(s) detected"))
        )
    )
  })
  
  output$journey_plot_ui <- renderUI({
    req(journey_data())
    div(class = "plot-card", style = "margin-bottom:20px;",
        tags$p(class = "section-header",
               paste0(journey_data()$team,
                      " \u2014 score vs previous PL season")),
        plotOutput("journey_plot", height = "280px"))
  })
  
  output$journey_plot <- renderPlot({
    req(journey_data())
    jd  <- journey_data()
    ac  <- active_avg_consec()
    ef  <- active_ecdf()
    
    # Convert distances to display values for the y axis
    disp <- if (input$score_mode == "scaled")
      sapply(jd$dists, function(d) to_score(d, ef))
    else jd$dists
    ac_disp <- if (input$score_mode == "scaled")
      to_score(ac, ef) else ac
    
    seg_df <- data.frame(x = jd$years[-jd$n_seasons],
                         xend = jd$years[-1], y = disp, gap = jd$year_gaps > 1)
    pt_df  <- data.frame(x = jd$years[-1], y = disp, gap = jd$year_gaps > 1)
    y_lab  <- if (input$score_mode == "scaled")
      "Percentile similarity vs previous season" else
        paste0("Distance vs previous season (", ifelse(input$dist_mode == "raw", "Euclidean", "Whitened"), ")")
    
    p <- ggplot() +
      geom_hline(yintercept = ac_disp, color = "#e6edf3",
                 linewidth = 0.6, linetype = "dashed") +
      annotate("text", x = min(jd$years) + 0.2,
               y = ac_disp + diff(range(disp)) * 0.04,
               label = paste("PL avg:", round(ac_disp, 1)),
               color = "#7d8590", size = 3, hjust = 0)
    
    if (any(!seg_df$gap))
      p <- p + geom_segment(data = seg_df[!seg_df$gap, ],
                            aes(x=x,xend=xend,y=y,yend=y),
                            color="#3fb950", linewidth=1.4)
    if (any(seg_df$gap))
      p <- p + geom_segment(data = seg_df[seg_df$gap, ],
                            aes(x=x,xend=xend,y=y,yend=y),
                            color="#e3b341", linewidth=1.4, linetype="dashed")
    
    p + geom_segment(data = pt_df,
                     aes(x=x,xend=x,y=min(disp)-diff(range(disp))*0.05,yend=y),
                     color="#21262d", linewidth=0.5) +
      geom_point(data = pt_df[!pt_df$gap,], aes(x=x,y=y),
                 color="#3fb950", size=3.5) +
      geom_point(data = pt_df[pt_df$gap,],  aes(x=x,y=y),
                 color="#e3b341", size=5, shape=18) +
      scale_x_continuous(breaks = seq(min(jd$years), max(jd$years), by=2)) +
      labs(x = "Season (end year)", y = y_lab) +
      theme_minimal(base_family = "sans") +
      theme(
        plot.background  = element_rect(fill="#161b22", color=NA),
        panel.background = element_rect(fill="#161b22", color=NA),
        panel.grid.major = element_line(color="#21262d"),
        panel.grid.minor = element_blank(),
        axis.text  = element_text(color="#7d8590", size=11),
        axis.title = element_text(color="#7d8590", size=11)
      )
  }, bg = "#161b22")
  
  output$journey_table <- renderUI({
    req(journey_data())
    jd <- journey_data()
    
    rows <- lapply(seq_len(jd$n_seasons - 1), function(k) {
      d    <- jd$dists[k]; gp <- jd$year_gaps[k]; band <- get_band(d)
      gap_tag <- if (gp > 1) tags$span(
        style = "font-size:11px;color:#e3b341;margin-left:8px;",
        paste0("(", gp-1, " yr gap)")) else NULL
      
      tags$tr(
        tags$td(tags$span(class="rank-badge", sprintf("#%02d", k))),
        tags$td(jd$labels[k]),
        tags$td(jd$labels[k+1], gap_tag),
        tags$td(div(class="dist-bar-wrap",
                    div(class="dist-bar-bg",
                        div(class="dist-bar-fill",
                            style=sprintf("width:%.0f%%",
                                          bar_pct(d, max(jd$dists))))),
                    span(class="dist-value", fmt(d)))),
        tags$td(tags$span(class=paste("similarity-band",band$cls), band$label))
      )
    })
    
    div(class = "card",
        tags$p(class = "section-header", "Season-by-season transitions"),
        tags$p(style = "font-size:12px;color:#7d8590;margin:-8px 0 14px;",
               "\u25cf Green = consecutive \u25c6 Yellow = return after relegation"),
        tags$table(class = "results-table",
                   tags$thead(tags$tr(tags$th("#"), tags$th("From"), tags$th("To"),
                                      tags$th("Score"), tags$th("Similarity"))),
                   tags$tbody(rows)
        )
    )
  })
}

# ── Run ──────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)

