#' @import shiny
#' @import shinydashboard
#' @importFrom plotly plotlyOutput
#' @importFrom DT DTOutput
#' @export
app_ui <- function() {
  register_chico_www()
  register_chico_markdown_assets()
  load(chico_system_file("data", "phylum_name.rda"))

  level_choices <- c("Phylum", "Class", "Order", "Family", "Genus", "Species")

  editorial_hero <- function(section_label, title, dek, meta = character(0), tone = c("ink", "wine", "forest")) {
    tone <- match.arg(tone)
    tags$section(
      class = paste("ed-hero", paste0("ed-hero--", tone)),
      tags$div(class = "ed-hero__grain"),
      tags$div(
        class = "ed-hero__inner",
        tags$div(class = "ed-hero__section", section_label),
        tags$h1(class = "ed-hero__title", title),
        tags$p(class = "ed-hero__dek", dek),
        if (length(meta) > 0) {
          tags$div(
            class = "ed-hero__meta",
            lapply(meta, function(x) tags$span(class = "ed-meta-pill", x))
          )
        }
      )
    )
  }

  stat_strip <- function(items) {
    tags$div(
      class = "ed-stat-strip",
      lapply(seq_along(items), function(i) {
        item <- items[[i]]
        tags$div(
          class = "ed-stat",
          tags$div(class = "ed-stat__label", item$label),
          tags$div(class = "ed-stat__value", item$value),
          tags$div(class = "ed-stat__note", item$note)
        )
      })
    )
  }

  article_shell <- function(title, kicker = NULL, badge = NULL, body) {
    tags$section(
      class = "article-shell",
      tags$header(
        class = "article-shell__head",
        tags$div(
          class = "article-shell__titles",
          if (!is.null(kicker)) tags$div(class = "article-shell__kicker", kicker),
          tags$h2(class = "article-shell__title", title)
        ),
        if (!is.null(badge)) tags$div(class = "article-shell__badge", badge)
      ),
      tags$div(class = "article-shell__rule"),
      tags$div(class = "article-shell__body", body)
    )
  }

  control_panel <- function() {
    tags$aside(
      class = "editor-panel",
      tags$div(class = "editor-panel__label", "Analysis Controls"),
      tags$h3(class = "editor-panel__title", "Build a comparison"),
      tags$p(
        class = "editor-panel__text",
        "Choose a display method, set taxonomy and cohort grouping, then generate the figure and summary table."
      ),
      
      tags$div(
        class = "editor-block",
        tags$div(class = "editor-block__title", "Display method"),
        selectInput(
          inputId = "display_method",
          label = "Method",
          choices = c(
            "Single taxon distribution" = "single_taxon_boxplot",
            "Top-N taxa stacked bar" = "topn_stacked_bar"
          ),
          selected = "single_taxon_boxplot",
          multiple = FALSE
        ),
        conditionalPanel(
          condition = "input.display_method === 'topn_stacked_bar'",
          numericInput(
            inputId = "top_n_taxa",
            label = "Top N taxa",
            value = 10,
            min = 2,
            max = 30,
            step = 1
          )
        )
      ),

      tags$div(
        class = "editor-block",
        tags$div(class = "editor-block__title", "Taxonomy"),
        selectInput(
          "bacteria_level", "Level",
          choices = level_choices,
          selected = "Phylum",
          multiple = FALSE
        ),
        conditionalPanel(
          condition = "input.display_method !== 'topn_stacked_bar'",
          selectInput(
            inputId = "bacteria_name",
            label = "Bacteria",
            choices = phylum_name,
            selected = phylum_name[1]
          )
        )
      ),

      tags$div(
        class = "editor-block",
        tags$div(class = "editor-block__title", "Grouping"),
        selectInput(
          inputId = "group_type",
          label = "Group type",
          choices = c("Affect or not", "HPV risk", "HPV persistent"),
          selected = "Affect or not",
          multiple = FALSE
        ),
        selectInput(
          inputId = "group",
          label = "Group",
          choices = c("Negative", "Positive"),
          selected = c("Negative", "Positive"),
          multiple = TRUE
        )
      ),

      tags$div(
        class = "editor-action",
        actionButton("generate_plot", "Generate plot", icon = icon("play"), class = "ed-btn"),
        tags$p(
          class = "editor-action__note",
          tags$strong("Editorial note: "),
          "Both modes require at least two selected groups with available samples."
        )
      )
    )
  }

  dashboardPage(
    skin = "blue",
    dashboardHeader(
      title = tags$div(
        class = "masthead-brand",
        tags$span(class = "masthead-brand__name", "CHICO REVIEW"),
        tags$span(class = "masthead-brand__tag", "Clinical HPV microbiome atlas")
      ),
      titleWidth = 340
    ),
    dashboardSidebar(
      width = 340,
      sidebarMenu(
        id = "main_tabs",
        menuItem("About", tabName = "about", icon = icon("book")),
        menuItem("Data Visualization", tabName = "viz", icon = icon("area-chart")),
        menuItem("Authors", tabName = "authors", icon = icon("users"))
      )
    ),
    dashboardBody(
      tags$head(
        tags$link(
          rel = "stylesheet",
          href = "https://fonts.googleapis.com/css2?family=Cormorant+Garamond:wght@500;600;700&family=IBM+Plex+Sans:wght@400;500;600;700&display=swap"
        ),
        tags$style(HTML("\n          :root {\n            --paper: #f6f1e8;\n            --paper-2: #efe7da;\n            --ink: #1e1a17;\n            --muted: #6b6259;\n            --line: rgba(30, 26, 23, 0.14);\n            --deep: #1f1b19;\n            --wine: #5a2230;\n            --forest: #203b35;\n            --accent: #b0432f;\n            --card: rgba(255,255,255,0.58);\n            --shadow: 0 22px 46px rgba(20, 15, 10, 0.10);\n          }\n\n          body, .wrapper, .content-wrapper, .right-side, .main-sidebar {\n            font-family: 'IBM Plex Sans', 'Helvetica Neue', Arial, sans-serif;\n          }\n\n          .content-wrapper, .right-side {\n            background:\n              linear-gradient(180deg, rgba(255,255,255,0.25), rgba(255,255,255,0.10)),\n              radial-gradient(circle at 8% 6%, rgba(176, 67, 47, 0.05), transparent 26%),\n              radial-gradient(circle at 92% 8%, rgba(32, 59, 53, 0.06), transparent 28%),\n              var(--paper);\n          }\n\n          .content {\n            padding: 22px 22px 126px;\n          }\n\n          .skin-blue .wrapper, .skin-blue .main-sidebar, .skin-blue .left-side {\n            background: #141210;\n          }\n\n          .skin-blue .main-header .logo {\n            height: 74px;\n            background: #141210;\n            color: #f5efe5;\n            border-bottom: 1px solid rgba(255,255,255,0.05);\n          }\n\n          .skin-blue .main-header .navbar {\n            min-height: 74px;\n            background: rgba(246, 241, 232, 0.82);\n            border-bottom: 1px solid rgba(30,26,23,0.10);\n            backdrop-filter: blur(10px);\n            box-shadow: 0 8px 24px rgba(20, 15, 10, 0.05);\n          }\n\n          .skin-blue .main-header .navbar .sidebar-toggle {\n            color: #2e2924;\n            padding-top: 27px;\n            padding-bottom: 27px;\n          }\n\n          .masthead-brand {\n            height: 74px;\n            display: flex;\n            flex-direction: column;\n            justify-content: center;\n            line-height: 1.05;\n            color: #f6f0e6;\n          }\n\n          .masthead-brand__name {\n            font-family: 'Cormorant Garamond', Georgia, serif;\n            font-size: 30px;\n            font-weight: 700;\n            letter-spacing: 0.03em;\n          }\n\n          .masthead-brand__tag {\n            font-size: 10px;\n            text-transform: uppercase;\n            letter-spacing: 0.18em;\n            opacity: 0.78;\n            margin-top: 4px;\n          }\n\n          .skin-blue .main-sidebar {\n            position: fixed;\n            top: 0;\n            left: 0;\n            bottom: 0;\n            padding-top: 74px;\n            background:\n              radial-gradient(circle at 16% 10%, rgba(176,67,47,0.07), transparent 36%),\n              radial-gradient(circle at 85% 14%, rgba(93,63,49,0.12), transparent 42%),\n              #141210;\n            border-right: 1px solid rgba(255,255,255,0.04);\n          }\n\n          .main-sidebar .sidebar {\n            height: calc(100vh - 74px);\n            overflow-y: auto;\n            overscroll-behavior: contain;\n            padding-bottom: 118px;\n          }\n\n          .folio-card {\n            margin: 16px 14px 10px;\n            border-radius: 16px;\n            padding: 16px;\n            background: linear-gradient(180deg, rgba(255,255,255,0.04), rgba(255,255,255,0.02));\n            border: 1px solid rgba(255,255,255,0.08);\n            color: #f2ebdf;\n          }\n\n          .folio-card__issue {\n            font-size: 11px;\n            text-transform: uppercase;\n            letter-spacing: 0.16em;\n            color: rgba(242,235,223,0.68);\n            margin-bottom: 8px;\n          }\n\n          .folio-card__title {\n            font-family: 'Cormorant Garamond', Georgia, serif;\n            font-size: 22px;\n            line-height: 1.05;\n            letter-spacing: 0.01em;\n            margin-bottom: 8px;\n          }\n\n          .folio-card__summary {\n            margin: 0;\n            color: rgba(242,235,223,0.80);\n            font-size: 12px;\n            line-height: 1.55;\n          }\n\n          .folio-index {\n            margin: 0 14px 10px;\n            padding: 12px 14px;\n            border-radius: 14px;\n            border: 1px solid rgba(255,255,255,0.06);\n            background: rgba(255,255,255,0.02);\n            color: rgba(242,235,223,0.90);\n          }\n\n          .folio-index__heading {\n            text-transform: uppercase;\n            letter-spacing: 0.16em;\n            font-size: 10px;\n            opacity: 0.7;\n          }\n\n          .folio-index__line {\n            height: 1px;\n            background: rgba(255,255,255,0.10);\n            margin: 10px 0;\n          }\n\n          .folio-index__item {\n            display: flex;\n            justify-content: space-between;\n            gap: 10px;\n            padding: 6px 0;\n            font-size: 12px;\n          }\n\n          .folio-index__item span:first-child {\n            color: rgba(242,235,223,0.65);\n            font-family: 'Cormorant Garamond', Georgia, serif;\n            font-size: 16px;\n            line-height: 1;\n          }\n\n          .skin-blue .sidebar-menu { margin-top: 10px; }\n          .skin-blue .sidebar-menu > li { margin: 5px 12px; }\n\n          .skin-blue .sidebar-menu > li > a {\n            border-left: none;\n            border-radius: 12px;\n            color: rgba(242,235,223,0.92);\n            padding: 12px 14px;\n            font-weight: 600;\n            transition: background .15s ease, transform .15s ease;\n          }\n\n          .skin-blue .sidebar-menu > li > a > .fa {\n            width: 20px;\n            text-align: center;\n            margin-right: 8px;\n          }\n\n          .skin-blue .sidebar-menu > li:hover > a {\n            background: rgba(255,255,255,0.05);\n            transform: translateX(2px);\n            color: #fff;\n          }\n\n          .skin-blue .sidebar-menu > li.active > a {\n            background: linear-gradient(135deg, rgba(176,67,47,0.18), rgba(255,255,255,0.04));\n            box-shadow: inset 0 0 0 1px rgba(176,67,47,0.22);\n            color: #fff;\n          }\n\n          .page-wrap {\n            max-width: 1320px;\n            margin: 0 auto;\n          }\n\n          .ed-hero {\n            position: relative;\n            overflow: hidden;\n            border-radius: 22px;\n            margin-bottom: 16px;\n            border: 1px solid rgba(30,26,23,0.10);\n            box-shadow: var(--shadow);\n          }\n\n          .ed-hero--ink { background: linear-gradient(160deg, #201c19, #2a2522 45%, #3d2f2a); }\n          .ed-hero--wine { background: linear-gradient(160deg, #331d22, #5a2230 48%, #7a2f36); }\n          .ed-hero--forest { background: linear-gradient(160deg, #182523, #203b35 48%, #2f564d); }\n\n          .ed-hero__grain {\n            position: absolute;\n            inset: 0;\n            opacity: 0.12;\n            background-image:\n              radial-gradient(circle at 15% 22%, rgba(255,255,255,.35) 0 1px, transparent 1.2px),\n              radial-gradient(circle at 78% 36%, rgba(255,255,255,.25) 0 1px, transparent 1.2px),\n              radial-gradient(circle at 38% 75%, rgba(255,255,255,.22) 0 1px, transparent 1.2px);\n            background-size: 12px 12px, 14px 14px, 16px 16px;\n          }\n\n          .ed-hero__inner {\n            position: relative;\n            z-index: 1;\n            padding: 24px 24px 22px;\n            color: #f7f1e9;\n          }\n\n          .ed-hero__section {\n            font-size: 11px;\n            text-transform: uppercase;\n            letter-spacing: 0.18em;\n            opacity: 0.80;\n            margin-bottom: 10px;\n          }\n\n          .ed-hero__title {\n            margin: 0 0 10px;\n            font-family: 'Cormorant Garamond', Georgia, serif;\n            font-size: 42px;\n            line-height: 0.95;\n            font-weight: 700;\n            letter-spacing: -0.01em;\n            max-width: 980px;\n          }\n\n          .ed-hero__dek {\n            margin: 0;\n            max-width: 900px;\n            font-size: 14px;\n            line-height: 1.7;\n            color: rgba(247,241,233,0.88);\n          }\n\n          .ed-hero__meta {\n            margin-top: 14px;\n            display: flex;\n            flex-wrap: wrap;\n            gap: 8px;\n          }\n\n          .ed-meta-pill {\n            border-radius: 999px;\n            padding: 7px 10px;\n            font-size: 11px;\n            border: 1px solid rgba(255,255,255,0.14);\n            background: rgba(255,255,255,0.06);\n          }\n\n          .ed-stat-strip {\n            display: grid;\n            grid-template-columns: repeat(3, minmax(0, 1fr));\n            gap: 12px;\n            margin-bottom: 16px;\n          }\n\n          .ed-stat {\n            border-radius: 16px;\n            background: rgba(255,255,255,0.62);\n            border: 1px solid rgba(30,26,23,0.10);\n            box-shadow: 0 10px 24px rgba(20,15,10,0.06);\n            padding: 14px 15px;\n          }\n\n          .ed-stat__label {\n            font-size: 10px;\n            text-transform: uppercase;\n            letter-spacing: 0.18em;\n            color: var(--muted);\n            margin-bottom: 8px;\n            font-weight: 700;\n          }\n\n          .ed-stat__value {\n            font-family: 'Cormorant Garamond', Georgia, serif;\n            font-size: 28px;\n            line-height: 0.95;\n            color: var(--ink);\n            margin-bottom: 6px;\n          }\n\n          .ed-stat__note {\n            color: #5e564d;\n            font-size: 12px;\n            line-height: 1.5;\n          }\n\n          .article-shell {\n            border-radius: 20px;\n            background: rgba(255,255,255,0.64);\n            border: 1px solid rgba(30,26,23,0.10);\n            box-shadow: var(--shadow);\n            overflow: hidden;\n          }\n\n          .article-shell__head {\n            display: flex;\n            justify-content: space-between;\n            gap: 14px;\n            align-items: flex-end;\n            padding: 18px 20px 12px;\n          }\n\n          .article-shell__kicker {\n            font-size: 10px;\n            text-transform: uppercase;\n            letter-spacing: .2em;\n            color: var(--muted);\n            margin-bottom: 6px;\n            font-weight: 700;\n          }\n\n          .article-shell__title {\n            margin: 0;\n            color: var(--ink);\n            font-family: 'Cormorant Garamond', Georgia, serif;\n            font-size: 30px;\n            line-height: 1;\n            font-weight: 700;\n          }\n\n          .article-shell__badge {\n            border-radius: 999px;\n            padding: 7px 10px;\n            border: 1px solid rgba(30,26,23,0.12);\n            background: rgba(255,255,255,0.7);\n            font-size: 11px;\n            font-weight: 700;\n            color: #3a332d;\n            white-space: nowrap;\n          }\n\n          .article-shell__rule {\n            height: 1px;\n            background: linear-gradient(90deg, transparent, rgba(30,26,23,0.14), transparent);\n          }\n\n          .article-shell__body {\n            padding: 20px;\n          }\n\n          .article-shell__body h1, .article-shell__body h2, .article-shell__body h3 {\n            font-family: 'Cormorant Garamond', Georgia, serif;\n            color: var(--ink);\n            line-height: 1.05;\n          }\n\n          .article-shell__body p, .article-shell__body li {\n            color: #342f2a;\n            line-height: 1.85;\n            font-size: 14px;\n          }\n\n          .article-shell__body img {\n            max-width: 100%;\n            height: auto;\n            border-radius: 12px;\n            box-shadow: 0 10px 24px rgba(20,15,10,0.08);\n          }\n\n          .author-img {\n            border-radius: 14px;\n            width: 120px;\n            height: 150px;\n            object-fit: cover;\n            box-shadow: 0 8px 18px rgba(20,15,10,0.12);\n          }\n\n          .viz-layout {\n            display: grid;\n            grid-template-columns: 360px minmax(0, 1fr);\n            gap: 16px;\n            align-items: start;\n          }\n\n          .editor-panel {\n            position: sticky;\n            top: 92px;\n            border-radius: 20px;\n            background: rgba(255,255,255,0.68);\n            border: 1px solid rgba(30,26,23,0.10);\n            box-shadow: var(--shadow);\n            padding: 18px;\n          }\n\n          .editor-panel__label {\n            font-size: 10px;\n            text-transform: uppercase;\n            letter-spacing: .20em;\n            color: var(--muted);\n            font-weight: 700;\n          }\n\n          .editor-panel__title {\n            margin: 8px 0 8px;\n            font-family: 'Cormorant Garamond', Georgia, serif;\n            font-size: 30px;\n            line-height: 0.95;\n            color: var(--ink);\n          }\n\n          .editor-panel__text {\n            margin: 0 0 14px;\n            color: #4f4942;\n            font-size: 13px;\n            line-height: 1.6;\n          }\n\n          .editor-block {\n            margin-bottom: 14px;\n            padding: 12px;\n            border-radius: 14px;\n            border: 1px solid rgba(30,26,23,0.08);\n            background: rgba(255,255,255,0.55);\n          }\n\n          .editor-block__title {\n            margin: 0 0 8px;\n            font-size: 11px;\n            text-transform: uppercase;\n            letter-spacing: .16em;\n            color: var(--muted);\n            font-weight: 700;\n          }\n\n          .form-group { margin-bottom: 12px; }\n          .form-group label {\n            color: #352f29;\n            font-size: 11px;\n            text-transform: uppercase;\n            letter-spacing: .12em;\n            font-weight: 700;\n            margin-bottom: 6px;\n          }\n\n          .selectize-input, .form-control {\n            min-height: 44px;\n            border-radius: 12px !important;\n            border: 1px solid rgba(30,26,23,0.14) !important;\n            background: rgba(255,255,255,0.92) !important;\n            box-shadow: none !important;\n            padding: 10px 12px !important;\n          }\n\n          .selectize-input.focus, .form-control:focus {\n            border-color: rgba(176,67,47,0.45) !important;\n            box-shadow: 0 0 0 3px rgba(176,67,47,0.08) !important;\n          }\n\n          .selectize-dropdown {\n            border-radius: 12px;\n            border: 1px solid rgba(30,26,23,0.14);\n            box-shadow: 0 14px 28px rgba(20,15,10,0.12);\n          }\n\n          .editor-action {\n            border-radius: 14px;\n            border: 1px solid rgba(176,67,47,0.14);\n            background: linear-gradient(180deg, rgba(176,67,47,0.06), rgba(176,67,47,0.03));\n            padding: 12px;\n          }\n\n          .ed-btn {\n            width: 100%;\n            min-height: 46px;\n            border: none;\n            border-radius: 10px;\n            background: linear-gradient(135deg, #8a3027, #b0432f);\n            box-shadow: 0 12px 22px rgba(126,42,32,0.20);\n            font-weight: 700;\n            letter-spacing: .02em;\n          }\n\n          .ed-btn:hover, .ed-btn:focus {\n            background: linear-gradient(135deg, #792821, #a43b2a);\n          }\n\n          .editor-action__note {\n            margin: 10px 0 0;\n            color: #544c44;\n            font-size: 12px;\n            line-height: 1.5;\n          }\n\n          .viz-stage {\n            display: grid;\n            gap: 16px;\n          }\n\n          .summary-card {\n            border-left: 3px solid rgba(176,67,47,0.34);\n          }\n\n          .summary-prose {\n            margin: 0;\n            color: #3d3731;\n            font-size: 14px;\n            line-height: 1.75;\n          }\n\n          .summary-prose strong {\n            color: #1f1a17;\n          }\n\n          .summary-quote {\n            margin-top: 12px;\n            padding: 10px 12px;\n            border-left: 2px solid rgba(176,67,47,0.28);\n            background: rgba(176,67,47,0.04);\n            color: #4b433c;\n            font-size: 13px;\n            line-height: 1.6;\n          }\n\n          .summary-grid {\n            margin-top: 14px;\n            display: grid;\n            grid-template-columns: repeat(2, minmax(0, 1fr));\n            gap: 10px 14px;\n          }\n\n          .summary-item {\n            border-top: 1px solid rgba(30,26,23,0.08);\n            padding-top: 8px;\n          }\n\n          .summary-item__label {\n            font-size: 10px;\n            text-transform: uppercase;\n            letter-spacing: .16em;\n            color: var(--muted);\n            font-weight: 700;\n            margin-bottom: 4px;\n          }\n\n          .summary-item__value {\n            color: #2f2a24;\n            font-size: 13px;\n            line-height: 1.45;\n          }\n\n          .feature-card {\n            border-radius: 20px;\n            background: rgba(255,255,255,0.66);\n            border: 1px solid rgba(30,26,23,0.10);\n            box-shadow: var(--shadow);\n            overflow: hidden;\n          }\n\n          .feature-card__head {\n            padding: 16px 18px 12px;\n          }\n\n          .feature-card__kicker {\n            font-size: 10px;\n            text-transform: uppercase;\n            letter-spacing: .20em;\n            color: var(--muted);\n            font-weight: 700;\n            margin-bottom: 6px;\n          }\n\n          .feature-card__title {\n            margin: 0;\n            font-family: 'Cormorant Garamond', Georgia, serif;\n            font-size: 28px;\n            line-height: 0.95;\n            color: var(--ink);\n          }\n\n          .feature-card__sub {\n            margin: 8px 0 0;\n            color: #575047;\n            font-size: 13px;\n            line-height: 1.55;\n          }\n\n          .feature-card__rule {\n            height: 1px;\n            background: linear-gradient(90deg, transparent, rgba(30,26,23,0.14), transparent);\n          }\n\n          .feature-card__body {\n            padding: 16px 18px 18px;\n          }\n\n          .feature-card__body--plot {\n            background:\n              radial-gradient(circle at 90% 8%, rgba(176,67,47,0.05), transparent 28%),\n              radial-gradient(circle at 8% 88%, rgba(32,59,53,0.06), transparent 30%);\n          }\n\n          .plot-frame {\n            padding: 10px;\n            border-radius: 14px;\n            background: rgba(255,255,255,0.78);\n            border: 1px solid rgba(30,26,23,0.08);\n          }\n\n          .plotly, .js-plotly-plot, .plotly html-widget {\n            border-radius: 10px;\n          }\n\n          .table-frame {\n            padding: 10px;\n            border-radius: 14px;\n            background: rgba(255,255,255,0.78);\n            border: 1px solid rgba(30,26,23,0.08);\n          }\n\n          .dataTables_wrapper .dataTables_filter input,\n          .dataTables_wrapper .dataTables_length select {\n            border-radius: 8px;\n            border: 1px solid rgba(30,26,23,0.14);\n            padding: 6px 8px;\n            background: rgba(255,255,255,0.9);\n          }\n\n          .dataTables_wrapper .dataTables_info,\n          .dataTables_wrapper .dataTables_paginate {\n            color: #61584f !important;\n            font-size: 12px;\n          }\n\n          .dataTables_wrapper table.dataTable thead th {\n            border-bottom: 1px solid rgba(30,26,23,0.14) !important;\n            color: #342e29;\n            font-size: 11px;\n            text-transform: uppercase;\n            letter-spacing: .10em;\n          }\n\n          .dataTables_wrapper table.dataTable tbody td {\n            color: #302a25;\n          }\n\n          .dataTables_wrapper table.dataTable tbody tr:hover {\n            background: rgba(176,67,47,0.025);\n          }\n\n          .dock-footer {\n            position: fixed;\n            left: 340px;\n            right: 0;\n            bottom: 0;\n            z-index: 100;\n            min-height: 82px;\n            display: flex;\n            align-items: center;\n            justify-content: space-between;\n            gap: 14px;\n            padding: 10px 16px;\n            background: rgba(246,241,232,0.88);\n            border-top: 1px solid rgba(30,26,23,0.10);\n            backdrop-filter: blur(10px);\n          }\n\n          .dock-footer__logos {\n            display: flex;\n            align-items: center;\n            gap: 8px;\n            flex-wrap: wrap;\n          }\n\n          .dock-footer__logo {\n            display: inline-flex;\n            align-items: center;\n            justify-content: center;\n            min-height: 52px;\n            padding: 8px 10px;\n            border-radius: 10px;\n            background: rgba(255,255,255,0.72);\n            border: 1px solid rgba(30,26,23,0.10);\n          }\n\n          .dock-footer__links {\n            display: flex;\n            align-items: center;\n            gap: 8px;\n            flex-wrap: wrap;\n          }\n\n          .dock-footer__links a {\n            color: #3a332d !important;\n            text-decoration: none;\n            font-weight: 600;\n            border-radius: 8px;\n            padding: 8px 10px;\n          }\n\n          .dock-footer__links a:hover {\n            background: rgba(30,26,23,0.05);\n          }\n\n          .dock-footer__links i {\n            color: var(--accent) !important;\n          }\n\n          .sidebar-collapse .dock-footer { left: 50px; }\n\n          @media (max-width: 1199px) {\n            .viz-layout { grid-template-columns: 330px minmax(0, 1fr); }\n            .ed-hero__title { font-size: 36px; }\n          }\n\n          @media (max-width: 991px) {\n            .main-sidebar, .left-side { position: absolute !important; }\n            .main-sidebar .sidebar { height: auto; overflow-y: visible; }\n            .ed-stat-strip { grid-template-columns: 1fr; }\n            .viz-layout { grid-template-columns: 1fr; }\n            .summary-grid { grid-template-columns: 1fr; }\n            .editor-panel { position: static; }\n            .ed-hero__title { font-size: 32px; }\n            .dock-footer {\n              left: 0;\n              min-height: 100px;\n              flex-direction: column;\n              align-items: flex-start;\n            }\n            .content { padding-bottom: 176px; }\n          }\n        "))
      ),
      tags$style(HTML("
        .theme-switcher {
          position: fixed;
          top: 84px;
          right: 18px;
          z-index: 2400;
          display: inline-flex;
          align-items: center;
          gap: 6px;
          padding: 6px;
          border-radius: 999px;
          border: 1px solid rgba(30,26,23,0.12);
          background: rgba(246,241,232,0.86);
          box-shadow: 0 10px 22px rgba(20,15,10,0.10);
          backdrop-filter: blur(10px);
        }
        .theme-switcher__btn {
          border: none;
          border-radius: 999px;
          padding: 7px 12px;
          background: transparent;
          color: #3a332d;
          font-size: 11px;
          line-height: 1;
          font-weight: 700;
          letter-spacing: .06em;
          text-transform: uppercase;
          cursor: pointer;
          transition: background .15s ease, color .15s ease, box-shadow .15s ease;
        }
        .theme-switcher__btn:hover { background: rgba(30,26,23,0.06); }
        .theme-switcher__btn.is-active {
          background: linear-gradient(135deg, #8a3027, #b0432f);
          color: #fff;
          box-shadow: 0 6px 14px rgba(126,42,32,0.24);
        }
        body.theme-dark { color-scheme: dark; }
        body.theme-dark .content-wrapper,
        body.theme-dark .right-side {
          background:
            linear-gradient(180deg, rgba(255,255,255,0.02), rgba(255,255,255,0.00)),
            radial-gradient(circle at 10% 8%, rgba(176,67,47,0.08), transparent 28%),
            radial-gradient(circle at 92% 10%, rgba(32,59,53,0.10), transparent 30%),
            #12100f;
        }
        body.theme-dark .skin-blue .main-header .navbar {
          background: rgba(20,18,16,0.84);
          border-bottom: 1px solid rgba(255,255,255,0.06);
          box-shadow: 0 8px 24px rgba(0,0,0,0.18);
        }
        body.theme-dark .skin-blue .main-header .navbar .sidebar-toggle { color: #ece4d8; }
        body.theme-dark .theme-switcher {
          background: rgba(20,18,16,0.88);
          border-color: rgba(255,255,255,0.10);
          box-shadow: 0 12px 24px rgba(0,0,0,0.26);
        }
        body.theme-dark .theme-switcher__btn { color: #e8ded1; }
        body.theme-dark .theme-switcher__btn:hover { background: rgba(255,255,255,0.07); }
        body.theme-dark .ed-stat,
        body.theme-dark .article-shell,
        body.theme-dark .editor-panel,
        body.theme-dark .editor-block,
        body.theme-dark .feature-card,
        body.theme-dark .plot-frame,
        body.theme-dark .table-frame,
        body.theme-dark .dock-footer,
        body.theme-dark .dock-footer__logo {
          background: rgba(28,24,22,0.78);
          border-color: rgba(255,255,255,0.08);
          box-shadow: 0 14px 28px rgba(0,0,0,0.20);
        }
        body.theme-dark .dock-footer {
          background: rgba(18,16,15,0.90);
          border-top-color: rgba(255,255,255,0.08);
        }
        body.theme-dark .article-shell__rule,
        body.theme-dark .feature-card__rule {
          background: linear-gradient(90deg, transparent, rgba(255,255,255,0.10), transparent);
        }
        body.theme-dark .summary-item { border-top-color: rgba(255,255,255,0.08); }
        body.theme-dark .ed-stat__value,
        body.theme-dark .article-shell__title,
        body.theme-dark .feature-card__title,
        body.theme-dark .editor-panel__title,
        body.theme-dark .article-shell__body h1,
        body.theme-dark .article-shell__body h2,
        body.theme-dark .article-shell__body h3,
        body.theme-dark .summary-prose strong { color: #f1e8db; }
        body.theme-dark .ed-stat__label,
        body.theme-dark .ed-stat__note,
        body.theme-dark .editor-panel__label,
        body.theme-dark .editor-panel__text,
        body.theme-dark .editor-block__title,
        body.theme-dark .feature-card__kicker,
        body.theme-dark .feature-card__sub,
        body.theme-dark .summary-prose,
        body.theme-dark .summary-quote,
        body.theme-dark .summary-item__label,
        body.theme-dark .summary-item__value,
        body.theme-dark .article-shell__kicker,
        body.theme-dark .article-shell__body p,
        body.theme-dark .article-shell__body li,
        body.theme-dark .dock-footer__links a { color: #d7ccbf !important; }
        body.theme-dark .article-shell__badge {
          color: #d7ccbf;
          background: rgba(255,255,255,0.04);
          border-color: rgba(255,255,255,0.10);
        }
        body.theme-dark .summary-quote {
          background: rgba(176,67,47,0.10);
          border-left-color: rgba(176,67,47,0.38);
        }
        body.theme-dark .editor-action {
          background: linear-gradient(180deg, rgba(176,67,47,0.10), rgba(176,67,47,0.05));
          border-color: rgba(176,67,47,0.22);
        }
        body.theme-dark .editor-action__note { color: #cfc3b6; }
        body.theme-dark .form-group label { color: #e6dbcd; }
        body.theme-dark .selectize-input,
        body.theme-dark .form-control {
          color: #efe5d8 !important;
          background: rgba(18,16,15,0.84) !important;
          border-color: rgba(255,255,255,0.10) !important;
        }
        body.theme-dark .selectize-input input { color: #efe5d8 !important; }
        body.theme-dark .selectize-dropdown {
          background: #1d1917;
          border-color: rgba(255,255,255,0.10);
          color: #ece1d3;
        }
        body.theme-dark .selectize-dropdown .option { color: #ece1d3; }
        body.theme-dark .selectize-dropdown .option.active {
          background: rgba(176,67,47,0.18);
          color: #fff;
        }
        body.theme-dark .dataTables_wrapper .dataTables_filter input,
        body.theme-dark .dataTables_wrapper .dataTables_length select {
          background: rgba(18,16,15,0.84);
          border-color: rgba(255,255,255,0.10);
          color: #ece1d3;
        }
        body.theme-dark .dataTables_wrapper .dataTables_info,
        body.theme-dark .dataTables_wrapper .dataTables_paginate { color: #cdbfae !important; }
        body.theme-dark .dataTables_wrapper table.dataTable thead th {
          color: #e9decf;
          border-bottom-color: rgba(255,255,255,0.10) !important;
        }
        body.theme-dark .dataTables_wrapper table.dataTable tbody td { color: #ddd0c0; }
        body.theme-dark .dataTables_wrapper table.dataTable tbody tr:hover { background: rgba(176,67,47,0.08); }
        body.theme-dark .dock-footer__links a:hover { background: rgba(255,255,255,0.06); }
        @media (max-width: 991px) {
          .theme-switcher {
            top: 80px;
            right: 12px;
            transform: scale(0.96);
            transform-origin: top right;
          }
        }
      ")),
      tags$script(HTML("
        (function() {
          var STORAGE_KEY = 'chico_theme_mode';
          var media = window.matchMedia ? window.matchMedia('(prefers-color-scheme: dark)') : null;
          function getStoredMode() {
            try {
              var m = window.localStorage.getItem(STORAGE_KEY);
              if (m === 'light' || m === 'dark' || m === 'auto') return m;
            } catch (e) {}
            return 'auto';
          }
          function resolveMode(mode) {
            if (mode === 'light' || mode === 'dark') return mode;
            return (media && media.matches) ? 'dark' : 'light';
          }
          function updateButtons(mode) {
            var btns = document.querySelectorAll('.theme-switcher__btn[data-theme-mode]');
            Array.prototype.forEach.call(btns, function(btn) {
              var active = btn.getAttribute('data-theme-mode') === mode;
              btn.classList.toggle('is-active', active);
              btn.setAttribute('aria-pressed', active ? 'true' : 'false');
            });
          }
          function applyMode(mode) {
            var resolved = resolveMode(mode);
            document.documentElement.setAttribute('data-theme-mode', mode);
            document.documentElement.setAttribute('data-theme-resolved', resolved);
            if (document.body) {
              document.body.classList.remove('theme-light', 'theme-dark');
              document.body.classList.add('theme-' + resolved);
            }
            updateButtons(mode);
          }
          function setMode(mode) {
            try { window.localStorage.setItem(STORAGE_KEY, mode); } catch (e) {}
            applyMode(mode);
          }
          function onClick(ev) {
            var btn = ev.target.closest('.theme-switcher__btn[data-theme-mode]');
            if (!btn) return;
            setMode(btn.getAttribute('data-theme-mode'));
          }
          function init() {
            applyMode(getStoredMode());
            document.addEventListener('click', onClick);
            if (!media) return;
            var onSystemChange = function() {
              if (getStoredMode() === 'auto') applyMode('auto');
            };
            if (typeof media.addEventListener === 'function') media.addEventListener('change', onSystemChange);
            else if (typeof media.addListener === 'function') media.addListener(onSystemChange);
          }
          if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', init);
          else init();
        })();
      ")),
      tags$div(
        class = "theme-switcher",
        role = "group",
        `aria-label` = "Theme mode",
        tags$button(type = "button", class = "theme-switcher__btn", `data-theme-mode` = "light", "Light"),
        tags$button(type = "button", class = "theme-switcher__btn", `data-theme-mode` = "dark", "Dark"),
        tags$button(type = "button", class = "theme-switcher__btn", `data-theme-mode` = "auto", "Automatic")
      ),

      tabItems(
        tabItem(
          tabName = "about",
          div(
            class = "page-wrap",
            editorial_hero(
              section_label = "Feature",
              title = "CHICO Study: Longitudinal cervical microbiome-HPV atlas",
              dek = "An editorial-style interface for the CHICO manuscript draft, combining cohort context, microbiome comparison modules, and manuscript-linked contributor information.",
              meta = c("Cohort context", "Manuscript summary", "Interactive modules"),
              tone = "ink"
            ),
            stat_strip(list(
              list(label = "Pages", value = "3", note = "About, Visualization, Authors"),
              list(label = "Interface", value = "Editorial", note = "Magazine-inspired visual hierarchy"),
              list(label = "Purpose", value = "Explore", note = "Study story + data comparison")
            )),
            article_shell(
              title = "Study Background",
              kicker = "Section 01",
              badge = "About",
              body = uiOutput("aboutContent")
            )
          )
        ),

        tabItem(
          tabName = "viz",
          div(
            class = "page-wrap",
            editorial_hero(
              section_label = "Analysis Desk",
              title = "Interactive microbiome comparison in an editorial layout",
              dek = "Explore manuscript-relevant cervical microbiome comparisons across HPV status, HPV risk, and persistence groupings using single-taxon distributions or Top-N compositional stacked bars.",
              meta = c("Taxonomic levels", "Single taxon", "Top-N composition", "HPV grouping"),
              tone = "forest"
            ),
            stat_strip(list(
              list(label = "Taxonomic levels", value = as.character(length(level_choices)), note = "Phylum to Species"),
              list(label = "Initial phyla", value = as.character(length(phylum_name)), note = "Loaded for default selection"),
              list(label = "Tests", value = "Wilcoxon / Kruskal", note = "Auto-selected by group count")
            )),
            tags$section(
              class = "feature-card summary-card",
              tags$header(
                class = "feature-card__head",
                tags$div(class = "feature-card__kicker", "Lead"),
                tags$h3(class = "feature-card__title", "Result Summary"),
                tags$p(
                  class = "feature-card__sub",
                  "Narrative summary of the current selection, display method, and comparison workflow."
                )
              ),
              tags$div(class = "feature-card__rule"),
              tags$div(class = "feature-card__body", uiOutput("vizEditorialSummary"))
            ),
            div(
              class = "viz-layout",
              control_panel(),
              tags$div(
                class = "viz-stage",
                tags$section(
                  class = "feature-card",
                  tags$header(
                    class = "feature-card__head",
                    tags$div(class = "feature-card__kicker", "Figure"),
                    tags$h3(class = "feature-card__title", "Visualization"),
                    tags$p(
                      class = "feature-card__sub",
                      "Interactive figure generated from the selected display method and HPV grouping configuration."
                    )
                  ),
                  tags$div(class = "feature-card__rule"),
                  tags$div(
                    class = "feature-card__body feature-card__body--plot",
                    tags$div(class = "plot-frame", plotlyOutput("box_plot", height = "580px"))
                  )
                ),
                tags$section(
                  class = "feature-card",
                  tags$header(
                    class = "feature-card__head",
                    tags$div(class = "feature-card__kicker", "Table"),
                    tags$h3(class = "feature-card__title", "Summary Table"),
                    tags$p(
                      class = "feature-card__sub",
                      "Single-taxon mode returns non-parametric test output; Top-N mode returns per-group compositional summaries."
                    )
                  ),
                  tags$div(class = "feature-card__rule"),
                  tags$div(
                    class = "feature-card__body",
                    tags$div(class = "table-frame", DTOutput("stats_table"))
                  )
                )
              )
            )
          )
        ),

        tabItem(
          tabName = "authors",
          div(
            class = "page-wrap",
            editorial_hero(
              section_label = "Contributors",
              title = "Authors, collaborators, and institutional affiliations",
              dek = "Featured profiles and manuscript-linked authorship notes for the CHICO study, including institutional affiliations and corresponding-author contacts.",
              meta = c("Manuscript roster", "Institutions", "Correspondence"),
              tone = "wine"
            ),
            stat_strip(list(
              list(label = "Directory", value = "Authors", note = "Profiles and affiliations"),
              list(label = "Collaboration", value = "Multi-site", note = "Academic + clinical partners"),
              list(label = "Contact", value = "Shen Lab", note = "Website, email, GitHub")
            )),
            article_shell(
              title = "Author Directory",
              kicker = "Section 03",
              badge = "Authors",
              body = uiOutput("authorContent")
            )
          )
        )
      ),

      tags$footer(
        div(
          class = "dock-footer",
          div(
            class = "dock-footer__logos",
            div(class = "dock-footer__logo", tags$img(src = "www/CHICO_logo.png", height = "34px", onerror = "this.onerror=null; this.src='www/default_logo.png';")),
            div(class = "dock-footer__logo", tags$img(src = "www/Chengdu_University_logo.png", height = "30px", onerror = "this.onerror=null; this.src='www/default_logo.png';")),
            div(class = "dock-footer__logo", tags$img(src = "www/Fourth_Military_Medical_University_logo.png", height = "30px", onerror = "this.onerror=null; this.src='www/default_logo.png';")),
            div(class = "dock-footer__logo", tags$img(src = "www/ntu_logo.png", height = "34px", onerror = "this.onerror=null; this.src='www/default_logo.png';"))
          ),
          div(
            class = "dock-footer__links",
            tags$a(href = "http://www.shen-lab.org", target = "_blank", tags$i(class = "fa fa-home"), " Shen Lab"),
            tags$a(href = "https://www.shen-lab.org/#contact", target = "_blank", tags$i(class = "fa fa-envelope"), " Email"),
            tags$a(href = "https://github.com/jaspershen-lab", target = "_blank", tags$i(class = "fa fa-github"), " GitHub")
          )
        )
      )
    )
  )
}
