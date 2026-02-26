#' @import shiny
#' @import ggplot2
#' @import dplyr
#' @import RColorBrewer
#' @import plotly
#' @import DT
#' @importFrom ggsignif geom_signif
#' @importFrom stats wilcox.test kruskal.test
#' @export
app_server <- function(input, output, session) {
  register_chico_markdown_assets()
  data_env <- environment()
  load_chico_data <- function(file_name) {
    load(chico_system_file("data", file_name), envir = data_env)
  }
  
  # =========================================================
  # 1. 加载数据 (原 global.R 内容)
  # 使用 system.file 定位 inst/data 下的文件
  # =========================================================
  load_chico_data("phylum_name.rda")
  load_chico_data("class_name.rda")
  load_chico_data("order_name.rda")
  load_chico_data("family_name.rda")
  load_chico_data("genus_name.rda")
  load_chico_data("species_name.rda")
  
  load_chico_data("expression_data_phylum.rda")
  load_chico_data("sample_info_phylum.rda")
  load_chico_data("variable_info_phylum.rda")
  
  load_chico_data("expression_data_class.rda")
  load_chico_data("sample_info_class.rda")
  load_chico_data("variable_info_class.rda")
  
  load_chico_data("expression_data_order.rda")
  load_chico_data("sample_info_order.rda")
  load_chico_data("variable_info_order.rda")
  
  load_chico_data("expression_data_family.rda")
  load_chico_data("sample_info_family.rda")
  load_chico_data("variable_info_family.rda")
  
  load_chico_data("expression_data_genus.rda")
  load_chico_data("sample_info_genus.rda")
  load_chico_data("variable_info_genus.rda")
  
  load_chico_data("expression_data_species.rda")
  load_chico_data("sample_info_species.rda")
  load_chico_data("variable_info_species.rda")
  
  # =========================================================
  # 2. 定义辅助函数和颜色常量
  # =========================================================
  get_level_dataset <- function(level) {
    switch(
      level,
      "Phylum"  = list(expression_data = expression_data_phylum, sample_info = sample_info_phylum, variable_info = variable_info_phylum),
      "Class"   = list(expression_data = expression_data_class, sample_info = sample_info_class, variable_info = variable_info_class),
      "Order"   = list(expression_data = expression_data_order, sample_info = sample_info_order, variable_info = variable_info_order),
      "Family"  = list(expression_data = expression_data_family, sample_info = sample_info_family, variable_info = variable_info_family),
      "Genus"   = list(expression_data = expression_data_genus, sample_info = sample_info_genus, variable_info = variable_info_genus),
      "Species" = list(expression_data = expression_data_species, sample_info = sample_info_species, variable_info = variable_info_species),
      NULL
    )
  }
  
  group_color <- c(
    "Negative" = "#2ca02c", "Positive" = "#ff7f0e",
    "Low_risk" = "#1f77b4", "High_risk" = "#d62728",
    "Non-Persistent" = "#9467bd", "Persistent" = "#8c564b"
  )
  
  group_levels <- c("Negative", "Positive", "Low_risk", "High_risk", "Non-Persistent", "Persistent")
  
  selected_group_var <- function(group_type) {
    switch(
      group_type,
      "Affect or not"  = "Affect",
      "HPV risk"       = "risk",
      "HPV persistent" = "persistent",
      "Affect"
    )
  }
  
  selected_method <- function() {
    if (is.null(input$display_method) || !nzchar(input$display_method)) {
      "single_taxon_boxplot"
    } else {
      input$display_method
    }
  }
  
  # =========================================================
  # 3. Server 核心逻辑
  # =========================================================
  
  output$aboutContent <- renderUI({
    # 修正路径：使用 system.file 读取 inst/markdown 下的文件
    html_path <- chico_system_file("markdown", "about.html", must_exist = FALSE)
    if (file.exists(html_path)) {
      includeHTML(html_path)
    } else {
      h3("File markdown/about.html not found.")
    }
  })
  
  output$authorContent <- renderUI({
    html_path <- chico_system_file("markdown", "authors.html", must_exist = FALSE)
    if (file.exists(html_path)) {
      includeHTML(html_path)
    } else {
      h3("File markdown/authors.html not found.")
    }
  })

  participant_info_df <- reactive({
    df <- sample_info_genus
    req(is.data.frame(df))

    keep_cols <- c("sample_id", "Age", "Affect", "virus", "virus_number", "risk", "persistent")
    keep_cols <- keep_cols[keep_cols %in% names(df)]
    df <- df[, keep_cols, drop = FALSE]

    if ("sample_id" %in% names(df)) df$sample_id <- as.character(df$sample_id)
    if ("Age" %in% names(df)) df$Age <- suppressWarnings(as.numeric(as.character(df$Age)))
    df
  })

  count_categorical <- function(df, col, preferred_order = NULL) {
    validate(need(col %in% names(df), paste("Column not found:", col)))

    vals <- as.character(df[[col]])
    vals[is.na(vals) | !nzchar(trimws(vals))] <- "Missing"

    out <- as.data.frame(table(vals), stringsAsFactors = FALSE)
    names(out) <- c("label_raw", "n")
    out$n <- as.integer(out$n)

    if (!is.null(preferred_order)) {
      ord <- c(preferred_order, setdiff(out$label_raw, preferred_order))
      out$label_raw <- factor(out$label_raw, levels = ord)
      out <- out[order(out$label_raw), , drop = FALSE]
      out$label_raw <- as.character(out$label_raw)
    } else {
      out <- out[order(out$n, decreasing = TRUE), , drop = FALSE]
    }

    out$label <- gsub("_", " ", out$label_raw, fixed = TRUE)
    out
  }

  make_hover_expand_pie <- function(count_df, title, colors = NULL) {
    req(nrow(count_df) > 0)

    if (is.null(colors)) {
      cols <- grDevices::hcl.colors(max(3, nrow(count_df)), "Dynamic")
      colors <- setNames(cols[seq_len(nrow(count_df))], count_df$label_raw)
    }
    marker_colors <- unname(colors[count_df$label_raw])
    marker_colors[is.na(marker_colors)] <- grDevices::hcl.colors(sum(is.na(marker_colors)), "Dynamic")

    p <- plot_ly(
      data = count_df,
      labels = ~label,
      values = ~n,
      type = "pie",
      sort = FALSE,
      textinfo = "label+percent",
      hovertemplate = "<b>%{label}</b><br>Count: %{value}<br>Percent: %{percent}<extra></extra>",
      marker = list(colors = marker_colors, line = list(color = "rgba(255,255,255,0.75)", width = 1)),
      pull = rep(0, nrow(count_df))
    ) %>%
      layout(
        title = list(text = title, x = 0.02, xanchor = "left", font = list(size = 18)),
        margin = list(l = 10, r = 10, t = 50, b = 10),
        showlegend = TRUE,
        legend = list(orientation = "h", y = -0.08, x = 0, font = list(size = 11))
      )

    htmlwidgets::onRender(
      p,
      "
      function(el, x) {
        var gd = document.getElementById(el.id);
        if (!gd || gd.__chicoHoverPieBound) return;
        gd.__chicoHoverPieBound = true;
        function setPull(activeIndex) {
          var n = 0;
          if (gd.data && gd.data[0] && gd.data[0].labels) n = gd.data[0].labels.length;
          if (!n) return;
          var pull = Array(n).fill(0);
          if (typeof activeIndex === 'number' && activeIndex >= 0 && activeIndex < n) {
            pull[activeIndex] = 0.09;
          }
          Plotly.restyle(gd, {pull: [pull]}, [0]);
        }
        gd.on('plotly_hover', function(e) {
          if (e && e.points && e.points.length) setPull(e.points[0].pointNumber);
        });
        gd.on('plotly_unhover', function() { setPull(null); });
      }
      "
    )
  }

  output$participantAgePlot <- renderPlotly({
    df <- participant_info_df()
    validate(need("Age" %in% names(df), "Column `Age` not found in sample_info_genus."))

    age_df <- df[!is.na(df$Age), c("Age", intersect(c("sample_id", "Affect", "risk"), names(df))), drop = FALSE]
    validate(need(nrow(age_df) > 0, "No valid age values available."))

    plot_ly(
      age_df,
      x = ~Age,
      type = "histogram",
      nbinsx = min(30, max(10, floor(sqrt(nrow(age_df))))),
      marker = list(color = "rgba(176,67,47,0.78)", line = list(color = "rgba(255,255,255,0.85)", width = 1)),
      hovertemplate = "Age: %{x}<br>Count: %{y}<extra></extra>"
    ) %>%
      layout(
        title = list(text = "Age Distribution", x = 0.02, xanchor = "left", font = list(size = 18)),
        xaxis = list(title = "Age", showgrid = TRUE, gridcolor = "grey88"),
        yaxis = list(title = "Participant count", showgrid = TRUE, gridcolor = "grey88"),
        bargap = 0.04,
        margin = list(l = 55, r = 20, t = 52, b = 55),
        plot_bgcolor = "white",
        paper_bgcolor = "white"
      )
  })

  output$affectPiePlot <- renderPlotly({
    df <- participant_info_df()
    cnt <- count_categorical(df, "Affect", preferred_order = c("Negative", "Positive"))
    make_hover_expand_pie(
      cnt,
      "Affect Group",
      colors = c("Negative" = "#2ca02c", "Positive" = "#ff7f0e", "Missing" = "#b8b2a8")
    )
  })

  output$riskPiePlot <- renderPlotly({
    df <- participant_info_df()
    cnt <- count_categorical(df, "risk", preferred_order = c("Negative", "Low_risk", "High_risk"))
    make_hover_expand_pie(
      cnt,
      "HPV Risk Group",
      colors = c("Negative" = "#2ca02c", "Low_risk" = "#1f77b4", "High_risk" = "#d62728", "Missing" = "#b8b2a8")
    )
  })

  output$persistentPiePlot <- renderPlotly({
    df <- participant_info_df()
    cnt <- count_categorical(df, "persistent", preferred_order = c("Non-Persistent", "Persistent", "No-follow-up"))
    make_hover_expand_pie(
      cnt,
      "HPV Persistent Group",
      colors = c(
        "Non-Persistent" = "#9467bd",
        "Persistent" = "#8c564b",
        "No-follow-up" = "#7f8c8d",
        "Missing" = "#b8b2a8"
      )
    )
  })

  output$participantInfoTable <- renderDT({
    df <- participant_info_df()
    keep_cols <- c("sample_id", "Age", "Affect", "virus", "virus_number", "risk", "persistent")
    keep_cols <- keep_cols[keep_cols %in% names(df)]
    validate(need(length(keep_cols) > 0, "No requested participant columns found."))

    datatable(
      df[, keep_cols, drop = FALSE],
      rownames = FALSE,
      options = list(
        pageLength = 15,
        lengthMenu = c(10, 15, 30, 50),
        scrollX = TRUE,
        autoWidth = TRUE
      )
    )
  })

  output$vizEditorialSummary <- renderUI({
    or_else <- function(x, y) {
      if (is.null(x) || length(x) == 0) y else x
    }

    selected_groups <- or_else(input$group, character(0))
    group_text <- if (length(selected_groups) > 0) {
      paste(selected_groups, collapse = ", ")
    } else {
      "None selected"
    }

    current_method <- selected_method()
    inferred_test <- if (current_method == "topn_stacked_bar") {
      "No hypothesis test (compositional comparison via stacked bar across selected groups)"
    } else if (length(selected_groups) < 2) {
      "Selection incomplete (need at least two groups)"
    } else if (length(selected_groups) == 2) {
      "Wilcoxon rank-sum test (two-group comparison)"
    } else {
      "Kruskal-Wallis test (multi-group comparison)"
    }
    
    method_label <- if (current_method == "topn_stacked_bar") {
      "Top-N taxa stacked bar"
    } else {
      "Single taxon distribution"
    }
    top_n_label <- if (current_method == "topn_stacked_bar") {
      as.character(if (is.null(input$top_n_taxa)) 10 else input$top_n_taxa)
    } else {
      "Not used"
    }

    last_run_note <- "Generate plot to create the latest filtered dataset summary."
    sample_count <- "Not generated yet"
    observed_groups <- "Not generated yet"

    if (!is.null(input$generate_plot) && isTRUE(input$generate_plot > 0)) {
      if (current_method == "topn_stacked_bar") {
        comp_try <- tryCatch(topn_composition_df(), error = function(e) NULL)
        if (!is.null(comp_try) && nrow(comp_try) > 0) {
          n_samples_attr <- attr(comp_try, "n_samples")
          sample_count <- if (is.null(n_samples_attr) || length(n_samples_attr) == 0) "Unknown" else as.character(n_samples_attr)
          observed_groups <- attr(comp_try, "observed_groups")
          if (is.null(observed_groups) || !length(observed_groups)) observed_groups <- "Unknown"
          if (length(observed_groups) > 1) observed_groups <- paste(observed_groups, collapse = ", ")
          last_run_note <- paste0(
            "Latest Top-N composition comparison used ", sample_count,
            " samples and summarized ", length(unique(as.character(comp_try$taxon_display))),
            " taxa segments (including Others when applicable)."
          )
        } else {
          last_run_note <- "Latest Top-N generate attempt did not return a valid comparison. Select at least two groups and try again."
        }
      } else {
        df_try <- tryCatch(microbiome_df(), error = function(e) NULL)
        if (!is.null(df_try) && nrow(df_try) > 0) {
          sample_count <- as.character(nrow(df_try))
          observed_groups <- paste(unique(as.character(stats::na.omit(df_try$group))), collapse = ", ")
          if (!nzchar(observed_groups)) observed_groups <- "No valid groups"
          last_run_note <- paste0(
            "Latest generated dataset contains ", nrow(df_try),
            " samples across ", length(unique(stats::na.omit(df_try$group))), " groups."
          )
        } else {
          last_run_note <- "Latest generate attempt did not return a valid dataset. Adjust filters and try again."
        }
      }
    }

    tags$div(
      tags$p(
        class = "summary-prose",
        "The current analytical setup focuses on ",
        tags$strong(or_else(input$bacteria_level, "an unspecified level")),
        " level abundance for ",
        tags$strong(or_else(input$bacteria_name, "an unspecified bacterium")),
        ", grouped by ",
        tags$strong(or_else(input$group_type, "an unspecified cohort rule")),
        ", using the ",
        tags$strong(method_label),
        " display mode. Based on the selected configuration, the app will use ",
        tags$strong(inferred_test),
        " when a valid dataset is generated."
      ),
      tags$blockquote(class = "summary-quote", last_run_note),
      tags$div(
        class = "summary-grid",
        tags$div(
          class = "summary-item",
          tags$div(class = "summary-item__label", "Current level"),
          tags$div(class = "summary-item__value", or_else(input$bacteria_level, "NA"))
        ),
        tags$div(
          class = "summary-item",
          tags$div(class = "summary-item__label", "Selected bacterium"),
          tags$div(class = "summary-item__value", or_else(input$bacteria_name, "NA"))
        ),
        tags$div(
          class = "summary-item",
          tags$div(class = "summary-item__label", "Group type"),
          tags$div(class = "summary-item__value", or_else(input$group_type, "NA"))
        ),
        tags$div(
          class = "summary-item",
          tags$div(class = "summary-item__label", "Selected groups"),
          tags$div(class = "summary-item__value", group_text)
        ),
        tags$div(
          class = "summary-item",
          tags$div(class = "summary-item__label", "Display method"),
          tags$div(class = "summary-item__value", method_label)
        ),
        tags$div(
          class = "summary-item",
          tags$div(class = "summary-item__label", "Inferred method"),
          tags$div(class = "summary-item__value", inferred_test)
        ),
        tags$div(
          class = "summary-item",
          tags$div(class = "summary-item__label", "Top N taxa"),
          tags$div(class = "summary-item__value", top_n_label)
        ),
        tags$div(
          class = "summary-item",
          tags$div(class = "summary-item__label", "Last generated samples"),
          tags$div(class = "summary-item__value", sample_count)
        ),
        tags$div(
          class = "summary-item",
          tags$div(class = "summary-item__label", "Observed groups (last run)"),
          tags$div(class = "summary-item__value", observed_groups)
        )
      )
    )
  })
  
  # 动态更新 Bacteria Name
  observeEvent(input$bacteria_level, {
    bacteria_name_choices <- switch(
      input$bacteria_level,
      "Phylum"  = phylum_name,
      "Class"   = class_name,
      "Order"   = order_name,
      "Family"  = family_name,
      "Genus"   = genus_name,
      "Species" = species_name,
      character(0)
    )
    
    updateSelectInput(
      session,
      inputId = "bacteria_name",
      choices = bacteria_name_choices,
      selected = if (length(bacteria_name_choices) > 0) bacteria_name_choices[1] else NULL
    )
  }, ignoreInit = FALSE)
  
  # 动态更新 Group
  observeEvent(input$group_type, {
    group_choices <- switch(
      input$group_type,
      "Affect or not"  = c("Negative", "Positive"),
      "HPV risk"       = c("Negative", "Low_risk", "High_risk"),
      "HPV persistent" = c("Non-Persistent", "Persistent"),
      character(0)
    )
    
    selected_groups <- if (input$group_type == "Affect or not") {
      c("Negative", "Positive")
    } else {
      group_choices
    }
    
    updateSelectInput(
      session,
      inputId = "group",
      choices = group_choices,
      selected = selected_groups
    )
  }, ignoreInit = FALSE)
  
  # 数据处理 reactive
  microbiome_df <- eventReactive(input$generate_plot, {
    req(input$bacteria_level, input$bacteria_name)
    req(input$group)
    validate(need(length(input$group) >= 2, "Please select at least two groups."))
    
    microbiome_dataset <- get_level_dataset(input$bacteria_level)
    req(!is.null(microbiome_dataset))
    
    validate(
      need(
        input$bacteria_name %in% rownames(microbiome_dataset$expression_data),
        "Selected bacteria not found in expression data."
      )
    )
    
    abund_vec <- as.numeric(microbiome_dataset$expression_data[input$bacteria_name, ])
    
    df <- data.frame(
      sample_id = colnames(microbiome_dataset$expression_data),
      abundance = abund_vec,
      stringsAsFactors = FALSE
    )
    
    group_var <- selected_group_var(input$group_type)
    
    sample_info <- microbiome_dataset$sample_info %>%
      dplyr::select(sample_id, !!sym(group_var))
    
    colnames(sample_info)[2] <- "group"
    
    df <- df %>%
      dplyr::left_join(sample_info, by = "sample_id")
    
    df <- df %>%
      dplyr::filter(group %in% input$group)
    df$group <- factor(df$group, levels = group_levels)
    
    shiny::validate(
      shiny::need(
        length(unique(df[["group"]])) >= 2,
        "Selected groups do not have enough data (need at least two groups with data)."
      )
    )
    
    df
  })
  
  topn_composition_df <- eventReactive(input$generate_plot, {
    req(input$bacteria_level, input$group_type, input$group)
    validate(need(length(input$group) >= 2, "Stacked bar comparison requires at least two selected groups."))
    
    microbiome_dataset <- get_level_dataset(input$bacteria_level)
    req(!is.null(microbiome_dataset))
    
    group_var <- selected_group_var(input$group_type)
    
    sample_info <- microbiome_dataset$sample_info %>%
      dplyr::select(sample_id, !!sym(group_var))
    colnames(sample_info)[2] <- "group"
    
    sample_info <- sample_info %>%
      dplyr::filter(group %in% input$group)
    
    common_samples <- intersect(colnames(microbiome_dataset$expression_data), sample_info$sample_id)
    validate(need(length(common_samples) > 0, "No overlapping samples found for selected groups."))
    
    sample_info <- sample_info %>%
      dplyr::filter(sample_id %in% common_samples)
    
    observed_groups <- unique(as.character(stats::na.omit(sample_info$group)))
    validate(need(length(observed_groups) >= 2, "Please select at least two groups with available data."))
    
    expr <- microbiome_dataset$expression_data[, sample_info$sample_id, drop = FALSE]
    sample_info$group <- as.character(sample_info$group)
    
    top_n <- suppressWarnings(as.integer(input$top_n_taxa))
    if (is.na(top_n) || top_n < 2) top_n <- 10L
    top_n <- min(top_n, nrow(expr))
    
    group_means_list <- lapply(unique(sample_info$group), function(g) {
      cols <- sample_info$sample_id[sample_info$group == g]
      vals <- if (length(cols) == 1) {
        as.numeric(expr[, cols, drop = TRUE])
      } else {
        rowMeans(expr[, cols, drop = FALSE], na.rm = TRUE)
      }
      data.frame(
        taxon = rownames(expr),
        group = g,
        abundance = as.numeric(vals),
        stringsAsFactors = FALSE
      )
    })
    comp_df <- do.call(rbind, group_means_list)
    
    overall_rank <- stats::aggregate(abundance ~ taxon, data = comp_df, FUN = mean, na.rm = TRUE)
    overall_rank <- overall_rank[order(overall_rank$abundance, decreasing = TRUE), , drop = FALSE]
    top_taxa <- head(overall_rank$taxon, top_n)
    
    comp_df$taxon_display <- ifelse(comp_df$taxon %in% top_taxa, comp_df$taxon, "Others")
    comp_df <- stats::aggregate(abundance ~ group + taxon_display, data = comp_df, FUN = sum, na.rm = TRUE)
    
    totals <- stats::aggregate(abundance ~ group, data = comp_df, FUN = sum, na.rm = TRUE)
    names(totals)[2] <- "group_total"
    comp_df <- merge(comp_df, totals, by = "group", all.x = TRUE, sort = FALSE)
    comp_df$proportion <- ifelse(comp_df$group_total > 0, comp_df$abundance / comp_df$group_total, NA_real_)
    
    tax_levels <- c(top_taxa, "Others")
    tax_levels <- tax_levels[tax_levels %in% unique(comp_df$taxon_display)]
    comp_df$taxon_display <- factor(comp_df$taxon_display, levels = rev(tax_levels))
    comp_df$group <- factor(comp_df$group, levels = group_levels)
    
    comp_df <- comp_df[order(comp_df$group, comp_df$taxon_display), , drop = FALSE]
    attr(comp_df, "n_samples") <- length(unique(sample_info$sample_id))
    attr(comp_df, "observed_groups") <- unique(sample_info$group)
    comp_df
  })
  
  # 绘图逻辑
  make_box_plot <- eventReactive(input$generate_plot, {
    df <- microbiome_df()
    req(nrow(df) > 0)
    
    p <- ggplot(df, aes(x = group, y = abundance)) +
      geom_boxplot(outlier.shape = NA, aes(color = group), show.legend = FALSE) +
      geom_jitter(
        width = 0.2, alpha = 0.6, shape = 21, size = 3,
        color = "black",
        aes(
          fill = group,
          text = paste0(
            "sample_id: ", sample_id,
            "<br>group: ", group,
            "<br>abundance: ", signif(abundance, 6)
          )
        ),
        show.legend = FALSE
      ) +
      labs(
        x     = input$group_type,
        y     = "Relative abundance",
        title = paste0(input$bacteria_level, " - ", input$bacteria_name)
      ) +
      theme_bw() +
      theme(
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "none"
      ) +
      scale_color_manual(values = group_color) +
      scale_fill_manual(values = group_color) +
      geom_signif(
        comparisons = combn(unique(df[["group"]]), 2, simplify = FALSE),
        map_signif_level = TRUE,
        test = "wilcox.test",
        tip_length = 0.01,
        textsize = 4
      )
    
    return(p)
  })
  
  make_topn_stacked_plot <- eventReactive(input$generate_plot, {
    comp_df <- topn_composition_df()
    req(nrow(comp_df) > 0)
    
    top_n <- suppressWarnings(as.integer(input$top_n_taxa))
    if (is.na(top_n) || top_n < 2) top_n <- 10L

    tax_levels <- levels(comp_df$taxon_display)
    if (is.null(tax_levels)) tax_levels <- unique(as.character(comp_df$taxon_display))
    tax_levels <- as.character(tax_levels)

    non_other_taxa <- setdiff(tax_levels, "Others")
    n_taxa <- length(non_other_taxa)

    palette_non_other <- if (n_taxa == 0) {
      character(0)
    } else if (exists("hcl.colors", where = asNamespace("grDevices"), mode = "function")) {
      grDevices::hcl.colors(n_taxa, palette = "Dynamic")
    } else if (n_taxa <= 12) {
      RColorBrewer::brewer.pal(max(3, n_taxa), "Set3")[seq_len(n_taxa)]
    } else {
      grDevices::colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))(n_taxa)
    }

    fill_values <- setNames(palette_non_other, non_other_taxa)
    if ("Others" %in% tax_levels) {
      fill_values <- c(fill_values, Others = "#b8b2a8")
    }
    
    p <- ggplot(comp_df, aes(x = group, y = proportion, fill = taxon_display)) +
      geom_col(width = 0.65, color = "white", size = 0.2) +
      scale_fill_manual(values = fill_values, drop = FALSE) +
      labs(
        x = input$group_type,
        y = "Mean relative abundance (proportion)",
        fill = "Taxa",
        title = paste0(input$bacteria_level, " - Top ", top_n, " taxa composition")
      ) +
      theme_bw() +
      theme(
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 20, hjust = 1),
        legend.position = "right",
        legend.key.height = grid::unit(0.45, "cm")
      )
    
    p
  })
  
  output$box_plot <- renderPlotly({
    p <- if (selected_method() == "topn_stacked_bar") {
      make_topn_stacked_plot()
    } else {
      make_box_plot()
    }

    gp <- if (selected_method() == "topn_stacked_bar") {
      ggplotly(p)
    } else {
      ggplotly(p, tooltip = "text")
    }

    # ggplotly may re-introduce boxplot outlier markers even when ggplot2
    # uses outlier.shape = NA, so disable them explicitly on box traces.
    if (selected_method() != "topn_stacked_bar" && !is.null(gp$x$data)) {
      for (i in seq_along(gp$x$data)) {
        trace_type <- gp$x$data[[i]]$type
        if (isTRUE(identical(trace_type, "box"))) {
          gp$x$data[[i]]$boxpoints <- FALSE
        }
      }
    }

    gp %>%
      layout(
        autosize = TRUE,
        plot_bgcolor  = "white",
        paper_bgcolor = "white",
        xaxis = list(showgrid = TRUE, gridcolor = "grey85"),
        yaxis = list(showgrid = TRUE, gridcolor = "grey85")
      )
  })
  
  # 统计表逻辑
  make_stats_table <- reactive({
    if (selected_method() == "topn_stacked_bar") {
      comp_df <- topn_composition_df()
      req(nrow(comp_df) > 0)
      
      out <- comp_df[, c("group", "taxon_display", "abundance", "proportion"), drop = FALSE]
      colnames(out) <- c("Group", "Taxon", "Mean_Abundance", "Proportion")
      out$Proportion <- round(out$Proportion, 4)
      out$Mean_Abundance <- round(out$Mean_Abundance, 6)
      out
    } else {
      df <- microbiome_df()
      req(nrow(df) > 0)
      
      groups <- unique(df[["group"]])
      
      # two groups：Wilcoxon rank-sum test
      if (length(groups) == 2) {
        g1 <- groups[1]
        g2 <- groups[2]
        x  <- df$abundance[df[["group"]] == g1]
        y  <- df$abundance[df[["group"]] == g2]
        
        wt <- tryCatch(
          wilcox.test(x, y),
          error = function(e) NULL
        )
        
        if (is.null(wt)) {
          data.frame(Test = "Wilcoxon rank-sum", Group1 = g1, Group2 = g2, p_value = NA_real_, stringsAsFactors = FALSE)
        } else {
          data.frame(Test = "Wilcoxon rank-sum", Group1 = g1, Group2 = g2, p_value = wt$p.value, stringsAsFactors = FALSE)
        }
        
      } else {
        # mulit-group：Kruskal–Wallis
        kt <- tryCatch(
          kruskal.test(abundance ~ group, data = df),
          error = function(e) NULL
        )
        
        data.frame(
          Test    = "Kruskal-Wallis",
          Groups  = paste(sort(groups), collapse = ", "),
          p_value = if (is.null(kt)) NA_real_ else kt$p.value,
          stringsAsFactors = FALSE
        )
      }
    }
  })
  
  output$stats_table <- renderDT({
    stats_table <- make_stats_table()
    datatable(stats_table)
  })
  
  # 下载逻辑
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("plot-", Sys.Date(), ".", tolower(input$filetype), sep = "")
    },
    content = function(file) {
      current_plot <- if (selected_method() == "topn_stacked_bar") {
        make_topn_stacked_plot()
      } else {
        make_box_plot()
      }

      if (input$filetype == "PNG") {
        png(file, width = input$width * 96, height = input$height * 96)
        print(current_plot)
        dev.off()
      } else if (input$filetype == "PDF") {
        pdf(file, width = input$width, height = input$height)
        print(current_plot)
        dev.off()
      }
    }
  )
}
