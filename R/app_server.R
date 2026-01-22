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
  
  # =========================================================
  # 1. 加载数据 (原 global.R 内容)
  # 使用 system.file 定位 inst/data 下的文件
  # =========================================================
  load(system.file("data/phylum_name.rda", package = "chicoshiny"))
  load(system.file("data/class_name.rda", package = "chicoshiny"))
  load(system.file("data/order_name.rda", package = "chicoshiny"))
  load(system.file("data/family_name.rda", package = "chicoshiny"))
  load(system.file("data/genus_name.rda", package = "chicoshiny"))
  load(system.file("data/species_name.rda", package = "chicoshiny"))
  
  load(system.file("data/expression_data_phylum.rda", package = "chicoshiny"))
  load(system.file("data/sample_info_phylum.rda", package = "chicoshiny"))
  load(system.file("data/variable_info_phylum.rda", package = "chicoshiny"))
  
  load(system.file("data/expression_data_class.rda", package = "chicoshiny"))
  load(system.file("data/sample_info_class.rda", package = "chicoshiny"))
  load(system.file("data/variable_info_class.rda", package = "chicoshiny"))
  
  load(system.file("data/expression_data_order.rda", package = "chicoshiny"))
  load(system.file("data/sample_info_order.rda", package = "chicoshiny"))
  load(system.file("data/variable_info_order.rda", package = "chicoshiny"))
  
  load(system.file("data/expression_data_family.rda", package = "chicoshiny"))
  load(system.file("data/sample_info_family.rda", package = "chicoshiny"))
  load(system.file("data/variable_info_family.rda", package = "chicoshiny"))
  
  load(system.file("data/expression_data_genus.rda", package = "chicoshiny"))
  load(system.file("data/sample_info_genus.rda", package = "chicoshiny"))
  load(system.file("data/variable_info_genus.rda", package = "chicoshiny"))
  
  load(system.file("data/expression_data_species.rda", package = "chicoshiny"))
  load(system.file("data/sample_info_species.rda", package = "chicoshiny"))
  load(system.file("data/variable_info_species.rda", package = "chicoshiny"))
  
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
  
  # =========================================================
  # 3. Server 核心逻辑
  # =========================================================
  
  output$aboutContent <- renderUI({
    # 修正路径：使用 system.file 读取 inst/markdown 下的文件
    html_path <- system.file("markdown/about.html", package = "chicoshiny")
    if(html_path != "") {
      includeHTML(html_path)
    } else {
      h3("File markdown/about.html not found.")
    }
  })
  
  output$authorContent <- renderUI({
    html_path <- system.file("markdown/authors.html", package = "chicoshiny")
    if(html_path != "") {
      includeHTML(html_path)
    } else {
      h3("File markdown/authors.html not found.")
    }
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
    
    group_var <- switch(
      input$group_type,
      "Affect or not"  = "Affect",
      "HPV risk"       = "risk",
      "HPV persistent" = "persistent",
      "Affect"          
    )
    
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
  
  # 绘图逻辑
  make_box_plot <- eventReactive(input$generate_plot, {
    df <- microbiome_df()
    req(nrow(df) > 0)
    
    p <- ggplot(df, aes(x = group, y = abundance)) +
      geom_boxplot(outlier.shape = NA, aes(color = group), show.legend = FALSE) +
      geom_jitter(
        width = 0.2, alpha = 0.6, shape = 21, size = 3,
        color = "black", aes(fill = group), show.legend = FALSE
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
  
  output$box_plot <- renderPlotly({
    p <- make_box_plot()
    ggplotly(p) %>%
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
      if (input$filetype == "PNG") {
        png(file, width = input$width * 96, height = input$height * 96)
        print(make_box_plot())
        dev.off()
      } else if (input$filetype == "PDF") {
        pdf(file, width = input$width, height = input$height)
        print(make_box_plot())
        dev.off()
      }
    }
  )
}