# Server
server <-
  function(input, output, session) {
    output$aboutContent <- renderUI({
      # Convert the RMarkdown to HTML
      tmp_about <- tempfile(fileext = ".html")
      # rmarkdown::render("markdown/about.Rmd", output_file = tmp_about)
      # Include the HTML content in the Shiny app
      includeHTML("markdown/about.html")
    })
    
    output$authorContent <- renderUI({
      # Convert the RMarkdown to HTML
      tmp_authors <- tempfile(fileext = ".html")
      # rmarkdown::render("markdown/authors.Rmd", output_file = tmp_authors)
      # Include the HTML content in the Shiny app
      includeHTML("markdown/authors.html")
    })
    
    # # Dynamic dropdown for molecules based on molecule type
    # observe({
    #   updateSelectInput(
    #     session = session,
    #     inputId = "molecule",
    #     choices = "",
    #     selected = ""
    #   )
    # })
    
    #####Dynamic dropdown for bacteria_name based on bacteria_level
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
        selected = if (length(bacteria_name_choices) > 0)
          bacteria_name_choices[1]
        else
          NULL
      )
    }, ignoreInit = FALSE)
    
    
    #####Dynamic dropdown for group based on group_type
    observeEvent(input$group_type, {
      group_choices <- switch(
        input$group_type,
        "Affect or not"  = c("Negative", "Positive"),
        "HPV risk"   = c("Negative", "Low_risk", "High_risk"),
        "HPV persistent"   = c("Non-Persistent", "Persistent"),
        character(0)
      )
      
      # Affect or not 默认选 Positive & Negative；其他类型默认全选
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
    
  
    ## ----------- 4. 只在点击按钮后，生成用于绘图/统计的数据 -----------
    
    microbiome_df <-
      eventReactive(input$generate_plot, {
        # 基本参数检查
        req(input$bacteria_level, input$bacteria_name)
        
        # 至少要选两个 group
        req(input$group)
        validate(need(length(input$group) >= 2, "Please select at least two groups."))
        
        microbiome_dataset <- get_level_dataset(input$bacteria_level)
        req(!is.null(microbiome_dataset))
        
        # 检查这个菌是否在表达矩阵里
        validate(
          need(
            input$bacteria_name %in% rownames(microbiome_dataset$expression_data),
            "Selected bacteria not found in expression data."
          )
        )
        
        # 当前菌的相对丰度
        abund_vec <-
          as.numeric(microbiome_dataset$expression_data[input$bacteria_name, ])
        
        df <- data.frame(
          sample_id = colnames(microbiome_dataset$expression_data),
          abundance = abund_vec,
          stringsAsFactors = FALSE
        )
        
        # 选哪个分组变量
        group_var <- switch(
          input$group_type,
          "Affect or not"  = "Affect",
          "HPV risk"       = "risk",
          # 如果列名不同，在这里改
          "HPV persistent" = "persistent",
          "Affect"         # 默认兜底
        )
        
        sample_info <-
          microbiome_dataset$sample_info %>%
          dplyr::select(sample_id, !!sym(group_var))
        
        colnames(sample_info)[2] <- "group"
        
        df <-
          df %>%
          dplyr::left_join(sample_info, by = "sample_id")
        
        # 按选定 group 过滤
        df <- df %>%
          dplyr::filter(group %in% input$group)
        df$group <-
          factor(df$group, levels = group_levels)
        
        shiny::validate(
          shiny::need(
            length(unique(df[["group"]])) >= 2,
            "Selected groups do not have enough data (need at least two groups with data)."
          )
        )
        
        df
      })
    
    
    make_box_plot <- eventReactive(input$generate_plot, {
      df <- microbiome_df()
      req(nrow(df) > 0)
      group_var <- "group"
      
      p <-
        ggplot(df, aes(x = group, y = abundance)) +
        geom_boxplot(outlier.shape = NA, aes(color = group),
                     show.legend = FALSE) +
        geom_jitter(
          width = 0.2,
          alpha = 0.6,
          shape = 21,
          size = 3,
          color = "black",
          aes(fill = group),
          show.legend = FALSE
        ) +
        labs(
          x     = input$group_type,
          y     = "Relative abundance",
          title = paste0(input$bacteria_level, " - ", input$bacteria_name)
        ) +
        theme_bw() +
        theme(panel.grid.minor = element_blank(),
              axis.text.x = element_text(angle = 30, hjust = 1),
              legend.position = "none") +
        scale_color_manual(values = group_color) +
        scale_fill_manual(values = group_color) +
        ggsignif::geom_signif(
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
          xaxis = list(
            showgrid = TRUE,
            gridcolor = "grey85"
          ),
          yaxis = list(
            showgrid = TRUE,
            gridcolor = "grey85"
          )
        )
    })
    
    
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
          data.frame(
            Test   = "Wilcoxon rank-sum",
            Group1 = g1,
            Group2 = g2,
            p_value = NA_real_,
            stringsAsFactors = FALSE
          )
        } else {
          data.frame(
            Test   = "Wilcoxon rank-sum",
            Group1 = g1,
            Group2 = g2,
            p_value = wt$p.value,
            stringsAsFactors = FALSE
          )
        }
        
      } else {
        # 多组：Kruskal–Wallis
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
    
    output$downloadPlot <- downloadHandler(
      filename = function() {
        paste("plot-", Sys.Date(), ".", tolower(input$filetype), sep = "")
      },
      content = function(file) {
        if (input$filetype == "PNG") {
          png(file,
              width = input$width * 96,
              height = input$height * 96)
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
