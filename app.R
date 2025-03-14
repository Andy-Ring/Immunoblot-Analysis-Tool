library(shiny)
library(bslib)
library(magick)
library(imager)
library(ggplot2)
library(DT)
library(ggpubr)
library(car)
library(dplyr)
library(rlang)
library(thematic)

thematic_shiny()

ui <- fluidPage(
  theme = bs_theme(
    bootswatch = "darkly",
    secondary = "#BA0C2F",
    "table-bg" = "primary"
  ),
  titlePanel("Immunoblot Analysis Tool"),
  sidebarLayout(
    sidebarPanel(
      HTML('<img src="logo.png" width="100%" height="auto">'),
      br(), br(),
      tabsetPanel(
        tabPanel("Image Quantification",
                 fileInput("image_upload", "Upload Immunoblot Image",
                           accept = c("image/png", "image/jpeg", "image/tiff")),
                 hr(),
                 h4("Image Preprocessing Options"),
                 checkboxInput("mirror_horiz", "Mirror Horizontally", value = FALSE),
                 checkboxInput("mirror_vert", "Mirror Vertically", value = FALSE),
                 sliderInput("rotate_angle", "Rotate Angle (degrees)", min = 0, max = 360, value = 0),
                 sliderInput("brightness", "Brightness Adjustment", 
                             min = -100, max = 100, value = 0),
                 sliderInput("contrast", "Contrast Adjustment", 
                             min = -100, max = 100, value = 0),
                 checkboxInput("grayscale", "Convert to Grayscale", value = TRUE),
                 hr(),
                 h4("Zoom Options"),
                 # Zoom slider: 100 = no zoom; higher values zoom in (smaller viewing window)
                 sliderInput("zoom", "Zoom Percentage", min = 100, max = 300, value = 100),
                 numericInput("center_x", "Center X", value = NA),
                 numericInput("center_y", "Center Y", value = NA),
                 actionButton("reset_zoom", "Reset Zoom")
                 
        ),
        tabPanel("Analysis",
                 fileInput("analysis_csv", "Upload Data CSV", accept = ".csv"),
                 uiOutput("col_select_ui"),
                 checkboxInput("pairwise", "Perform Pairwise Comparisons", value = FALSE),
                 actionButton("recommendTest", "Recommend Best Test"),
                 verbatimTextOutput("recommendedTest"),
                 selectInput("selectedTest", "Select Statistical Test", choices = c()),
                 selectInput("control_group", "Select Control Group for Normalization", choices = NULL),
                 actionButton("runAnalysis", "Run Analysis"),
                 plotOutput("analysisPlot")
        ),
        tabPanel("Plot Customization",
                 # Reordered inputs for better flow:
                 textInput("plot_title", "Plot Title", value = "Analysis Plot"),
                 sliderInput("title_text_size", "Plot Title Text Size", min = 10, max = 30, value = 16),
                 textInput("x_title", "X-axis Title", value = "Treatment Group"),
                 textInput("y_title", "Y-axis Title", value = "Normalized Expression"),
                 sliderInput("axis_title_text_size", "Axis Titles Text Size", min = 8, max = 20, value = 12),
                 textInput("legend_title", "Group Legend Title", value = "Group"),
                 sliderInput("legend_title_text_size", "Legend Title Text Size", min = 8, max = 30, value = 12),
                 selectizeInput("group_colors_select", "Select Group Colors",
                                choices = c(
                                  "Black" = "#000000",
                                  "White" = "#FFFFFF",
                                  "Red" = "#FF0000",
                                  "Blue" = "#0000FF",
                                  "Green" = "#008000",
                                  "Yellow" = "#FFFF00",
                                  "Orange" = "#FFA500",
                                  "Purple" = "#800080",
                                  "Pink" = "#FFC0CB",
                                  "Brown" = "#A52A2A",
                                  "Gray" = "#808080",
                                  "Cyan" = "#00FFFF",
                                  "Magenta" = "#FF00FF",
                                  "Lime" = "#00FF00",
                                  "Navy" = "#000080",
                                  "Maroon" = "#800000",
                                  "Olive" = "#808000",
                                  "Teal" = "#008080",
                                  "Violet" = "#EE82EE",
                                  "Gold" = "#FFD700"
                                ),
                                selected = NULL, multiple = TRUE),
                 sliderInput("group_text_size", "X-axis (Group Names) Text Size", min = 8, max = 20, value = 12),
                 sliderInput("y_axis_text_size", "Y-axis Tick Text Size", min = 8, max = 20, value = 12),
                 textInput("group_order", "Group Order (comma-separated, optional)", value = ""),
                 selectInput("ggplot_theme", "Choose ggplot Theme", 
                             choices = c("theme_gray", "theme_bw", "theme_light", "theme_minimal", "theme_classic"), 
                             selected = "theme_gray"),
                 # New slider for significance label text size:
                 sliderInput("sig_label_text_size", "Significance Label Text Size", min = 1, max = 10, value = 5),
                 br(),
                 actionButton("update_customizations", "Update Plot Customizations")
        )
      ),
      br(),
      tags$p("Created by Andy Ring"),
      tags$p("Version 1.0.0 | March 14th, 2025")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Image Quantification",
                 layout_columns(
                   # Increase plot height for better viewing
                   plotOutput("interactive_image", height = "600px", 
                              brush = brushOpts(id = "image_brush", resetOnNew = FALSE)),
                   card(card_title("Lane Selection"),
                        actionButton("set_background", "Set Background Region"),
                        actionButton("add_region", "Add Lane Region"),
                        downloadButton("downloadData", "Download Regions Data")
                   ),
                   card(card_title("Results Table"),
                        DTOutput("regions_table")
                   ),
                   col_widths = c(12,3,9)
                 )
        ),
        tabPanel("Analysis",
                 layout_columns(
                   card(plotOutput("analysisPlot")),
                   card( card_title("Statistical Test Results"),
                     DT::dataTableOutput("stat_test_table")  # New card for test results table
                   ),
                   card(
                     downloadButton("downloadPlot", "Download Plot"),
                     downloadButton("downloadRDS", "Download RDS File")
                   ),
                   col_widths = c(12,9,3)
                 )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  ### ----------------- Image Quantification Code ----------------- ###
  
  processed_image <- reactive({
    req(input$image_upload)
    img <- magick::image_read(input$image_upload$datapath)
    if (input$mirror_horiz) {
      img <- magick::image_flop(img)  # mirror horizontally
    }
    if (input$mirror_vert) {
      img <- magick::image_flip(img)  # mirror vertically
    }
    if (input$rotate_angle != 0) {
      img <- magick::image_rotate(img, input$rotate_angle)
    }
    if (input$grayscale) {
      img <- magick::image_convert(img, colorspace = "gray")
    }
    brightness <- 100 + input$brightness  # 100 means no change
    img <- magick::image_modulate(img, brightness = brightness)
    if (input$contrast != 0) {
      n <- abs(round(input$contrast / 10))
      for (i in seq_len(n)) {
        img <- magick::image_contrast(img, sharpen = (input$contrast > 0))
      }
    }
    img
  })
  
  img_info <- reactive({
    req(processed_image())
    info <- magick::image_info(processed_image())
    list(width = info$width, height = info$height)
  })
  
  imager_img <- reactive({
    req(processed_image())
    tmpfile <- tempfile(fileext = ".png")
    magick::image_write(processed_image(), path = tmpfile, format = "png")
    imager::load.image(tmpfile)
  })
  
  observeEvent(processed_image(), {
    info <- img_info()
    updateNumericInput(session, "center_x", value = info$width / 2)
    updateNumericInput(session, "center_y", value = info$height / 2)
  })
  
  observeEvent(input$reset_zoom, {
    req(img_info())
    info <- img_info()
    updateNumericInput(session, "center_x", value = info$width / 2)
    updateNumericInput(session, "center_y", value = info$height / 2)
    updateSliderInput(session, "zoom", value = 100)
  })
  
  current_brush <- reactiveVal(NULL)
  observeEvent(input$image_brush, {
    current_brush(input$image_brush)
  })
  
  # Initialize regions without the redundant norm_intensity column
  regions <- reactiveVal(data.frame(
    Region = character(),
    xmin = numeric(),
    xmax = numeric(),
    ymin = numeric(),
    ymax = numeric(),
    intensity = numeric(),
    stringsAsFactors = FALSE
  ))
  
  background_intensity <- reactiveVal(NULL)
  
  # Reactive value to store lane dimensions from region 1
  lane_dims <- reactiveVal(NULL)
  
  output$interactive_image <- renderPlot({
    req(processed_image(), img_info())
    info <- img_info()
    full_width <- info$width
    full_height <- info$height
    
    # Zoom logic: slider value / 100 gives zoom factor (100 = factor 1)
    zoom_factor <- input$zoom / 100  
    viewWidth <- full_width / zoom_factor
    viewHeight <- full_height / zoom_factor
    
    center_x <- ifelse(is.null(input$center_x) || is.na(input$center_x), full_width / 2, input$center_x)
    center_y <- ifelse(is.null(input$center_y) || is.na(input$center_y), full_height / 2, input$center_y)
    
    xlim <- c(center_x - viewWidth / 2, center_x + viewWidth / 2)
    ylim <- c(center_y + viewHeight / 2, center_y - viewHeight / 2)
    
    xlim[1] <- max(0, xlim[1])
    xlim[2] <- min(full_width, xlim[2])
    ylim[1] <- min(full_height, ylim[1])
    ylim[2] <- max(0, ylim[2])
    
    plot(1, type = "n", xlim = xlim, ylim = ylim,
         xlab = "", ylab = "", asp = 1, axes = FALSE)
    
    img_raster <- as.raster(processed_image())
    rasterImage(img_raster, 0, 0, full_width, full_height)
    
    # Draw previously added regions
    if (nrow(regions()) > 0) {
      for (i in seq_len(nrow(regions()))) {
        reg <- regions()[i, ]
        rect(reg$xmin, reg$ymin, reg$xmax, reg$ymax, border = "blue", lwd = 2)
        text((reg$xmin + reg$xmax) / 2, (reg$ymin + reg$ymax) / 2,
             labels = reg$Region, col = "blue")
      }
    }
    
    # Draw preview rectangle for current brush using fixed dimensions:
    # Use the brush's coordinates for both x and y if no previous region exists.
    br <- current_brush()
    if (!is.null(br)) {
      x_min_br <- floor(br$xmin) + 1
      dims <- lane_dims()
      if (is.null(dims)) {
        fixed_width <- floor(br$xmax) - floor(br$xmin) + 1
        fixed_y_min <- floor(br$ymin) + 1
        fixed_y_max <- floor(br$ymax) + 1
      } else {
        fixed_width <- dims$width
        fixed_y_min <- dims$y_min
        fixed_y_max <- dims$y_max
      }
      x_max_fixed <- min(x_min_br + fixed_width, full_width)
      rect(x_min_br, fixed_y_min, x_max_fixed, fixed_y_max, border = "red", lwd = 2, lty = 2)
    }
  })
  
  
  observeEvent(input$set_background, {
    req(current_brush(), imager_img(), img_info())
    br <- current_brush()
    info <- img_info()
    full_width <- info$width
    full_height <- info$height
    
    # Use brush for horizontal limits only; vertical fixed to full height
    x_min <- floor(br$xmin) + 1
    x_max <- floor(br$xmax) + 1
    x_min <- max(1, x_min)
    x_max <- min(full_width, x_max)
    
    im_img <- imager_img()
    bg_img <- imager::imsub(im_img, x %in% c(x_min:x_max), y %in% c(1:full_height))
    
    bg_val <- 1 - mean(bg_img)
    background_intensity(bg_val)
    
    showNotification(paste("Background mean intensity set to", round(bg_val, 2)))
    current_brush(NULL)
  })
  
  observeEvent(input$add_region, {
    req(current_brush(), imager_img(), img_info())
    br <- current_brush()
    info <- img_info()
    full_width <- info$width
    full_height <- info$height
    
    x_min <- floor(br$xmin) + 1
    dims <- lane_dims()
    if (is.null(dims)) {
      fixed_width <- floor(br$xmax) - floor(br$xmin) + 1
      fixed_y_min <- floor(br$ymin) + 1
      fixed_y_max <- floor(br$ymax) + 1
    } else {
      fixed_width <- dims$width
      fixed_y_min <- dims$y_min
      fixed_y_max <- dims$y_max
    }
    
    x_max <- min(x_min + fixed_width, full_width)
    y_min <- fixed_y_min
    y_max <- fixed_y_max
    
    im_img <- imager_img()
    sub_img <- imager::imsub(im_img, x %in% c(x_min:x_max), y %in% c(y_min:y_max))
    inverted_img <- 1 - sub_img  # Invert grayscale values
    lane_intensity <- sum(inverted_img > background_intensity())  # Count pixels above background
    
    
    new_region <- data.frame(
      Region = paste0("Region_", nrow(regions()) + 1),
      xmin = x_min,
      xmax = x_max,
      ymin = y_min,
      ymax = y_max,
      intensity = lane_intensity,
      stringsAsFactors = FALSE
    )
    
    # For the first region, store both the fixed width and the vertical limits
    if (nrow(regions()) == 0) {
      lane_dims(list(width = fixed_width, y_min = fixed_y_min, y_max = fixed_y_max))
    }
    
    regions(rbind(regions(), new_region))
    current_brush(NULL)
  })
  
  
  output$regions_table <- renderDT({
    datatable(regions(), options = list(pageLength = 5), editable = TRUE)
  })
  
  observeEvent(input$regions_table_cell_edit, {
    info <- input$regions_table_cell_edit
    i <- info$row
    j <- info$col
    v <- info$value
    new_regions <- regions()
    new_regions[i, j] <- v
    regions(new_regions)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("regions_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(regions(), file, row.names = FALSE)
    }
  )
  
  ### ----------------- Analysis Panel Code ----------------- ###
  
  observeEvent(processedData(), {
    req(processedData())
    updateSelectInput(session, "control_group", 
                      choices = unique(processedData()[[input$group_col]]),
                      selected = unique(processedData()[[input$group_col]])[1])
  })
  
  
  analysisData <- reactive({
    req(input$analysis_csv)
    read.csv(input$analysis_csv$datapath, stringsAsFactors = FALSE)
  })
  
  output$col_select_ui <- renderUI({
    req(analysisData())
    cols <- names(analysisData())
    tagList(
      selectInput("group_col", "Group (Treatment) Column", choices = cols, selected = "Group"),
      selectInput("protExp_col", "Protein Expression Column", choices = cols, selected = "ProtIntensity"),
      selectInput("loadControl_col", "Loading Control Column", choices = cols, selected = "LoadingIntensity")
    )
  })
  
  processedData <- reactive({
    req(analysisData(), input$group_col, input$protExp_col, input$loadControl_col)
    df <- analysisData()
    prot_values <- as.numeric(gsub("[^0-9.-]", "", df[[input$protExp_col]]))
    load_values <- as.numeric(gsub("[^0-9.-]", "", df[[input$loadControl_col]]))
    if(mean(is.na(prot_values)) > 0.5 || mean(is.na(load_values)) > 0.5) {
      showNotification("Selected columns do not contain valid numeric data. Please select correct columns.", type = "error")
      return(NULL)
    }
    df[[input$protExp_col]] <- prot_values
    df[[input$loadControl_col]] <- load_values
    df$calc_intensity <- with(df, ifelse(load_values == 0, NA, prot_values / load_values))
    df[[input$group_col]] <- as.factor(df[[input$group_col]])
    df
  })
  
  basePlotObj <- reactiveVal(NULL)
  analysisPlotObj <- reactiveVal(NULL)
  
  observeEvent(input$recommendTest, {
    req(processedData())
    df <- processedData()
    groups <- levels(df[[input$group_col]])
    bestTest <- ""
    recommendation <- ""
    
    if(length(groups) < 2) {
      bestTest <- "None"
      recommendation <- "No statistical test recommended because there is only one group."
    } else if(input$pairwise) {
      if(length(groups) == 2) {
        normTest <- tryCatch(shapiro.test(df$calc_intensity)$p.value, error = function(e) NA)
        varTest <- tryCatch(car::leveneTest(calc_intensity ~ df[[input$group_col]], data = df)$`Pr(>F)`[1], error = function(e) NA)
        if(!is.na(normTest) && normTest > 0.05) {
          if(!is.na(varTest) && varTest > 0.05) {
            bestTest <- "t.test"
            recommendation <- "Pairwise t-test: Data is normally distributed with equal variances."
          } else {
            bestTest <- "welch.test"
            recommendation <- "Pairwise Welch's t-test: Data is normally distributed but variances are unequal."
          }
        } else {
          bestTest <- "wilcox.test"
          recommendation <- "Pairwise Wilcox Test: Data is not normally distributed."
        }
      } else {
        normTest <- tryCatch(shapiro.test(df$calc_intensity)$p.value, error = function(e) NA)
        varTest <- tryCatch(car::leveneTest(calc_intensity ~ df[[input$group_col]], data = df)$`Pr(>F)`[1], error = function(e) NA)
        if(!is.na(normTest) && normTest > 0.05) {
          if(!is.na(varTest) && varTest > 0.05) {
            bestTest <- "t.test"
            recommendation <- "Pairwise t-test: Data is normally distributed with equal variances."
          } else {
            bestTest <- "welch.test"
            recommendation <- "Pairwise Welch's t-test: Data is normally distributed but variances are unequal."
          }
        } else {
          bestTest <- "wilcox.test"
          recommendation <- "Pairwise Wilcox Test: Data is not normally distributed."
        }
      }
    } else {
      if(length(groups) == 2) {
        normTest <- tryCatch(shapiro.test(df$calc_intensity)$p.value, error = function(e) NA)
        varTest <- tryCatch(car::leveneTest(calc_intensity ~ df[[input$group_col]], data = df)$`Pr(>F)`[1], error = function(e) NA)
        if(!is.na(normTest) && normTest > 0.05) {
          if(!is.na(varTest) && varTest > 0.05) {
            bestTest <- "t.test"
            recommendation <- "T-test: Data is normally distributed with equal variances."
          } else {
            bestTest <- "welch.test"
            recommendation <- "Welch's t-test: Data is normally distributed but variances are unequal."
          }
        } else {
          bestTest <- "wilcox.test"
          recommendation <- "Wilcox Test: Data is not normally distributed."
        }
      } else {
        normTest <- tryCatch(shapiro.test(df$calc_intensity)$p.value, error = function(e) NA)
        varTest <- tryCatch(car::leveneTest(calc_intensity ~ df[[input$group_col]], data = df)$`Pr(>F)`[1], error = function(e) NA)
        if(!is.na(normTest) && normTest > 0.05) {
          if(!is.na(varTest) && varTest > 0.05) {
            bestTest <- "anova"
            recommendation <- "ANOVA: Data is normally distributed with equal variances."
          } else {
            bestTest <- "welch.anova"
            recommendation <- "Welch's ANOVA: Data is normally distributed but variances are unequal."
          }
        } else {
          bestTest <- "kruskal.test"
          recommendation <- "Kruskal-Wallis Test: Data is not normally distributed."
        }
      }
    }
    
    output$recommendedTest <- renderText({
      paste("Recommended Test:", bestTest, "\n", recommendation)
    })
    
    updateSelectInput(session, "selectedTest", choices = c(bestTest), selected = bestTest)
  })
  
  observeEvent(input$runAnalysis, {
    req(processedData())
    df <- processedData()
    
    # Get mean intensity of the selected control group
    control_mean <- mean(df$calc_intensity[df[[input$group_col]] == input$control_group], na.rm = TRUE)
    
    # Normalize all intensities to the control group mean
    df$normalized_intensity <- df$calc_intensity / control_mean
    
    
    # Compute summary statistics for each group
    summaryData <- df %>%
      dplyr::group_by(!!sym(input$group_col)) %>%
      dplyr::summarise(mean_intensity = mean(normalized_intensity, na.rm = TRUE),
                       se = sd(normalized_intensity, na.rm = TRUE)/sqrt(n()),
                       .groups = "drop")
    
    p <- ggplot() +
      geom_bar(data = summaryData,
               aes_string(x = input$group_col, y = "mean_intensity", fill = input$group_col),
               stat = "identity", position = position_dodge(width = 0.9))+
      geom_errorbar(data = summaryData,
                    aes_string(x = input$group_col,
                               ymin = "mean_intensity - se",
                               ymax = "mean_intensity + se"),
                    color = "black",
                    position = position_dodge(width = 0.9), width = 0.2) +
      geom_jitter(data = df,
                  aes_string(x = input$group_col, y = "normalized_intensity"),
                  position = position_jitter(width = 0.2), size = 2, color = "black", alpha = 0.7) +
      labs(title = input$plot_title, x = input$x_title, y = input$y_title) +
      theme_light()
    
    formula_str <- paste("normalized_intensity ~", input$group_col)
    
    # Calculate the global maximum error bar top (mean + se)
    global_max <- max(summaryData$mean_intensity + summaryData$se, na.rm = TRUE)
    offset <- 0.1 * global_max  # 10% offset above the tallest error bar
    base_y <- global_max + offset
    
    if (input$pairwise) {
      comp_results <- ggpubr::compare_means(as.formula(formula_str), 
                                            data = df, 
                                            method = input$selectedTest)
      group_levels <- levels(df[[input$group_col]])
      comp_results$xmin <- sapply(comp_results$group1, function(x) which(group_levels == x))
      comp_results$xmax <- sapply(comp_results$group2, function(x) which(group_levels == x))
      
      # Set all significance labels to the same global y position above the tallest error bar
      comp_results$y.position <- rep(base_y, nrow(comp_results))
      
      p <- p + ggpubr::stat_pvalue_manual(comp_results, 
                                          label = "p.signif", 
                                          tip.length = 0.01,
                                          step.increase = 0.1,
                                          size = input$sig_label_text_size)
      
      # Remove unwanted columns from the results table
      output$stat_test_table <- DT::renderDataTable({
        comp_results_clean <- comp_results[, !names(comp_results) %in% c("xmin", "xmax", "y.position", ".y.", "p.adj")]
        comp_results_clean
      })
      
    } else {
      overall <- ggpubr::compare_means(as.formula(formula_str), 
                                       data = df, 
                                       method = input$selectedTest)
      p <- p + annotate("text", 
                        x = median(as.numeric(factor(df[[input$group_col]]))), 
                        y = base_y, 
                        label = paste0("p = ", overall$p.format[1]),
                        size = input$sig_label_text_size,
                        color = "black")
      
      output$stat_test_table <- DT::renderDataTable({
        overall_clean <- overall[, !names(overall) %in% c("xmin", "xmax", "y.position", "y")]
        overall_clean
      })
    }
    
    basePlotObj(p)
    analysisPlotObj(p)
    output$analysisPlot <- renderPlot({ analysisPlotObj() })
  })
  
  observeEvent(input$update_customizations, {
    req(basePlotObj(), processedData())
    p <- basePlotObj()
    
    theme_fun <- get(input$ggplot_theme, mode = "function")
    p <- p + theme_fun()
    
    if(length(input$group_colors_select) > 0) {
      df <- processedData()
      n_groups <- length(levels(df[[input$group_col]]))
      p <- p + scale_fill_manual(name = input$legend_title, 
                                 values = rep(input$group_colors_select, length.out = n_groups))
    } else {
      p <- p + labs(fill = input$legend_title)
    }
    
    if(nchar(trimws(input$group_order)) > 0) {
      new_order <- trimws(unlist(strsplit(input$group_order, ",")))
      p <- p + scale_x_discrete(limits = new_order)
    }
    
    p <- p + theme(axis.text.x = element_text(angle = -45, hjust = 0, size = input$group_text_size),
                   axis.text.y = element_text(size = input$y_axis_text_size),
                   legend.text = element_text(size = input$legend_text_size),
                   legend.title = element_text(size = input$legend_title_text_size),
                   plot.title = element_text(size = input$title_text_size),
                   axis.title = element_text(size = input$axis_title_text_size))
    
    analysisPlotObj(p)
    output$analysisPlot <- renderPlot({ analysisPlotObj() })
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste0("analysis_plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(analysisPlotObj())
      final_plot <- analysisPlotObj() + theme_light()
      ggsave(file, plot = final_plot, device = "png", width = 8, height = 6)
    }
  )
  
  output$downloadRDS <- downloadHandler(
    filename = function() {
      paste0("analysis_plot_", Sys.Date(), ".rds")
    },
    content = function(file) {
      req(analysisPlotObj())
      saveRDS(analysisPlotObj(), file = file)
    }
  )
}

shinyApp(ui, server)








