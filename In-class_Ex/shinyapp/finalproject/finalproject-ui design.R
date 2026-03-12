#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(cluster)
library(DT)

# =========================
# Load data
# =========================
df <- read_excel("data/data.xlsx")

# =========================
# Clean data
# =========================
df$country  <- str_trim(str_to_lower(df$country))
df$category <- str_trim(str_to_lower(df$category))
df$year     <- as.integer(df$year)
df$month    <- as.integer(df$month)
df$total    <- as.numeric(df$total)

df <- df[!is.na(df$country) &
           !is.na(df$category) &
           !is.na(df$year) &
           !is.na(df$month) &
           !is.na(df$total), ]

# =========================
# Wide format
# =========================
df_wide <- pivot_wider(
  df,
  names_from = category,
  values_from = total
)

df_wide <- df_wide[order(df_wide$country, df_wide$year, df_wide$month), ]

df_wide$trade_balance <- df_wide[["output"]] - df_wide[["input"]]
df_wide$trade_ratio <- ifelse(
  df_wide[["input"]] != 0,
  df_wide[["output"]] / df_wide[["input"]],
  NA
)

# =========================
# UI
# =========================
ui <- dashboardPage(
  dashboardHeader(title = "Clustering Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Country Trade Behaviour Segmentation",
        tabName = "cluster1",
        icon = icon("chart-bar")
      ),
      menuItem(
        "Seasonal Pattern Clustering",
        tabName = "cluster2",
        icon = icon("chart-line")
      ),
      menuItem(
        "Cluster Comparison Summary",
        tabName = "cluster3",
        icon = icon("table-cells")
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # =========================
      # Part 1
      # =========================
      tabItem(
        tabName = "cluster1",
        
        fluidRow(
          box(
            title = "Desired Characteristic",
            width = 3,
            status = "danger",
            solidHeader = TRUE,
            
            sliderInput(
              "year_range",
              "Select Year Range:",
              min = min(df_wide$year, na.rm = TRUE),
              max = max(df_wide$year, na.rm = TRUE),
              value = c(min(df_wide$year, na.rm = TRUE), max(df_wide$year, na.rm = TRUE)),
              sep = ""
            ),
            
            sliderInput(
              "k",
              "Number of Clusters:",
              min = 2,
              max = 5,
              value = 3
            ),
            
            actionButton("run_cluster", "Run Cluster"),
            
            br(), br(),
            
            box(
              title = "Chart Interpretation",
              width = 12,
              status = "danger",
              solidHeader = TRUE,
              collapsible = TRUE,
              p("This module groups countries with similar trade behaviour based on average input, output, trade balance, trade ratio, and variability."),
              p("Users can compare cluster sizes, cluster characteristics, and the country-level summary table.")
            )
          ),
          
          box(
            title = "Cluster Charts",
            width = 4,
            status = "danger",
            solidHeader = TRUE,
            
            tabsetPanel(
              tabPanel(
                "Cluster Proportion",
                br(),
                plotOutput("cluster_count_plot", height = "350px")
              ),
              tabPanel(
                "Cluster Characteristics",
                br(),
                plotOutput("cluster_char_plot", height = "460px"),
                br(),
                plotOutput("cluster_ratio_plot", height = "320px")
              )
            )
          ),
          
          box(
            title = "Country Summary Table",
            width = 5,
            status = "danger",
            solidHeader = TRUE,
            DTOutput("country_table")
          )
        )
      ),
      
      # =========================
      # Part 2
      # =========================
      tabItem(
        tabName = "cluster2",
        
        fluidRow(
          box(
            title = "Desired Characteristic",
            width = 3,
            status = "danger",
            solidHeader = TRUE,
            
            sliderInput(
              "season_year_range",
              "Select Year Range:",
              min = min(df_wide$year, na.rm = TRUE),
              max = max(df_wide$year, na.rm = TRUE),
              value = c(min(df_wide$year, na.rm = TRUE), max(df_wide$year, na.rm = TRUE)),
              sep = ""
            ),
            
            selectInput(
              "season_var",
              "Select Trade Variable:",
              choices = c(
                "Output" = "avg_output",
                "Input" = "avg_input",
                "Balance" = "avg_balance"
              ),
              selected = "avg_output"
            ),
            
            actionButton("run_season", "Run Seasonal Analysis"),
            
            br(), br(),
            
            box(
              title = "Chart Interpretation",
              width = 12,
              status = "danger",
              solidHeader = TRUE,
              collapsible = TRUE,
              p("This module compares monthly trade patterns across the clusters identified earlier."),
              p("Users can examine whether different country groups also show different temporal behaviour across the year.")
            )
          ),
          
          box(
            title = "Seasonal Charts",
            width = 5,
            status = "danger",
            solidHeader = TRUE,
            tabsetPanel(
              tabPanel(
                "Monthly Pattern",
                br(),
                plotOutput("season_line_plot", height = "380px")
              ),
              tabPanel(
                "Monthly Summary",
                br(),
                tableOutput("season_table")
              )
            )
          ),
          
          box(
            title = "Seasonal Insights",
            width = 4,
            status = "danger",
            solidHeader = TRUE,
            htmlOutput("season_insight")
          )
        )
      ),
      
      # =========================
      # Part 3
      # =========================
      tabItem(
        tabName = "cluster3",
        
        fluidRow(
          box(
            title = "Desired Characteristic",
            width = 3,
            status = "danger",
            solidHeader = TRUE,
            
            sliderInput(
              "summary_year_range",
              "Select Year Range:",
              min = min(df_wide$year, na.rm = TRUE),
              max = max(df_wide$year, na.rm = TRUE),
              value = c(min(df_wide$year, na.rm = TRUE), max(df_wide$year, na.rm = TRUE)),
              sep = ""
            ),
            
            sliderInput(
              "summary_k",
              "Number of Clusters:",
              min = 2,
              max = 5,
              value = 3
            ),
            
            actionButton("run_summary", "Run Summary"),
            
            br(), br(),
            
            box(
              title = "Chart Interpretation",
              width = 12,
              status = "danger",
              solidHeader = TRUE,
              collapsible = TRUE,
              p("This module provides a consolidated comparison of the trade clusters identified earlier."),
              p("Users can compare cluster profiles across key trade indicators and interpret the overall meaning of each cluster.")
            )
          ),
          
          box(
            title = "Cluster Comparison Heatmap",
            width = 5,
            status = "danger",
            solidHeader = TRUE,
            plotOutput("summary_heatmap", height = "420px")
          ),
          
          box(
            title = "Cluster Interpretation",
            width = 4,
            status = "danger",
            solidHeader = TRUE,
            htmlOutput("summary_insight")
          )
        )
      )
    )
  )
)

# =========================
# Server
# =========================
server <- function(input, output, session) {
  
  # -------------------------
  # Part 1
  # -------------------------
  result_data <- eventReactive(input$run_cluster, {
    df_filtered <- df_wide[df_wide$year >= input$year_range[1] &
                             df_wide$year <= input$year_range[2], ]
    
    country_features <- df_filtered %>%
      group_by(country) %>%
      summarise(
        avg_input   = mean(.data[["input"]], na.rm = TRUE),
        avg_output  = mean(.data[["output"]], na.rm = TRUE),
        avg_balance = mean(trade_balance, na.rm = TRUE),
        avg_ratio   = mean(trade_ratio, na.rm = TRUE),
        sd_input    = sd(.data[["input"]], na.rm = TRUE),
        sd_output   = sd(.data[["output"]], na.rm = TRUE),
        .groups = "drop"
      )
    
    country_features <- country_features[complete.cases(country_features), ]
    
    cluster_data <- country_features[, c(
      "avg_input", "avg_output", "avg_balance",
      "avg_ratio", "sd_input", "sd_output"
    )]
    
    keep_cols <- sapply(cluster_data, function(x) sd(x, na.rm = TRUE) > 0)
    cluster_data <- cluster_data[, keep_cols, drop = FALSE]
    
    cluster_data_scaled <- scale(cluster_data)
    
    set.seed(123)
    km <- kmeans(cluster_data_scaled, centers = input$k, nstart = 25)
    
    country_features$cluster <- as.factor(km$cluster)
    country_features
  }, ignoreNULL = FALSE)
  
  output$cluster_count_plot <- renderPlot({
    plot_table <- result_data() %>%
      count(cluster)
    
    barplot(
      height = plot_table$n,
      names.arg = plot_table$cluster,
      col = c("#F28B82", "#81C784", "#90CAF9", "#FFF59D", "#CE93D8")[1:nrow(plot_table)],
      main = "Number of Countries in Each Cluster",
      xlab = "Cluster",
      ylab = "Count"
    )
  })
  
  output$cluster_char_plot <- renderPlot({
    plot_table2 <- result_data() %>%
      group_by(cluster) %>%
      summarise(
        avg_input = mean(avg_input, na.rm = TRUE),
        avg_output = mean(avg_output, na.rm = TRUE),
        avg_balance = mean(avg_balance, na.rm = TRUE),
        .groups = "drop"
      )
    
    mat <- rbind(
      "Balance" = plot_table2$avg_balance,
      "Input"   = plot_table2$avg_input,
      "Output"  = plot_table2$avg_output
    )
    
    # more space at top for legend
    par(mar = c(5, 4, 6, 2))
    
    barplot(
      mat,
      beside = TRUE,
      col = c("#F28B82", "#81C784", "#90CAF9"),
      main = "Average Input, Output and Balance by Cluster",
      xlab = "Cluster",
      ylab = "Value",
      names.arg = plot_table2$cluster
    )
    
    legend(
      "top",
      inset = c(0, -0.08),
      legend = rownames(mat),
      fill = c("#F28B82", "#81C784", "#90CAF9"),
      horiz = TRUE,
      bty = "n",
      xpd = TRUE,
      cex = 0.8
    )
  })
  
  output$cluster_ratio_plot <- renderPlot({
    plot_table3 <- result_data() %>%
      group_by(cluster) %>%
      summarise(
        avg_ratio = mean(avg_ratio, na.rm = TRUE),
        .groups = "drop"
      )
    
    barplot(
      height = plot_table3$avg_ratio,
      names.arg = plot_table3$cluster,
      col = c("#F28B82", "#81C784", "#90CAF9", "#FFF59D", "#CE93D8")[1:nrow(plot_table3)],
      main = "Average Trade Ratio by Cluster",
      xlab = "Cluster",
      ylab = "Average Trade Ratio"
    )
  })
  
  output$country_table <- renderDT({
    result_data() %>%
      mutate(
        avg_input = round(avg_input, 2),
        avg_output = round(avg_output, 2),
        avg_balance = round(avg_balance, 2),
        avg_ratio = round(avg_ratio, 2)
      ) %>%
      select(country, avg_input, avg_output, avg_balance, avg_ratio, cluster)
  },
  options = list(
    scrollX = TRUE,
    pageLength = 10,
    autoWidth = TRUE
  ))
  
  # -------------------------
  # Part 2
  # -------------------------
  season_data <- eventReactive(input$run_season, {
    df_filtered <- df_wide[df_wide$year >= input$season_year_range[1] &
                             df_wide$year <= input$season_year_range[2], ]
    
    country_features <- df_filtered %>%
      group_by(country) %>%
      summarise(
        avg_input   = mean(.data[["input"]], na.rm = TRUE),
        avg_output  = mean(.data[["output"]], na.rm = TRUE),
        avg_balance = mean(trade_balance, na.rm = TRUE),
        avg_ratio   = mean(trade_ratio, na.rm = TRUE),
        sd_input    = sd(.data[["input"]], na.rm = TRUE),
        sd_output   = sd(.data[["output"]], na.rm = TRUE),
        .groups = "drop"
      )
    
    country_features <- country_features[complete.cases(country_features), ]
    
    cluster_data <- country_features[, c(
      "avg_input", "avg_output", "avg_balance",
      "avg_ratio", "sd_input", "sd_output"
    )]
    
    keep_cols <- sapply(cluster_data, function(x) sd(x, na.rm = TRUE) > 0)
    cluster_data <- cluster_data[, keep_cols, drop = FALSE]
    
    cluster_data_scaled <- scale(cluster_data)
    
    set.seed(123)
    km <- kmeans(cluster_data_scaled, centers = input$k, nstart = 25)
    
    country_features$cluster <- as.factor(km$cluster)
    
    df_clustered <- left_join(
      df_filtered,
      country_features[, c("country", "cluster")],
      by = "country"
    )
    
    df_clustered %>%
      group_by(cluster, month) %>%
      summarise(
        avg_input = mean(.data[["input"]], na.rm = TRUE),
        avg_output = mean(.data[["output"]], na.rm = TRUE),
        avg_balance = mean(trade_balance, na.rm = TRUE),
        .groups = "drop"
      )
  }, ignoreNULL = FALSE)
  
  output$season_line_plot <- renderPlot({
    plot_data <- season_data()
    y_var <- input$season_var
    
    plot(
      NULL,
      xlim = c(1, 12),
      ylim = range(plot_data[[y_var]], na.rm = TRUE),
      xlab = "Month",
      ylab = gsub("avg_", "Average ", y_var),
      main = "Monthly Pattern by Cluster"
    )
    
    clusters <- sort(unique(plot_data$cluster))
    cols <- c("#F28B82", "#81C784", "#90CAF9", "#FFF59D", "#CE93D8")
    
    for (i in seq_along(clusters)) {
      cl <- clusters[i]
      temp <- plot_data[plot_data$cluster == cl, ]
      lines(temp$month, temp[[y_var]], type = "b", pch = 16, col = cols[i], lwd = 2)
    }
    
    legend(
      "topleft",
      legend = paste("Cluster", clusters),
      col = cols[seq_along(clusters)],
      lty = 1,
      pch = 16,
      bty = "n"
    )
  })
  
  output$season_table <- renderTable({
    season_data() %>%
      mutate(
        avg_input = round(avg_input, 2),
        avg_output = round(avg_output, 2),
        avg_balance = round(avg_balance, 2)
      )
  },
  striped = TRUE,
  bordered = TRUE,
  spacing = "s")
  
  output$season_insight <- renderUI({
    HTML(
      paste0(
        "<b>Key observations:</b><br><br>",
        "1. This page compares monthly trade behaviour across the clusters identified in Part 1.<br><br>",
        "2. Users can switch between output, input, and balance to compare temporal behaviour.<br><br>",
        "3. The monthly trajectories help determine whether cluster differences are stable across the year or vary seasonally."
      )
    )
  })
  
  # -------------------------
  # Part 3
  # -------------------------
  summary_data <- eventReactive(input$run_summary, {
    df_filtered <- df_wide[df_wide$year >= input$summary_year_range[1] &
                             df_wide$year <= input$summary_year_range[2], ]
    
    country_features <- df_filtered %>%
      group_by(country) %>%
      summarise(
        avg_input   = mean(.data[["input"]], na.rm = TRUE),
        avg_output  = mean(.data[["output"]], na.rm = TRUE),
        avg_balance = mean(trade_balance, na.rm = TRUE),
        avg_ratio   = mean(trade_ratio, na.rm = TRUE),
        sd_input    = sd(.data[["input"]], na.rm = TRUE),
        sd_output   = sd(.data[["output"]], na.rm = TRUE),
        .groups = "drop"
      )
    
    country_features <- country_features[complete.cases(country_features), ]
    
    cluster_data <- country_features[, c(
      "avg_input", "avg_output", "avg_balance",
      "avg_ratio", "sd_input", "sd_output"
    )]
    
    keep_cols <- sapply(cluster_data, function(x) sd(x, na.rm = TRUE) > 0)
    cluster_data <- cluster_data[, keep_cols, drop = FALSE]
    
    cluster_data_scaled <- scale(cluster_data)
    
    set.seed(123)
    km <- kmeans(cluster_data_scaled, centers = input$summary_k, nstart = 25)
    
    country_features$cluster <- as.factor(km$cluster)
    
    cluster_summary <- country_features %>%
      group_by(cluster) %>%
      summarise(
        avg_input = mean(avg_input, na.rm = TRUE),
        avg_output = mean(avg_output, na.rm = TRUE),
        avg_balance = mean(avg_balance, na.rm = TRUE),
        avg_ratio = mean(avg_ratio, na.rm = TRUE),
        .groups = "drop"
      )
    
    heatmap_data <- as.data.frame(scale(cluster_summary[, -1]))
    heatmap_data$cluster <- cluster_summary$cluster
    
    heatmap_long <- heatmap_data %>%
      pivot_longer(
        cols = c(avg_input, avg_output, avg_balance, avg_ratio),
        names_to = "variable",
        values_to = "value"
      )
    
    list(
      cluster_summary = cluster_summary,
      heatmap_long = heatmap_long
    )
  }, ignoreNULL = FALSE)
  
  output$summary_heatmap <- renderPlot({
    plot_data <- summary_data()$heatmap_long
    
    vars <- unique(plot_data$variable)
    cls <- unique(plot_data$cluster)
    
    x_pos <- match(plot_data$variable, vars)
    y_pos <- match(plot_data$cluster, cls)
    
    cols <- colorRampPalette(c("#90CAF9", "white", "#F28B82"))(100)
    z <- plot_data$value
    z_scaled <- round(
      (z - min(z, na.rm = TRUE)) /
        (max(z, na.rm = TRUE) - min(z, na.rm = TRUE)) * 99
    ) + 1
    
    fill_cols <- cols[z_scaled]
    
    par(mar = c(6, 6, 4, 2))
    
    plot(
      NA,
      xlim = c(0.5, length(vars) + 0.5),
      ylim = c(0.5, length(cls) + 0.5),
      xaxt = "n",
      yaxt = "n",
      xlab = "",
      ylab = "",
      main = "Cluster Comparison Heatmap"
    )
    
    for (i in seq_along(x_pos)) {
      rect(
        xleft = x_pos[i] - 0.5,
        ybottom = y_pos[i] - 0.5,
        xright = x_pos[i] + 0.5,
        ytop = y_pos[i] + 0.5,
        col = fill_cols[i],
        border = "grey80"
      )
      
      text(
        x = x_pos[i],
        y = y_pos[i],
        labels = round(z[i], 2),
        cex = 0.9
      )
    }
    
    axis(
      1,
      at = 1:length(vars),
      labels = c("Input", "Output", "Balance", "Ratio"),
      las = 2
    )
    axis(
      2,
      at = 1:length(cls),
      labels = paste("Cluster", cls),
      las = 1
    )
  })
  
  output$summary_insight <- renderUI({
    HTML(
      paste0(
        "<b>Key observations:</b><br><br>",
        "1. This page provides a consolidated view of the relative strengths and weaknesses of each cluster across input, output, balance, and trade ratio.<br><br>",
        "2. Clusters with stronger positive values in the heatmap indicate relatively higher levels for that trade characteristic compared with the other groups.<br><br>",
        "3. This summary complements the earlier segmentation and seasonal pages by translating the clustering result into a clearer overall interpretation.<br><br>",
        "4. Together, the results suggest that the clusters are not only statistically separable, but also economically meaningful."
      )
    )
  })
}

shinyApp(ui, server)

shinyApp(ui, server)