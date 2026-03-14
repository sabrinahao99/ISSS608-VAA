#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(lubridate)
library(cluster)
library(ggstatsplot)
library(plotly)
library(DT)

# =========================================================
#  Trade Cluster Analytics Shiny App
#  Enhanced version with meaningful controls for A+ level
# =========================================================

# -----------------------------
# 1. Load and prepare data
# -----------------------------
import_raw <- read_excel("data/Import.xlsx")
export_raw <- read_excel("data/Export.xlsx")

import_df <- import_raw[-1, ]
names(import_df)[1] <- "date"
import_df$date <- as.Date(as.numeric(import_df$date), origin = "1899-12-30")
import_df <- import_df %>% mutate(across(-date, as.numeric))

export_df <- export_raw[-1, ]
names(export_df)[1] <- "date"
export_df$date <- as.Date(as.numeric(export_df$date), origin = "1899-12-30")
export_df <- export_df %>% mutate(across(-date, as.numeric))

import_long <- import_df %>%
  pivot_longer(cols = -date, names_to = "country", values_to = "import_value")

export_long <- export_df %>%
  pivot_longer(cols = -date, names_to = "country", values_to = "export_value")

trade_long <- full_join(import_long, export_long, by = c("date", "country")) %>%
  mutate(
    import_value = replace_na(import_value, 0),
    export_value = replace_na(export_value, 0),
    total_trade = import_value + export_value,
    trade_balance = export_value - import_value,
    trade_ratio = if_else(import_value > 0, export_value / import_value, NA_real_),
    year = year(date),
    month = month(date)
  ) %>%
  filter(year >= 2005)

exclude_regions <- c("Asia", "Europe", "America", "EU", "Oceania", "Africa")

trade_long_clean <- trade_long %>%
  filter(!country %in% exclude_regions)

# -----------------------------
# 2. UI
# -----------------------------
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Trade Cluster Analytics"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Page 1: Segmentation", tabName = "page1", icon = icon("chart-pie")),
      menuItem("Page 2: Seasonal Pattern", tabName = "page2", icon = icon("chart-line")),
      menuItem("Page 3: Cluster Summary", tabName = "page3", icon = icon("compass"))
    ),
    hr(),
    h4("Analysis Controls"),
    
    # Common controls shown on all pages
    sliderInput("top_n", "Number of Countries:", min = 10, max = 40, value = 30, step = 5),
    
    selectInput(
      "rank_by",
      "Rank Countries By:",
      choices = c(
        "Total Trade" = "total_trade_sum",
        "Average Trade" = "avg_trade_rank",
        "Growth Ratio" = "growth_ratio_rank",
        "Trade Balance" = "avg_balance_rank"
      ),
      selected = "total_trade_sum"
    ),
    
    selectInput(
      "k_clusters",
      "Number of Clusters:",
      choices = c(3, 4),
      selected = 3
    ),
    
    # Page 1 only
    conditionalPanel(
      condition = "input.tabs == 'page1'",
      selectInput(
        "comparison_metric",
        "Comparison Metric:",
        choices = c(
          "Average Trade" = "avg_trade",
          "Growth Ratio" = "growth_ratio",
          "Seasonal Range" = "seasonal_range",
          "Volatility (CV)" = "cv_trade",
          "Trade Balance" = "avg_balance"
        ),
        selected = "avg_trade"
      )
    ),
    
    # Page 2 only
    conditionalPanel(
      condition = "input.tabs == 'page2'",
      sliderInput(
        "year_range",
        "Year Range:",
        min = 2005, max = 2024,
        value = c(2017, 2024),
        step = 1, sep = ""
      )
    ),
    
    conditionalPanel(
      condition = "input.tabs == 'page2'",
      selectInput(
        "seasonal_metric",
        "Seasonal Metric:",
        choices = c(
          "Average Trade" = "avg_trade",
          "Average Import" = "avg_import",
          "Average Export" = "avg_export",
          "Average Balance" = "avg_balance"
        ),
        selected = "avg_trade"
      )
    ),
    
    # Page 3 only
    conditionalPanel(
      condition = "input.tabs == 'page3'",
      selectInput(
        "bubble_size",
        "Bubble Encoding:",
        choices = c(
          "Seasonal Range" = "seasonal_range",
          "Volatility (CV)" = "cv_trade",
          "Trade Balance" = "avg_balance"
        ),
        selected = "seasonal_range"
      )
    ),
    
    conditionalPanel(
      condition = "input.tabs == 'page3'",
      selectInput(
        "highlight_cluster",
        "Highlight Cluster:",
        choices = c(
          "All Clusters" = "all",
          "Cluster 1" = "1",
          "Cluster 2" = "2",
          "Cluster 3" = "3",
          "Cluster 4" = "4"
        ),
        selected = "all"
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "page1",
        fluidRow(
          box(
            title = "How to Read This Page",
            width = 12,
            status = "warning",
            solidHeader = TRUE,
            HTML("This page introduces the segmentation structure of Singapore’s trade partners. Start with the cluster distribution, then compare cluster profiles in the heatmap, and finally validate whether the clusters differ significantly in key trade indicators.")
          )
        ),
        fluidRow(
          box(title = "1. Cluster Distribution", width = 6, status = "primary", solidHeader = TRUE,
              plotOutput("cluster_distribution_plot", height = 260)),
          box(title = "2. Cluster Characteristics Heatmap", width = 6, status = "primary", solidHeader = TRUE,
              plotOutput("cluster_heatmap_plot", height = 260))
        ),
        fluidRow(
          box(
            title = "3. Confirmatory Comparison of Cluster Differences",
            width = 7,
            status = "primary",
            solidHeader = TRUE,
            plotOutput("comparison_stat_plot", height = 320)
          ),
          box(title = "4. Cluster Summary Table", width = 5, status = "primary", solidHeader = TRUE,
              DTOutput("cluster_summary_table"))
        )
      ),
      
      tabItem(
        tabName = "page2",
        fluidRow(
          box(
            title = "How to Read This Page",
            width = 12,
            status = "warning",
            solidHeader = TRUE,
            HTML("This page focuses on monthly and seasonal behaviour across clusters. Use the line chart to compare monthly patterns, the heatmap to identify seasonal intensity, and the confirmatory plot to assess whether seasonal fluctuation differs significantly across clusters.")
          )
        ),
        fluidRow(
          box(
            title = "5. Monthly Cluster Pattern",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            tabsetPanel(
              tabPanel("Monthly Trade Pattern", plotOutput("monthly_trade_plot", height = 240)),
              tabPanel("Cycle Plot", plotOutput("cycle_plot", height = 240))
            )
          ),
          box(title = "6. Seasonal Intensity Heatmap", width = 6, status = "primary", solidHeader = TRUE,
              plotOutput("seasonal_heatmap_plot", height = 240))
        ),
        fluidRow(
          box(title = "7. Confirmatory Comparison of Seasonal Range", width = 6, status = "primary", solidHeader = TRUE,
              plotOutput("seasonal_range_stat_plot", height = 260)),
          box(title = "8. Monthly Cluster Summary", width = 6, status = "primary", solidHeader = TRUE,
              DTOutput("monthly_summary_table_output"))
        )
      ),
      
      tabItem(
        tabName = "page3",
        fluidRow(
          box(
            title = "How to Read This Page",
            width = 12,
            status = "warning",
            solidHeader = TRUE,
            HTML("This page summarises the positioning and interpretation of Singapore’s trade partner clusters. Use the scatter plot to understand how clusters differ in trade scale and growth, and refer to the interpretation panel for the business meaning of each cluster.")
          )
        ),
        fluidRow(
          box(title = "9. Trade Partner Cluster Scatter Plot", width = 8, status = "primary", solidHeader = TRUE,
              plotlyOutput("positioning_map_plot", height = 500)),
          box(title = "10. Cluster Interpretation", width = 4, status = "primary", solidHeader = TRUE,
              htmlOutput("cluster_definition_panel"))
        )
      )
    )
  )
)

# -----------------------------
# 3. Server
# -----------------------------
server <- function(input, output, session) {
  
  top_countries_data <- reactive({
    ranking_tbl <- trade_long_clean %>%
      group_by(country) %>%
      summarise(
        total_trade_sum = sum(total_trade, na.rm = TRUE),
        avg_trade_rank = mean(total_trade, na.rm = TRUE),
        growth_ratio_rank = (mean(total_trade[year >= max(year, na.rm = TRUE) - 2], na.rm = TRUE) + 1) /
          (mean(total_trade[year <= min(year, na.rm = TRUE) + 2], na.rm = TRUE) + 1),
        avg_balance_rank = mean(trade_balance, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(.data[[input$rank_by]]))
    
    top_countries <- ranking_tbl %>%
      slice_head(n = input$top_n) %>%
      pull(country)
    
    trade_long_clean %>%
      filter(country %in% top_countries)
  })
  
  cluster_results <- reactive({
    trade_top <- top_countries_data()
    
    min_year <- min(trade_top$year, na.rm = TRUE)
    max_year <- max(trade_top$year, na.rm = TRUE)
    
    country_base <- trade_top %>%
      group_by(country) %>%
      summarise(
        avg_trade = mean(total_trade, na.rm = TRUE),
        sd_trade = sd(total_trade, na.rm = TRUE),
        avg_import = mean(import_value, na.rm = TRUE),
        avg_export = mean(export_value, na.rm = TRUE),
        avg_balance = mean(trade_balance, na.rm = TRUE),
        .groups = "drop"
      )
    
    recent_trade_tbl <- trade_top %>%
      filter(year >= max_year - 2) %>%
      group_by(country) %>%
      summarise(recent_trade = mean(total_trade, na.rm = TRUE), .groups = "drop")
    
    early_trade_tbl <- trade_top %>%
      filter(year <= min_year + 2) %>%
      group_by(country) %>%
      summarise(early_trade = mean(total_trade, na.rm = TRUE), .groups = "drop")
    
    seasonal_features <- trade_top %>%
      group_by(country, month) %>%
      summarise(monthly_avg_trade = mean(total_trade, na.rm = TRUE), .groups = "drop") %>%
      group_by(country) %>%
      summarise(
        seasonal_range = max(monthly_avg_trade, na.rm = TRUE) - min(monthly_avg_trade, na.rm = TRUE),
        .groups = "drop"
      )
    
    country_features <- country_base %>%
      left_join(recent_trade_tbl, by = "country") %>%
      left_join(early_trade_tbl, by = "country") %>%
      left_join(seasonal_features, by = "country") %>%
      mutate(
        growth_ratio = log((recent_trade + 1) / (early_trade + 1)),
        cv_trade = if_else(avg_trade > 0, sd_trade / avg_trade, NA_real_)
      )
    
    country_features2_clean <- country_features %>%
      select(country, avg_trade, growth_ratio, cv_trade, seasonal_range, avg_balance) %>%
      drop_na() %>%
      filter(
        is.finite(avg_trade),
        is.finite(growth_ratio),
        is.finite(cv_trade),
        is.finite(seasonal_range),
        is.finite(avg_balance)
      )
    
    cluster_data <- country_features2_clean %>%
      select(avg_trade, growth_ratio, cv_trade, seasonal_range, avg_balance)
    
    cluster_data_scaled <- scale(cluster_data)
    
    set.seed(123)
    km <- kmeans(cluster_data_scaled, centers = as.numeric(input$k_clusters), nstart = 25)
    country_features2_clean$cluster <- as.factor(km$cluster)
    
    trade_clustered <- trade_top %>%
      left_join(country_features2_clean %>% select(country, cluster), by = "country")
    
    monthly_cluster_summary <- trade_clustered %>%
      group_by(cluster, month) %>%
      summarise(
        avg_trade = mean(total_trade, na.rm = TRUE),
        avg_import = mean(import_value, na.rm = TRUE),
        avg_export = mean(export_value, na.rm = TRUE),
        avg_balance = mean(trade_balance, na.rm = TRUE),
        .groups = "drop"
      )
    
    cluster_year_month <- trade_clustered %>%
      filter(year >= input$year_range[1], year <= input$year_range[2]) %>%
      group_by(cluster, year, month) %>%
      summarise(avg_trade = mean(total_trade, na.rm = TRUE), .groups = "drop")
    
    cluster_year_month$month <- factor(
      cluster_year_month$month,
      levels = 1:12,
      labels = month.abb,
      ordered = TRUE
    )
    
    hline_data <- cluster_year_month %>%
      group_by(cluster, month) %>%
      summarise(avgvalue = mean(avg_trade, na.rm = TRUE), .groups = "drop")
    
    list(
      country_features2_clean = country_features2_clean,
      trade_clustered = trade_clustered,
      monthly_cluster_summary = monthly_cluster_summary,
      cluster_year_month = cluster_year_month,
      hline_data = hline_data
    )
  })
  
  # -----------------------------
  # Page 1
  # -----------------------------
  output$cluster_distribution_plot <- renderPlot({
    plot_table1 <- cluster_results()$country_features2_clean %>%
      count(cluster) %>%
      mutate(
        share = n / sum(n),
        label = paste0("n = ", n, "\n", scales::percent(share, accuracy = 0.1))
      )
    
    ggplot(plot_table1, aes(x = factor(cluster), y = n, fill = factor(cluster))) +
      geom_col() +
      geom_text(aes(label = label), vjust = -0.2, size = 4) +
      scale_y_continuous(
        limits = c(0, max(plot_table1$n) * 1.15),
        expand = expansion(mult = c(0, 0.05))
      ) +
      coord_cartesian(clip = "off") +
      labs(
        title = "Number and Share of Countries in Each Cluster",
        x = "Cluster",
        y = "Count",
        fill = "Cluster"
      ) +
      theme_minimal() +
      theme(
        plot.margin = margin(10, 30, 10, 10)
      )
  })
  
  output$cluster_heatmap_plot <- renderPlot({
    plot_table2_scaled <- cluster_results()$country_features2_clean %>%
      group_by(cluster) %>%
      summarise(
        avg_trade = mean(avg_trade, na.rm = TRUE),
        growth_ratio = mean(growth_ratio, na.rm = TRUE),
        cv_trade = mean(cv_trade, na.rm = TRUE),
        seasonal_range = mean(seasonal_range, na.rm = TRUE),
        avg_balance = mean(avg_balance, na.rm = TRUE),
        .groups = "drop"
      )
    
    plot_table2_scaled_num <- plot_table2_scaled %>%
      select(-cluster) %>%
      scale() %>%
      as.data.frame()
    
    plot_table2_scaled_num$cluster <- plot_table2_scaled$cluster
    
    plot_table2_scaled_long <- plot_table2_scaled_num %>%
      pivot_longer(cols = -cluster, names_to = "variable", values_to = "value")
    
    ggplot(plot_table2_scaled_long, aes(x = variable, y = cluster, fill = value)) +
      geom_tile(color = "white") +
      geom_text(aes(label = round(value, 2)), size = 3.5) +
      scale_fill_gradient2(low = "#90CAF9", mid = "white", high = "#F28B82", midpoint = 0) +
      scale_x_discrete(labels = c(
        "avg_trade" = "Average Trade",
        "growth_ratio" = "Log Growth",
        "cv_trade" = "Volatility (CV)",
        "seasonal_range" = "Seasonal Range",
        "avg_balance" = "Balance"
      )) +
      labs(
        title = "Standardised Cluster Characteristics Heatmap",
        x = "Trade Indicator",
        y = "Cluster",
        fill = "Scaled Value"
      ) +
      theme_minimal()
  })
  
  output$comparison_stat_plot <- renderPlot({
    metric_labels <- c(
      avg_trade = "Average Trade",
      growth_ratio = "Growth Ratio",
      seasonal_range = "Seasonal Range",
      cv_trade = "Volatility (CV)",
      avg_balance = "Trade Balance"
    )
    
    plot_data <- cluster_results()$country_features2_clean %>%
      mutate(selected_metric = .data[[input$comparison_metric]])
    
    ggbetweenstats(
      data = plot_data,
      x = cluster,
      y = selected_metric,
      type = "np",
      pairwise.comparisons = TRUE,
      pairwise.display = "s",
      p.adjust.method = "fdr",
      messages = FALSE,
      xlab = "Cluster",
      ylab = metric_labels[[input$comparison_metric]],
      title = paste("Confirmatory Comparison of", metric_labels[[input$comparison_metric]], "by Cluster"),
      centrality.label.args = list(size = 3)
    )
  })
  
  output$cluster_summary_table <- renderDT({
    summary_table <- cluster_results()$country_features2_clean %>%
      group_by(cluster) %>%
      summarise(
        n_countries = n(),
        share_pct = round(100 * n() / nrow(cluster_results()$country_features2_clean), 1),
        avg_trade = round(mean(avg_trade, na.rm = TRUE), 2),
        growth_ratio = round(mean(growth_ratio, na.rm = TRUE), 2),
        cv_trade = round(mean(cv_trade, na.rm = TRUE), 2),
        seasonal_range = round(mean(seasonal_range, na.rm = TRUE), 2),
        cluster_label = case_when(
          cluster == "1" ~ "Large-scale and seasonal partners",
          cluster == "2" ~ "Emerging and high growth partners",
          cluster == "3" ~ "Mainstream lower-volatility partners",
          TRUE ~ "Additional cluster"
        ),
        .groups = "drop"
      ) %>%
      distinct()
    
    datatable(
      summary_table,
      rownames = FALSE,
      options = list(
        pageLength = 5,
        lengthChange = FALSE,
        autoWidth = TRUE,
        scrollX = TRUE
      )
    )
  })
  
  # -----------------------------
  # Page 2
  # -----------------------------
  output$monthly_trade_plot <- renderPlot({
    metric_labels <- c(
      avg_trade = "Average Monthly Trade",
      avg_import = "Average Monthly Import",
      avg_export = "Average Monthly Export",
      avg_balance = "Average Monthly Balance"
    )
    
    ggplot(
      cluster_results()$monthly_cluster_summary,
      aes(x = month, y = .data[[input$seasonal_metric]], color = cluster, group = cluster)
    ) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      scale_x_continuous(breaks = 1:12) +
      labs(
        title = paste(metric_labels[[input$seasonal_metric]], "Pattern by Cluster"),
        x = "Month",
        y = metric_labels[[input$seasonal_metric]],
        color = "Cluster"
      ) +
      theme_minimal()
  })
  
  output$cycle_plot <- renderPlot({
    ggplot() +
      geom_line(
        data = cluster_results()$cluster_year_month,
        aes(x = year, y = avg_trade, group = month),
        colour = "black"
      ) +
      geom_hline(
        data = cluster_results()$hline_data,
        aes(yintercept = avgvalue),
        linetype = 2,
        colour = "red",
        linewidth = 0.4
      ) +
      facet_grid(cluster ~ month) +
      scale_x_continuous(breaks = seq(input$year_range[1], input$year_range[2], by = 3)) +
      labs(title = "Cycle Plot of Monthly Trade by Cluster", x = "", y = "Average Monthly Trade") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 6, angle = 45, hjust = 1),
        strip.text = element_text(size = 10)
      )
  })
  
  output$seasonal_heatmap_plot <- renderPlot({
    metric_labels <- c(
      avg_trade = "Average Trade",
      avg_import = "Average Import",
      avg_export = "Average Export",
      avg_balance = "Average Balance"
    )
    
    ggplot(
      cluster_results()$monthly_cluster_summary,
      aes(x = factor(month), y = cluster, fill = .data[[input$seasonal_metric]])
    ) +
      geom_tile(color = "white") +
      geom_text(aes(label = round(.data[[input$seasonal_metric]], 0)), size = 3) +
      scale_fill_gradient(low = "#E3F2FD", high = "#1565C0") +
      labs(
        title = paste("Seasonal Heatmap of", metric_labels[[input$seasonal_metric]], "by Cluster and Month"),
        x = "Month",
        y = "Cluster",
        fill = metric_labels[[input$seasonal_metric]]
      ) +
      theme_minimal()
  })
  
  output$seasonal_range_stat_plot <- renderPlot({
    ggbetweenstats(
      data = cluster_results()$country_features2_clean,
      x = cluster,
      y = seasonal_range,
      type = "np",
      pairwise.comparisons = TRUE,
      pairwise.display = "s",
      p.adjust.method = "fdr",
      messages = FALSE,
      xlab = "Cluster",
      ylab = "Seasonal Range",
      title = "Confirmatory Comparison of Seasonal Range by Cluster",
      centrality.label.args = list(size = 3)
    )
  })
  
  output$monthly_summary_table_output <- renderDT({
    monthly_table <- cluster_results()$monthly_cluster_summary %>%
      transmute(
        cluster = cluster,
        month = month,
        avg_trade = round(avg_trade, 2),
        avg_balance = round(avg_balance, 2)
      )
    
    datatable(
      monthly_table,
      rownames = FALSE,
      options = list(
        pageLength = 6,
        lengthChange = FALSE,
        searching = FALSE,
        info = FALSE,
        scrollX = TRUE
      )
    )
  })
  
  # -----------------------------
  # Page 3
  # -----------------------------
  output$positioning_map_plot <- renderPlotly({
    cf <- cluster_results()$country_features2_clean
    
    cf <- cf %>%
      mutate(
        highlight_flag = if_else(
          input$highlight_cluster == "all" | as.character(cluster) == input$highlight_cluster,
          "Highlighted",
          "Background"
        )
      )
    
    p <- ggplot(
      cf,
      aes(
        x = growth_ratio,
        y = avg_trade,
        size = .data[[input$bubble_size]],
        color = factor(cluster),
        alpha = highlight_flag,
        text = paste0(
          "Country: ", country,
          "<br>Cluster: ", cluster,
          "<br>Growth Ratio: ", round(growth_ratio, 2),
          "<br>Average Trade: ", round(avg_trade, 2),
          "<br>Seasonal Range: ", round(seasonal_range, 2),
          "<br>Volatility (CV): ", round(cv_trade, 2),
          "<br>Trade Balance: ", round(avg_balance, 2)
        )
      )
    ) +
      geom_point() +
      scale_alpha_manual(values = c("Highlighted" = 0.85, "Background" = 0.15), guide = "none") +
      geom_vline(
        xintercept = mean(cf$growth_ratio, na.rm = TRUE),
        linetype = "dashed",
        color = "grey50"
      ) +
      geom_hline(
        yintercept = mean(cf$avg_trade, na.rm = TRUE),
        linetype = "dashed",
        color = "grey50"
      ) +
      labs(
        title = "Trade Partner Cluster Scatter Plot",
        x = "Log Growth",
        y = "Average Trade",
        size = "Bubble Size",
        color = "Cluster"
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$cluster_definition_panel <- renderUI({
    k_val <- as.numeric(input$k_clusters)
    
    if (k_val == 3) {
      HTML(
        "<h4>Cluster 1: Large-Scale and Seasonal Partners</h4>
         <p>This group combines relatively high trade scale with stronger seasonal fluctuation, representing the core trade partners in the portfolio.</p>
         <h4>Cluster 2: Emerging and High-Growth Partners</h4>
         <p>This group shows stronger growth dynamics than current scale, appearing as rising trade partners with relatively stronger recent momentum.</p>
         <h4>Cluster 3: Mainstream and Low-Volatility Partners</h4>
         <p>This group contains the majority of countries, with more moderate trade scale, lower volatility, and more typical trade behaviour overall.</p>"
      )
    } else {
      HTML(
        "<h4>Cluster Interpretation</h4>
         <p>The 4-cluster solution is shown for exploratory comparison. Final labels should be updated after inspecting the revised cluster heatmap, summary table, seasonal pattern plots, and positioning scatter plot.</p>"
      )
    }
  })
}

# -----------------------------
# 4. Launch app
# -----------------------------
shinyApp(ui = ui, server = server)

