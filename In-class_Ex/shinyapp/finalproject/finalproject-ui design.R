#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
library(shiny)
library(shinydashboard)
library(DT)
library(readxl)
library(dplyr)
library(tidyverse)
library(lubridate)

# =========================
# 1. Read and prepare data
# =========================
import_raw <- read_excel("data/Import.xlsx")
export_raw <- read_excel("data/Export.xlsx")

# Clean import
import_df <- import_raw[-1, ]
names(import_df)[1] <- "date"
import_df$date <- as.Date(as.numeric(import_df$date), origin = "1899-12-30")
import_df <- import_df %>%
  mutate(across(-date, as.numeric))

# Clean export
export_df <- export_raw[-1, ]
names(export_df)[1] <- "date"
export_df$date <- as.Date(as.numeric(export_df$date), origin = "1899-12-30")
export_df <- export_df %>%
  mutate(across(-date, as.numeric))

# Long format
import_long <- import_df %>%
  pivot_longer(
    cols = -date,
    names_to = "country",
    values_to = "import_value"
  )

export_long <- export_df %>%
  pivot_longer(
    cols = -date,
    names_to = "country",
    values_to = "export_value"
  )

# Merge
trade_long <- full_join(import_long, export_long, by = c("date", "country")) %>%
  mutate(
    country = str_trim(country),
    import_value = replace_na(import_value, 0),
    export_value = replace_na(export_value, 0),
    total_trade = import_value + export_value,
    trade_balance = export_value - import_value,
    trade_ratio = if_else(import_value > 0, export_value / import_value, NA_real_),
    year = year(date),
    month = month(date)
  )

# Remove region aggregates
exclude_regions <- c("Asia", "Europe", "America", "EU", "Oceania", "Africa")

trade_long_clean <- trade_long %>%
  filter(!country %in% exclude_regions)

year_min <- min(trade_long_clean$year, na.rm = TRUE)
year_max <- max(trade_long_clean$year, na.rm = TRUE)

# =========================
# 2. Helper function
# =========================
build_cluster_data <- function(data, year_range, top_n, k_val) {
  
  df_filtered <- data %>%
    filter(year >= year_range[1], year <= year_range[2])
  
  top_countries <- df_filtered %>%
    group_by(country) %>%
    summarise(
      total_trade_sum = sum(total_trade, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(total_trade_sum)) %>%
    slice_head(n = top_n) %>%
    pull(country)
  
  trade_top <- df_filtered %>%
    filter(country %in% top_countries)
  
  min_year <- min(trade_top$year, na.rm = TRUE)
  max_year <- max(trade_top$year, na.rm = TRUE)
  
  country_base <- trade_top %>%
    group_by(country) %>%
    summarise(
      avg_trade = mean(total_trade, na.rm = TRUE),
      sd_trade = sd(total_trade, na.rm = TRUE),
      active_month_ratio = mean(total_trade > 0, na.rm = TRUE),
      avg_import = mean(import_value, na.rm = TRUE),
      avg_export = mean(export_value, na.rm = TRUE),
      avg_balance = mean(trade_balance, na.rm = TRUE),
      .groups = "drop"
    )
  
  recent_trade_tbl <- trade_top %>%
    filter(year >= max_year - 2) %>%
    group_by(country) %>%
    summarise(
      recent_trade = mean(total_trade, na.rm = TRUE),
      .groups = "drop"
    )
  
  early_trade_tbl <- trade_top %>%
    filter(year <= min_year + 2) %>%
    group_by(country) %>%
    summarise(
      early_trade = mean(total_trade, na.rm = TRUE),
      .groups = "drop"
    )
  
  seasonal_features <- trade_top %>%
    group_by(country, month) %>%
    summarise(
      monthly_avg_trade = mean(total_trade, na.rm = TRUE),
      .groups = "drop"
    ) %>%
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
  
  country_features2 <- country_features %>%
    select(
      country,
      avg_trade,
      growth_ratio,
      cv_trade,
      active_month_ratio,
      seasonal_range,
      avg_balance
    ) %>%
    drop_na()
  
  cluster_data <- country_features2 %>%
    select(
      avg_trade,
      growth_ratio,
      cv_trade,
      active_month_ratio,
      seasonal_range,
      avg_balance
    )
  
  # protect against impossible k
  if (nrow(cluster_data) < 2) {
    country_features2$cluster <- factor(1)
  } else {
    k_use <- min(k_val, nrow(cluster_data))
    if (k_use < 2) k_use <- 2
    
    set.seed(123)
    km <- kmeans(scale(cluster_data), centers = k_use, nstart = 25)
    country_features2$cluster <- as.factor(km$cluster)
  }
  
  trade_clustered <- trade_top %>%
    left_join(
      country_features2 %>% select(country, cluster),
      by = "country"
    )
  
  monthly_cluster_summary <- trade_clustered %>%
    group_by(cluster, month) %>%
    summarise(
      avg_trade = mean(total_trade, na.rm = TRUE),
      avg_import = mean(import_value, na.rm = TRUE),
      avg_export = mean(export_value, na.rm = TRUE),
      avg_balance = mean(trade_balance, na.rm = TRUE),
      .groups = "drop"
    )
  
  list(
    trade_top = trade_top,
    country_features2 = country_features2,
    trade_clustered = trade_clustered,
    monthly_cluster_summary = monthly_cluster_summary
  )
}

# =========================
# 3. UI
# =========================
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "Clustering Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Country Trade Behaviour Segmentation", tabName = "page1", icon = icon("chart-bar")),
      menuItem("Seasonal Pattern Clustering", tabName = "page2", icon = icon("chart-line")),
      menuItem("Cluster Comparison Summary", tabName = "page3", icon = icon("bullseye"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f6f9;
        }
        .box {
          border-radius: 8px;
        }
      "))
    ),
    
    tabItems(
      
      # =========================
      # Page 1
      # =========================
      tabItem(
        tabName = "page1",
        fluidRow(
          box(
            title = "Desired Characteristic",
            width = 3,
            status = "danger",
            solidHeader = TRUE,
            
            sliderInput("year_range_1", "Select Year Range:",
                        min = year_min, max = year_max,
                        value = c(year_min, year_max), sep = ""),
            
            sliderInput("k_1", "Number of Clusters:",
                        min = 2, max = 5, value = 3, step = 1),
            
            numericInput("top_n_1", "Top N Countries:",
                         value = 30, min = 10, max = 50, step = 5),
            
            br(),
            
            box(
              title = "Chart Interpretation",
              width = 12,
              status = "danger",
              solidHeader = TRUE,
              collapsible = TRUE,
              HTML("
                <p>This module groups countries with similar trade behaviour based on 
                average trade, growth, volatility, balance, activity level and seasonal range.</p>
                <p>Users can compare cluster size, cluster characteristics and country-level
                membership through the summary table.</p>
              ")
            )
          ),
          
          box(
            title = "Cluster Charts",
            width = 5,
            status = "danger",
            solidHeader = TRUE,
            
            tabsetPanel(
              tabPanel(
                "Cluster Proportion",
                br(),
                plotOutput("cluster_size_plot", height = "320px")
              ),
              tabPanel(
                "Cluster Characteristics",
                br(),
                plotOutput("cluster_heatmap_plot", height = "320px"),
                br(),
                plotOutput("cluster_growth_plot", height = "250px")
              )
            )
          ),
          
          box(
            title = "Country Summary Table",
            width = 4,
            status = "danger",
            solidHeader = TRUE,
            DTOutput("country_summary_table")
          )
        )
      ),
      
      # =========================
      # Page 2
      # =========================
      tabItem(
        tabName = "page2",
        fluidRow(
          box(
            title = "Desired Characteristic",
            width = 3,
            status = "danger",
            solidHeader = TRUE,
            
            sliderInput("year_range_2", "Select Year Range:",
                        min = year_min, max = year_max,
                        value = c(year_min, year_max), sep = ""),
            
            sliderInput("k_2", "Number of Clusters:",
                        min = 2, max = 5, value = 3, step = 1),
            
            numericInput("top_n_2", "Top N Countries:",
                         value = 30, min = 10, max = 50, step = 5),
            
            br(),
            
            box(
              title = "Chart Interpretation",
              width = 12,
              status = "danger",
              solidHeader = TRUE,
              collapsible = TRUE,
              HTML("
                <p>This module compares monthly trade patterns across the clusters identified earlier.</p>
                <p>Users can examine whether different country groups also show different temporal behaviour across the year.</p>
              ")
            )
          ),
          
          box(
            title = "Seasonal Charts",
            width = 6,
            status = "danger",
            solidHeader = TRUE,
            
            tabsetPanel(
              tabPanel(
                "Monthly Pattern",
                br(),
                plotOutput("monthly_trade_plot", height = "280px"),
                br(),
                plotOutput("monthly_balance_plot", height = "280px")
              ),
              tabPanel(
                "Seasonal Heatmap",
                br(),
                plotOutput("seasonal_heatmap_plot", height = "600px")
              )
            )
          ),
          
          box(
            title = "Monthly Summary Table",
            width = 3,
            status = "danger",
            solidHeader = TRUE,
            DTOutput("monthly_summary_table")
          )
        )
      ),
      
      # =========================
      # Page 3
      # =========================
      tabItem(
        tabName = "page3",
        fluidRow(
          box(
            title = "Desired Characteristic",
            width = 3,
            status = "danger",
            solidHeader = TRUE,
            
            sliderInput("year_range_3", "Select Year Range:",
                        min = year_min, max = year_max,
                        value = c(year_min, year_max), sep = ""),
            
            sliderInput("k_3", "Number of Clusters:",
                        min = 2, max = 5, value = 3, step = 1),
            
            numericInput("top_n_3", "Top N Countries:",
                         value = 30, min = 10, max = 50, step = 5),
            
            selectInput("bubble_size_var_3", "Bubble Size Variable:",
                        choices = c("Seasonal Range", "Volatility (CV)", "Activity Ratio"),
                        selected = "Seasonal Range"),
            
            br(),
            
            box(
              title = "Summary Notes",
              width = 12,
              status = "danger",
              solidHeader = TRUE,
              collapsible = TRUE,
              HTML("
                <p>This page summarises the relative position of each cluster in terms of trade scale, growth and a user-selected third dimension.</p>
                <p>The positioning map provides a more strategic and high-level overview of the cluster structure.</p>
              ")
            )
          ),
          
          box(
            title = "Trade Cluster Positioning Map",
            width = 6,
            status = "danger",
            solidHeader = TRUE,
            plotOutput("positioning_map_plot", height = "650px")
          ),
          
          box(
            title = "Cluster Interpretation",
            width = 3,
            status = "danger",
            solidHeader = TRUE,
            htmlOutput("cluster_interpretation_text")
          )
        )
      )
    )
  )
)

# =========================
# 4. Server
# =========================
server <- function(input, output, session) {
  
  page1_data <- reactive({
    build_cluster_data(trade_long_clean, input$year_range_1, input$top_n_1, input$k_1)
  })
  
  page2_data <- reactive({
    build_cluster_data(trade_long_clean, input$year_range_2, input$top_n_2, input$k_2)
  })
  
  page3_data <- reactive({
    build_cluster_data(trade_long_clean, input$year_range_3, input$top_n_3, input$k_3)
  })
  
  # Page 1
  output$cluster_size_plot <- renderPlot({
    df <- page1_data()$country_features2 %>%
      count(cluster)
    
    ggplot(df, aes(x = cluster, y = n, fill = cluster)) +
      geom_col() +
      labs(
        title = "Number of Countries in Each Cluster",
        x = "Cluster",
        y = "Count"
      ) +
      theme_minimal()
  })
  
  output$cluster_heatmap_plot <- renderPlot({
    df <- page1_data()$country_features2 %>%
      group_by(cluster) %>%
      summarise(
        avg_trade = mean(avg_trade, na.rm = TRUE),
        growth_ratio = mean(growth_ratio, na.rm = TRUE),
        cv_trade = mean(cv_trade, na.rm = TRUE),
        active_month_ratio = mean(active_month_ratio, na.rm = TRUE),
        seasonal_range = mean(seasonal_range, na.rm = TRUE),
        avg_balance = mean(avg_balance, na.rm = TRUE),
        .groups = "drop"
      )
    
    df_scaled <- df %>%
      select(-cluster) %>%
      scale() %>%
      as.data.frame()
    
    df_scaled$cluster <- df$cluster
    
    df_long <- df_scaled %>%
      pivot_longer(
        cols = -cluster,
        names_to = "variable",
        values_to = "value"
      )
    
    ggplot(df_long, aes(x = variable, y = cluster, fill = value)) +
      geom_tile(color = "white") +
      geom_text(aes(label = round(value, 2)), size = 3.5) +
      scale_fill_gradient2(
        low = "#90CAF9",
        mid = "white",
        high = "#F28B82",
        midpoint = 0
      ) +
      scale_x_discrete(
        labels = c(
          "avg_trade" = "Average Trade",
          "growth_ratio" = "Log Growth",
          "cv_trade" = "Volatility (CV)",
          "active_month_ratio" = "Activity Ratio",
          "seasonal_range" = "Seasonal Range",
          "avg_balance" = "Balance"
        )
      ) +
      labs(
        title = "Standardised Cluster Characteristics Heatmap",
        x = "Trade Indicator",
        y = "Cluster",
        fill = "Scaled Value"
      ) +
      theme_minimal()
  })
  
  output$cluster_growth_plot <- renderPlot({
    df <- page1_data()$country_features2 %>%
      group_by(cluster) %>%
      summarise(
        avg_growth_ratio = mean(growth_ratio, na.rm = TRUE),
        .groups = "drop"
      )
    
    ggplot(df, aes(x = cluster, y = avg_growth_ratio)) +
      geom_segment(aes(xend = cluster, y = 0, yend = avg_growth_ratio), color = "grey60") +
      geom_point(aes(color = cluster), size = 4) +
      labs(
        title = "Average Log Growth by Cluster",
        x = "Cluster",
        y = "Average Log Growth"
      ) +
      theme_minimal()
  })
  
  output$country_summary_table <- renderDT({
    df <- page1_data()$country_features2 %>%
      mutate(
        avg_trade = round(avg_trade, 2),
        growth_ratio = round(growth_ratio, 2),
        cv_trade = round(cv_trade, 2),
        active_month_ratio = round(active_month_ratio, 2),
        seasonal_range = round(seasonal_range, 2),
        avg_balance = round(avg_balance, 2)
      ) %>%
      arrange(cluster, desc(avg_trade))
    
    datatable(df, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Page 2
  output$monthly_trade_plot <- renderPlot({
    df <- page2_data()$monthly_cluster_summary
    
    ggplot(df, aes(x = month, y = avg_trade, color = cluster, group = cluster)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      scale_x_continuous(breaks = 1:12) +
      labs(
        title = "Monthly Trade Pattern by Cluster",
        x = "Month",
        y = "Average Monthly Trade",
        color = "Cluster"
      ) +
      theme_minimal()
  })
  
  output$monthly_balance_plot <- renderPlot({
    df <- page2_data()$monthly_cluster_summary
    
    ggplot(df, aes(x = month, y = avg_balance, color = cluster, group = cluster)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
      scale_x_continuous(breaks = 1:12) +
      labs(
        title = "Monthly Balance Pattern by Cluster",
        x = "Month",
        y = "Average Monthly Balance",
        color = "Cluster"
      ) +
      theme_minimal()
  })
  
  output$seasonal_heatmap_plot <- renderPlot({
    df <- page2_data()$monthly_cluster_summary
    
    ggplot(df, aes(x = factor(month), y = cluster, fill = avg_trade)) +
      geom_tile(color = "white") +
      geom_text(aes(label = round(avg_trade, 0)), size = 3) +
      scale_fill_gradient(
        low = "#E3F2FD",
        high = "#1565C0"
      ) +
      labs(
        title = "Seasonal Intensity Heatmap by Cluster and Month",
        x = "Month",
        y = "Cluster",
        fill = "Average Trade"
      ) +
      theme_minimal()
  })
  
  output$monthly_summary_table <- renderDT({
    df <- page2_data()$monthly_cluster_summary %>%
      mutate(
        avg_trade = round(avg_trade, 2),
        avg_import = round(avg_import, 2),
        avg_export = round(avg_export, 2),
        avg_balance = round(avg_balance, 2)
      )
    
    datatable(df, options = list(pageLength = 12, scrollX = TRUE))
  })
  
  # Page 3
  output$positioning_map_plot <- renderPlot({
    df <- page3_data()$country_features2
    
    size_var <- switch(
      input$bubble_size_var_3,
      "Seasonal Range" = "seasonal_range",
      "Volatility (CV)" = "cv_trade",
      "Activity Ratio" = "active_month_ratio"
    )
    
    ggplot(df, aes(x = growth_ratio, y = avg_trade, size = .data[[size_var]], color = cluster)) +
      geom_point(alpha = 0.7) +
      geom_vline(
        xintercept = mean(df$growth_ratio, na.rm = TRUE),
        linetype = "dashed",
        color = "grey50"
      ) +
      geom_hline(
        yintercept = mean(df$avg_trade, na.rm = TRUE),
        linetype = "dashed",
        color = "grey50"
      ) +
      labs(
        title = "Trade Cluster Positioning Map",
        x = "Log Growth",
        y = "Average Trade",
        size = input$bubble_size_var_3,
        color = "Cluster"
      ) +
      theme_minimal()
  })
  
  output$cluster_interpretation_text <- renderUI({
    HTML("
      <h4>Key Insights</h4>
      <p><b>Cluster 1: Large-Scale Partners</b><br/>
      Higher trade scale and stronger long-term trade presence, with relatively larger seasonal range.</p>
      
      <p><b>Cluster 2: Mainstream Partners</b><br/>
      Represents the largest group of countries with moderate and more typical trade behaviour.</p>
      
      <p><b>Cluster 3: Emerging High-Growth Partners</b><br/>
      Smaller in number but characterised by stronger recent growth.</p>
    ")
  })
}

# =========================
# 5. Run app
# =========================
shinyApp(ui = ui, server = server)