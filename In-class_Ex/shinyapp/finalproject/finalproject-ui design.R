library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(visNetwork)

read_prepared_data <- function(file_path = "data/cluster_plot_input_tables_2005_2025_corrected.xlsx") {
  trend_df <- read_excel(file_path, sheet = "cluster_trend_data")
  members_df <- read_excel(file_path, sheet = "cluster_members")
  nodes_df <- read_excel(file_path, sheet = "constellation_nodes")
  edges_df <- read_excel(file_path, sheet = "constellation_edges")
  
  trend_df <- as.data.frame(trend_df)
  members_df <- as.data.frame(members_df)
  nodes_df <- as.data.frame(nodes_df)
  edges_df <- as.data.frame(edges_df)
  
  names(trend_df)[1] <- "Date"
  trend_df$Date <- as.Date(trend_df$Date)
  
  list(
    trend = trend_df,
    members = members_df,
    nodes = nodes_df,
    edges = edges_df
  )
}

prepare_members_data <- function(members_df) {
  members_df %>%
    transmute(
      Country = as.character(Country),
      Cluster = as.character(Cluster),
      Avg_Trade_SGD_mn = as.numeric(Avg_Trade_SGD_mn),
      Dev_Group = as.character(Dev_Group)
    ) %>%
    filter(!is.na(Country), Country != "") %>%
    filter(!is.na(Cluster), Cluster != "") %>%
    mutate(
      Country = trimws(Country),
      Cluster = trimws(Cluster),
      Dev_Group = trimws(Dev_Group)
    ) %>%
    group_by(Cluster, Country) %>%
    summarise(
      Avg_Trade_SGD_mn = max(Avg_Trade_SGD_mn, na.rm = TRUE),
      Dev_Group = first(Dev_Group),
      .groups = "drop"
    )
}

prepare_constellation_nodes <- function(nodes_df) {
  nodes_df %>%
    transmute(
      Node_ID = as.character(Node_ID),
      Label = as.character(Label),
      Cluster = as.character(Cluster),
      Node_Type = as.character(Node_Type),
      X = as.numeric(X),
      Y = as.numeric(Y),
      Dev_Group = as.character(Dev_Group),
      Country = as.character(Country)
    ) %>%
    mutate(
      Label = ifelse(is.na(Label), "", trimws(Label)),
      Cluster = trimws(Cluster),
      Node_Type = trimws(Node_Type),
      Dev_Group = ifelse(is.na(Dev_Group), "", trimws(Dev_Group)),
      Country = ifelse(is.na(Country), "", trimws(Country))
    )
}

prepare_constellation_edges <- function(edges_df) {
  edges_df %>%
    transmute(
      From_Node = as.character(From_Node),
      To_Node = as.character(To_Node),
      Cluster = as.character(Cluster),
      Edge_Type = as.character(Edge_Type)
    ) %>%
    mutate(
      From_Node = trimws(From_Node),
      To_Node = trimws(To_Node),
      Cluster = trimws(Cluster),
      Edge_Type = trimws(Edge_Type)
    )
}

make_cluster_trend_plot <- function(trend_df, selected_cluster, top_n = 6) {
  if (selected_cluster == "All clusters") {
    return(
      ggplot() +
        annotate("text", x = 1, y = 1, label = "Please select a specific cluster for the trend plot.", size = 6) +
        theme_void()
    )
  }
  
  plot_df <- trend_df %>% filter(Cluster == selected_cluster)
  
  if (nrow(plot_df) == 0) {
    return(
      ggplot() +
        annotate("text", x = 1, y = 1, label = "No data available for this cluster.", size = 6) +
        theme_void()
    )
  }
  
  top_countries <- plot_df %>%
    distinct(Country, Avg_Trade_SGD_mn) %>%
    arrange(desc(Avg_Trade_SGD_mn)) %>%
    slice_head(n = top_n) %>%
    pull(Country)
  
  plot_df <- plot_df %>% filter(Country %in% top_countries)
  
  ggplot(plot_df, aes(x = Date, y = Index_Value, color = Country)) +
    geom_line(linewidth = 0.8) +
    scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
    labs(
      title = "Cluster Time-Series Plot (2005–2025)",
      subtitle = selected_cluster,
      x = "Dates",
      y = NULL,
      color = NULL
    ) +
    theme_gray(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 17),
      plot.subtitle = element_text(face = "bold", size = 13),
      legend.position = "bottom"
    )
}

make_integrated_network_map <- function(
    members_df,
    nodes_df,
    edges_df,
    selected_cluster = "All clusters",
    max_nodes_per_cluster = 20,
    selected_dev_groups = c("Emerging", "Newly Developed", "Developed"),
    show_labels = TRUE
) {
  if (nrow(members_df) == 0 || nrow(nodes_df) == 0 || nrow(edges_df) == 0) {
    return(NULL)
  }
  
  if (is.null(selected_dev_groups) || length(selected_dev_groups) == 0) {
    return(
      visNetwork(
        data.frame(
          id = "empty",
          label = "Please select at least one development group.",
          x = 0, y = 0, shape = "text", fixed = TRUE
        ),
        data.frame(from = character(0), to = character(0)),
        width = "100%",
        height = "720px"
      ) %>%
        visPhysics(enabled = FALSE)
    )
  }
  
  dev_color_map <- c(
    "Emerging" = "#FF6A00",
    "Newly Developed" = "#D1C100",
    "Developed" = "#16A34A"
  )
  
  cluster_bg_map <- c(
    "Cluster D" = "rgba(142,227,157,0.18)",
    "Cluster C" = "rgba(247,204,138,0.18)",
    "Cluster B" = "rgba(241,234,174,0.18)",
    "Cluster A" = "rgba(114,214,245,0.18)"
  )
  
  members_use <- members_df %>%
    filter(Dev_Group %in% selected_dev_groups)
  
  if (selected_cluster != "All clusters") {
    members_use <- members_use %>%
      filter(Cluster == selected_cluster)
  }
  
  members_top <- members_use %>%
    group_by(Cluster) %>%
    arrange(desc(Avg_Trade_SGD_mn), .by_group = TRUE) %>%
    slice_head(n = max_nodes_per_cluster) %>%
    ungroup()
  
  if (nrow(members_top) == 0) {
    return(
      visNetwork(
        data.frame(
          id = "empty",
          label = "No countries match the selected filters.",
          x = 0, y = 0, shape = "text", fixed = TRUE
        ),
        data.frame(from = character(0), to = character(0)),
        width = "100%",
        height = "720px"
      ) %>%
        visPhysics(enabled = FALSE)
    )
  }
  
  nodes_use <- nodes_df
  edges_use <- edges_df
  
  if (selected_cluster != "All clusters") {
    nodes_use <- nodes_use %>% filter(Cluster == selected_cluster)
    edges_use <- edges_use %>% filter(Cluster == selected_cluster)
  }
  
  keep_country_keys <- paste(members_top$Cluster, members_top$Country, sep = "___")
  
  keep_country_nodes <- nodes_use %>%
    filter(Node_Type == "country") %>%
    mutate(key = paste(Cluster, Country, sep = "___")) %>%
    filter(key %in% keep_country_keys)
  
  if (nrow(keep_country_nodes) == 0) {
    return(
      visNetwork(
        data.frame(
          id = "empty",
          label = "No countries remain after filtering.",
          x = 0, y = 0, shape = "text", fixed = TRUE
        ),
        data.frame(from = character(0), to = character(0)),
        width = "100%",
        height = "720px"
      ) %>%
        visPhysics(enabled = FALSE)
    )
  }
  
  keep_mid_edges <- edges_use %>%
    filter(Edge_Type == "mid_to_country", To_Node %in% keep_country_nodes$Node_ID)
  
  keep_mid_ids <- unique(keep_mid_edges$From_Node)
  
  keep_primary_edges <- edges_use %>%
    filter(Edge_Type == "primary_to_mid", To_Node %in% keep_mid_ids)
  
  keep_node_ids <- unique(c(
    keep_country_nodes$Node_ID,
    keep_mid_edges$From_Node,
    keep_primary_edges$From_Node,
    keep_primary_edges$To_Node
  ))
  
  final_nodes <- nodes_use %>%
    filter(Node_ID %in% keep_node_ids)
  
  final_edges <- bind_rows(keep_primary_edges, keep_mid_edges)
  
  backbone_nodes <- data.frame()
  backbone_edges <- data.frame()
  
  if (selected_cluster == "All clusters") {
    primary_nodes <- final_nodes %>%
      filter(Node_Type == "primary_cluster") %>%
      arrange(X)
    
    if (nrow(primary_nodes) >= 2) {
      bb_list <- list()
      be_list <- list()
      
      for (i in 1:(nrow(primary_nodes) - 1)) {
        left_node <- primary_nodes[i, ]
        right_node <- primary_nodes[i + 1, ]
        
        bb_id <- paste0("backbone_", i)
        bb_x <- (left_node$X + right_node$X) / 2
        bb_y <- (left_node$Y + right_node$Y) / 2 + 0.15
        
        bb_list[[i]] <- data.frame(
          Node_ID = bb_id,
          Label = "",
          Cluster = "Backbone",
          Node_Type = "backbone",
          X = bb_x,
          Y = bb_y,
          Dev_Group = "",
          Country = "",
          stringsAsFactors = FALSE
        )
        
        be_list[[length(be_list) + 1]] <- data.frame(
          From_Node = left_node$Node_ID,
          To_Node = bb_id,
          Cluster = "Backbone",
          Edge_Type = "backbone",
          stringsAsFactors = FALSE
        )
        
        be_list[[length(be_list) + 1]] <- data.frame(
          From_Node = bb_id,
          To_Node = right_node$Node_ID,
          Cluster = "Backbone",
          Edge_Type = "backbone",
          stringsAsFactors = FALSE
        )
      }
      
      backbone_nodes <- bind_rows(bb_list)
      backbone_edges <- bind_rows(be_list)
    }
  }
  
  final_nodes_all <- bind_rows(final_nodes, backbone_nodes)
  final_edges_all <- bind_rows(final_edges, backbone_edges)
  
  vis_nodes <- final_nodes_all %>%
    mutate(
      id = Node_ID,
      label = case_when(
        Node_Type == "primary_cluster" ~ Label,
        Node_Type == "country" & show_labels ~ Label,
        TRUE ~ ""
      ),
      title = case_when(
        Node_Type == "country" ~ paste0(
          "<b>", Label, "</b><br>",
          "Cluster: ", Cluster, "<br>",
          "Development Group: ", Dev_Group
        ),
        Node_Type == "primary_cluster" ~ paste0("<b>", Cluster, "</b>"),
        TRUE ~ ""
      ),
      x = X * 40,
      y = -Y * 120,
      shape = "dot",
      size = case_when(
        Node_Type == "primary_cluster" ~ 28,
        Node_Type == "mid_node" ~ 8,
        Node_Type == "backbone" ~ 8,
        Node_Type == "country" ~ 14,
        TRUE ~ 10
      ),
      color.background = case_when(
        Node_Type == "primary_cluster" ~ "black",
        Node_Type == "mid_node" ~ "#7a7a7a",
        Node_Type == "backbone" ~ "#7a7a7a",
        Node_Type == "country" ~ unname(dev_color_map[Dev_Group]),
        TRUE ~ "#999999"
      ),
      color.border = case_when(
        Node_Type == "primary_cluster" ~ "black",
        Node_Type == "mid_node" ~ "#7a7a7a",
        Node_Type == "backbone" ~ "#7a7a7a",
        Node_Type == "country" ~ "#666666",
        TRUE ~ "#999999"
      ),
      color.highlight.background = color.background,
      color.highlight.border = color.border,
      font.color = ifelse(Node_Type == "primary_cluster", "white", "black"),
      font.size = ifelse(Node_Type == "country", 18, 20),
      fixed = TRUE
    ) %>%
    select(
      id, label, title, x, y, shape, size,
      color.background, color.border,
      color.highlight.background, color.highlight.border,
      font.color, font.size, fixed
    )
  
  vis_edges <- final_edges_all %>%
    transmute(
      from = From_Node,
      to = To_Node
    )
  
  pnodes <- final_nodes_all %>%
    filter(Node_Type == "primary_cluster") %>%
    arrange(X)
  
  background_css <- ""
  
  if (nrow(pnodes) > 0) {
    cluster_boxes <- pnodes %>%
      transmute(
        cluster = Cluster,
        left = X * 40 - 140,
        width = 280,
        color = unname(cluster_bg_map[Cluster])
      )
    
    cluster_divs <- paste0(
      "<div style='position:absolute;
                  left:", cluster_boxes$left, "px;
                  top:40px;
                  width:", cluster_boxes$width, "px;
                  height:620px;
                  background:", cluster_boxes$color, ";
                  border:1px solid rgba(120,120,120,0.15);
                  border-radius:18px;
                  z-index:0;'></div>",
      collapse = ""
    )
    
    background_css <- paste0(
      "<div style='position:absolute; inset:0; pointer-events:none; z-index:0;'>",
      cluster_divs,
      "</div>"
    )
  }
  
  graph_html <- visNetwork(vis_nodes, vis_edges, width = "100%", height = "720px") %>%
    visNodes(shadow = FALSE) %>%
    visEdges(
      color = list(color = "#8a8a8a", highlight = "#2c3e50"),
      smooth = list(enabled = TRUE, type = "dynamic")
    ) %>%
    visInteraction(
      dragNodes = TRUE,
      dragView = TRUE,
      zoomView = TRUE,
      navigationButtons = TRUE,
      hover = TRUE
    ) %>%
    visPhysics(enabled = FALSE) %>%
    visOptions(
      highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
      nodesIdSelection = FALSE
    )
  
  htmltools::tagList(
    htmltools::div(
      style = "position:relative; width:100%; height:720px;",
      htmltools::HTML(background_css),
      htmltools::div(
        style = "position:relative; z-index:1;",
        graph_html
      )
    )
  )
}

ui <- fluidPage(
  titlePanel("Cluster Plots from Prepared Tables"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "top_n",
        "Countries shown in trend plot:",
        min = 3, max = 10, value = 6, step = 1
      ),
      sliderInput(
        "map_n",
        "Countries shown in network map per cluster:",
        min = 3, max = 20, value = 8, step = 1
      ),
      selectInput(
        "selected_cluster",
        "Select cluster:",
        choices = c("All clusters", "Cluster A", "Cluster B", "Cluster C", "Cluster D"),
        selected = "All clusters",
        selectize = FALSE
      ),
      checkboxGroupInput(
        "selected_dev_groups",
        "Development group:",
        choices = c("Emerging", "Newly Developed", "Developed"),
        selected = c("Emerging", "Newly Developed", "Developed")
      ),
      checkboxInput(
        "show_labels",
        "Show country labels",
        value = TRUE
      ),
      h4("How clusters are defined"),
      htmlOutput("cluster_explanation")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Constellation Plot", br(), uiOutput("constellation_plot_ui")),
        tabPanel("Cluster Trend Plot", br(), plotOutput("cluster_plot", height = "620px"))
      )
    )
  )
)

server <- function(input, output, session) {
  prepared_data <- reactive({
    dat <- read_prepared_data()
    dat$members <- prepare_members_data(dat$members)
    dat$nodes <- prepare_constellation_nodes(dat$nodes)
    dat$edges <- prepare_constellation_edges(dat$edges)
    dat
  })
  
  output$cluster_explanation <- renderUI({
    HTML(paste0(
      "<b>Cluster A–D are data-driven groups.</b><br>",
      "They were defined from the cleaned trade data by grouping countries with similar trade-series patterns, ",
      "then relabelled as <b>Cluster A/B/C/D</b> according to cluster-average trade level from lower to higher.<br>",
      "The constellation plot shows the full network by default, and can also be filtered to a specific cluster.<br>",
      "Countries are displayed according to the selected node limit and development group filter.<br>",
      "Orange / yellow / green represent <b>Emerging / Newly Developed / Developed</b> economies. ",
      "These colours describe development group, not the clustering rule itself."
    ))
  })
  
  output$constellation_plot_ui <- renderUI({
    dat <- prepared_data()
    make_integrated_network_map(
      members_df = dat$members,
      nodes_df = dat$nodes,
      edges_df = dat$edges,
      selected_cluster = input$selected_cluster,
      max_nodes_per_cluster = input$map_n,
      selected_dev_groups = input$selected_dev_groups,
      show_labels = input$show_labels
    )
  })
  
  output$cluster_plot <- renderPlot({
    dat <- prepared_data()
    make_cluster_trend_plot(dat$trend, input$selected_cluster, top_n = input$top_n)
  })
}

shinyApp(ui = ui, server = server)
