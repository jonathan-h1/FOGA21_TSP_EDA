#### preliminary settings ####

library(tidyverse)
library(shiny)
library(DT)
library(heatmaply)
library(markdown)

# combine the data from different files
feats_combined <- read_csv("feature_set_zero.csv")
performance_data <- read_csv("combined.csv")
autoenc_norm <- read_csv('autoencoder_mlp_normed_2.csv') %>% select(-X1) %>% rename(autoencoder_x1 = X1_1, autoencoder_x2 = X2)
autoenc_unnorm <- read_csv('autoencoder_mlp_unnormed_2.csv') %>% select(-X1) %>% rename(autoencoder_x1 = X1_1, autoencoder_x2 = X2)

autoencoder_norm <- read_csv('autoencoder_normed_combined_full_set.csv') %>% select(-X1) %>% rename_with(~ gsub("X1", "autoencoder_x1", gsub("X2", "autoencoder_x2", .x, fixed = TRUE), fixed = TRUE))
autoencoder_unnorm <- read_csv('autoencoder_unnormed_combined_full_set.csv') %>% select(-X1) %>% rename_with(~ gsub("X1", "autoencoder_x1", gsub("X2", "autoencoder_x2", .x, fixed = TRUE), fixed = TRUE))

data_combined <- inner_join(feats_combined, performance_data)
data.split <- group_by(data_combined, norm) %>% group_split()

tb.unnorm <- left_join(data.split[[1]], autoenc_unnorm)
tb.norm <- left_join(data.split[[2]], autoenc_norm) 

tb.unnorm <- left_join(tb.unnorm, autoencoder_unnorm)
tb.norm <- left_join(tb.norm, autoencoder_norm) 

colnames_num <- tb.unnorm %>% select(where(is.numeric)) %>% colnames()
colnames_num <- c(colnames_num, 'size')

# options to selected from
solver <- c(
  'EAX' = "eax.",
  'LKH' = "lkh.",
  'Concorde' = "concorde."
)

groups <- c(
  "national-500-5000", "netgen", "netgen_morphed", "rue", "tsplib", 
  "vlsi-500-5000", "eax---lkh---simple", "eax---lkh---sophisticated", 
  "lkh---eax---simple", "lkh---eax---sophisticated", "tspgen"
)

root <- unique(tb.unnorm$root)

size <- c(
  "other", "500", "1000", "1500", "2000"
)

nng_and_mst <- c(
  "nng_3_n_weak", "nng_3_n_strong", 
  "nng_3_weak_components_mean", "nng_3_weak_components_median", 
  "nng_3_weak_components_min", "nng_3_weak_components_max", 
  "nng_3_weak_components_span", "nng_3_strong_components_mean", 
  "nng_3_strong_components_median", "nng_3_strong_components_min", 
  "nng_3_strong_components_max", "nng_3_strong_components_span", 
  "nng_5_n_weak", "nng_5_n_strong", 
  "nng_5_weak_components_mean", "nng_5_weak_components_median", 
  "nng_5_weak_components_min", "nng_5_weak_components_max", 
  "nng_5_weak_components_span", "nng_5_strong_components_mean", 
  "nng_5_strong_components_median", "nng_5_strong_components_min", 
  "nng_5_strong_components_max", "nng_5_strong_components_span", 
  "nng_7_n_weak", "nng_7_n_strong", 
  "nng_7_weak_components_mean", "nng_7_weak_components_median", 
  "nng_7_weak_components_min", "nng_7_weak_components_max", 
  "nng_7_weak_components_span", "nng_7_strong_components_mean", 
  "nng_7_strong_components_median", "nng_7_strong_components_min", 
  "nng_7_strong_components_max", "nng_7_strong_components_span",
  "mst_depth_mean", "mst_depth_median", "mst_depth_min", "mst_depth_max", 
  "mst_depth_span"
)

feat_groups <- list(
  'NNG and MST' = nng_and_mst
)

measure <- list(
  'PQR10' = "pqr10",
  'PAR10' = "par10",
  'Logarithmic PQR10' = "pqr10.log1p",
  'Logarithmic PAR10' = "par10.log1p"
)

join_vals = c("root", "group", "prob")

# Set up ggplot2 defaults

theme_set(theme_bw())

#### end settings ####


#### start UI ####

ui <- fluidPage(theme = shinythemes::shinytheme("united"),
                titlePanel("TSP Feature Normalization EDAshboard"),
                
                #### sidebar ####
                sidebarLayout(
                  sidebarPanel(
                    selectInput("solver_id", label="Solver", choices=names(solver), selected="LKH"),
                    selectInput("measure_id", label="Measure", choices=names(measure), selected="Logarithmic PQR10"),
                    checkboxGroupInput("instance_groups", "Instance Types", choices = groups, selected = groups),
                    checkboxGroupInput("instance_root", "Instance Root", choices = root, selected = root),
                    checkboxGroupInput("instance_size", "Instance Sizes", choices = size, selected = size)
                  ),
                  
                  #### main panel ####
                  
                  mainPanel(
                    tabsetPanel(type = "tabs",
                                
                                #### read me ####
                                
                                tabPanel('Read Me',
                                         br(),
                                         br(),
                                         
                                         fluidRow(
                                           column(1),
                                           column(9,
                                                  includeMarkdown("README.md")))
                                ),
                                
                                #### Tab 1: Data table of the data set ####
                                
                                tabPanel("Data Table",
                                         br(),
                                         DTOutput("datatable_complete")),
                                
                                #### Tab 2: Pair plots with specific variables ####
                                
                                tabPanel("Pair Plot",
                                         br(),
                                         
                                         fluidRow(
                                           column(4,
                                                  selectInput("feature_pair_x", "X", choices = c(colnames(tb.norm), size), selected = 'nng_3_n_strong')),
                                           column(4,
                                                  selectInput("feature_pair_y", "Y", choices = c(colnames(tb.norm), size), selected = 'nng_3_strong_components_max')),
                                           column(4,
                                                  checkboxInput("perf_color", "Color instances according to measure", TRUE))
                                         ),
                                         br(),
                                         fluidRow(
                                           splitLayout(cellWidths = c("50%", "50%"),
                                                       h4("Normalized Values"),
                                                       h4("Unnormalized Values")
                                           )
                                         ),
                                         
                                         fluidRow(
                                           splitLayout(cellWidths = c("50%", "50%"),
                                                       plotOutput("plot_pair_norm",
                                                                  brush = "pair_norm_brush"),
                                                       plotOutput("plot_pair_unnorm",
                                                                  brush = "pair_unnorm_brush")
                                           )
                                         ),
                                         
                                         h4("Brushed Points Norm"),
                                         
                                         DTOutput("datatable_pair_norm_brushed"),
                                         
                                         h4("Brushed Points Unnorm"),
                                         
                                         DTOutput("datatable_pair_unnorm_brushed")),
                                
                                #### Tab 3: Feature correlation heatmap plot ####
                                
                                tabPanel("Feature Cor Plot",
                                         br(),
                                         fluidRow(
                                           
                                           column(6,
                                                  checkboxInput("cluster", "Cluster features", FALSE)),
                                           conditionalPanel(
                                             condition = "input.cluster",
                                             column(6,
                                                    sliderInput("num_feat_cor_clust", "Number of Clusters",
                                                                min = 1, max = floor(length(colnames_num) / 2),
                                                                value = 4, step = 1))
                                           ),
                                         ),
                                         br(),
                                         fluidRow(
                                           h4('Normalized Features'),
                                           plotlyOutput("feat_cor_norm", height = 800),
                                           br(),
                                           h4('Unnormalized Features'),
                                           plotlyOutput("feat_cor_unnorm", height = 800)
                                         )
                                ),
                                
                                #### Tab 4: Correlation of variables with the solver performance ####
                                
                                tabPanel("Cor Plot",
                                         br(),
                                         
                                         fluidRow(
                                           column(4,
                                                  selectInput("solver_y", "Solver Y-axis", choices = names(solver))),
                                           column(4,
                                                  checkboxInput("arrows", "Show change of position", FALSE)),
                                           conditionalPanel(
                                             condition = "input.arrows",
                                             column(4,
                                                    sliderInput("arrow_length", "Min change",
                                                                min = 0, max = 3,
                                                                value = c(0.0, 3.0), step = 0.01))
                                           ),
                                         ),
                                         br(),
                                         fluidRow(
                                           splitLayout(cellWidths = c("50%", "50%"),
                                                       h4("Normalized Features"),
                                                       h4("Unnormalized Features")
                                           )
                                         ),
                                         fluidRow(
                                           splitLayout(cellWidths = c("48%", "48%"),
                                                       plotOutput("plot_cor_norm",
                                                                  brush = "cor_norm_brush"),
                                                       plotOutput("plot_cor_unnorm",
                                                                  brush = "cor_unnorm_brush")
                                           )
                                         ),
                                         
                                         h4("Brushed Points Norm"),
                                         
                                         DTOutput("datatable_cor_norm_brushed"),
                                         
                                         h4("Brushed Points Unnorm"),
                                         
                                         DTOutput("datatable_cor_unnorm_brushed")
                                )
                    )
                  )
                )
)

#### end UI ####

#### start server ####

server <- function(input, output, session) {

  #### Get current data set ####
  
  current_data_norm <- reactive({
    tb.norm.sel <- tb.norm %>% 
      mutate(
        size_str = case_when(
          substr(prob, 1, 4) %in% c("1000", "1500", "2000") ~ substr(prob, 1, 4),
          substr(prob, 1, 3) == "500" ~ "500",
          TRUE ~ "other"
        )
      ) %>%
      mutate(
        size = case_when(
          substr(prob, 1, 4) == '2000' ~ 2000,
          substr(prob, 1, 4) == '1500' ~ 1500,
          substr(prob, 1, 4) == '1000' ~ 1000,
          substr(prob, 1, 3) == "500" ~ 500,
          !is.na(as.integer(str_sub(prob, -8, -5))) ~ as.double(as.integer(str_sub(prob, -8, -5))),
          !is.na(as.integer(str_sub(prob, -7, -5))) ~ as.double(as.integer(str_sub(prob, -7, -5))),
          prob == 'lu980.tsp' ~ 634,
          prob == 'rw1621.tsp' ~ 866,
          TRUE ~ 0
        )
      ) 
    
    
    tb.norm.sel$group <- factor(tb.norm.sel$group)
    tb.norm.sel$size_str <- relevel(factor(tb.norm.sel$size_str), '500')
    
    tb.norm.sel %>% filter(group %in% input$instance_groups) %>% 
      filter(root %in% input$instance_root) %>% 
      filter(size_str %in% input$instance_size)
  })
  
  current_data_unnorm <- reactive({
    tb.unnorm.sel <- tb.unnorm %>% 
      mutate(size_str = case_when(
        substr(prob, 1, 4) %in% c("1000", "1500", "2000") ~ substr(prob, 1, 4),
        substr(prob, 1, 3) == "500" ~ "500",
        TRUE ~ "other"
      )) %>%
      mutate(
        size = case_when(
          substr(prob, 1, 4) == '2000' ~ 2000,
          substr(prob, 1, 4) == '1500' ~ 1500,
          substr(prob, 1, 4) == '1000' ~ 1000,
          substr(prob, 1, 3) == "500" ~ 500,
          !is.na(as.integer(str_sub(prob, -8, -5))) ~ as.double(as.integer(str_sub(prob, -8, -5))),
          !is.na(as.integer(str_sub(prob, -7, -5))) ~ as.double(as.integer(str_sub(prob, -7, -5))),
          prob == 'lu980.tsp' ~ 634,
          prob == 'rw1621.tsp' ~ 866,
          TRUE ~ 0
        )
      )
    
    tb.unnorm.sel$group <- factor(tb.unnorm.sel$group)
    tb.unnorm.sel$size_str <- relevel(factor(tb.unnorm.sel$size_str), '500')
    
    tb.unnorm.sel %>%
      filter(group %in% input$instance_groups) %>% 
      filter(root %in% input$instance_root) %>% 
      filter(size_str %in% input$instance_size)
  })
  
  
  #### Outputs ####
  
    #### Tab 1: Data table of the data set ####
  
  output$datatable_complete <- renderDT({
    data_combined
  })
  
  
    #### Tab 2: Pair plots with specific variables ####
  
  # the normalized data
  output$plot_pair_norm <- renderPlot({
    measure_cur <- paste0(solver[[input$solver_id]], measure[[input$measure_id]])
    tb.norm.sel <- current_data_norm()
    # color according to performance of the selected solver on the instances
    if (input$perf_color) {
      p <- ggplot(tb.norm.sel, aes_string(x = input$feature_pair_x, y = input$feature_pair_y, shape = "group", colour = measure_cur))+
        scale_shape_manual(values = 1:nlevels(tb.norm.sel$group)) +
        scale_color_viridis_c() +
        geom_point() + labs(
          color = paste0(input$solver_id, ' ', input$measure_id, ' Score'),
          shape = "Instance Group"
        )
      return(p)
    # color according to the instance size
    } else {
      p <- ggplot(tb.norm.sel, aes_string(x = input$feature_pair_x, y = input$feature_pair_y, shape = "group", colour = 'size_str'))+
        scale_shape_manual(values=1:nlevels(tb.norm.sel$group)) +
        scale_color_brewer(palette ="Set1") +
        geom_point() + labs(
          color = 'Instance Size',
          shape = "Instance Group"
        )
      return(p)
    }
    
  })
  
  # the unnormalized data
  output$plot_pair_unnorm <- renderPlot({
    measure_cur <- paste0(solver[[input$solver_id]], measure[[input$measure_id]])
    tb.unnorm.sel <- current_data_unnorm()
    # color according to performance of the selected solver on the instances
    if (input$perf_color) {
      p <- ggplot(tb.unnorm.sel, aes_string(x = input$feature_pair_x, y = input$feature_pair_y, shape = "group", colour = measure_cur))+
        scale_shape_manual(values=1:nlevels(tb.unnorm.sel$group)) +
        scale_color_viridis_c() +
        geom_point() + labs(
          color = paste0(input$solver_id, ' ', input$measure_id, ' Score'),
          shape = "Instance Group"
        )
    # color according to the instance size
    } else {
      p <- ggplot(tb.unnorm.sel, aes_string(x = input$feature_pair_x, y = input$feature_pair_y, shape = "group", colour = 'size_str'))+
        scale_shape_manual(values=1:nlevels(tb.unnorm.sel$group)) +
        scale_color_brewer(palette ="Set1") +
        geom_point() + labs(
          color = 'Instance Size',
          shape = "Instance Group"
        )
    }
    return(p)
  })
  
  # brushed points in normalized plot
  output$datatable_pair_norm_brushed <- renderDataTable({
    measure_x <- paste0(solver[[input$solver_id]], measure[[input$measure_id]])
    tb.norm.sel <- current_data_norm()
    brushedPoints(tb.norm.sel[, c("group", "prob", measure_x, input$feature_pair_x, input$feature_pair_y)], input$pair_norm_brush)
  })
  
  # brushed points in unnormalized plot
  output$datatable_pair_unnorm_brushed <- renderDataTable({
    measure_x <- paste0(solver[[input$solver_id]], measure[[input$measure_id]])
    tb.unnorm.sel <- current_data_unnorm()
    brushedPoints(tb.unnorm.sel[, c("group", "prob", measure_x, input$feature_pair_x, input$feature_pair_y)], input$pair_unnorm_brush)
  })
  
    #### Tab 3: Feature correlation heatmap plot ####
  
  # the normalized features
  output$feat_cor_norm <- renderPlotly({
    colnames_sel <- colnames_num
    colnames_sel <- feat_groups[['NNG and MST']]
    
    cor_data_un <- current_data_unnorm() %>% select(all_of(colnames_sel)) %>%
      select(where(~ !is.na(.x) && !is.na(var(.x)) && var(.x) != 0))
    hclust_ord <- hclust(as.dist(1 - cor(cor_data_un)))
    
    cor_data <- current_data_norm() %>% select(all_of(colnames_sel)) %>%
      select(where(~ !is.na(.x) && !is.na(var(.x)) && var(.x) != 0)) %>%
      select(hclust_ord$order)
    
    heatmaply_cor(
      cor(cor_data),
      xlab = "Features", 
      ylab = "Features",
      Rowv = input$cluster,
      Colv = input$cluster,
      k_col = input$num_feat_cor_clust, 
      k_row = input$num_feat_cor_clust
    )
  })
  
  # the unnormalized features
  output$feat_cor_unnorm <- renderPlotly({
    colnames_sel <- colnames_num
    colnames_sel <- feat_groups[['NNG and MST']]
    cor_data <- current_data_unnorm() %>% select(all_of(colnames_sel)) %>%
      select(where(~ !is.na(.x) && !is.na(var(.x)) && var(.x) != 0))
    
    hclust_ord <- hclust(as.dist(1 - cor(cor_data)))
    
    cor_data <- cor_data %>% select(hclust_ord$order)
    
    heatmaply_cor(
      cor(cor_data),
      xlab = "Features", 
      ylab = "Features",
      Rowv = input$cluster,
      Colv = input$cluster,
      k_col = input$num_feat_cor_clust, 
      k_row = input$num_feat_cor_clust
    )
  })
    #### Tab 4: Correlation of variables with the solver performance ####
  
  # the normalized features
  output$plot_cor_norm <- renderPlot({
    # select data
    measure_x <- paste0(solver[[input$solver_id]], measure[[input$measure_id]])
    measure_y <- paste0(solver[[input$solver_y]], measure[[input$measure_id]])
    colnames_sel <- feat_groups[['NNG and MST']]
    
    tb.norm.sel <- as_tibble(current_data_norm())
    corr_x <- cor(tb.norm.sel[, colnames_sel], tb.norm.sel[, measure_x], method = "spearman")
    corr_y <- cor(tb.norm.sel[, colnames_sel], tb.norm.sel[, measure_y], method = "spearman")
    data <- as_tibble(cbind(corr_x, corr_y))
    
    # create base plot
    plot <- ggplot(data, aes_string(x = measure_x, y = measure_y)) +
      geom_point() + 
      annotate(
        "rect", xmin = -Inf, xmax = 0, ymin = 0, ymax = Inf,
        fill = scales::muted("green", l = 60), alpha = 0.25
      ) +
      annotate(
        "rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = 0,
        fill = scales::muted("green", l = 60), alpha = 0.25
      ) +
      annotate(
        "rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = 0,
        fill = scales::muted("orange", l = 60), alpha = 0.25
      ) +
      annotate(
        "rect", xmin = 0, xmax = Inf, ymin = 0, ymax = Inf,
        fill = scales::muted("orange", l = 60), alpha = 0.25
      ) + labs(
        x = paste0("correlation with the ", input$solver_id, ' ', input$measure_id, ' score'),
        y = paste0("correlation with the ", input$solver_y, ' ', input$measure_id, ' score')
      )
    
    # visualize the movement if requested
    if (input$arrows) {
      measure_x_un <- measure_x
      measure_y_un <- measure_y
      tb.unnorm.sel <- as_tibble(current_data_unnorm())
      corrun_x <- cor(tb.unnorm.sel[, colnames_sel], tb.unnorm.sel[, measure_x_un], method = "spearman")
      corrun_y <- cor(tb.unnorm.sel[, colnames_sel], tb.unnorm.sel[, measure_y_un], method = "spearman")
      data_un <- as_tibble(cbind(corrun_x, corrun_y))
      names(data_un) <- c('old_x', 'old_y')
      data_ar <- as_tibble(cbind(data, data_un)) 
      data_ar <- data_ar %>% 
        dplyr::mutate(length_ar = sqrt((.data[[measure_x]] - .data[['old_x']])**2 + (.data[[measure_y]] - .data[['old_y']])**2)) %>%
        dplyr::filter(input$arrow_length[1] < length_ar & input$arrow_length[2] > length_ar)
      
      return(
        plot + geom_segment(data = data_ar, aes_string(x = 'old_x', y = 'old_y', xend = measure_x, yend = measure_y),
                            arrow = arrow(), color='#377EB8', alpha=0.175)
      )
    }
    else {
      return(plot)
    }
  })
  
  # the unnormalized features
  output$plot_cor_unnorm <- renderPlot({
    # select data
    measure_x <- paste0(solver[[input$solver_id]], measure[[input$measure_id]])
    measure_y <- paste0(solver[[input$solver_y]], measure[[input$measure_id]])
    colnames_sel <- feat_groups[['NNG and MST']]
    
    tb.unnorm.sel <- current_data_unnorm()
    corr_x <- cor(tb.unnorm.sel[, colnames_sel], tb.unnorm.sel[, measure_x], method = "spearman")
    corr_y <- cor(tb.unnorm.sel[, colnames_sel], tb.unnorm.sel[, measure_y], method = "spearman")
    data <- as_tibble(cbind(corr_x, corr_y))
    
    # create and return the plot
    ggplot(data, aes_string(x = measure_x, y = measure_y)) +
      geom_point() + 
      annotate(
        "rect", xmin = -Inf, xmax = 0, ymin = 0, ymax = Inf,
        fill = scales::muted("green", l = 60), alpha = 0.25
      ) +
      annotate(
        "rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = 0,
        fill = scales::muted("green", l = 60), alpha = 0.25
      ) +
      annotate(
        "rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = 0,
        fill = scales::muted("orange", l = 60), alpha = 0.25
      ) +
      annotate(
        "rect", xmin = 0, xmax = Inf, ymin = 0, ymax = Inf,
        fill = scales::muted("orange", l = 60), alpha = 0.25
      ) + labs(
        x = paste0("correlation with the ", input$solver_id, ' ', input$measure_id, ' score'),
        y = paste0("correlation with the ", input$solver_y, ' ', input$measure_id, ' score')
      )
  })
  
  # select the brushed normalized features
  output$datatable_cor_norm_brushed <- renderDataTable({
    measure_x <- paste0(solver[[input$solver_id]], measure[[input$measure_id]])
    measure_y <- paste0(solver[[input$solver_y]], measure[[input$measure_id]])
    colnames_sel <- feat_groups[['NNG and MST']]
    
    tb.norm.sel <- current_data_norm()
    corr_norm_x <- cor(tb.norm.sel[, colnames_sel], tb.norm.sel[, measure_x], method = "spearman")
    corr_norm_y <- cor(tb.norm.sel[, colnames_sel], tb.norm.sel[, measure_y], method = "spearman")
    data_norm <- as_tibble(cbind(corr_norm_x, corr_norm_y), rownames = "Feature")
    brushedPoints(data_norm, input$cor_norm_brush)
  })
  
  # select the brushed unnormalized features
  output$datatable_cor_unnorm_brushed <- renderDataTable({
    measure_x <- paste0(solver[[input$solver_id]], measure[[input$measure_id]])
    measure_y <- paste0(solver[[input$solver_y]], measure[[input$measure_id]])
    colnames_sel <- feat_groups[['NNG and MST']]
    
    tb.unnorm.sel <- current_data_unnorm()
    corr_unnorm_x <- cor(tb.unnorm.sel[, colnames_sel], tb.unnorm.sel[, measure_x], method = "spearman")
    corr_unnorm_y <- cor(tb.unnorm.sel[, colnames_sel], tb.unnorm.sel[, measure_y], method = "spearman")
    data_unnorm <- as_tibble(cbind(corr_unnorm_x, corr_unnorm_y), rownames = "Feature")
    brushedPoints(data_unnorm, input$cor_unnorm_brush)
  })
  
}

#### end server ####

shinyApp(ui, server)
