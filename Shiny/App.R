library(shiny)
library(dplyr)
library(readr)
library(tibble)
library(purrr)

source("R/Model_functions.R")

final_kmeans_model <- readRDS(
  "../Models/Final_kmeans_model.rds"
)

final_model_features <- readRDS(
  "../Models/Final_model_features.rds"
)

cluster_labels <- readRDS(
  "../Models/Cluster_labels.rds"
)

scaling_parameters <- read_csv(
  "../Models/Scaling_parameters.csv",
  show_col_types = FALSE
)

player_cluster_profiles <- readRDS(
  "../Data/Final/Player_cluster_profiles.rds"
)

ui <- fluidPage(
  
  titlePanel(
    "MiLB Hitter Archetype Explorer"
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h3("Player Inputs"),
      
      textInput(
        inputId = "player_name",
        label = "Player Name",
        value = "Test Player"
      ),
      
      numericInput(
        inputId = "plate_appearances",
        label = "Plate Appearances",
        value = 450,
        min = 1
      ),
      
      numericInput(
        inputId = "strikeouts",
        label = "Strikeouts",
        value = 110,
        min = 0
      ),
      
      numericInput(
        inputId = "pitches_per_plate_appearance",
        label = "Pitches per Plate Appearance",
        value = 3.95,
        min = 0
      ),
      
      numericInput(
        inputId = "iso",
        label = "ISO",
        value = 0.180,
        min = 0
      ),
      
      numericInput(
        inputId = "babip",
        label = "BABIP",
        value = 0.310,
        min = 0,
        max = 1
      ),
      
      numericInput(
        inputId = "stolen_bases",
        label = "Stolen Bases",
        value = 15,
        min = 0
      ),
      
      numericInput(
        inputId = "caught_stealing",
        label = "Caught Stealing",
        value = 4,
        min = 0
      ),
      
      numericInput(
        inputId = "ground_outs_to_air_outs",
        label = "Ground Outs to Air Outs",
        value = 1.10,
        min = 0
      ),
      
      actionButton(
        inputId = "analyze_player",
        label = "Analyze Player"
      )
    ),
    
    mainPanel(
      
      h3("Player Analysis"),
      
      p(
        "Enter a hitter's statistics and click Analyze Player.",
        "The model assigns the hitter to a MiLB archetype and",
        "returns the most similar historical players."
      ),
      
      hr(),
      
      h4("Assigned Archetype"),
      
      tableOutput(
        outputId = "assignment_summary"
      ),
      
      hr(),
      
      h4("Similar Historical Players"),
      
      tableOutput(
        outputId = "similar_players"
      )
      
    )
  )
)

server <- function(input, output, session) {
  player_assignment <- eventReactive(
    input$analyze_player,
    {
      
      assign_new_player_cluster(
        player_name = input$player_name,
        plate_appearances = input$plate_appearances,
        strikeouts = input$strikeouts,
        pitches_per_plate_appearance =
          input$pitches_per_plate_appearance,
        iso = input$iso,
        babip = input$babip,
        stolen_bases = input$stolen_bases,
        caught_stealing = input$caught_stealing,
        ground_outs_to_air_outs =
          input$ground_outs_to_air_outs
      )
      
    }
  )
  similar_player_results <- eventReactive(
    input$analyze_player,
    {
      
      find_similar_new_player(
        plate_appearances = input$plate_appearances,
        strikeouts = input$strikeouts,
        pitches_per_plate_appearance =
          input$pitches_per_plate_appearance,
        iso = input$iso,
        babip = input$babip,
        stolen_bases = input$stolen_bases,
        caught_stealing = input$caught_stealing,
        ground_outs_to_air_outs =
          input$ground_outs_to_air_outs,
        player_profiles =
          player_cluster_profiles,
        final_model_features =
          final_model_features,
        scaling_parameters =
          scaling_parameters,
        n_matches = 10
      )
      
    }
  )
  output$assignment_summary <- renderTable({
    
    assignment_table <- player_assignment()$summary 
    
    assignment_table <- assignment_table |>
      mutate(
        distance_to_center =
          round(distance_to_center, 3),
        second_closest_distance =
          round(second_closest_distance, 3),
        distance_gap =
          round(distance_gap, 3)
      )
    
    names(assignment_table) <- c(
      "Player",
      "Cluster",
      "Archetype",
      "Distance to Center",
      "Second Closest Distance",
      "Distance Gap",
      "Interpretation"
    )
    
    assignment_table
  })
  
  output$similar_players <- renderTable({
    similar_table <- similar_player_results()
    
    similar_table <- similar_table |> 
      mutate(similarity_distance = round(similarity_distance, 3))
    
    names(similar_table) <- c(
      "Player",
      "Season",
      "Level",
      "League",
      "Team",
      "Cluster",
      "Archetype",
      "Distance"
    )
    
    similar_table
  })
}

shinyApp(
  ui = ui,
  server = server
)
