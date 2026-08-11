library(shiny)
library(dplyr)
library(readr)
library(tibble)
library(purrr)

source("R/Model_functions.R")

final_kmeans_model <- readRDS(
  "Models/Final_kmeans_model.rds"
)

final_model_features <- readRDS(
  "Models/Final_model_features.rds"
)

cluster_labels <- readRDS(
  "Models/Cluster_labels.rds"
)

scaling_parameters <- read_csv(
  "Models/Scaling_parameters.csv",
  show_col_types = FALSE
)

player_cluster_profiles <- readRDS(
  "Data/Player_cluster_profiles.rds"
)

ui <- fluidPage(
  
  tags$head(tags$link(
    rel = "stylesheet",
    type = "text/css",
    href = "styles.css"
  )),
  
  titlePanel(
    "MiLB Hitter Archetype Explorer"
  ),
  
  navbarPage(
    
    title = NULL,
    
    tabPanel(
      "New Player Analysis",
      
      br(),
      
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
            label = "Analyze Player",
            icon = icon("magnifying-glass")
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
          
          uiOutput(
            outputId = "assignment_card"
          ),
          
          hr(),
          
          h4("Similar Historical Players"),
          
          tableOutput(
            outputId = "similar_players"
          )
          
        )
        
      )
      
    ),
    
    tabPanel(
      "Historical Player Search",
      
      br(),
      
      h3("Historical Player Search"),
      
      p(
        "Select a historical MiLB hitter to view their",
        "archetype and closest player comparisons."
      ),
      
      selectizeInput(
        inputId = "historical_player_id",
        label = "Select Player",
        choices = NULL,
        options = list(
          placeholder = "Type a player name...",
          maxOptions = 50
        )
      ),
      
      actionButton(
        inputId = "search_historical_player",
        label = "View Player",
        icon = icon("magnifying-glass")
      ),
      
      hr(),
      
      uiOutput(
        outputId = "historical_player_card"
      ),
      
      h4("Similar Historical Players"),
      
      tableOutput(
        outputId = "historical_similar_players"
      )
    ),
    
    tabPanel(
      "Archetypes",
      
      br(),
      
      h3("Hitter Archetypes"),
      
      p(
        "The K-means model groups hitters into four offensive archetypes.",
        "Each archetype reflects a different combination of plate approach,",
        "power, contact, baserunning, and batted-ball tendencies."
      ),
      hr(),
      
      fluidRow(
        column(
          width = 6,
          
          div(
            class = "archetype-card cluster-1",
            h3("Cluster 1"),
            h4("Power Hitter"),
            p("Strikeout-prone power hitters")
          )
      ),
      
      column(
        width = 6,
        
        div(
          class = "archetype-card cluster-2",
          h3("Cluster 2"),
          h4("Contact Hitter"),
          p(
            "Contact hitters who swing early."
          )
        )
      )
      ),
    
    fluidRow(
      
      column(
        width = 6,
        
        div(
          class = "archetype-card cluster-3",
          h3("Cluster 3"),
          h4("Power Hitter"),
          p(
            "Second power hitting group with moderate speed."
          )
        )
      ),
      
      column(
        width = 6,
        
        div(
          class = "archetype-card cluster-4",
          h3("Cluster 4"),
          h4("Contact Hitter"),
          p(
            "Fast and aggressive contact hitters."
          )
        )
      )
    )
  ),
    
    
  tabPanel(
    "About",
    
    br(),
    
    h3("About the Model"),
    
    p(
      "This application uses K-means clustering to identify",
      "offensive player archetypes among qualified Minor League",
      "Baseball hitters from 2022 through 2025."
    ),
    
    h4("Model Features"),
    
    tags$ul(
      tags$li("Pitches per plate appearance"),
      tags$li("Strikeout rate"),
      tags$li("ISO"),
      tags$li("BABIP"),
      tags$li("Stolen base attempt rate"),
      tags$li("Ground-ball to air-ball ratio")
    ),
    
    h4("How It Works"),
    
    p(
      "Player statistics are transformed and standardized before",
      "being grouped using K-means clustering. New players are",
      "assigned to the nearest cluster center using the same",
      "training transformations."
    ),
    
    p(
      "Similar players are identified using Euclidean distance",
      "across the standardized model features."
    ),
    
    h4("Important Note"),
    
    p(
      "The archetypes are descriptive groupings rather than",
      "projections of future performance. Similarity distance",
      "should also be interpreted as a comparative measure rather",
      "than a probability."
    )
  )
  )
)
  


server <- function(input, output, session) {
  
  observe({
    updateSelectizeInput(
      session,
      inputId = "historical_player_id",
      choices = setNames(
        player_cluster_profiles$player_league_season_id,
        paste(
          player_cluster_profiles$playerName,
          "-",
          player_cluster_profiles$requested_season,
          "-",
          player_cluster_profiles$requested_level
        )
      ),
      server = TRUE
    )
  })
  
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
  output$assignment_card <- renderUI({
    
    result <- player_assignment()$summary
    
    div(
      class = "assignment-card",
      
      h2(
        result$archetype_name
      ),
      
      h4(
        paste(
          result$player_name,
          "- Cluster",
          result$assigned_cluster
        )
      ),
      
      p(
        result$assignment_note
      ),
      
      p(
        strong("Distance to cluster center: "),
        round(
          result$distance_to_center,
          3
        )
      ),
      
      p(
        strong("Distance gap: "),
        round(
          result$distance_gap,
          3
        )
      )
    )
    
  })
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
  
  historical_player_result <- eventReactive(
    input$search_historical_player,
    {
      player_cluster_profiles |> 
        filter(player_league_season_id == input$historical_player_id)
    }
  )
  
  output$historical_player_card <- renderUI({
    
    result <- historical_player_result()
    
    div(
      class = "assignment-card",
      
      h2(
        result$archetype_name
      ),
      
      h4(
        paste(
          result$playerName,
          "-",
          result$requested_season,
          result$requested_level
        )
      ),
      
      p(
        strong("Team: "),
        result$teamName
      ),
      
      p(
        strong("Cluster: "),
        result$cluster
      )
    )
    
  })
  
  historical_similar_results <- eventReactive(
    input$search_historical_player,
    {
      
      find_similar_historical_players(
        player_id =
          input$historical_player_id,
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
  
  output$historical_similar_players <- renderTable({
    
    historical_table <-
      historical_similar_results()
    
    historical_table <- historical_table |>
      mutate(
        similarity_distance =
          round(similarity_distance, 3)
      )
    
    names(historical_table) <- c(
      "Player",
      "Season",
      "Level",
      "League",
      "Team",
      "Cluster",
      "Archetype",
      "Distance"
    )
    
    historical_table
    
  })
}

shinyApp(
  ui = ui,
  server = server
)
