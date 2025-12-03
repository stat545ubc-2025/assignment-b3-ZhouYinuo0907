#Penguin Distribution
#This is a shiny app enabling the users to explore the distribution of 
#penguin species across different islands using the palmerpenguins dataset.
#This shiny app contains 3 features
# 1. Dynamic plot (penguin distribution grouped by islands and species)
# 2. Interactive table showing mean body mass of selected penguin species and islands
# 3. Download the plot created according to user input





#library the packages 
library(shiny)
library(dplyr)
library(ggplot2)
library(palmerpenguins)

#Define UI
ui <- fluidPage(
  
  
  titlePanel("Penguins: Island and Species Filter"),
  
  #Instruction
  wellPanel(
    h4("Instructions:"),
    p("1. Select one or more islands from the list."),
    p("2. Select one or more penguin species to filter the data."),
    p("3. The table will show the species on the selected islands and their average body mass."),
    p("4. The histogram shows the count of selected species by island."),
    p("5. You can download the histogram using the 'Download Current Plot' button.")
  ),
  
  #User input
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "island",
        "Select island(s):",
        choices = unique(na.omit(penguins$island)),
        selected = unique(na.omit(penguins$island)),
        multiple = TRUE
      ),
      
      selectInput(
        "species",
        "Select species:",
        choices = unique(na.omit(penguins$species)),
        selected = unique(na.omit(penguins$species)),
        multiple = TRUE
      ),
      
      #Download button
      downloadButton("downloadPlot", "Download Current Plot")
    ),
    
    mainPanel(
      h3("Penguin Summary Table"),
      tableOutput("penguinTable"),
      
      h3("Histogram Based on User Selection"),
      plotOutput("penguinPlot")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Filter Data
  filtered_data <- reactive({
    penguins %>%
      filter(
        island %in% input$island,
        species %in% input$species
      )
  })
  
  # Table: mean body mass defined by islands and species
  output$penguinTable <- renderTable({
    filtered_data() %>%
      group_by(island, species) %>%
      summarise(
        mean_body_mass_g = mean(body_mass_g, na.rm = TRUE)
      ) %>%
      arrange(island, species)
  })
  
  # Plot: distribution grouped by islands and species
  plot_reactive <- reactive({
    ggplot(filtered_data(), aes(x = species, fill = island)) +
      geom_histogram(stat = "count", position = "dodge") +
      labs(
        title = "Histogram of Penguin Species by Island",
        x = "Species",
        y = "Count"
      ) +
      theme_minimal()
  })
  
  output$penguinPlot <- renderPlot({
    plot_reactive()
  })
  
  # Download plot
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("penguin_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(
        file,
        plot = plot_reactive(),
        width = 7,
        height = 5,
        dpi = 300
      )
    }
  )
}

# Run the Shiny App
shinyApp(ui, server)

