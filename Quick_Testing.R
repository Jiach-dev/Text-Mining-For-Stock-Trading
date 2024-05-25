library(shiny)
library(wordcloud2)
library(dplyr)
library(tidytext)

# Example function to simulate the cleaned term probabilities
cleaned_term_probabilities_val <- function() {
  list(
    data.frame(term = c("example1", "example2", "example3"), probability = c(0.1, 0.2, 0.7)),
    data.frame(term = c("example4", "example5", "example6"), probability = c(0.2, 0.3, 0.5)),
    data.frame(term = c("example7", "example8", "example9"), probability = c(0.3, 0.3, 0.4)),
    data.frame(term = c("example10", "example11", "example12"), probability = c(0.4, 0.4, 0.2))
  )
}

server <- function(input, output, session) {
  
  # Observe the cleaned term probabilities
  observe({
    cleaned_term_probabilities <- cleaned_term_probabilities_val()
    
    # Render the UI for wordcloud plots dynamically
    output$wordcloud_plots <- renderUI({
      plot_outputs <- lapply(seq_along(cleaned_term_probabilities), function(i) {
        column(6, wordcloud2Output(outputId = paste0("wordcloud_plot_", i)))
      })
      do.call(fluidRow, plot_outputs)
    })
  })
  
  observe({
    cleaned_term_probabilities <- cleaned_term_probabilities_val()
    print(cleaned_term_probabilities)
    
    lapply(seq_along(cleaned_term_probabilities), function(i) {
      local_i <- i  # Capture the current value of i
      
      # Plot wordcloud with sentiment information
      output[[paste0("wordcloud_plot_", local_i)]] <- renderWordcloud2({
        
        # Prepare data for word cloud
        words <- cleaned_term_probabilities[[local_i]]$term
        freq <- cleaned_term_probabilities[[local_i]]$probability
        
        # Convert data to a data frame
        wordcloud_data <- data.frame(word = words, freq = freq)
        
        wordcloud_data <- aggregate(freq ~ word, data = wordcloud_data, sum)
        
        # Order the freq column
        wordcloud_data <- wordcloud_data[order(wordcloud_data$freq, decreasing = TRUE), ]
        
        # Scale frequencies to integers (adjust scale as needed)
        wordcloud_data$freq <- as.integer(wordcloud_data$freq * 100)
        
        # Remove rows with missing values
        wordcloud_data <- na.omit(wordcloud_data)
        
        # Create word cloud
        wordcloud2(data = wordcloud_data)
      })
    })
  })
}


ui <- fluidPage(
  navbarPage(
    "Wordclouds",
    tabPanel(
      "Dynamic Wordclouds",
      sidebarLayout(
        sidebarPanel(
          # Sidebar elements (if needed)
        ),
        mainPanel(
          uiOutput("wordcloud_plots")
        )
      )
    )
  )
)

shinyApp(ui = ui, server = server)
