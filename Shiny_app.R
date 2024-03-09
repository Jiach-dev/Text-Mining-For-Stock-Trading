library(shiny)
library(DBI)
library(RMySQL)
library(tidyverse)  # For data manipulation
library(yaml)       # For reading YAML config file
library(shinycssloaders)
library(topicmodels)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(gridExtra)

# Read the configuration file
config <- yaml::read_yaml("config.yaml")

# Read custom stop words from file
custom_stopwords <- readLines("custom_stopwords.txt")

# Access database credentials from the configuration
db_host <- config$database$host
db_name <- config$database$database
db_user <- config$database$user
db_password <- config$database$password

default_tickers <- c("AAPL", "GOOGL", "MSFT")


# Function to fetch data from the database
fetch_data <- function(tickers) {
  print(tickers)
  withSpinner({
    # Establish a connection to the MySQL database
    con <- dbConnect(MySQL(), 
                     dbname = db_name,
                     host = db_host,
                     user = db_user,
                     password = db_password)
    
    # Construct the query to fetch news data with joins to dimension tables
    get_news_query <- glue::glue("SELECT nh.InsertionDate, nh.Ticker, nh.News1, nh.News2, nh.News3, nh.News4, nh.News5, nh.News6, nh.News7, nh.News8, nh.News9, nh.News10,
                                 dc.Company, ds.Sector, di.Industry, dco.Country
                                 FROM Ratios_Tech.Stocks_News_headlines nh
                                 INNER JOIN Dimension_Company dc ON nh.Ticker = dc.Ticker
                                 INNER JOIN Dimension_Sector ds ON nh.Ticker = ds.Ticker
                                 INNER JOIN Dimension_Industry di ON dc.Ticker = di.Ticker
                                 INNER JOIN Dimension_Country dco ON dc.Ticker = dco.Ticker
                                 WHERE nh.Ticker IN ('AAPL');")
    
    # Print the constructed SQL query for debugging
    print(get_news_query)
    
    # Fetch data from the database
    ticker_data <- tryCatch({
      dbGetQuery(con, get_news_query)
    }, error = function(e) {
      # Print the error message for debugging
      cat("Error in fetch_data:", conditionMessage(e), "\n")
      return(NULL)  # Return NULL or an empty data frame
    })
    
    # Close the database connection
    dbDisconnect(con)
    
    return(ticker_data)
  })
}


# Define a function to preprocess the text
preprocess_text <- function(text) {
  withSpinner({
    # Remove text within brackets and the brackets themselves at the end of the text
    text <- gsub("\\[.*?\\]\\s*$", "", text)
    # Remove the news source in parentheses at the end of the text
    text <- gsub("\\([^()]*\\)\\s*$", "", text)
    # Remove text within parentheses and the parentheses themselves
    text <- gsub("\\(.*?\\)", "", text)
    # Remove non-alphanumeric characters
    text <- gsub("[,'\\:\\.\\?\\;]", "", text)
    # Convert text to UTF-8 encoding
    text <- iconv(text, to = "UTF-8")
    # Remove non-ASCII characters
    text <- gsub("[^[:alnum:][:space:]]", "", text)
    # Convert the text to lowercase
    text <- tolower(text)
    # Tokenize the text
    tokens <- unlist(strsplit(text, "\\s+"))
    # Remove stopwords
    tokens <- tokens[!tokens %in% stopwords("en")]
    # Remove custom stopwords
    tokens <- tokens[!tokens %in% custom_stopwords]
    # Remove short words (e.g., one or two characters)
    tokens <- tokens[nchar(tokens) > 2]
    # Lemmatize tokens
    tokens <- textstem::lemmatize_words(tokens)
    # Remove unigrams
    tokens <- tokens[sapply(tokens, function(token) nchar(token) > 1)]
    # Concatenate tokens into bigrams
    bigrams <- sapply(2:length(tokens), function(i) {
      if (grepl("[[:alnum:]]", gsub("[[:punct:]]", "", tokens[i - 1])) && 
          grepl("[[:alnum:]]", gsub("[[:punct:]]", "", tokens[i]))) {
        paste(tokens[i - 1], tokens[i], sep = " ")
      } else {
        tokens[i]
      }
    })
    return(bigrams)
  }, type = 3)  # Type 3: Spinner with text
}

# Define UI
ui <- fluidPage(
  titlePanel("Text Mining and Topic Modeling"),
  sidebarLayout(
    sidebarPanel(
      # Input controls for tickers
      selectInput("ticker", "Select Ticker", choices = default_tickers, multiple = TRUE),
      selectInput("company", "Select Company", choices = NULL, multiple = TRUE),     
      selectInput("sector", "Select Sector", choices = NULL, multiple = TRUE),
      selectInput("industry", "Select Industry", choices = NULL, multiple = TRUE),
      selectInput("country", "Select Country", choices = NULL, multiple = TRUE)
    ),
    mainPanel(
      # Output elements for visualizations
      uiOutput("wordcloud_plots")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Establish a connection to the MySQL database
  con <- dbConnect(MySQL(), 
                   dbname = db_name,
                   host = db_host,
                   user = db_user,
                   password = db_password)
  
  # Function to fetch distinct values from the database
  get_distinct_values <- function(column_name, YourTable) {
    query <- glue::glue("SELECT DISTINCT {column_name} FROM {YourTable};")
    distinct_values <- dbGetQuery(con, query)
    return(distinct_values[[1]])
  }
  
  # ReactiveValues to store selected tickers
  selected_tickers <- reactiveValues(tickers = default_tickers)
  
  
  # Reactive expression to fetch data based on selected tickers
  ticker_data <- reactive({
    fetch_data(selected_tickers$tickers)
  })
  
  # Update choices for selectInput elements with default tickers
  updateSelectInput(session, "ticker", choices = default_tickers)
  
  # Update choices for selectInput elements
  updateSelectInput(session, "company", choices = get_distinct_values("Company", "Dimension_Company"))
  updateSelectInput(session, "sector", choices = get_distinct_values("Sector", "Dimension_Sector"))
  updateSelectInput(session, "industry", choices = get_distinct_values("Industry", "Dimension_Industry"))
  updateSelectInput(session, "country", choices = get_distinct_values("Country", "Dimension_Country"))
  
  # Perform text mining and topic modeling on fetched data
  observe({
    ticker_data_value <- ticker_data()  # Access the value of the reactive expression
    
    # Combine news columns into a single text column
    ticker_data_value$Combined_News <- apply(ticker_data_value[, 3:7], 1, paste, collapse = " ")
    
    # Preprocess the text
    processed_texts <- lapply(ticker_data_value$Combined_News, preprocess_text)
    
    # Assign the processed texts to ticker_data$Processed_Text
    ticker_data_value$Processed_Text <- processed_texts
    
    # Create document-term matrix
    dtm <- DocumentTermMatrix(Corpus(VectorSource(ticker_data_value$Processed_Text)))
    
    # Create the Document-Term Matrix with the custom tokenizer
    lda_model <- LDA(dtm, k = 4, control = list(seed = 1234))
    
    # Get the posterior probabilities of terms
    posterior_terms <- posterior(lda_model)$terms
    
    # Function to calculate term probabilities for each topic
    calculate_term_probabilities <- function(top_terms, lda_model) {
      term_probabilities <- list()
      term_topic_probs <- posterior(lda_model)$terms
      
      num_terms <- min(ncol(top_terms), ncol(term_topic_probs))
      
      for (i in 1:num_terms) {
        topic_terms <- top_terms[, i]
        topic_probs <- term_topic_probs[, i]
        
        topic_probs <- topic_probs / sum(topic_probs)
        
        term_probabilities[[i]] <- data.frame(term = topic_terms, probability = topic_probs)
      }
      
      return(term_probabilities)
    }
    
    # Extract top terms for each topic
    top_terms <- terms(lda_model, 1000)
    
    # Calculate term probabilities for each topic
    term_probabilities <- calculate_term_probabilities(top_terms, lda_model)
    
    cleaned_term_probabilities <- lapply(term_probabilities, function(topic) {
      topic$term <- gsub('[[:punct:]]', '', topic$term)
      topic$term <- gsub('"|\'', '', topic$term)
      return(topic)
    })
    
    # Aggregate probabilities by summing them up for duplicate terms
    aggregated_term_probabilities <- lapply(cleaned_term_probabilities, function(topic) {
      aggregate(probability ~ term, data = topic, sum)
    })
    
    # Remove terms containing numbers from the term probabilities
    cleaned_term_probabilities <- lapply(aggregated_term_probabilities, function(topic) {
      topic[!grepl("\\d", topic$term), ]
    })
    
    # Initialize a list to store aggregated term probabilities
    aggregated_term_probabilities <- list()
    
    # Iterate over each element (topic) in the cleaned_term_probabilities list
    for (i in seq_along(cleaned_term_probabilities)) {
      # Extract the current topic's term probabilities
      topic <- cleaned_term_probabilities[[i]]
      
      # Aggregate probabilities by summing them up for duplicate terms
      aggregated_topic <- aggregate(probability ~ term, data = topic, sum)
      
      # Append the aggregated topic to the list
      aggregated_term_probabilities[[i]] <- aggregated_topic
    }
    
    # Remove terms containing numbers from the term probabilities
    cleaned_term_probabilities <- lapply(aggregated_term_probabilities, function(topic) {
      topic[!grepl("\\d", topic$term), ]
    })
    
    # Print cleaned_term_probabilities
    
    text_colors <- brewer.pal(8, "Dark2")

    # Store cleaned term probabilities as a reactiveVal
    cleaned_term_probabilities_val <- reactiveVal(NULL)
    
    # When the data is available, preprocess it and store the result
    observeEvent(ticker_data(), {
      cleaned_term_probabilities_val(
        lapply(term_probabilities, function(topic) {
          topic$term <- gsub('[[:punct:]]', '', topic$term)
          topic$term <- gsub('"|\'', '', topic$term)
          return(topic)
        })
      )
    })
    
    output$wordcloud_plots <- renderUI({
      plot_outputs <- lapply(seq_along(cleaned_term_probabilities), function(i) {
        plotOutput(outputId = paste0("wordcloud_plot_", i))
      })
      
      fluidPage(
        fluidRow(
          column(6, align = "center", plot_outputs[[1]]),
          column(6, offset = 0),
          column(6, align = "center", plot_outputs[[2]]) # Add space between columns
        ),
        fluidRow(
          column(6, align = "center", plot_outputs[[3]]),
          column(6, offset = 0),
          column(6, align = "center", plot_outputs[[4]]) # Add space between columns
        )
      )
    })
    
    # Render each plot separately
    # Render each plot separately
    observe({
      text_colors <- brewer.pal(8, "Dark2")
      
      cleaned_term_probabilities <- cleaned_term_probabilities_val()
      
      for (i in seq_along(cleaned_term_probabilities)) {
        output[[paste0("wordcloud_plot_", i)]] <- renderPlot({
          par(bg = "Navyblue", mar = rep(0, 4) + 1.2)
          wordcloud(words = cleaned_term_probabilities[[i]]$term, 
                    freq = cleaned_term_probabilities[[i]]$probability,
                    scale = c(1.2, .8), min.freq = 1e-20, max.words = 1000, random.order = FALSE,
                    rot.per = 0.35, colors = text_colors,
                    main = paste("Topic", i),
                    random.color = FALSE)  # Set background color to black
          title(main = paste("Topic", i), col.main = "white", cex.main = 1.5)  # Adjust main title size
        })
      }
    })
    
    
  })
  
  # Clean up: Close the database connection when the session ends
  session$onSessionEnded(function() {
    dbDisconnect(con)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
