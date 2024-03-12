# Source the Libraries.R file to load required libraries
source("Libraries.R")

# Read the configuration file
config <- yaml::read_yaml("config.yaml")

# Read custom stop words from file
custom_stopwords <- readLines("custom_stopwords.txt")

# Access database credentials from the configuration
db_host <- config$database$host
db_name <- config$database$database
db_user <- config$database$user
db_password <- config$database$password

con <- dbConnect(MySQL(), 
                 dbname = db_name,
                 host = db_host,
                 user = db_user,
                 password = db_password)

# Function to fetch data from the database
fetch_data <- function(tickers, sectors, industries, countries,date_range) {
  print("starting fetching")
  withSpinner({
    con <- dbConnect(MySQL(), 
                     dbname = db_name,
                     host = db_host,
                     user = db_user,
                     password = db_password)
    
    # Construct the base query
    base_query <- "SELECT nh.InsertionDate, nh.Ticker, nh.News1, nh.News2, nh.News3, nh.News4, nh.News5, nh.News6, nh.News7, nh.News8, nh.News9, nh.News10,
                   nh.News11, nh.News12, nh.News13, nh.News14, nh.News15, nh.News16, nh.News17, nh.News18, nh.News19, nh.News20,
                   nh.News21, nh.News22, nh.News23, nh.News24, nh.News25, nh.News26, nh.News27, nh.News28, nh.News29, nh.News30,
                   dc.Company, ds.Sector, di.Industry, dco.Country
                   FROM Ratios_Tech.Stocks_News_headlines nh
                   INNER JOIN Dimension_Company dc ON nh.Ticker = dc.Ticker
                   INNER JOIN Dimension_Sector ds ON nh.Ticker = ds.Ticker
                   INNER JOIN Dimension_Industry di ON dc.Ticker = di.Ticker
                   INNER JOIN Dimension_Country dco ON dc.Ticker = dco.Ticker"
    
    # Construct the WHERE clause based on selected inputs
    where_clause <- ""
    if (length(tickers) > 0) {
      tickers_str <- paste0("'", tickers, "'", collapse = ", ")
      where_clause <- paste(where_clause, "nh.Ticker IN (", tickers_str, ")", sep = " ")
    }
    if (length(sectors) > 0) {
      sectors_str <- paste0("'", sectors, "'", collapse = ", ")
      where_clause <- paste(where_clause, "AND ds.Sector IN (", sectors_str, ")", sep = " ")
    }
    if (length(industries) > 0) {
      industries_str <- paste0("'", industries, "'", collapse = ", ")
      where_clause <- paste(where_clause, "AND di.Industry IN (", industries_str, ")", sep = " ")
    }
    if (length(countries) > 0) {
      countries_str <- paste0("'", countries, "'", collapse = ", ")
      where_clause <- paste(where_clause, "AND dco.Country IN (", countries_str, ")", sep = " ")
    }
    
    date_range_clause <- paste("nh.InsertionDate BETWEEN '", format(date_range[1], "%Y-%m-%d"), "' AND '", format(date_range[2], "%Y-%m-%d"), "'", sep = "")
    print("printing dates length")
    print(date_range_clause)
    # Append the date range clause to the base query
    if (nchar(date_range_clause) > 0) {
      where_clause <- paste(where_clause, "AND", date_range_clause)
    }
    
    # Append the WHERE clause to the base query
    if (nchar(where_clause) > 0) {
      final_query <- paste(base_query, "WHERE", where_clause)
    } else {
      final_query <- base_query
    }

    
    print(final_query)  # Print the constructed query for debugging
    
    # Fetch data from the database
    ticker_data <- tryCatch({
      #print("data querying")
      dbGetQuery(con, final_query)
    }, error = function(e) {
      cat("Error in fetch_data:", conditionMessage(e), "\n")
      return(NULL)

    })
    #print("data querying done")
    #print(head(ticker_data,3))
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

# Function to fetch distinct values from the database for tickers, sectors, industries, and countries
fetch_distinct_values <- function(column_name, table_name) {
  query <- glue::glue("SELECT DISTINCT {column_name} FROM {table_name};")
  distinct_values <- dbGetQuery(con, query)
  return(distinct_values[[1]])
}

min_date <- Sys.Date() - 30  # Set min_date to one year ago
max_date <- Sys.Date()  # Set max_date to the current date

# Define UI
ui <- fluidPage(
  navbarPage(
    "Text Mining and Topic Modeling",
    tabPanel(
      "Text Analysis",
      sidebarLayout(
        sidebarPanel(
          # Input controls for tickers
          selectInput("ticker", "Select Ticker", choices = fetch_distinct_values("Ticker", "Dimension_Company"), multiple = TRUE),
          # Input controls for company, sector, industry, and country
          selectInput("company", "Select Company", choices = fetch_distinct_values("Company", "Dimension_Company"), multiple = TRUE),     
          selectInput("sector", "Select Sector", choices = fetch_distinct_values("Sector", "Dimension_Sector"), multiple = TRUE),
          selectInput("industry", "Select Industry", choices = fetch_distinct_values("Industry", "Dimension_Industry"), multiple = TRUE),
          selectInput("country", "Select Country", choices = fetch_distinct_values("Country", "Dimension_Country"), multiple = TRUE),
          # Date range selector
          dateRangeInput("date_range", "Select Date Range", start = min_date, end = max_date)
        ),
        mainPanel(
          # Output elements for visualizations
          uiOutput("wordcloud_plots"),
          textOutput("available_dates")
        )
      )
    ),
    tabPanel(
      "Average Sentiment",
      dataTableOutput("average_sentiment_table")
    )
  )
)


# Define server logic
server <- function(input, output, session) {
  
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
  min_date <- Sys.Date() - 30  # Set min_date to one year ago
  max_date <- Sys.Date()  # Set max_date to the current date
  
  # Reactive expression to fetch data based on selected tickers, sectors, industries, and countries
  ticker_data <- reactive({
    # Extract selected inputs
    selected_tickers <- input$ticker
    selected_sectors <- input$sector
    selected_industries <- input$industry
    selected_countries <- input$country
    date_range <- input$date_range 
    
    # Initialize placeholders for SQL query components
    tickers_str <- ""
    sectors_str <- ""
    industries_str <- ""
    countries_str <- ""
    date_range_str <- ""
    
    print(date_range)
    
    # Check if all selected inputs are empty
    if (all(sapply(list(selected_tickers, selected_sectors, selected_industries, selected_countries), function(x) all(is.na(x) | x == "")))) {
      # If all inputs are empty, set the defaults
      selected_tickers <- "AAPL"
      selected_sectors <- "Technology"
      selected_industries <- "Consumer Electronics"
      selected_countries <- "USA"
      date_range <- c(Sys.Date() - 30, Sys.Date())  # Default date range (last 30 days)
      print("yes for defaults")
    } else {
      # Convert lists to strings for SQL query
      tickers_str <- paste("'", selected_tickers, "'", collapse = ",")
      sectors_str <- paste("'", selected_sectors, "'", collapse = ",")
      industries_str <- paste("'", selected_industries, "'", collapse = ",")
      countries_str <- paste("'", selected_countries, "'", collapse = ",")
      date_range_str <- paste("DateInsert BETWEEN '", format(date_range[1], "%Y-%m-%d"), "' AND '", format(date_range[2], "%Y-%m-%d"), "'", sep = "")
    }
    
    # Call the fetch_data function with selected inputs
    fetch_data(selected_tickers, selected_sectors, selected_industries, selected_countries,date_range)
  })
  
  # Render available dates text
  output$available_dates <- renderText({
    # Extract selected date range
    start_date <- input$date_range[1]
    end_date <- input$date_range[2]
    
    # Format the date range as a string
    date_range_string <- paste("Selected Date Range:", format(start_date, "%Y-%m-%d"), "-", format(end_date, "%Y-%m-%d"))
    
    return(date_range_string)
  })
  
  # Update choices for selectInput elements
  observe({
    updateSelectInput(session, "company", choices = get_distinct_values("Company", "Dimension_Company"))
    updateSelectInput(session, "sector", choices = get_distinct_values("Sector", "Dimension_Sector"))
    updateSelectInput(session, "industry", choices = get_distinct_values("Industry", "Dimension_Industry"))
    updateSelectInput(session, "country", choices = get_distinct_values("Country", "Dimension_Country"))
    # Render the date range input
    updateDateRangeInput(session, "date_range", start = min_date, end = max_date)
    
  })
  
  # Perform text mining and topic modeling on fetched data
  observeEvent(ticker_data(), {
    ticker_data_value <- ticker_data()  # Access the value of the reactive expression
    #print("inspecting ticker_data_value")
    #print(str(ticker_data_value))
    #print(dim(ticker_data_value))
    
    # Combine news columns into a single text column
    ticker_data_value$Combined_News <- apply(ticker_data_value[, 3:26], 1, paste, collapse = " ")

    # Preprocess the text
    processed_texts <- lapply(ticker_data_value$Combined_News, preprocess_text)
    
    # Assign the processed texts to ticker_data$Processed_Text
    ticker_data_value$Processed_Text <- processed_texts
    
    print("Processed_Text")
    print(ticker_data_value$Processed_Text)
    
    # Create document-term matrix
    dtm <- DocumentTermMatrix(Corpus(VectorSource(ticker_data_value$Processed_Text)))
    
    # Create the Document-Term Matrix with the custom tokenizer
    lda_model <- LDA(dtm, k = 4, control = list(seed = 1234))
    print("lda done")
    
    # Get the posterior probabilities of terms
    posterior_terms <- posterior(lda_model)$terms
    print("length of terms")
    print(length(posterior_terms))
    
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
    
    # Print cleaned_term_probabilities
    
    text_colors <- brewer.pal(8, "Dark2")
    
    # Store cleaned term probabilities as a reactiveVal
    cleaned_term_probabilities_val <- reactiveVal(NULL)
    
    # When the data is available, preprocess it and store the result
    observe({
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
    
    observe({
      text_colors <- brewer.pal(8, "Dark2")
      
      cleaned_term_probabilities <- cleaned_term_probabilities_val()
      #print(length(cleaned_term_probabilities))
      
      # Reactive conductor to isolate each iteration
      lapply(seq_along(cleaned_term_probabilities), function(i) {
        local_i <- i  # Capture the current value of i
        
        output[[paste0("wordcloud_plot_", local_i)]] <- renderPlot({
          par(bg = "Navyblue", mar = rep(0, 4) + 1.2)
          suppressWarnings(
          wordcloud(words = cleaned_term_probabilities[[local_i]]$term, 
                    freq = cleaned_term_probabilities[[local_i]]$probability,
                    scale = c(0.8, .6), min.freq = 1e-10, max.words = 1000, random.order = FALSE,
                    rot.per = 0.35, colors = text_colors,
                    main = paste("Topic", local_i),
                    random.color = TRUE)
          )
          # Set background color to black
          title(main = paste("Topic", local_i), col.main = "white", cex.main = 1.5)  # Adjust main title size
        })
      })
    })

    # Function to preprocess and extract sentiment scores from text using AFINN lexicon
    preprocess_and_extract_sentiment <- function(text) {
      processed_text <- preprocess_text(text)
      #print(processed_text)
      
      # Tokenize the processed text
      tokens <- tibble(word = unlist(str_split(processed_text, "\\s+")))
      
      # Join tokens with sentiment scores from the AFINN lexicon
      sentiment_scores <- tokens %>%
        inner_join(get_sentiments("afinn"), by = c(word = "word")) %>%
        summarise(sentiment_score = sum(value, na.rm = TRUE)) %>%
        mutate(Normalized_Score = sentiment_score / max(abs(sentiment_score)))
      
      # Calculate the total sentiment score
      total_sentiment_score <- sentiment_scores$Normalized_Score
      
      return(total_sentiment_score)
    }
    
    # Iterate over each specified number of days to create combined news columns and perform sentiment analysis
    for (i in c(1, 3, 5, 7,14,21,30)) {
      # Create combined news column
      combined_column <- apply(ticker_data_value[, (2 + i):(11 + i)], 1, paste, collapse = " ")
      col_name <- paste0("Combined_News_", i)
      
      # Add the combined news column to the data frame
      ticker_data_value[[col_name]] <- combined_column
      
      # Perform sentiment analysis and store sentiment scores in a new column
      ticker_data_value[[paste0("Sentiment_Scores_", i)]] <- sapply(combined_column, preprocess_and_extract_sentiment)
    }
  
  # Function to calculate average sentiment for a given combination and number of days
  calculate_average_sentiment <- function(data, combination, days) {
    # Filter data for the last 'days' days
    filtered_data <- data[data$InsertionDate >= (Sys.Date() - days), ]
    
    # Ensure the combination column exists
    if (!(combination %in% colnames(filtered_data))) {
      return(NA)  # Return NA if the combination column doesn't exist
    }
    
    # Calculate average sentiment score for the given combination
    average_sentiment <- mean(filtered_data[[combination]],na.rm = TRUE)
    return(average_sentiment)
  }
  
  # Prepare combinations and number of days
  combinations <- c(1, 3, 5, 7,14,21,30)
  days <- c(1, 3, 5, 7,14,21,30)
  
  # Calculate average sentiment for each combination and number of days
  average_sentiments <- sapply(days, function(day) {
    sapply(combinations, function(combination) {
      calculate_average_sentiment(ticker_data_value, paste0("Sentiment_Scores_", combination), day)
      #print("Calculation done for average sentiments")
    })
  })
  
  # Define the table for displaying average sentiments
  output$average_sentiment_table <- renderDataTable({
    sentiment_data <- data.frame(
      #Days = days,
      t(average_sentiments)
    )
    # Round the values in the sentiment_data data frame
    sentiment_data <- round(sentiment_data, digits = 2)
    
    # Apply conditional formatting to the data frame

    # Define the DataTable with customized options and conditional formatting
    datatable(
      sentiment_data,
      rownames = c("Last day", "Last 3 days", "Last 5 days", "Last 7 days","Last 14 days","Last 21 days","Last 30 days"),  # Custom row names
      colnames = c("Last New", "Top 3 News", "Top 5 News", "Top 7 News","Top 14 News","Top 21 News","Top 30 News"),  # Custom column names
      options = list(
        callback = JS(
          "function(table) {",
          "  var cells = table.cells({'row': 'all', 'column': 'all'}).nodes();",
          "  cells.each(function(i, cell) {",
          "    var cellValue = parseFloat($(cell).text());",
          "    if (!isNaN(cellValue)) {",
          "      var className = cellValue > 0.1 ? 'green' : (cellValue < -0.1 ? 'red' : 'yellow');",
          "      $(cell).addClass(className);",
          "    }",
          "  });",
          "}"
        ),
        columnDefs = list(list(targets = "_all", className = "dt-center")),  # Center-align all columns
        rowCallback = JS(
          "function(row, data, index) {",
          "  $('td', row).each(function(colIndex) {",
          "    var cellValue = parseFloat($(this).text());",
          "    if (!isNaN(cellValue)) {",
          "      var className = cellValue > 0.1 ? 'green' : (cellValue < -0.1 ? 'red' : 'yellow');",
          "      $(this).addClass(className);",
          "    }",
          "  });",
          "}"
        )  # Apply conditional formatting to cells
      )
    )
    

    
    
  })
  
  # Define UI for the average sentiment table
  average_sentiment_table_ui <- dataTableOutput("average_sentiment_table")
  
  # Define UI for the separate page to display the average sentiment table
  average_sentiment_page <- tabPanel("Average Sentiment", average_sentiment_table_ui)
  
    observe({
    print("Inside observe")
    print(ticker_data_value)
    if (is.null(ticker_data_value)) {
      print("Ticker data is NULL")
    } else {
      print("Ticker data is NOT NULL")
      print(names(ticker_data_value))
    }
  })
  
  observe({
    print("Inside observe for average sentiment")
    print(average_sentiments)
  })
  
  # Print any errors to the console
  observe({
    if (!is.null(average_sentiments)) {
      print("Average sentiments:")
      print(average_sentiments)
    }
  })
  onStop(function() {
    dbDisconnect(con)
  })
  
  })
}



# Run the application
shinyApp(ui = ui, server = server)
