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
fetch_data <- function(tickers, companies, sectors, industries, countries, date_range) {
  #print("starting fetching")
  #print(tickers)
  #print(companies)
  #print(sectors)
  #print(industries)
  #print(countries)
  #print(date_range)
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
    where_conditions <- character()
    if (length(tickers) > 0) {
      tickers_str <- paste0("'", tickers, "'", collapse = ", ")
      where_conditions <- c(where_conditions, paste("nh.Ticker IN (", tickers_str, ")", sep = " "))
    }
    if (length(companies) > 0) {
      companies_str <- paste0("'", companies, "'", collapse = ", ")
      where_conditions <- c(where_conditions, paste("dc.Company IN (", companies_str, ")", sep = " "))
    }
    if (length(sectors) > 0) {
      sectors_str <- paste0("'", sectors, "'", collapse = ", ")
      where_conditions <- c(where_conditions, paste("ds.Sector IN (", sectors_str, ")", sep = " "))
    }
    if (length(industries) > 0) {
      industries_str <- paste0("'", industries, "'", collapse = ", ")
      where_conditions <- c(where_conditions, paste("di.Industry IN (", industries_str, ")", sep = " "))
    }
    if (length(countries) > 0) {
      countries_str <- paste0("'", countries, "'", collapse = ", ")
      where_conditions <- c(where_conditions, paste("dco.Country IN (", countries_str, ")", sep = " "))
    }
    
    date_range_clause <- paste("nh.InsertionDate BETWEEN '", format(date_range[1], "%Y-%m-%d"), "' AND '", format(date_range[2], "%Y-%m-%d"), "'", sep = "")
    
    # Append the date range clause to the base query
    if (nchar(date_range_clause) > 0 && length(where_conditions) > 0 ) {
      where_conditions <- paste(where_conditions,"AND",date_range_clause)
    }else if (nchar(date_range_clause) > 0) {
      where_conditions <- paste(where_conditions,date_range_clause)
    }
    
    # Concatenate the WHERE conditions with the AND operator
    where_clause <- if (length(where_conditions) > 0) {
      paste("WHERE", paste(where_conditions, collapse = " AND "))
    } else {
      ""
    }
    
    
    # Construct the final query by appending the WHERE clause
    final_query <- paste(base_query, where_clause)
    
    print(final_query)  # Print the constructed query for debugging
    
    # Fetch data from the database
    ticker_data <- tryCatch({
      dbGetQuery(con, final_query)
    }, error = function(e) {
      cat("Error in fetch_data:", conditionMessage(e), "\n")
      return(NULL)
    })
    
    dbDisconnect(con)
    return(ticker_data)
  })
}



# Define a function to preprocess the text
preprocess_text <- function(text) {
  withSpinner({
    # Print original text
    print("Original Text:")
    print(text)
    
    # Remove text within brackets and the brackets themselves at the end of the text
    text <- gsub("\\[.*?\\]\\s*$", "", text)
    # Print text after removing text within brackets
    print("Text after removing text within brackets:")
    print(text)
    
    # Remove the news source in parentheses at the end of the text
    text <- gsub("\\([^()]*\\)\\s*$", "", text)
    # Print text after removing news source in parentheses
    print("Text after removing news source in parentheses:")
    print(text)
    
    # Remove text within parentheses and the parentheses themselves
    text <- gsub("\\(.*?\\)", "", text)
    # Print text after removing text within parentheses
    print("Text after removing text within parentheses:")
    print(text)
    
    # Remove non-alphanumeric characters and numbers
    text <- gsub("[^[:alnum:]0-9[:space:]]", "", text)
    # Print text after removing non-alphanumeric characters and numbers
    print("Text after removing non-alphanumeric characters and numbers:")
    print(text)
    
    # Convert text to UTF-8 encoding
    text <- iconv(text, to = "UTF-8")
    # Print text after converting to UTF-8 encoding
    print("Text after converting to UTF-8 encoding:")
    print(text)
    
    # Remove non-ASCII characters
    text <- gsub("[^[:alnum:][:space:]]", "", text)
    # Print text after removing non-ASCII characters
    print("Text after removing non-ASCII characters:")
    print(text)
    
    # Convert the text to lowercase
    text <- tolower(text)
    # Print text after converting to lowercase
    print("Text after converting to lowercase:")
    print(text)
    
    # Tokenize the text
    tokens <- unlist(strsplit(text, "\\s+"))
    # Print tokens
    print("Tokens:")
    print(tokens)
    
    # Remove stopwords
    tokens <- tokens[!tokens %in% stopwords("en")]
    # Print tokens after removing stopwords
    print("Tokens after removing stopwords:")
    print(tokens)
    
    # Remove custom stopwords
    tokens <- tokens[!tokens %in% custom_stopwords]
    # Print tokens after removing custom stopwords
    print("Tokens after removing custom stopwords:")
    print(tokens)
    
    # Remove short words (e.g., one or two characters)
    tokens <- tokens[nchar(tokens) > 2]
    # Print tokens after removing short words
    print("Tokens after removing short words:")
    print(tokens)
    
    # Lemmatize tokens
    tokens <- textstem::lemmatize_words(tokens)
    # Print lemmatized tokens
    print("Lemmatized Tokens:")
    print(tokens)
    
    # Remove unigrams
    tokens <- tokens[sapply(tokens, function(token) nchar(token) > 1)]
    # Print bigrams
    print("Bigrams:")
    print(tokens)
    
    # Concatenate tokens into bigrams
    bigrams <- sapply(2:length(tokens), function(i) {
      if (grepl("[[:alnum:]]", gsub("[[:punct:]]", "", tokens[i - 1])) && 
          grepl("[[:alnum:]]", gsub("[[:punct:]]", "", tokens[i]))) {
        paste(tokens[i - 1], tokens[i], sep = " ")
      } else {
        tokens[i]
      }
    })
    # Print bigrams
    print("Bigrams after concatenating tokens:")
    print(bigrams)
    
    return(bigrams)
  }, type = 3)  # Type 3: Spinner with text
}


min_date <- Sys.Date() - 30  # Set min_date to one year ago
max_date <- Sys.Date()  # Set max_date to the current date

# Define UI

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML(
        "
        body {
          background-color: #f4f4f9;
          font-family: 'Arial', sans-serif;
        }
        .navbar {
          background-color: #343a40;
        }
        .navbar .navbar-brand, .navbar-nav .nav-link {
          color: #ffffff !important;
        }
        .explanation {
          background-color: #ffffff; /* White background */
          padding: 20px;
          border-radius: 8px;
          margin-bottom: 20px;
          box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
          font-size: 18px; /* Adjust the text size as needed */
          color: #343a40;
        }
        .wordcloud-title {
          font-size: 24px;
          font-weight: bold;
          padding: 10px;
          border-radius: 8px;
          margin-bottom: 10px;
          text-align: center;
          box-shadow: 2px 2px 5px rgba(0, 0, 0, 0.1);
        }
        .positive {
          background-color: #d4edda;
          color: #155724;
        }
        .negative {
          background-color: #f8d7da;
          color: #721c24;
        }
        .neutral {
          background-color: #fff3cd;
          color: #856404;
        }
        .btn-custom {
          background-color: #007bff;
          color: #ffffff;
          border: none;
          padding: 10px 20px;
          font-size: 14px;
          border-radius: 4px;
          margin-top: 10px;
        }
        .btn-custom:hover {
          background-color: #0056b3;
        }
        .sidebar {
          background-color: #ffffff;
          padding: 20px;
          font-size: 16px;
          border-radius: 8px;
          font-weight: bold;
          box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
        }
        .main-panel {
          padding: 20px;
          background-color: #ffffff;
          border-radius: 8px;
          box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
        }
        .plot-container {
          margin-top: 20px;
          height: 600px; /* Adjust the height as needed */
        }
        .wordcloud-container {
          margin: 20px 0;
          padding: 10px;
          border: 1px solid #ccc;
          border-radius: 5px;
          background-color: #f0f0f0;
        }
        "
      )
    )
  ),
  
  navbarPage(
    tags$h1("Stocks Text Analytics", style = "font-weight: bold; font-size: 24px; color: #ffffff;"),
    tabPanel(
      tags$h2("Topic Modelling and Sentiment Analysis", style = "font-size: 20px;"),
      sidebarLayout(
        sidebarPanel(
          class = "sidebar",
          uiOutput("ticker_select"),
          uiOutput("company_select"),
          uiOutput("sector_select"),
          uiOutput("industry_select"),
          uiOutput("country_select"),
          # Date range selector
          dateRangeInput("date_range", "Select Date Range", start = min_date, end = max_date),
          fluidRow(
            column(4, 
                   actionButton("reset_button", "Reset Selections", class = "btn-custom")
            ),
            column(4, 
                   actionButton("apply_button", "Apply Selections", class = "btn-custom")
            ),
            column(4, 
                   actionButton("refresh_data", "Refresh Data", class = "btn-custom")
            )
          ),
          fluidRow(
            column(12,
                   div(class = "plot-container", 
                       plotOutput("news_sources_plot",height = "600px")
                   )
            )
          )
        ),
        mainPanel(
          class = "main-panel",
          # Explanation paragraph for topic modelling
          div(class = "explanation",
              p(HTML("<span style='font-size: 28px;'>☕️</span> <span style='font-size: 18px; color: #0056b3;'>Dive into the intricacies of text analysis with <span style='color: #ff6347;'>Topic Modeling</span> and <span style='color: #32cd32;'>Sentiment Analysis</span> - exploring the dynamic interplay between topics and sentiments in news, articles, and more.</span>"))
          ),
          # Output elements for visualizations
          uiOutput("wordcloud_plots")
        )
      )
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
  
  min_date <- Sys.Date() - 30  # Set min_date to one year ago
  max_date <- Sys.Date()  # Set max_date to the current date
  
  # Define reactive values to store selected values
  selected_values <- reactiveValues(
    ticker = NULL,
    company = NULL,
    sector = NULL,
    industry = NULL,
    country = NULL
  )
  
  # Define reactiveValues for storing previous selections
  previous_selection <- reactiveValues(
    ticker = NULL,
    company = NULL,
    sector = NULL,
    industry = NULL,
    country = NULL
  )
  
  # Function to fetch distinct values from the database
  get_distinct_values <- function(column_name, YourTable, selected_values) {
    # Initialize an empty vector to store conditions
    conditions <- c()
    
    # Add conditions based on selected values
    if (!is.null(selected_values$ticker) && !("None" %in% selected_values$ticker)) {
      conditions <- c(conditions, paste0("Dimension_Company.Ticker IN ('", paste(selected_values$ticker, collapse = "','"), "')"))
    }
    if (!is.null(selected_values$company) && !("None" %in% selected_values$company)) {
      conditions <- c(conditions, paste0("Dimension_Company.company IN ('", paste(selected_values$company, collapse = "','"), "')"))
    }
    if (!is.null(selected_values$sector) && !("None" %in% selected_values$sector)) {
      conditions <- c(conditions, paste0("Dimension_Sector.Sector IN ('", paste(selected_values$sector, collapse = "','"), "')"))
    }
    if (!is.null(selected_values$industry) && !("None" %in% selected_values$industry)) {
      conditions <- c(conditions, paste0("Dimension_Industry.Industry IN ('", paste(selected_values$industry, collapse = "','"), "')"))
    }
    if (!is.null(selected_values$country) && !("None" %in% selected_values$country)) {
      conditions <- c(conditions, paste0("Dimension_Country.Country IN ('", paste(selected_values$country, collapse = "','"), "')"))
    }
    
    # Combine conditions into a single string
    additional_conditions <- paste(conditions, collapse = " AND ")
    
    # Construct the SQL query with a JOIN statement
    query <- glue::glue("
      SELECT DISTINCT a.{column_name} 
      FROM {YourTable} a
      JOIN Dimension_Company ON a.Ticker = Dimension_Company.Ticker 
      JOIN Dimension_Sector ON a.Ticker  = Dimension_Sector.Ticker 
      JOIN Dimension_Industry ON a.Ticker  = Dimension_Industry.Ticker 
      JOIN Dimension_Country ON a.Ticker  = Dimension_Country.Ticker
  ")
    
    # Append the WHERE clause if conditions are present
    if (nchar(additional_conditions) > 0) {
      query <- glue::glue("{query} WHERE {additional_conditions}")
    }
    print(query)
    
    # Execute the query with selected values as additional conditions
    distinct_values <- dbGetQuery(con, query)
    
    # Return the distinct values
    return(distinct_values[[1]])
  }
  
  
  # Function to render select inputs
  renderSelectInput <- function(name, label, column_name, YourTable, selected_values) {
    output_id <- paste0(name, "_select")
    output[[output_id]] <- renderUI({
      selectInput(name, label, choices = get_distinct_values(column_name, YourTable, selected_values), multiple = TRUE)
    })
  }
  
  # Define inputs for topic modeling
  inputs_topic_modeling <- list(
    list(name = "ticker", label = "Select Ticker", column_name = "Ticker", YourTable = "Dimension_Company"),
    list(name = "company", label = "Select Company", column_name = "Company", YourTable = "Dimension_Company"),
    list(name = "sector", label = "Select Sector", column_name = "Sector", YourTable = "Dimension_Sector"),
    list(name = "industry", label = "Select Industry", column_name = "Industry", YourTable = "Dimension_Industry"),
    list(name = "country", label = "Select Country", column_name = "Country", YourTable = "Dimension_Country")
  )
  
  # Observer to initialize UI inputs to default/reset state
  observe({
    # Set default values for UI inputs
    default_selection <- list(
      ticker = NULL,
      company = NULL,
      sector = NULL,
      industry = NULL,
      country = NULL
    )
    
    # Render select input elements with default values
    lapply(inputs_topic_modeling, function(input_def) {
      renderSelectInput(input_def$name, input_def$label, input_def$column_name, input_def$YourTable, selected_values = default_selection)
    })
    
    # Reset date range input to default values
    updateDateRangeInput(session, "date_range", start = min_date, end = max_date)
  })
  
  
  # Observer to update UI elements based on selected inputs
  observeEvent(input$apply_button, {
    # Get the selected values for all inputs
    selected <- list(
      ticker = input$ticker,
      company = input$company,
      sector = input$sector,
      industry = input$industry,
      country = input$country
    )
    
    # Update selected_values reactiveValues with current selections
    previous_selection$ticker <- c(previous_selection$ticker, selected$ticker)
    previous_selection$company <- c(previous_selection$company, selected$company)
    previous_selection$sector <- c(previous_selection$sector, selected$sector)
    previous_selection$industry <- c(previous_selection$industry, selected$industry)
    previous_selection$country <- c(previous_selection$country, selected$country)
    
    # Update UI for topic modeling and sentiment analysis inputs
    lapply(inputs_topic_modeling, function(input_def) {
      updateSelectInput(session, input_def$name, choices = c("None", get_distinct_values(input_def$column_name, input_def$YourTable, previous_selection)))
    })
    
    updateDateRangeInput(session, "date_range", start = min_date, end = max_date)
  })
  
  
  # Observer to reset inputs when the reset button is clicked
  observeEvent(input$reset_button, {
    
    # Reset selected_values to NULL
    previous_selection$ticker <- NULL
    previous_selection$company <- NULL
    previous_selection$sector <- NULL
    previous_selection$industry <- NULL
    previous_selection$country <- NULL
    
    # Update UI for topic modeling and sentiment analysis inputs
    lapply(inputs_topic_modeling, function(input_def) {
      updateSelectInput(session, input_def$name, choices = c("None", get_distinct_values(input_def$column_name, input_def$YourTable, previous_selection)))
    })
    
    # Reset date range input
    updateDateRangeInput(session, "date_range", start = min_date, end = max_date)
  })
  
  
  output$news_sources_plot <- renderPlot({
    news_sources <- c("DigiTimes", "GlobeNewswire", "WWD", "American Banker", "Forkast News", "MarketWatch", "Observer", "The Economist", "PR Newswire", "WSJ", "GuruFocus.com", "Nation's Restaurant News", "Investing.com", "Business Wire", "Entrepreneur: Stocks", "Chain Store Age", "TipRanks", "Zacks", "Associated Press Finance", "Fortune", "South China Morning Post", "The Telegraph", "Moneywise", "InvestorPlace", "Financial Times", "Benzinga", "Fox Business", "Above Avalon", "Quartz", "TechCrunch", "TheStreet.com", "CNN Business", "Investopedia", "The Wall Street Journal", "Reuters", "CNBC TV", "Barrons.com", "Bloomberg", "Yahoo Finance", "Yahoo Finance Video", "Insider Monkey", "AppleInsider", "Investor's Business Daily", "Motley Fool")
    frequency <- c("0.1%", "0.1%", "0.1%", "0.1%", "0.1%", "0.1%", "0.1%", "0.1%", "0.2%", "0.2%", "0.2%", "0.2%", "0.4%", "0.4%", "0.5%", "0.6%", "0.6%", "0.6%", "0.6%", "0.6%", "0.6%", "0.7%", "0.9%", "0.9%", "1.1%", "1.2%", "1.2%", "1.4%", "1.4%", "1.4%", "1.9%", "2.3%", "2.8%", "4.2%", "4.4%", "4.4%", "5.2%", "5.7%", "6.0%", "6.0%", "6.7%", "8.9%", "9.9%", "14.9%")
    
    # Clean the frequency data and convert to numeric
    frequency_numeric <- as.numeric(sub("%", "", frequency)) / 100
    
    # Create a data frame
    data <- data.frame(news_sources, frequency_numeric)
    
    # Sort the data by frequency
    data <- data[order(data$frequency_numeric, decreasing = TRUE), ]
    
    # Plot the horizontal bar chart with a black background and enhanced aesthetics
    library(ggplot2)
    ggplot(data, aes(x = reorder(news_sources, frequency_numeric), y = frequency_numeric)) +
      geom_bar(stat = "identity", aes(fill = frequency_numeric), show.legend = FALSE) +
      scale_fill_gradient(low = "#56B4E9", high = "#E69F00") + # Gradient fill from blue to orange
      labs(title = "Top News Sources by Frequency", 
           subtitle = "Data Source: Various News Outlets", # Adding a subtitle
           x = "", y = "Frequency") +
      theme_minimal(base_size = 15) + # Minimal theme with increased base font size
      theme(
        plot.background = element_rect(fill = "#333333", color = NA),
        panel.background = element_rect(fill = "#333333", color = NA),
        axis.text.y = element_text(color = "white", size = 10),
        axis.title.y = element_blank(),
        axis.text.x = element_text(color = "white", size = 12),
        axis.title.x = element_blank(),
        panel.grid.major = element_line(color = "#666666"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(color = "white", size = 20, hjust = 0.5),
        plot.subtitle = element_text(color = "white", size = 16, hjust = 0.5),
        axis.line = element_line(color = "white")
      ) +
      coord_flip()
  })
  
  
  # Reactive expression to fetch data based on selected tickers, sectors, industries, and countries
  ticker_data <- eventReactive(input$refresh_data, {
    # Call the fetch_data function with selected inputs
    fetch_data(previous_selection$ticker, previous_selection$company, previous_selection$sector, previous_selection$industry, previous_selection$country, input$date_range)
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
    #print(ticker_data_value$Processed_Text)
    
    # Create document-term matrix
    dtm <- DocumentTermMatrix(Corpus(VectorSource(ticker_data_value$Processed_Text)))
    
    # Create the Document-Term Matrix with the custom tokenizer
    lda_model <- LDA(dtm, k = 4, control = list(seed = 1234))
    print("lda done")
    
    # Get the posterior probabilities of terms
    posterior_terms <- posterior(lda_model)$terms
    print("length of terms OK")
    #print(length(posterior_terms))
    
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
    top_terms <- terms(lda_model, 500)
    
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
    
    # Observe the cleaned term probabilities
    observe({
      cleaned_term_probabilities <- cleaned_term_probabilities_val()
      
      # Render the UI for wordcloud plots dynamically in a 2x2 grid
      output$wordcloud_plots <- renderUI({
        tagList(
          fluidRow(
            column(6, div(class = "wordcloud-container",
              h4(uiOutput("wordcloud_title_1")),
              wordcloud2Output(outputId = "wordcloud_plot_1")
            )),
            column(6, div(class = "wordcloud-container",
              h4(uiOutput("wordcloud_title_2")),
              wordcloud2Output(outputId = "wordcloud_plot_2")
            ))
          ),
          fluidRow(
            column(6, div(class = "wordcloud-container",
              h4(uiOutput("wordcloud_title_3")),
              wordcloud2Output(outputId = "wordcloud_plot_3")
            )),
            column(6, div(class = "wordcloud-container",
              h4(uiOutput("wordcloud_title_4")),
              wordcloud2Output(outputId = "wordcloud_plot_4")
            ))
          )
        )
      })
    })
    
    observe({
      cleaned_term_probabilities <- cleaned_term_probabilities_val()
      
      lapply(seq_along(cleaned_term_probabilities), function(i) {
        local_i <- i  # Capture the current value of i
        
        # Calculate the sentiment score for each term in the current topic
        term_sentiments <- inner_join(cleaned_term_probabilities[[local_i]], get_sentiments("afinn"), by = c(term = "word"))
        
        # Calculate the weighted average sentiment score for the topic
        average_sentiment <- sum(term_sentiments$probability * term_sentiments$value) / sum(term_sentiments$probability)
        
        # Function to categorize sentiment score
        categorize_sentiment <- function(score) {
          if (score > 0.1) {
            return("Positive")
          } else if (score < -0.1) {
            return("Negative")
          } else {
            return("Neutral")
          }
        }
        
        # Determine sentiment category
        sentiment_category <- categorize_sentiment(average_sentiment)
        

        # Update the title with the calculated sentiment and format with colors
        output[[paste0("wordcloud_title_", local_i)]] <- renderUI({
          sentiment_class <- switch(sentiment_category,
                                    "Positive" = "positive",
                                    "Negative" = "negative",
                                    "Neutral" = "neutral")
          div(class = paste("wordcloud-title", sentiment_class), 
              paste("Topic ", local_i, " - Sentiment: ", sentiment_category))
        })
        
        # Plot wordcloud with sentiment information
        output[[paste0("wordcloud_plot_", local_i)]] <- renderWordcloud2({
          # Prepare data for word cloud
          words <- cleaned_term_probabilities[[local_i]]$term
          freq <- cleaned_term_probabilities[[local_i]]$probability
          
          # Convert data to a data frame
          wordcloud_data <- data.frame(word = words, freq = freq)
          
          # Aggregate frequencies
          wordcloud_data <- aggregate(freq ~ word, data = wordcloud_data, sum)
          
          # Order the freq column
          wordcloud_data <- wordcloud_data[order(wordcloud_data$freq, decreasing = TRUE), ]
          
          # Scale frequencies to integers (adjust scale as needed)
          wordcloud_data$freq <- as.integer(wordcloud_data$freq * 100)
          
          # Remove rows with missing values
          wordcloud_data <- na.omit(wordcloud_data)
          
          shape <- switch(sentiment_category,
                          "Positive" = 'Smiley face',
                          "Negative" = 'Lightning bolt',
                          "Neutral" = 'Cloud')
          
          # Create word cloud
          #wordcloud2(data = wordcloud_data, size = 1, figPath = shape_image, color = "random-light", backgroundColor = "white")
          wordcloud2(data = wordcloud_data, size = 0.13,minSize = 0, gridSize = 2, shape = shape, rotateRatio = 0.35,ellipticity = 0.7)
        })
      })
    })
    
  onStop(function() {
    dbDisconnect(con)
  })
  
  })
}



# Run the application
shinyApp(ui = ui, server = server)
