# Source the Libraries.R file to load required libraries
source("Libraries.R")

# Read the configuration file
config <- yaml::read_yaml("config.yaml")

# Access database credentials from the configuration
db_host <- config$database$host
db_name <- config$database$database
db_user <- config$database$user
db_password <- config$database$password


# Establish a connection to the MySQL database
con <- dbConnect(MySQL(), 
                 dbname = db_name,
                 host = db_host,
                 user = db_user,
                 password = db_password)

# List of tickers to process
get_tickers_query <- "SELECT DISTINCT Ticker FROM Ratios_Tech.Dimension_Company where Ticker in ('AAPL');"
tickers <- dbGetQuery(con, get_tickers_query)$Ticker

# Iterate through each ticker to collect the data
for (ticker in tickers) {
  cat("Querying for", ticker, "\n")
  get_news_query <- paste("SELECT InsertionDate, Ticker, News1, News2, News3, News4, News5 FROM Stocks_News_headlines WHERE Ticker = '", ticker, "';", sep = "")
  ticker_data <- dbGetQuery(con, get_news_query)
}

cat("Processing for", ticker, "\n")

# Combine news columns into a single text column
ticker_data$Combined_News <- apply(ticker_data[, 3:7], 1, paste, collapse = " ")

# Read custom stop words from file
custom_stopwords <- readLines("custom_stopwords.txt")

# Combine custom stop words with other stop words
custom_stopwords <- c(stopwords("en"), custom_stopwords)

# Define a function to preprocess the text
preprocess_text <- function(text) {
  # Remove text within brackets and the brackets themselves at the end of the text
  text <- gsub("\\[.*?\\]\\s*$", "", text)
  # Remove the news source in parentheses at the end of the text
  text <- gsub("\\([^()]*\\)\\s*$", "", text)
  # Remove text within parentheses and the parentheses themselves
  text <- gsub("\\(.*?\\)", "", text)
  # Remove non-alphanumeric characters
  #text <- gsub("[^[:alnum:]\\s]", "", text)
  # Remove specific characters like ', :.
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
  #bigrams <- sapply(2:length(bigrams), function(i) paste(bigrams[i - 1], bigrams[i], sep = " "))
  # Create bigrams
  bigrams <- sapply(2:length(tokens), function(i) {
    if (grepl("[[:alnum:]]", gsub("[[:punct:]]", "", tokens[i - 1])) && 
        grepl("[[:alnum:]]", gsub("[[:punct:]]", "", tokens[i]))) {
      paste(tokens[i - 1], tokens[i], sep = " ")
    } else {
      tokens[i]
    }
  })
  #####
  return(bigrams)
}

# Apply the preprocess_text function to each element of ticker_data$Combined_News
processed_texts <- lapply(ticker_data$Combined_News, preprocess_text)

# Assign the processed texts to ticker_data$Processed_Text
ticker_data$Processed_Text <- processed_texts

### Hey, have a look!
print(ticker_data$Processed_Text)

### Place to the LDA
# Create document-term matrix
dtm <- DocumentTermMatrix(Corpus(VectorSource(ticker_data$Processed_Text)))
# Create the Document-Term Matrix with the custom tokenizer
lda_model <- LDA(dtm, k = 4, control = list(seed = 1234))
# Get the posterior probabilities of terms
posterior_terms <- posterior(lda_model)$terms

print(posterior_terms) ### Now have a look at the posterior distribution


# Function to calculate term probabilities for each topic
calculate_term_probabilities <- function(top_terms, lda_model) {
  term_probabilities <- list()
  
  # Get the term-topic probabilities from the model
  term_topic_probs <- (posterior(lda_model)$terms)
  
  # Ensure the number of terms requested is not greater than the actual number of terms available
  num_terms <- min(ncol(top_terms), ncol(term_topic_probs))
  
  # Iterate through each topic
  for (i in 1:num_terms) {
    topic_terms <- top_terms[, i]
    topic_probs <- term_topic_probs[, i]
    
    # Normalize probabilities
    topic_probs <- topic_probs / sum(topic_probs)
    
    term_probabilities[[i]] <- data.frame(term = topic_terms, probability = topic_probs)
  }
  
  return(term_probabilities)
}

# Extract top terms for each topic
top_terms <- terms(lda_model, 1000)  # Adjust the number as needed

# Calculate term probabilities for each topic
term_probabilities <- calculate_term_probabilities(top_terms, lda_model)

# Print the term probabilities for each topic
for (i in 1:length(term_probabilities)) {
  cat("Topic", i, ":\n")
  print(term_probabilities[[i]])
}

cleaned_term_probabilities <- lapply(term_probabilities, function(topic) {
  topic$term <- gsub('[[:punct:]]', '', topic$term)
  topic$term <- gsub('"|\'', '', topic$term)
  return(topic)
})

################################################################### 
#### I am doing this because i am having some duplicates terms ####
###################################################################

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
print(cleaned_term_probabilities)

#####################################
####### Place to visualization ######
#####################################

# Define custom color palette for text
text_colors <- brewer.pal(8, "Dark2")

par(mfrow = c(2, 2), mar = c(0, 1, 2, 0) + 0.1,bg = "Navyblue")

# Plot word clouds for each topic
for (local_i in 1:length(cleaned_term_probabilities)) {
  # Create a word cloud plot for the current topic
  plot.new()
  words <- cleaned_term_probabilities[[local_i]]$term
  freq <- cleaned_term_probabilities[[local_i]]$probability
  
  # Convert data to a data frame
  wordcloud_data <- data.frame(word = words, freq = freq)
  
  # Check if data looks correct
  
  print(head(wordcloud_data,10))
  
  # Create word cloud
  wordcloud2(data=wordcloud_data, size = 0.8, minSize = 0, shuffle = FALSE,
               rotateRatio = 0.35, color = "random-dark", backgroundColor = "black",
               fontFamily = "Arial", shape = "diamond")
  # Add customizations to improve appearance
  title(paste("Topic", local_i), cex.main = 1.2,col.main = "white")  # Adjust main title size
}