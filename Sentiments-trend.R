# Load required libraries
require("rvest")
require("stringr")
require("RMySQL")
require("zoo")
require("tidyr")
require("dplyr")
require("tm")
require("topicmodels")
require("wordcloud")
require("ggplot2")
library(textstem)
library(yaml)
library(RMySQL)
library(tm)
library(topicmodels)
library(wordcloud)
library(dplyr)
library(tidyr)
library(tidytext)
library(ggridges)


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
tickers <- c('TSLA', 'AAPL', 'MSFT', 'AMZN', 'GOOGL')

# Constructing the query to fetch news data for all tickers
get_news_query <- paste("SELECT InsertionDate, Ticker, News1, News2, News3, News4, News5,News6,News7,News8,News9,News10 FROM Stocks_News_headlines WHERE Ticker IN ('", paste(tickers, collapse = "','"), "');", sep = "")

# Fetching data for all tickers
ticker_data <- dbGetQuery(con, get_news_query)

# Step 1: Combine news columns into a single text column
ticker_data$Combined_News <- apply(ticker_data[, 3:12], 1, paste, collapse = " ")

# Step 2: Read custom stop words from file
custom_stopwords <- readLines("custom_stopwords.txt")

# Step 3: Combine custom stop words with other stop words
custom_stopwords <- c(stopwords("en"), custom_stopwords)

# Step 4: Define a function to preprocess the text
preprocess_text <- function(text) {
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
  # Reconstruct the text
  cleaned_text <- paste(tokens, collapse = " ")
  return(cleaned_text)
}

# Apply preprocessing to the combined news text
ticker_data$Cleaned_News <- sapply(ticker_data$Combined_News, preprocess_text)

# Now, you can proceed with sentiment analysis and plotting

# Perform sentiment analysis using the 'afinn' lexicon
daily_sentiment_scores <- ticker_data %>%
  unnest_tokens(word, Cleaned_News) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(Ticker, InsertionDate) %>%
  summarise(Sentiment_Score = sum(value)) %>% 
  mutate(Normalized_Score = Sentiment_Score / max(abs(Sentiment_Score)))

##### Ley's plot lines 
# Convert InsertionDate to date format
daily_sentiment_scores$InsertionDate <- as.Date(daily_sentiment_scores$InsertionDate)

ggplot(daily_sentiment_scores, aes(x = InsertionDate, y = Normalized_Score, color = Ticker, group = Ticker)) +
  geom_line(size = 0.8, alpha = 0.5,aes(linetype = Ticker)) +  # Set line thickness and transparency
  labs(title = "Daily Sentiment Analysis for Tickers",
       x = "",
       y = "Normalized Sentiment Score") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "navyblue"),  # Set plot background color
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "white"),  # Set title color to white
    axis.title = element_text(size = 14, color = "white"),  # Set axis title color to white
    axis.text.x = element_text(angle = 90,color = "white", hjust = 1),  # Rotate x-axis labels
    axis.text.y = element_text(color = "white"),     legend.title = element_blank(),
    legend.position = c(0.97, 0.90),  # Position legend at top right
    legend.background = element_rect(color = "white", size = 0.2),  # Set legend border color and size
    legend.text = element_text(size = 5, color = "navyblue"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.key.size = unit(1, "lines")  # Increase legend key size
  ) +
  guides(color = guide_legend(override.aes = list(size = 1)))  # Set legend key size


# We can also visualize with a completely different graphic type : the Ridge Plot

# Ridge Plot with navy blue background
ggplot(daily_sentiment_scores, aes(x = Normalized_Score, y = Ticker, fill = Ticker)) +
  geom_density_ridges(alpha = 0.6, scale = 1.5, color = "white") +
  labs(title = "Sentiment Distribution",
       x = "",
       y = "") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "white"),  # Set title color to white
    axis.title = element_text(size = 14, color = "white"),  # Set axis title color to white
    axis.text.x = element_text(color = "white", hjust = 1),  # customize x axis
    axis.text.y = element_text(color = "white"),     
    plot.background = element_rect(fill = "navyblue"),  # Set plot background color to navy blue
    legend.position = "none"  # Remove legend
  ) +
  scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e"))  # Scientific colors


# Let's try a Plot the area chart also

ggplot(daily_sentiment_scores, aes(x = InsertionDate, y = Normalized_Score, fill = Ticker)) +
  geom_area(alpha = 0.7) +
  labs(title = "Daily Sentiment",
       x = "Date",
       y = "Normalized Sentiment Score") +
  scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e")) +  # Scientific colors
  theme_minimal()+
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "white"),  # Set title color to white
    axis.title.y = element_text(size = 14, color = "white"),  # Set axis title color to white
    axis.title.x = element_blank(),
    axis.text.x = element_text(color = "white", hjust = 1),  # customize x axis
    axis.text.y = element_text(color = "white"),     
    plot.background = element_rect(fill = "navyblue"),  # Set plot background color to navy blue
    legend.text = element_text(size = 10, color = "white"),
    legend.title = element_blank(),  # Remove legend title
    panel.grid = element_blank()  # Remove gridlines
  )