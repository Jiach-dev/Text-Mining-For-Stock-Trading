# Stock Text Mining and Sentiment Analysis
#Overview

This project focuses on mining and analyzing text data from various news sources to understand sentiment trends and topics related to different companies and stocks. The application leverages a range of technologies and libraries to extract, process, and visualize the data.

**watch the app demo here :** https://www.youtube.com/watch?v=N7wPNshQbUA

# Features
  **1. Text Mining:** Extracts text data from news articles.
  
  **2. Topic Modeling:** Uses Latent Dirichlet Allocation (LDA) to identify topics within the text data.
  
  **3. Sentiment Analysis:** Analyzes the sentiment of the extracted text from each topic to determine positive, negative, or neutral sentiments.

  **4. Interactive Visualization:** Provides interactive visualizations using Shiny for better insights.
  
# Key Technologies Used

* R: The core programming language used for data analysis and text mining.
* Shiny: A framework for building interactive web applications with R.
* MySQL: A relational database used for storing and querying data.
* ggplot2: A powerful library for creating visualizations in R.
* tidyverse: A collection of R packages designed for data science.
* LDA: Latent Dirichlet Allocation, a topic modeling technique used for discovering topics in text data.
* wordcloud2: An R library for creating word clouds, used to visualize the most frequent terms in each topic.
* Sentiment Analysis Libraries: Used to determine the sentiment of text data.


# Getting Started
## Prerequisites

* R: Make sure you have R installed. You can download it from CRAN.
* RStudio: An integrated development environment for R.
* MySQL: Ensure you have MySQL installed and a database set up.

# Installation
## 1. Clone the repository:
```
git clone https://github.com/Profesor-JH/Text-Mining-For-Stock-Trading.git
cd Stock-Textmining-R
```

## 2. Install required R packages:
you have to install all the packages in the Libraries.R
```
install.packages(c("shiny", "DBI", "RMySQL", "ggplot2", "tidyverse", "topicmodels", "wordcloud2", "sentimentr"))
```

## 3. Configure the database:

* Update the config.yaml file with your MySQL database credentials.

# Running the Application
## 1. Launch the Shiny app:

```
library(shiny)
runApp("Shiny_app.R")
```
## 2. Access the application:

* Open your web browser and go to http://localhost:3838.

# Usage

* Select Tickers, Companies, Sectors, Industries, and Countries: Use the dropdowns to filter the data.

* Date Range Selection: Adjust the date range to refine the analysis period.

* Apply and Reset Filters: Use the apply button to fetch data based on selected filters and the reset button to clear selections.

* View Visualizations: Interact with various plots to gain insights into the topics and sentiments.

# Contribution
Contributions are welcome! Please fork the repository and create a pull request with your changes.

# License
This project is licensed under the MIT License.

# Contact
For questions or suggestions, please contact:

Jean Henock: [henocksjean@gmail.com]



