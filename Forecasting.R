# Source the Libraries.R file to load required libraries
source("Libraries_Forecasting.R")

# Read the configuration file
config <- yaml::read_yaml("config.yaml")

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
fetch_data <- function(con, ticker) {
  base_query <- paste0("select Date, Close from Ratios_Tech.Trading_Data where Ticker = '", ticker, "'")
  data <- dbGetQuery(con, base_query)
  data$Date <- as.Date(data$Date)
  data$Close <- as.double(data$Close)
  return(data)
}

# Define tickers
company_1 <- c('Apple Inc', 'Alphabet Inc', 'Amazon.com Inc.', 'Microsoft Corporation')
tickers_1 <- c('AAPL', 'GOOGL', 'AMZN', 'MSFT')
tickers <- c('AAPL')
company <- c('Apple Inc')
company[1]
# Set up the plotting environment
par(mfrow = c(2, 2))

# Initialize a list to store plots
plots <- list()

# Loop over each ticker
for (ticker in tickers) {
  company = company[i]
  # Fetch data for the current ticker
  data <- fetch_data(con, ticker)
  
  print(paste("Ticker:", ticker))
  print(head(data))
  
  # Let's visualize the series
  plot1 <- plot(data$Date, data$Close, type = "l", main = paste("Time Series -", ticker), xlab = "Date", ylab = "Close")
  
  # Let's use time_series_split to set train/test
  splits <- data %>%
    time_series_split(assess = "1 months", cumulative = TRUE)
  
  # Let's visualize the train/test
  plot2 <- splits %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(Date, Close, .interactive = FALSE)
  
  
  #### Place to Modeling part
  
  ### 1. Automatic Models
  
  ### Auto ARIMA
  
  model_fit_arima <- arima_reg() %>%
    set_engine("auto_arima") %>%
    fit(Close ~ Date, training(splits))
  
  model_fit_arima
  
  ### Prophet
  
  model_fit_prophet <- prophet_reg(seasonality_yearly = TRUE) %>%
    set_engine("prophet") %>%
    fit(Close ~ Date, training(splits))
  
  model_fit_prophet
  
  ### 2. Machine Learning Models
  
  ### Preprocessing Recipe
  recipe_spec <- recipe(Close ~ Date, training(splits)) %>%
    step_timeseries_signature(Date) %>%
    step_rm(contains("am.pm"), contains("hour"), contains("minute"),
            contains("second"), contains("xts")) %>%
    step_fourier(Date, period = 365, K = 5) %>%
    step_dummy(all_nominal())
  
  recipe_spec %>% prep() %>% juice()
  
  ### Elastic Net
  model_spec_glmnet <- linear_reg(penalty = 0.01, mixture = 0.5) %>%
    set_engine("glmnet")
  
  workflow_fit_glmnet <- workflow() %>%
    add_model(model_spec_glmnet) %>%
    add_recipe(recipe_spec %>% step_rm(Date)) %>%
    fit(training(splits))
  
  ### Random Forest
  
  model_spec_rf <- rand_forest(trees = 500, min_n = 50, mode = "regression") %>%
    set_engine("randomForest")
  
  workflow_fit_rf <- workflow() %>%
    add_model(model_spec_rf) %>%
    add_recipe(recipe_spec %>% step_rm(Date)) %>%
    fit(training(splits))
  
  ### 3. Hybrid ML Models
  ### Prophet Boost
  
  model_spec_prophet_boost <- prophet_boost(seasonality_yearly = TRUE) %>%
    set_engine("prophet_xgboost") 
  
  workflow_fit_prophet_boost <- workflow() %>%
    add_model(model_spec_prophet_boost) %>%
    add_recipe(recipe_spec) %>%
    fit(training(splits))
  
  workflow_fit_prophet_boost
  
  ### The Modeltime Workflow
  
  model_table <- modeltime_table(
    model_fit_arima, 
    model_fit_prophet,
    workflow_fit_glmnet,
    workflow_fit_rf,
    workflow_fit_prophet_boost
  ) 
  
  model_table
  
  ### Calibration
  
  calibration_table <- model_table %>%
    modeltime_calibrate(testing(splits))
  
  
  
  ### Forecast (Testing Set)
  
  plot3 <- calibration_table %>%
    modeltime_forecast(actual_data = data) %>%
    plot_modeltime_forecast(.interactive = FALSE)
  
  ### Accuracy (Testing Set)
  
  accuracy_data<-calibration_table %>%
    modeltime_accuracy() %>%
    table_modeltime_accuracy(.interactive = FALSE)
  
  
  accuracy_metric <- accuracy_data$`_data`
  accuracy_metric
  
  # Extract accuracy metrics
  mape <- accuracy_metric$mape
  mase <- accuracy_metric$mase
  rmse <- accuracy_metric$rmse
  
  str(accuracy_metric)
  
  ### Refit and Forecast Forward
  library(ggplot2)
  # Refit and forecast forward
  plot4 <- calibration_table %>%
    modeltime_refit(data) %>%
    modeltime_forecast(h = "3 months", actual_data = data) %>%
    plot_modeltime_forecast(
      .interactive = FALSE,
      .conf_interval_show = TRUE,
      .conf_interval_fill = "grey20",
      .conf_interval_alpha = 0.2,
      .smooth = FALSE,
      .legend_show = TRUE,
      .legend_max_width = 40,
      .facet_ncol = 1,
      .facet_nrow = 1,
      .facet_scales = "free_y",
      .title = paste("Forecast Plot for", company),
      .x_lab = "",
      .y_lab = "",
      .color_lab = "Legend"
    )
  
  # Initialize a list to store annotations
  annotations <- list()
  
  # Calculate the number of annotations
  num_annotations <- nrow(accuracy_metric)
  
  # Iterate through each model and add annotations
  for (i in 1:num_annotations) {
    label <- paste(
      "Algo: ", 
      accuracy_metric$.model_desc[i], 
      "\nMAPE:", 
      round(accuracy_metric$mape[i], 2), 
      "\nMASE:", 
      round(accuracy_metric$mase[i], 2), 
      "\nRMSE:", 
      round(accuracy_metric$rmse[i], 2)
    )
    
    # Add annotation to the list
    annotations[[i]] <- list(
      x = -.2 + 0.25 * i,  # Adjust the position as needed
      y = 1,  # Positioned at the top
      xref = "paper",
      yref = "paper",
      text = label,
      showarrow = FALSE,
      font = list(size = 14, color = model_colors[i], family = "Arial", bold = TRUE),
      #bgcolor = "rgba(255, 255, 255, 0.7)",
      bordercolor = "rgba(0, 0, 0, 0.5)",
      borderwidth = 1,
      borderpad = 5,
      borderdash = "solid"
    )
  }
  
  # Add the annotations to the plot
  plot4 <- plot4 + 
    annotate(
      "text",
      x = rep(0.5, num_annotations),
      y = rep(0.95, num_annotations),
      label = unlist(lapply(annotations, function(x) x$text)),
      fontface = "bold",
      color = unlist(lapply(annotations, function(x) x$font$color)),
      family = unlist(lapply(annotations, function(x) x$font$family))
    )
  
  # Hide the legend
  plot4 <- plot4 + theme(legend.position = "none")
  print(plot4)
  
  # Store each plot in the list
  plots[[ticker]] <- list(plot1, plot2, plot3, plot4)
}

# Print all plots
for (i in seq_along(tickers)) {
  for (j in seq_along(plots[[i]])) {
    print(plots[[i]][[j]])
  }
}

print(plots$MSFT)