
# Install GitHub Version 
#remotes::install_github("business-science/modeltime.gluonts")


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
fetch_data <- function(con) {
  base_query <- "select Date, Close from  Ratios_Tech.Trading_Data where Ticker = 'AAPL'"
  data <- dbGetQuery(con, base_query)
  return(data)
}
company = 'Apple Inc'  
data <- fetch_data(con)

# Let's set Data columns to the right format class for the analysis
data$Date <- as.Date(data$Date)
data$Close <- as.double(data$Close)

# Let's visualize the serie
data %>%
  plot_time_series(Date, Close, .interactive = FALSE)

# Let's use time_series_split to set train/test
splits <- data %>%
  time_series_split(assess = "6 months", cumulative = TRUE)

# Let's visualize the train/test
splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(Date, Close, .interactive = FALSE)

    
#### Place to Modelling part

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

calibration_table %>%
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
forecast_results <- calibration_table %>%
  modeltime_refit(data) %>%
  modeltime_forecast(h = "6 months", actual_data = data)

# Define colors for annotations and legend background
legend_color <- "#FFFFFF"  # White
model_colors <- c("red", "green", "gold", "skyblue", "blue")

# Plot forecast with interactive mode enabled
plot <- forecast_results %>%
  plot_modeltime_forecast(  .data,
                            .conf_interval_show = TRUE,
                            .conf_interval_fill = "grey20",
                            .conf_interval_alpha = 0.3,
                            .smooth = FALSE,
                            .legend_show = TRUE,
                            .legend_max_width = 40,
                            .facet_ncol = 1,
                            .facet_nrow = 1,
                            .facet_scales = "free_y",
                            .title = paste("Forecast Plot for", company),
                            .x_lab = "",
                            .y_lab = "",
                            .color_lab = "Legend",
                            .interactive = TRUE,
                            .plotly_slider = FALSE,
                            .trelliscope = FALSE)

plot

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
    font = list(size = 14, color = model_colors[i], family = "Arial",bold = TRUE),
    #bgcolor = "rgba(255, 255, 255, 0.7)",
    bordercolor = "rgba(0, 0, 0, 0.5)",
    borderwidth = 1,
    borderpad = 5,
    borderdash = "solid"
  )
}

# Add the annotations to the plot
plot <- plot %>%
  layout(
    annotations = annotations,
    #legend = list(bgcolor = legend_color, x = 1.1, y = 0.5)  # Adjust the legend position as needed
  )

# Hide the legend
plot <- plot %>%
  layout(
    showlegend = FALSE
  )

# Show the plot
plot

