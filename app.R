library(readr)    
library(caret)  
library(shiny)
library(ggplot2)
library(dplyr)
library(randomForest)
library(rsconnect) 

# Read  dataset
co2_data <- read_csv("co2.csv")

# Rename columns without units or special characters
co2_data <- co2_data %>% rename('Engine.Size' = `Engine Size(L)`)
co2_data <- co2_data %>% rename('Vehicle.Class' = `Vehicle Class`)
co2_data <- co2_data %>% rename('Fuel.Type' = `Fuel Type`)
co2_data <- co2_data %>% rename('Fuel.Consumption.City' = `Fuel Consumption City (L/100 km)`)
co2_data <- co2_data %>% rename('Fuel.Consumption.Hwy' = `Fuel Consumption Hwy (L/100 km)`)
co2_data <- co2_data %>% rename('Fuel.Consumption.Comb.L_per_100_km' = `Fuel Consumption Comb (L/100 km)`)
co2_data <- co2_data %>% rename('Fuel.Consumption.Comb.mpg' = `Fuel Consumption Comb (mpg)`)
co2_data <- co2_data %>% rename('CO2.Emissions' = `CO2 Emissions(g/km)`) %>% mutate(CO2.Emissions = CO2.Emissions * 1.60934)

# Create train-test split
set.seed(12)
train_indices <- createDataPartition(co2_data[["CO2.Emissions"]], p = 0.8, list = FALSE)
train_data <- co2_data[train_indices, ]
test_data <- co2_data[-train_indices, ]

target_variable <- "CO2.Emissions"
numeric_features <- c("Engine.Size", "Cylinders", "Fuel.Consumption.Comb.mpg")
categorical_features <- c("Vehicle.Class", "Fuel.Type")

# Show interactions between numerical features
pairs(co2_data[c("Engine.Size", "Cylinders", "Fuel.Consumption.Comb.mpg", "CO2.Emissions")], pch = 18, col = "steelblue")

# REGRESSION MODEL
formula <- as.formula("`CO2.Emissions` ~ .")
model <- lm(formula, data = train_data[, c("CO2.Emissions", "Engine.Size", "Cylinders", "Fuel.Consumption.Comb.mpg", "Make", "Vehicle.Class", "Fuel.Type")])

# Performance metrics
train_predictions <- predict(model, newdata = train_data)
test_predictions <- predict(model, newdata = test_data)

train_metrics <- postResample(train_predictions, train_data[[target_variable]])
test_metrics <- postResample(test_predictions, test_data[[target_variable]])

cat("Training RMSE: ", train_metrics["RMSE"], "\n")
cat("Training R-squared: ", train_metrics["Rsquared"], "\n")
cat("Testing RMSE: ", test_metrics["RMSE"], "\n")
cat("Testing R-squared: ", test_metrics["Rsquared"], "\n")

train_residuals <- train_data[[target_variable]] - train_predictions
test_residuals <- test_data[[target_variable]] - test_predictions

train_squared_errors <- train_residuals^2
test_squared_errors <- test_residuals^2

train_median_squared_error <- median(train_squared_errors)
test_median_squared_error <- median(test_squared_errors)

cat("Training Median Squared Error: ", train_median_squared_error, "\n")
cat("Testing Median Squared Error: ", test_median_squared_error, "\n")

cat("\nModel Summary:\n")
print(summary(model))

# Scatter plot of actual vs predicted for test data
test_results <- data.frame(Actual = test_data[[target_variable]],Predicted = test_predictions)
ggplot(test_results, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(
    title = "Actual vs Predicted CO2 Emissions",
    x = "Actual CO2 Emissions (g/mi)",
    y = "Predicted CO2 Emissions (g/mi)"
  ) +
  theme_minimal()



# RANDOM FOREST MODEL (Not actually used in app)
#rf_model <- randomForest(formula, data = train_data[, c(target_variable, numeric_features, categorical_features)],
#                         ntree = 50,   
#                         mtry = 5,      
#                         nodesize = 5,  
#                         maxnodes = 20)
# Performance metrics
#train_predictions_rf <- predict(rf_model, newdata = train_data)
#test_predictions_rf <- predict(rf_model, newdata = test_data)
#train_metrics_rf <- postResample(train_predictions_rf, train_data[[target_variable]])
#test_metrics_rf <- postResample(test_predictions_rf, test_data[[target_variable]])
#cat("\nRandom Forest Metrics:\n")
#cat("Training MSE: ", train_metrics_rf["RMSE"]^2, "\n")
#cat("Training R-squared: ", train_metrics_rf["Rsquared"], "\n")
#cat("Testing MSE: ", test_metrics_rf["RMSE"]^2, "\n")
#cat("Testing R-squared: ", test_metrics_rf["Rsquared"], "\n")


# Create shiny app
ui <- fluidPage(
  tags$style(HTML("
    .panel {
      border: 2px solid black;
      padding: 10px;
      margin-bottom: 20px;
    }
    .prediction-box {
      border: 2px solid black;
      padding: 15px;
      background-color: #f9f9f9;
      font-size: 18px;
      text-align: center;
      margin-top: 10px;
    }
    .irs-bar {
      background: lightgreen !important;
    }
    .irs-line {
      background: #d9f2d9 !important;
      border-color: lightgreen !important;
    }
    .irs-bar-edge {
      background: lightgreen !important;
      border-color: lightgreen !important;
    }
    .irs-slider {
      background: lightgreen !important;
    }
    .irs-single {
      background: lightgreen !important;
      color: white !important;
    }
    #sidebar {
      height: 600px; /* Set a fixed height for the sidebar */
    }
    #main {
      height: 600px; /* Match the sidebar height */
      overflow-y: auto; /* Add a scroll bar for content that overflows */
    }
  ")),
  
  titlePanel("CO2 Emissions Predictor"),
  
  sidebarLayout(
    sidebarPanel(
      div(class = "panel", 
          sliderInput("engine_size", "Engine Size (L):", min = 0.5, max = 20.0, value = 2.0, step = 0.1),
          selectInput("cylinders", "Cylinders:", choices = c(2, 3, 4, 6, 8, 10, 12, 16, 18), selected = 4),
          sliderInput("fuel_comb_mpg", "Fuel Consumption (mpg):", min = 5, max = 50, value = 35, step = 1),
          selectInput("make", "Vehicle Make:", choices = unique(co2_data[["Make"]])),
          selectInput("vehicle_class", "Vehicle Class:", choices = unique(co2_data[["Vehicle.Class"]]), selected = "SUV - STANDARD"),
          selectInput("fuel_type", "Fuel Type:", choices = unique(co2_data[["Fuel.Type"]]), selected = "X"),
          actionButton("predict", "Predict CO2 Emissions")
      ),
      div(class = "prediction-box", textOutput("prediction_output"))
    ),
    
    mainPanel(
      class = "main",
      div(class = "panel",
          h3("Average CO2 Emissions by Vehicle Make"),
          plotOutput("average_emissions_plot", height = "300px"),
          sliderInput("top_k", "Number of Top Makes to Display:", min = 1, max = 20, value = 10, step = 1)
      ),
      div(class = "panel",
          h3("City vs. Highway Fuel Consumption"),
          plotOutput("fuel_consumption_scatter", height = "300px")
      )
    )
  )
)
  

server <- function(input, output, session) {
  prediction_text <- reactiveVal("Fill out fields and press 'Predict CO2 Emissions' to see the value")
  observeEvent(input$predict, {
    new_data <- data.frame(
      `Engine.Size` = input$engine_size,
      Cylinders = as.numeric(input$cylinders),
      `Fuel.Consumption.Comb.mpg` = input$fuel_comb_mpg,
      `Make` = input$make,
      `Vehicle.Class` = input$vehicle_class,
      `Fuel.Type` = input$fuel_type,
      stringsAsFactors = FALSE
    )
    prediction <- predict(model, newdata = new_data)
    prediction_text(paste("Predicted CO2 Emissions (g/mi):", round(prediction, 2)))
  })
  
  output$prediction_output <- renderText({
    prediction_text()
  })
  
  # Plot average emissions per vehicle make
  output$average_emissions_plot <- renderPlot({
    avg_emissions <- co2_data %>%
      group_by(Make) %>%
      summarise(Average_CO2 = mean(CO2.Emissions, na.rm = TRUE)) %>%
      arrange(desc(Average_CO2))
    
    top_makes <- avg_emissions %>%
      slice_head(n = input$top_k)
    
    ggplot(top_makes, aes(x = reorder(Make, -Average_CO2), y = Average_CO2)) +
      geom_bar(stat = "identity", fill = "lightgreen", color = "black") + 
      geom_text(aes(label = round(Average_CO2, 0)), vjust = 0.0, hjust = -0.5, size = 5) + 
      coord_flip() +
      labs(
        title = "Average CO2 Emissions by Vehicle Make",
        x = "Vehicle Make",
        y = "Average CO2 Emissions (g/mi)"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5), 
            axis.text.x = element_text(angle = 45, hjust = 1),
            panel.border = element_rect(color = "black", fill = NA, size = 1))
  })
  
  
  # Plot City vs. Highway fuel consumption
  output$fuel_consumption_scatter <- renderPlot({
    filtered_data <- co2_data[co2_data$Make == input$make, ]
    ggplot(filtered_data, aes(
      x = 235.215 / `Fuel.Consumption.City`, 
      y = 235.215 / `Fuel.Consumption.Hwy`
    )) +
      geom_point(color = "darkgreen", size = 3) +
      labs(
        title = paste("City vs. Highway Fuel Consumption for", input$make),
        x = "City Fuel Consumption (mpg)",
        y = "Highway Fuel Consumption (mpg)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(color = "black", fill = NA, size = 1)
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
