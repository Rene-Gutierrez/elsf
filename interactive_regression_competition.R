library(shiny)
library(ggplot2)

# UI
ui <- fluidPage(
  
  titlePanel("Simple Linear Regression Simulation with MSE Calculation"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("simulate", "Simulate Data"),
      actionButton("compute_mse", "Compute MSE"),
      textOutput("mse_output")
    ),
    
    mainPanel(
      plotOutput("plot", click = "plot_click")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  data <- reactiveVal(NULL)  
  added_points <- reactiveVal(data.frame(x = numeric(), y = numeric()))  
  
  observeEvent(input$simulate, {
    set.seed(123)
    x <- seq(1, 100)
    y <- 3 + 0.5 * x + rnorm(100, 0, 5)
    df <- data.frame(x = x, y = y)
    
    data(df)
    added_points(data.frame(x = numeric(), y = numeric()))  
    output$mse_output <- renderText("")  # Clear MSE when data is reset
  })
  
  observeEvent(input$plot_click, {
    if (!is.null(data())) {
      new_point <- data.frame(x = input$plot_click$x, y = input$plot_click$y)
      added_points(rbind(added_points(), new_point))  
    }
  })
  
  output$plot <- renderPlot({
    if (is.null(data())) return()
    
    ggplot(data(), aes(x, y)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "blue") +
      geom_point(data = added_points(), aes(x, y), color = "red", size = 3) +
      labs(title = "Click the plot to add points!")
  })
  
  observeEvent(input$compute_mse, {
    if (!is.null(data()) && nrow(added_points()) > 0) {
      fit <- lm(y ~ x, data = data())  
      predicted_y <- predict(fit, newdata = added_points())  
      mse <- mean((added_points()$y - predicted_y)^2)
      
      output$mse_output <- renderText(paste("MSE of added points:", round(mse, 2)))
    } else {
      output$mse_output <- renderText("No points added yet.")
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
