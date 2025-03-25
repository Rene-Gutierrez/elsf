library(shiny)
# UI
ui <- fluidPage(
  
  titlePanel("Simple Linear Regression Simulation with MSE Calculation"),
  
  sidebarLayout(
    sidebarPanel(
      div(
        h4("Level 1"),
        actionButton("level_1", "Simulation 1"),
        actionButton("results", "Results 1"),
        textOutput("mse_output")
      ),
      tags$hr(),  # Horizontal line
      div(
        h4("Level 2"),
        actionButton("level_2", "Simulation 2"),
        actionButton("results", "Results 2"),
        textOutput("mse_output")
      )
    ),
    
    mainPanel(
      plotOutput("plot", click = "plot_click", height = "900px", width = "100%")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  data            <- reactiveVal(NULL)  
  added_points    <- reactiveVal(data.frame(x = numeric(), y = numeric()))
  predict_points  <- reactiveVal(NULL)
  plot_points     <- reactiveVal(NULL)
  
  observeEvent(input$level_1, {
    set.seed(123)
    # Reference Points
    x <- seq(1, 100)
    y <- 3 + 0.5 * x + rnorm(100, 0, 5)
    df <- data.frame(x = x, y = y)
    # Prediction Points
    x_pre  <- runif(n = 5, min = 0, max = 100)
    y_pre  <- 3 + 0.5 * x_pre + rnorm(100, 0, 5)
    df_pre <- data.frame(x = x_pre, y = y_pre)
    predict_points(df_pre)
    
    # Prediction Points
    data(df)
    added_points(data.frame(x = numeric(), y = numeric()))  
    output$mse_output <- renderText("")  # Clear MSE when data is reset
  })
  
  
  
  observeEvent(input$level_2, {
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
    
    plot(data(), pch = 19)
    abline(v   = predict_points()$x,
           col = rgb(0.1, 0.1, 0.1, 0.01),
           lwd = 10)
    points(added_points(),
           col = rgb(1, 0, 0),
           pch = 19)
    
    # ggplot(data(), aes(x, y)) +
    #   geom_point() +
    #   geom_smooth(method = "lm", se = FALSE, color = "blue") +
    #   geom_point(data = added_points(), aes(x, y), color = "red", size = 3) +
    #   labs(title = "Click to add your predictions!")
  })
  
  observeEvent(input$results, {
    if (!is.null(data()) && nrow(added_points()) > 0) {
      fit       <- lm(y ~ x, data = data())  
      machine_y <- predict(fit, newdata = predict_points())  
      mse       <- mean((predict_points()$y - machine_y)^2)
      
      output$mse_output <- renderText(paste("Your Score:", round(mse, 2)))
    } else {
      output$mse_output <- renderText("No points added yet.")
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
