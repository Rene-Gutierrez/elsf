library(shiny)
# UI
ui <- fluidPage(
  tags$style(
    HTML("
      .grade_style {
        color: #0073e6;      /* Blue text */
        font-weight: bold;   /* Bold text */
        font-size: 20px;     /* Larger font */
        margin-top: 15px;    /* Add spacing */
      }
    ")
  ),
  
  titlePanel("Beat an Ancient Algorithm"),
  
  sidebarLayout(
    sidebarPanel(
      div(
        h4("Level 1"),
        actionButton("level_1", "Simulation 1"),
        actionButton("results_1", "Results 1"),
        textOutput("r2_machine_1"),
        textOutput("r2_person_1"),
        div(textOutput("grade_person_1"),
            textOutput("winner_1"),
            class = "grade_style")
      ),
      tags$hr(),  # Horizontal line
      div(
        h4("Level 2"),
        actionButton("level_2", "Simulation 2"),
        actionButton("results_2", "Results 2"),
        textOutput("r2_machine_2"),
        textOutput("r2_person_2"),
        div(textOutput("grade_person_2"),
            textOutput("winner_2"),
            class = "grade_style")
      ),
      tags$hr(),  # Horizontal line
      div(
        h4("Level 3"),
        actionButton("level_3", "Simulation 3"),
        actionButton("results_3", "Results 3"),
        textOutput("r2_machine_3"),
        textOutput("r2_person_3"),
        div(textOutput("grade_person_3"),
            textOutput("winner_3"),
            class = "grade_style")
      ),
      tags$hr(),  # Horizontal line
      div(
        h4("Level 4"),
        actionButton("level_4", "Simulation 4"),
        actionButton("results_4", "Results 4"),
        textOutput("r2_machine_4"),
        textOutput("r2_person_4"),
        div(textOutput("grade_person_4"),
            textOutput("winner_4"),
            class = "grade_style")
      ),
      tags$hr(),  # Horizontal line
      div(
        actionButton("clear", "Clear ALL")
      )
    ),
    
    mainPanel(
      plotOutput("plot", click = "plot_click", height = "700px", width = "100%")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  data            <- reactiveVal(NULL)  
  added_points    <- reactiveVal(data.frame(x = numeric(), y = numeric()))
  shown_points    <- reactiveVal(data.frame(x = numeric(), y = numeric()))
  predict_points  <- reactiveVal(NULL)
  plot_points     <- reactiveVal(NULL)
  generation_line <- reactiveVal(NULL)
  regression_line <- reactiveVal(NULL)
  machine_line    <- reactiveVal(NULL)
  
  observeEvent(input$clear, {
    data(data.frame(x = numeric(), y = numeric()))
    added_points(data.frame(x = numeric(), y = numeric()))
    shown_points(data.frame(x = numeric(), y = numeric()))
    predict_points(data.frame(x = numeric(), y = numeric()))
    plot_points(data.frame(x = numeric(), y = numeric()))
    generation_line(data.frame(x = numeric(), y = numeric()))
    regression_line(data.frame(x = numeric(), y = numeric()))
    machine_line(data.frame(x = numeric(), y = numeric()))
    # Clear MSE when data is reset
    output$r2_machine_1 <- renderText("")
    output$r2_person_1  <- renderText("")
    output$grade_1      <- renderText("")
    output$winner_1     <- renderText("") 
    output$r2_machine_2 <- renderText("")
    output$r2_person_2  <- renderText("")
    output$grade_2      <- renderText("")
    output$winner_2     <- renderText("") 
    output$r2_machine_3 <- renderText("")
    output$r2_person_3  <- renderText("")
    output$grade_3      <- renderText("")
    output$winner_3     <- renderText("") 
    output$r2_machine_4 <- renderText("")
    output$r2_person_4  <- renderText("")
    output$grade_4      <- renderText("")
    output$winner_4     <- renderText("")  
  })
  
  observeEvent(input$level_1, {
    predict_points(data.frame(x = numeric(), y = numeric()))
    shown_points(data.frame(x = numeric(), y = numeric()))
    regression_line(data.frame(x = numeric(), y = numeric()))
    machine_line(data.frame(x = numeric(), y = numeric()))
    a <- runif(n = 1, min = 49,  max = 51)
    b <- runif(n = 1, min = 0.9, max = 1.1)
    # Reference Points
    x  <- seq(1, 100)
    y  <- a + b * x + rnorm(100, 0, 5)
    df <- data.frame(x = x, y = y)
    data(df)
    # Prediction Points
    x_pre  <- c(runif(n = 1, min = 0,  max = 10),
                runif(n = 1, min = 21, max = 40),
                runif(n = 1, min = 41, max = 60),
                runif(n = 1, min = 61, max = 80),
                runif(n = 1, min = 81, max = 100))
    y_pre  <- a + b * x_pre + rnorm(n = 5, 0, 10)
    df_pre <- data.frame(x = x_pre, y = y_pre)
    predict_points(df_pre)
    # Regression Line
    y  <- a + b * x
    df <- data.frame(x = x, y = y)
    generation_line(df)
    # Prediction Points
    added_points(data.frame(x = numeric(), y = numeric()))
  })
  
  observeEvent(input$level_2, {
    # Resets Plotable Values
    predict_points(data.frame(x = numeric(), y = numeric()))
    shown_points(data.frame(x = numeric(), y = numeric()))
    regression_line(data.frame(x = numeric(), y = numeric()))
    machine_line(data.frame(x = numeric(), y = numeric()))
    # Simulation Line
    a <- runif(n = 1, min = 49,  max = 51)
    b <- runif(n = 1, min = 0.9, max = 1.1)
    predict_points(data.frame(x = numeric(), y = numeric()))
    shown_points(data.frame(x = numeric(), y = numeric()))
    # Reference Points
    x  <- seq(1, 100)
    y  <- a + b * (x -50)^2 / 75 + rnorm(100, 0, 5)
    df <- data.frame(x = x, y = y)
    data(df)
    # Prediction Points
    x_pre  <- c(runif(n = 1, min = 0,  max = 10),
                runif(n = 1, min = 21, max = 40),
                runif(n = 1, min = 41, max = 60),
                runif(n = 1, min = 61, max = 80),
                runif(n = 1, min = 81, max = 100))
    y_pre  <- a + b * (x_pre - 50)^2 / 75 + rnorm(n = 5, 0, 10)
    df_pre <- data.frame(x = x_pre, y = y_pre)
    predict_points(df_pre)
    # Regression Line
    y  <- a + b * (x -50)^2 / 75
    df <- data.frame(x = x, y = y)
    generation_line(df)
    # Prediction Points
    added_points(data.frame(x = numeric(), y = numeric()))
  })
  
  observeEvent(input$level_3, {
    # Resets Plotable Values
    predict_points(data.frame(x = numeric(), y = numeric()))
    shown_points(data.frame(x = numeric(), y = numeric()))
    regression_line(data.frame(x = numeric(), y = numeric()))
    machine_line(data.frame(x = numeric(), y = numeric()))
    # Simulation Line
    a <- runif(n = 1, min = 49,  max = 51)
    b <- runif(n = 1, min = 0.9, max = 1.1)
    predict_points(data.frame(x = numeric(), y = numeric()))
    shown_points(data.frame(x = numeric(), y = numeric()))
    # Reference Points
    x  <- seq(1, 100)
    y  <- a * 10 + b * (x - 50)^3 / 1000 - b * x^2 / 50 + rnorm(100, 0, 10)
    df <- data.frame(x = x, y = y)
    data(df)
    # Prediction Points
    x_pre  <- c(runif(n = 1, min = 0,  max = 10),
                runif(n = 1, min = 21, max = 40),
                runif(n = 1, min = 41, max = 60),
                runif(n = 1, min = 61, max = 80),
                runif(n = 1, min = 81, max = 100))
    y_pre  <- a * 10 + b * (x_pre - 50)^3 / 1000 - b * x_pre^2 / 50 + rnorm(n = 5, 0, 5)
    print(x_pre)
    print(y_pre)
    df_pre <- data.frame(x = x_pre, y = y_pre)
    predict_points(df_pre)
    # Regression Line
    y  <- a * 10 + b * (x - 50)^3 / 1000 - b * x^2 / 50
    df <- data.frame(x = x, y = y)
    generation_line(df)
    # Prediction Points
    added_points(data.frame(x = numeric(), y = numeric()))
  })
  
  
  observeEvent(input$level_4, {
    # Resets Plotable Values
    predict_points(data.frame(x = numeric(), y = numeric()))
    shown_points(data.frame(x = numeric(), y = numeric()))
    regression_line(data.frame(x = numeric(), y = numeric()))
    machine_line(data.frame(x = numeric(), y = numeric()))
    # Simulation Line
    a <- runif(n = 1, min = 49,  max = 51)
    b <- runif(n = 1, min = 0.9, max = 1.1)
    predict_points(data.frame(x = numeric(), y = numeric()))
    shown_points(data.frame(x = numeric(), y = numeric()))
    # Reference Points
    x  <- seq(1, 100)
    y  <- a * 10 + b * (x - 50)^3 / 1000 - b * x^2 / 50 + rnorm(100, 0, 20)
    df <- data.frame(x = x, y = y)
    data(df)
    # Prediction Points
    x_pre  <- c(runif(n = 1, min = 0,  max = 10),
                runif(n = 1, min = 21, max = 40),
                runif(n = 1, min = 41, max = 60),
                runif(n = 1, min = 61, max = 80),
                runif(n = 1, min = 81, max = 100))
    y_pre  <- a * 10 + b * (x_pre - 50)^3 / 1000 - b * x_pre^2 / 50 + rnorm(n = 5, 0, 20)
    print(x_pre)
    print(y_pre)
    df_pre <- data.frame(x = x_pre, y = y_pre)
    predict_points(df_pre)
    # Regression Line
    y  <- a * 10 + b * (x - 50)^3 / 1000 - b * x^2 / 50
    df <- data.frame(x = x, y = y)
    generation_line(df)
    # Prediction Points
    added_points(data.frame(x = numeric(), y = numeric()))
  })
  
  observeEvent(input$plot_click, {
    if (!is.null(data())) {
      new_point <- data.frame(x = input$plot_click$x, y = input$plot_click$y)
      added_points(rbind(added_points(), new_point))  
    }
  })
  
  output$plot <- renderPlot({
    if (is.null(data())) return()
    
    ymin <- min(data()$y)
    ymax <- max(data()$y)
    xmin <- min(data()$x)
    xmax <- max(data()$x)
    plot(data(), pch = 19,
         ylim = c(ymin, ymax),
         xlim = c(xmin, xmax))
    abline(v   = predict_points()$x,
           col = rgb(0.1, 0.1, 0.1, 0.1),
           lwd = 10)
    points(added_points(),
           col = rgb(1, 0, 0),
           pch = 19,
           cex = 2)
    points(shown_points(),
           col = rgb(0, 1, 0),
           pch = 19,
           cex = 2)
    par(new = TRUE)
    plot(x = regression_line()$x,
         y = regression_line()$y,
         ylim = c(ymin, ymax),
         xlim = c(xmin, xmax),
         type = "l",
         xaxt = "n",
         yaxt = "n",
         xlab = "",
         ylab = "",
         col  = rgb(0, 0, 1, 0.3),
         lwd  = 5)
    par(new = TRUE)
    plot(x    = machine_line()$x,
         y    = machine_line()$y,
         ylim = c(ymin, ymax),
         xlim = c(xmin, xmax),
         type = "l",
         xaxt = "n",
         yaxt = "n",
         xlab = "",
         ylab = "",
         col  = rgb(1, 0, 1, 0.3),
         lwd  = 5)
    
  })
  
  observeEvent(input$results_1, {
    if (!is.null(data()) && nrow(added_points()) > 0) {
      # Sows points
      df <- data.frame(x = predict_points()$x, y = predict_points()$y)
      shown_points(df)
      # Shows the regression line
      regression_line(generation_line())
      # Machine MSE
      fit         <- lm(y ~ x, data = data())
      # Shows the fit
      machine_y   <- predict(fit, newdata = predict_points())  
      mse_machine <- mean((predict_points()$y - machine_y)^2)
      r2_machine  <- 1 - mse_machine / var(predict_points()$y)
      # Machine Line
      x <- generation_line()$x
      y <- predict(fit, newdata = generation_line())
      df <- data.frame(x = x, y = y)
      machine_line(df)
      # Person MSE
      y_person    <- added_points()$y[order(added_points()$x)]
      y_predict   <- predict_points()$y[order(predict_points()$x)]
      mse_person  <- mean((y_predict - y_person)^2)
      r2_person   <- 1 - mse_person / var(predict_points()$y)
      # Text Outputs
      output$r2_machine_1 <- renderText(paste("Machine Score:", round(r2_machine * 100, 2)))
      output$r2_person_1  <- renderText(paste("Your Score:",    round(r2_person * 100, 2)))
      numeric_grade <- r2_person / r2_machine
      if(numeric_grade >= 0.9){
        grade <- "A"
      } else if(numeric_grade >= 0.8){
        grade <- "B"
      } else if(numeric_grade >= 0.7){
        grade <- "C"
      } else if(numeric_grade >= 0.6){
        grade <- "D"
      } else {
        grade <- "F"
      }
      output$grade_person_1  <- renderText(paste("Your Grade:", grade))
      if(numeric_grade < 0.9){
        output$winner_1  <- renderText(paste("You Loose."))
      } else {
        output$winner_1  <- renderText(paste("You Win!"))
      }
    } else {
      output$mse_machine_1 <- renderText("No points added yet.")
    }
  })
  
  observeEvent(input$results_2, {
    if (!is.null(data()) && nrow(added_points()) > 0) {
      # Sows points
      df <- data.frame(x = predict_points()$x, y = predict_points()$y)
      shown_points(df)
      # Shows the regression line
      regression_line(generation_line())
      # Machine MSE
      fit         <- lm(y ~ x + I(x^2), data = data())
      # Shows the fit
      machine_y   <- predict(fit, newdata = predict_points())  
      mse_machine <- mean((predict_points()$y - machine_y)^2)
      r2_machine  <- 1 - mse_machine / var(predict_points()$y)
      # Machine Line
      x <- generation_line()$x
      y <- predict(fit, newdata = generation_line())
      df <- data.frame(x = x, y = y)
      machine_line(df)
      # Person MSE
      y_person    <- added_points()$y[order(added_points()$x)]
      y_predict   <- predict_points()$y[order(predict_points()$x)]
      mse_person  <- mean((y_predict - y_person)^2)
      r2_person   <- 1 - mse_person / var(predict_points()$y)
      # Text Outputs
      output$r2_machine_2 <- renderText(paste("Machine Score:", round(r2_machine * 100, 2)))
      output$r2_person_2  <- renderText(paste("Your Score:",    round(r2_person * 100, 2)))
      numeric_grade <- r2_person / r2_machine
      if(numeric_grade >= 0.9){
        grade <- "A"
      } else if(numeric_grade >= 0.8){
        grade <- "B"
      } else if(numeric_grade >= 0.7){
        grade <- "C"
      } else if(numeric_grade >= 0.6){
        grade <- "D"
      } else {
        grade <- "F"
      }
      output$grade_person_2  <- renderText(paste("Your Grade:", grade))
      if(numeric_grade < 0.9){
        output$winner_2  <- renderText(paste("You Loose."))
      } else {
        output$winner_2  <- renderText(paste("You Win!"))
      }
    } else {
      output$mse_machine_2 <- renderText("No points added yet.")
    }
  })
  
  observeEvent(input$results_3, {
    if (!is.null(data()) && nrow(added_points()) > 0) {
      # Sows points
      df <- data.frame(x = predict_points()$x, y = predict_points()$y)
      shown_points(df)
      # Shows the regression line
      regression_line(generation_line())
      # Machine MSE
      fit         <- lm(y ~ x + I(x^2) + I(x^3), data = data())
      # Shows the fit
      machine_y   <- predict(fit, newdata = predict_points())  
      mse_machine <- mean((predict_points()$y - machine_y)^2)
      r2_machine  <- 1 - mse_machine / var(predict_points()$y)
      # Machine Line
      x <- generation_line()$x
      y <- predict(fit, newdata = generation_line())
      df <- data.frame(x = x, y = y)
      machine_line(df)
      # Person MSE
      y_person    <- added_points()$y[order(added_points()$x)]
      y_predict   <- predict_points()$y[order(predict_points()$x)]
      mse_person  <- mean((y_predict - y_person)^2)
      r2_person   <- 1 - mse_person / var(predict_points()$y)
      # Text Outputs
      output$r2_machine_3 <- renderText(paste("Machine Score:", round(r2_machine * 100, 2)))
      output$r2_person_3  <- renderText(paste("Your Score:",    round(r2_person * 100, 2)))
      numeric_grade <- r2_person / r2_machine
      if(numeric_grade >= 0.9){
        grade <- "A"
      } else if(numeric_grade >= 0.8){
        grade <- "B"
      } else if(numeric_grade >= 0.7){
        grade <- "C"
      } else if(numeric_grade >= 0.6){
        grade <- "D"
      } else {
        grade <- "F"
      }
      output$grade_person_3  <- renderText(paste("Your Grade:", grade))
      if(numeric_grade < 0.9){
        output$winner_3  <- renderText(paste("You Loose."))
      } else {
        output$winner_3  <- renderText(paste("You Win!"))
      }
    } else {
      output$mse_machine_3 <- renderText("No points added yet.")
    }
  })
  observeEvent(input$results_4, {
    if (!is.null(data()) && nrow(added_points()) > 0) {
      # Sows points
      df <- data.frame(x = predict_points()$x, y = predict_points()$y)
      shown_points(df)
      # Shows the regression line
      regression_line(generation_line())
      # Machine MSE
      fit         <- lm(y ~ x + I(x^2) + I(x^3), data = data())
      # Shows the fit
      machine_y   <- predict(fit, newdata = predict_points())  
      mse_machine <- mean((predict_points()$y - machine_y)^2)
      r2_machine  <- 1 - mse_machine / var(predict_points()$y)
      # Machine Line
      x <- generation_line()$x
      y <- predict(fit, newdata = generation_line())
      df <- data.frame(x = x, y = y)
      machine_line(df)
      # Person MSE
      y_person    <- added_points()$y[order(added_points()$x)]
      y_predict   <- predict_points()$y[order(predict_points()$x)]
      mse_person  <- mean((y_predict - y_person)^2)
      r2_person   <- 1 - mse_person / var(predict_points()$y)
      # Text Outputs
      output$r2_machine_4 <- renderText(paste("Machine Score:", round(r2_machine * 100, 2)))
      output$r2_person_4  <- renderText(paste("Your Score:",    round(r2_person * 100, 2)))
      numeric_grade <- r2_person / r2_machine
      if(numeric_grade >= 0.9){
        grade <- "A"
      } else if(numeric_grade >= 0.8){
        grade <- "B"
      } else if(numeric_grade >= 0.7){
        grade <- "C"
      } else if(numeric_grade >= 0.6){
        grade <- "D"
      } else {
        grade <- "F"
      }
      output$grade_person_4  <- renderText(paste("Your Grade:", grade))
      if(numeric_grade < 0.9){
        output$winner_4  <- renderText(paste("You Loose."))
      } else {
        output$winner_4  <- renderText(paste("You Win!"))
      }
    } else {
      output$mse_machine_4 <- renderText("No points added yet.")
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
