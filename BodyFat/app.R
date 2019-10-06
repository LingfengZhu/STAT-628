# This is a Shiny web application for the BodyFat project of STAT 628 Module 2

library(shiny)

# Define UI
ui <- fluidPage(

   # Application title
   titlePanel("BodyFat Percentage Predictor", 
              windowTitle = "BodyFat Percentage Predictor"),
   # the line
   tags$hr(),
   
   # Sidebar
   sidebarLayout(
      sidebarPanel(
        selectInput(
          "type", "Model Selection:", 
          list("Model 1" = "model1", 
               "Model 2" = "model2")
          ),
        conditionalPanel(
          condition = "input.type == 'model1'",
          helpText("Model 1 requires TWO input values as following:")
        ),
        conditionalPanel(
          condition = "input.type == 'model2'",
          helpText("Model 2 requires FOUR input values as following:")
        ),
        numericInput(
          "abdomen",
          h4("Abdomen Circumference (cm):"),
          value = NULL,
          min = 65,
          max = 150
        ),
        helpText("Valid input range: 65 ~ 150 cm"),
        conditionalPanel(
          condition = "input.type == 'model1'",
          numericInput(
            "weight",
            h4("weight (lbs):"),
            value = NULL,
            min = 115,
            max = 370
          ),
          helpText("Valid input range: 115 ~ 370 lbs")
        ),
        conditionalPanel(
          condition = "input.type == 'model2'",
          numericInput(
            "age",
            h4("Age (years):"),
            value = NULL,
            min = 1,
            max = 90
          ),
          helpText("Valid input range: 1 ~ 90 years"),
          numericInput(
            "height",
            h4("Height (inches):"),
            value = NULL,
            min = 20,
            max = 80
          ),
          helpText("Valid input range: 20 ~ 80 inches"),
          numericInput(
            "wrist",
            h4("Wrist Circumference (cm):"),
            value = NULL,
            min = 10,
            max = 30
          ),
          helpText("Valid input range: 10 ~ 30 cm")
        ),
        div(style="display:inline-block",actionButton("submit", "Submit"))
        # div(style="display:inline-block",actionButton("reset", "Reset"))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         h2("Predicted BodyFat Percentage"),
         helpText("The predicted body fat percentage (%) obtained from 
                  the given body data based on the chosen model can be seen bellow:"),
         verbatimTextOutput("bodyfat"),
         h2("Information about the result"),
         helpText("Here are our suggestions about the predicted Body Fat Percentage:"),
         verbatimTextOutput("suggestion"),
         h4("If you have any question, please contact Lingfeng Zhu by:", align = "right"),
         h4("lzhu88@wisc.edu", align = "right"),
         # div(style="display:inline-block",img(src = "badger.jpg", height = 200, width = 200)),
         # div(style="display:inline-block",img(src = "logo.jpg", height = 160, width = 400)),
         HTML('<center><img src="badger.jpg" width="100"><img src="logo.jpg" width="200"></center>')
      )
   )
)

# Define server
server <- function(input, output) {
  output$bodyfat = renderPrint({
    # take a dependency on input$submit
    input$submit
    # use isolate() to avoid reaction before the click of "Submit"
    if (input$type == "model1") {
      weight = isolate(input$weight)
      abdomen = isolate(input$abdomen)
      if (is.na(weight) | is.na(abdomen)) {
        "Please Enter legal input values to obtain the result."
      } else if (weight > 370 | weight < 115 | abdomen > 150 | abdomen < 65) {
        "Error: Input value out of prediction range, please retry."
      } else {
        fat = -0.11553 * weight + 0.87794 * abdomen + -41.60272
        if (fat <= 0 | fat >= 100) {
          "Error: output value out of common range, please retry."
        } else {
          paste("Body fat:", fat, "%")
        }
      }
    } else { # model2
      age = isolate(input$age)
      abdomen = isolate(input$abdomen)
      height = isolate(input$height)
      wrist = isolate(input$wrist)
      if (is.na(age) | is.na(abdomen) | is.na(height) | is.na(wrist)) {
        "Please Enter legal input values to obtain the result."
      } else if (age > 90 | age < 1 | abdomen > 150 | abdomen < 65 
                 | height > 80 | height < 20 | wrist > 30 | wrist < 10) {
        "Error: Input value out of prediction range, please retry."
      } else {
        fat = 0.05205 * age + 0.69798 * abdomen + -1.71840 * wrist +  -0.27799 * height + 2.94338
        if (fat <= 0 | fat >= 100) {
          "Error: output value out of common range, please retry."
        } else {
          paste("Body fat:", fat, "%")
        }
      }
    }
  })
  output$suggestion = renderPrint({
    # take a dependency on input$submit
    input$submit
    # use isolate() to avoid reaction before the click of "Submit"
    if (input$type == "model1") {
      weight = isolate(input$weight)
      abdomen = isolate(input$abdomen)
      if (is.na(weight) | is.na(abdomen)) {
        "Please Enter legal input values to obtain the suggestion."
      } else if (weight > 370 | weight < 115 | abdomen > 150 | abdomen < 65) {
        "Error: Input value out of prediction range, please retry."
      } else {
        fat = -0.11553 * weight + 0.87794 * abdomen + -41.60272
        if (fat <= 0 | fat >= 100) {
          "Error: output value out of common range, please retry."
        } else {
          if (fat >= 25) {
            "Your Body Fat Percentage is above 25%, which means you are kind of fat, please try to reduce your body fat percentage!"
          } else if (fat <= 10) {
            "Your Body Fat Percentage is less than 10%, which is less than standard level, please try to increase your body fat percentage!"
          } else {
            "Congratulations! Your Body Fat Percentage is in the normal range! Try to keep it!"
          }
        }
      }
    } else { # model2
      age = isolate(input$age)
      abdomen = isolate(input$abdomen)
      height = isolate(input$height)
      wrist = isolate(input$wrist)
      if (is.na(age) | is.na(abdomen) | is.na(height) | is.na(wrist)) {
        "Please Enter legal input values to obtain the suggestion."
      } else if (age > 90 | age < 1 | abdomen > 150 | abdomen < 65 
                 | height > 80 | height < 20 | wrist > 30 | wrist < 10) {
        "Error: Input value out of prediction range, please retry."
      } else {
        fat = 0.05205 * age + 0.69798 * abdomen + -1.71840 * wrist +  -0.27799 * height + 2.94338
        if (fat <= 0 | fat >= 100) {
          "Error: output value out of common range, please retry."
        } else {
          if (fat >= 25) {
            "Your Body Fat Percentage is above 25%, which means you are kind of fat, please try to reduce your body fat percentage!"
          } else if (fat <= 10) {
            "Your Body Fat Percentage is less than 10%, which is less than standard level, please try to increase your body fat percentage!"
          } else {
            "Congratulations! Your Body Fat Percentage is in the normal range! Try to keep it!"
          }
        }
      }
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
# Application successfully deployed to https://lingfengzhu.shinyapps.io/bodyfat/