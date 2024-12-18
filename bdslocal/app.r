# This is a Backward Digit Span App which can be hosted on a local system

# The R script named app.r is the major script. 

# Inorder to avoid confusion I suggest opening the R project which will open the folder 'bdslocal' as the directory 

# Then if you run the script with the required packages installed, it will load the shiny app. 

# The results of the test ie, the participant ID and level reached will be saved 
# in the 'results.csv' file in the 'data' folder in the directory. 


# Install the required packages if not already installed 


if (!require("shiny")) install.packages("shiny")
if (!require("shinyjs")) install.packages("shinyjs")
if (!require("readr")) install.packages("readr")

# Required Libraries

library(shiny)
library(shinyjs)
library(readr)


#####################################################
# UI Definition #####################################
#####################################################

ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  
  # Apply custom CSS for styling
  tags$style(HTML("
    body {
      background-color: #f0f8ff;
      font-family: 'Arial', sans-serif;
    }
    h3 {
      color: #003366;
    }
    .btn {
      background-color: #4CAF50;
      color: white;
      border: none;
      padding: 10px 20px;
      font-size: 16px;
      cursor: pointer;
      border-radius: 5px;
    }
    .btn:hover {
      background-color: #45a049;
    }
    .btn-clear {
      background-color: #f44336;
    }
    .btn-clear:hover {
      background-color: #e53935;
    }
    .panel {
      margin-top: 20px;
      padding: 20px;
      background-color: #ffffff;
      border-radius: 8px;
      box-shadow: 0px 0px 10px rgba(0, 0, 0, 0.1);
    }
    .current-level {
      font-weight: bold;
      font-size: 24px;
      color: #0066cc;
    }
    .result-text {
      color: #333333;
      font-size: 18px;
    }
    .level-end-message {
      color: #ff7043;
      font-size: 20px;
    }
  ")),
  
  # Title Panel
  titlePanel("Backward Digit Span Test"),
  
  # Input for participant ID
  textInput("participant_id", "Enter Participant ID:", ""),
  
  # Display current level after participant ID input
  tags$h3(class = "current-level", textOutput("current_level")),
  
  # Start button to begin the test
  actionButton("start_button", "Start Test", class = "btn"),
  
  # Input for backward digits
  textInput("digits_input", "Enter digits in reverse order:", ""),
  
  # Submit and Clear buttons on the same line, aligned left
  fluidRow(
    column(6, 
           actionButton("submit_button", "Submit Answer", class = "btn"),
           style = "display: inline-block; margin-right: 10px;"),
    column(6, 
           actionButton("clear_button", "Clear", class = "btn btn-clear"),
           style = "display: inline-block;")
  ),
  
  # Messages for results and end of test
  div(class = "panel", verbatimTextOutput("result")),
  
  # Message for level reached
  div(class = "panel", verbatimTextOutput("level_end_message"))
)

# Function to check if a number is palindromic
is_palindrome <- function(digits) {
  return(identical(digits, rev(digits)))
}

#####################################################
######## Server Logic ###############################
#####################################################

server <- function(input, output, session) {
  # Reactive values to track the state of the test
  rv <- reactiveValues(
    digits = NULL,
    level = 2,
    attempts = 0,
    test_ended = FALSE
  )
  
  # Update the current level display
  output$current_level <- renderText({
    paste("Current Level:", rv$level)
  })
  
  observeEvent(input$start_button, {
    if (rv$test_ended) {
      # Reset test if it has ended
      rv$level <- 2
      rv$attempts <- 0
      rv$test_ended <- FALSE
      output$level_end_message <- renderText("")
    }
    
    # Generate random digits for the current level and ensure it's not palindromic
    repeat {
      rv$digits <- sample(0:9, rv$level, replace = TRUE)
      if (!is_palindrome(rv$digits)) {
        break  # Exit the loop if the digits are not palindromic
      }
    }
    
    output$result <- renderText("Digits generated. Listen carefully!")
    
    # Speak digits one at a time
    for (digit in rv$digits) {
      session$sendCustomMessage("speakDigit", digit)
      Sys.sleep(0.5)  # Reduced delay between digits
    }
  })
  
  observeEvent(input$submit_button, {
    if (rv$test_ended) return()  # Prevent further actions if the test has ended
    
    correct_answer <- paste(rev(rv$digits), collapse = "")  # Correct reversed digits
    
    if (input$digits_input == correct_answer) {
      output$result <- renderText("Correct! Proceeding to the next level.")
      rv$level <- rv$level + 1
      rv$attempts <- 0  # Reset attempts for the next level
      
      if (rv$level > 12) {
        # End test after level 12
        rv$test_ended <- TRUE
        save_results(input$participant_id, rv$level - 1)  # Save the last valid level
        output$level_end_message <- renderText("Congratulations! You completed all levels.")
      }
    } else {
      rv$attempts <- rv$attempts + 1
      if (rv$attempts == 2) {
        rv$test_ended <- TRUE
        save_results(input$participant_id, rv$level)  # Save the level at which test ended
        output$result <- renderText(paste("Incorrect twice. The test ends at level", rv$level))
        output$level_end_message <- renderText("Test ended.")
      } else {
        output$result <- renderText("Incorrect! Try again.")
      }
    }
    updateTextInput(session, "digits_input", value = "")  # Clear input field
  })
  
  observeEvent(input$clear_button, {
    # Reset all states and fields
    rv$level <- 2
    rv$attempts <- 0
    rv$test_ended <- FALSE
    rv$digits <- NULL
    output$result <- renderText("")
    output$level_end_message <- renderText("")
    updateTextInput(session, "participant_id", value = "")
    updateTextInput(session, "digits_input", value = "")
    output$current_level <- renderText("Current Level: 2")
  })
  
  # Function to save results
  save_results <- function(participant_id, level_reached) {
    result_data <- data.frame(
      participant_id = participant_id,
      level_reached = level_reached
    )
    dir.create("data", showWarnings = FALSE)
    file_path <- "data/results.csv"
    if (file.exists(file_path)) {
      write_csv(result_data, file_path, append = TRUE)
    } else {
      write_csv(result_data, file_path)
    }
  }
  
  # JavaScript for speech synthesis
  jsCode <- "
  Shiny.addCustomMessageHandler('speakDigit', function(digit) {
    var digit_words = ['zero', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine'];
    var msg = new SpeechSynthesisUtterance();
    msg.text = digit_words[digit];
    window.speechSynthesis.speak(msg);
  });
  "
  runjs(jsCode)
}

# Run the app
shinyApp(ui = ui, server = server)


# end of script
