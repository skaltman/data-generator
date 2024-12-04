
library(shiny)
library(elmer)
library(DT)
library(janitor)
library(shinychat)

source("helper.R") # Ensure helper.R contains the functions `generate_data()` and `preprocess_csv()`

ui <- fluidPage(
  titlePanel("Interactive Dataset Chat"),
  sidebarLayout(
    sidebarPanel(
      shinychat::chat_ui("chat"), # Chat is the main interaction point
      downloadButton("download_csv", "Download CSV")
    ),
    mainPanel(
      card(
        card_header("Data preview"),
        DTOutput("data_preview")
      )
    )
  )
)

server <- function(input, output, session) {
  # Create a chat object
  chat <- chat_openai(
    model = "gpt-3.5-turbo-0125",
    system_prompt = "You are a helpful assistant that generates fake datasets."
  )

  # Initialize a reactive data container
  data <- reactiveVal()

  # Handle chat input and generate or update dataset
  observeEvent(input$chat_user_input, {
    user_input <- input$chat_user_input
    prompt <- paste(
      "Generate a fake dataset with at least two variables as a CSV string based on this description:",
      user_input,
      "Include a header row. Limit to 10 rows of data. Ensure all rows have the same number of columns. Do not include any additional text or explanations."
    )

    # Execute chat model synchronously
    csv_string <- chat$chat(prompt)

    # Process the CSV string response
    new_data <- preprocess_csv(csv_string)

    # Update reactiveVal with new data
    data(new_data)

    # Add the response back to the chat
    shinychat::chat_append("chat", csv_string)
  })

  # Render the updated dataset preview
  output$data_preview <- renderDT({
    req(data())
    datatable(head(data(), 10))
  })

  # File download handler
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("dataset", Sys.Date(), ".csv", sep = "_")
    },
    content = function(file) {
      write.csv(data(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)