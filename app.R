library(shiny)
library(elmer)
library(DT)
library(janitor)
library(shinychat)
library(bslib)
library(tibble)
library(dotenv)

source("helper.R") # Ensure helper.R contains the functions `generate_data()` and `preprocess_csv()`

ui <- page_sidebar(
  sidebar =
    sidebar(
      shinychat::chat_ui("chat"), # Chat is the main interaction point
      downloadButton("download_csv", "Download CSV")
    ),
  card(
    card_header("Data preview"),
    DTOutput("data_preview")
  )
)

server <- function(input, output, session) {
  chat <- chat_openai(
    model = "gpt-4o-mini",
    system_prompt = paste(collapse = "\n", readLines("data-prompt.md", warn = FALSE)),
    echo = "none"
  )

  data <- reactiveVal()

  observeEvent(input$chat_user_input, {
    user_input <- input$chat_user_input
    prompt <- paste(
      "Generate a fake dataset with at least two variables as a CSV string based on this description:",
      user_input,
      "Include a header row. Ensure all rows have the same number of columns. Do not include any additional text or explanations."
    )

    csv_string <- chat$chat(prompt)
    new_data <- preprocess_csv(csv_string)
    data(new_data)

    summary <- chat$chat("Either 1) Summarize the data or the changes you made to the data or 2) Answer the user's question, whichever is most applicable.")
    shinychat::chat_append("chat", csv_string)
    shinychat::chat_append("chat", summary)
  })

  # Render the updated dataset preview
  output$data_preview <- renderDT({
    req(data())
    datatable(data())
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