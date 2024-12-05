library(shiny)
library(elmer)
library(DT)
library(janitor)
library(shinychat)
library(bslib)
library(tibble)
library(dplyr)
library(ggplot2)
library(dotenv)
library(thematic)

preprocess_csv <- function(csv_string) {
  csv_string <- gsub("\\\\n", "\n", csv_string)
  read.csv(text = csv_string) |> as_tibble()
}

ui <- page_sidebar(
  title = "Data Simulator",
  sidebar =
    sidebar(
      shinychat::chat_ui("chat"),
      downloadButton("download_csv", "Download CSV")
    ),
  layout_column_wrap(
    card(
      card_header("Data preview"),
      DTOutput("data_preview")
    ),
    card(
      card_header("Plot"),
      layout_column_wrap(
        selectInput("plot_var_x", "X-axis variable:", choices = NULL),
        selectInput("plot_var_y", "Y-axis variable:", choices = NULL),
      ),
      plotOutput("plot")
    )
  )
)

server <- function(input, output, session) {
  chat <- chat_openai(
    model = "gpt-4o-mini",
    system_prompt = paste(collapse = "\n", readLines("data-prompt.md", warn = FALSE)),
    echo = "none"
  )

  chat_append("chat", "Describe the data you want to generate.")

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

  observe({
    updateSelectInput(session, "plot_var_x", choices = colnames(data()))
  })

  observe({
    updateSelectInput(session, "plot_var_y", choices = colnames(data()))
  })

  # Render the updated dataset preview
  output$data_preview <- renderDT({
    req(data())
    datatable(data(), filter = "none")
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

  output$plot <- renderPlot({
    df <- data()
    req(df)
    req(input$plot_var_x)
    req(input$plot_var_y)

    if (is.numeric(df[[input$plot_var_x]]) & is.numeric(df[[input$plot_var_y]])) {
        geom <- geom_point
    } else if (is.numeric(df[[input$plot_var_x]]) | is.numeric(df[[input$plot_var_y]])) {
      geom <- geom_boxplot
    } else {
      geom <- geom_count
    }

    tryCatch(
      {
        p <-
          ggplot(df, aes_string(x = input$plot_var_x, y = input$plot_var_y)) +
          geom() +
          theme_minimal()
      },
      error = function(e) {
        stop(e)
      }
    )

    p
  })
}

shinyApp(ui = ui, server = server)