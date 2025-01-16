library(shiny)
library(bslib)
library(promises)
library(fastmap)
library(duckdb)
library(DBI)
library(fontawesome)
library(DT)
library(here)
library(plotly)
library(ggplot2)
library(ggridges)
library(dplyr)
library(elmer)
library(shinychat)
library(dotenv)

# Open the duckdb database
conn <- dbConnect(duckdb(), dbdir = ":memory:", read_only = FALSE)
# Close the database when the app stops
onStop(\() dbDisconnect(conn))

openai_model <- "gpt-4o"

system_prompt_str <- readLines(here("prompt.md"), warn = FALSE)

icon_explain <- tags$img(src = "stars.svg")

ui <- page_sidebar(
  style = "background-color: rgb(248, 248, 248);",
  title = "LLM Data Simulator",
  includeCSS(here("styles.css")),
  sidebar = sidebar(
    width = 400,
    style = "height: 100%;",
    chat_ui("chat", height = "100%", fill = TRUE)
  ),
  useBusyIndicators(),

  layout_columns(
    style = "min-height: 450px;",

    # 🔍 Data table
    card(
      style = "height: 500px;",
      card_header("Data preview"),
      DTOutput("table")
    )
  )
)

server <- function(input, output, session) {
  # 🔄 Reactive state/computation --------------------------------------------

  # current_query <- reactiveVal("")
  data <- reactiveVal()

  # This object must always be passed as the `.ctx` argument to query(), so that
  # tool functions can access the context they need to do their jobs; in this
  # case, the database connection that query() needs.
  ctx <- list(conn = conn)

  # 🔍 Data table ------------------------------------------------------------

  output$table <- renderDT({
    req(data())
    datatable(data(), filter = "none")
  })


  # ✨ Sidebot ✨ -------------------------------------------------------------

  append_output <- function(...) {
    txt <- paste0(...)
    shinychat::chat_append_message(
      "chat",
      list(role = "assistant", content = txt),
      chunk = TRUE,
      operation = "append",
      session = session
    )
  }

  #' Creates a new table in a database using the given SQL query.
  #' @param query A DuckDB SQL query; must be a CREATE TABLE statement.
  #' @returns A string representing the query
  simulate_data <- function(query) {
    print("in simulate data")
    start <- Sys.time()
    # append_output("\n```sql\n", query, "\n```\n\n")

    tryCatch(
      {
        print("in the try catch")
        # Try it to see if it errors; if so, the LLM will see the error
        dbExecute(conn, query)
        print("code executed")
      },
      error = function(err) {
        print("in the error")
        # append_output("> Error: ", conditionMessage(err), "\n\n")
        stop(err)
      }
    )

    if (!is.null(query)) {
      df <- dbGetQuery(conn, "SELECT * FROM data")
      data(df)
      end <- Sys.time()
      print(start - end)
      return(query)
    }
  }

  #' Updates an existing table in a database using the given SQL query.
  #' @param query A DuckDB SQL query; must be an UPDATE or ALTER statement.
  #' @returns A string representing the query
  update_data <- function(query) {

    tryCatch(
      {
        # Try it to see if it errors; if so, the LLM will see the error
        dbExecute(conn, query)
      },
      error = function(err) {
        # append_output("> Error: ", conditionMessage(err), "\n\n")
        stop(err)
      }
    )

    if (!is.null(query)) {
      df <- dbGetQuery(conn, "SELECT * FROM data")
      data(df)
      return(query)
    }
  }

  chat <- chat_openai(model = openai_model, system_prompt = system_prompt_str)

  chat_append("chat", "Describe a data set you want to simulate.")

  chat$register_tool(tool(
    simulate_data,
    "Creates a new table in the database using the provided SQL statement.",
    query = type_string("A DuckDB SQL query.")
  ))

  chat$register_tool(tool(
    update_data,
    "Updates the existing `data` table in the database using the provided SQL statement.",
    query = type_string("A DuckDB SQL query.")
  ))

  observeEvent(input$chat_user_input, {
    stream <- chat$stream_async(input$chat_user_input)
    chat_append("chat", stream)
  })

}

shinyApp(ui, server)
