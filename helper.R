preprocess_csv <- function(csv_string) {
  csv_pattern <- "(?s)(.+?\\n(?:[^,\n]+(?:,[^,\n]+)*\n){2,})"
  csv_match <- stringr::str_extract(csv_string, csv_pattern)

  if (is.na(csv_match)) {
    stop("No valid CSV data found in the response.")
  }

  lines <- stringr::str_split(csv_match, "\n")[[1]]
  lines <- lines[lines != ""]

  header <- stringr::str_split(lines[1], ",")[[1]]
  num_cols <- length(header)

  processed_lines <- sapply(lines[-1], function(line) {
    cols <- stringr::str_split(line, ",")[[1]]
    if (length(cols) < num_cols) {
      cols <- c(cols, rep("", num_cols - length(cols)))
    } else if (length(cols) > num_cols) {
      cols <- cols[1:num_cols]
    }
    cols
  })

  tibble::tibble(!!!setNames(as.list(as.data.frame(t(processed_lines))), header))
}

generate_data <- function(dataset_description, row_max = 30) {
  chat <- chat_openai(
    model = "gpt-3.5-turbo-0125",
    system_prompt = "You are a helpful assistant that generates fake datasets.",
    echo = "none"
  )

  prompt <- paste(
    "Generate a fake dataset with at least two variables as a CSV string based on this description:",
    dataset_description,
    "Include a header row. Limit to ", row_max, " rows of data. Ensure all rows have the same number of columns. Do not include any additional text or explanations."
  )

  csv_string <- chat$chat(prompt)

  df <- preprocess_csv(csv_string) %>%
    janitor::clean_names() %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ suppressWarnings(ifelse(!is.na(as.numeric(.)), as.numeric(.), as.character(.)))))

  return(df)
}

get_chat_content <- function(stream) {
  # Start with an empty string to accumulate the CSV content
  csv_string <- ""

  # Iterate over the stream to accumulate content
  coro::loop(for (chunk in stream) {
    # Accumulate chunks to the csv_string
    csv_string <- paste0(csv_string, chunk)
  })

  return(csv_string)
}