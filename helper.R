preprocess_csv <- function(csv_string) {
  csv_string <- gsub("\\\\n", "\n", csv_string)
  read.csv(text = csv_string) |> as_tibble()
}
