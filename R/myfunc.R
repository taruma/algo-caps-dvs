rubricscore <- function(rubric) {
  rubric |>
    as.integer() |>
    sum()
}
