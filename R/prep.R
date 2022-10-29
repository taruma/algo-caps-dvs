require(readr)

library(dplyr)

eurovision <- readr::read_csv('data/eurovision.csv')

factor_columns <- c(
  "host_city", "event", "host_country", "section", "artist_country"
)

integer_columns <- c(
  "year", "running_order", "total_points", "rank"
)

eurovision <- eurovision |> 
  mutate(across(all_of(factor_columns), as.factor)) |> 
  mutate(across(all_of(integer_columns), as.integer))
