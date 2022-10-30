eurovision <- readr::read_csv("data/eurovision.csv")

factor_columns <- c(
  "host_city", "event", "host_country", "section", "artist_country"
)

integer_columns <- c(
  "year", "running_order", "total_points", "rank"
)

eurovision <- eurovision |>
  mutate(across(all_of(factor_columns), as.factor)) |>
  mutate(across(all_of(integer_columns), as.integer))

eurovision_grandfinal <- eurovision |>
  filter(section %in% c("final", "grand-final"))

total_participant <- eurovision |>
  select(artist_country) |>
  unique() |>
  nrow()

winner_summary <- eurovision_grandfinal |>
  group_by(year, section) |>
  summarise(total_winners = sum(winner), .groups = "keep") |>
  ungroup()

total_winner_country <- eurovision_grandfinal |>
  filter(winner) |>
  select(artist_country) |>
  unique() |>
  nrow()

year_multiple_winner <- winner_summary |>
  filter(total_winners > 1) |>
  pull(year) |> 
  paste(collapse = ",")

year_missing <- winner_summary |>
  filter(total_winners == 0) |>
  pull(year) |> 
  paste(collapse = ",")

total_running_year <- winner_summary |>
  filter(total_winners >= 1) |>
  nrow()

myplot <- function(year_selected, section_selected, top_n = 10) {
  
  eurovision |> 
    filter(year == year_selected, section %in% section_selected) |> 
    select(artist_country, total_points) |> 
    arrange(desc(total_points)) |>
    head(top_n) |> 
    mutate(
      # artist_country = factor(artist_country, levels=artist_country)
      artist_country = forcats::fct_reorder(artist_country, total_points, .desc = TRUE)
    ) |> 
    ggplot(aes(x = artist_country, y = total_points)) +
    geom_col()
}

section_tb <- eurovision |> select(year, section) |> distinct()

total_contestant_tb <- eurovision |>
  select(artist_country, year, section) |>
  group_by(year, section) |>
  summarise(total_contestant = n(), .groups = "keep") |>
  ungroup()

