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

myplot <- function(year_selected, section_selected,
                   top_n = 10, order_run = TRUE) {
  if (order_run) {
    order_column <- "running_order"
  } else {
    order_column <- "total_points"
  }

  # https://stackoverflow.com/questions/27034655

  eurovision_filtered <- eurovision |>
    filter(year == year_selected, section %in% section_selected) |>
    select(
      artist_country, total_points, running_order,
      artist, song
    ) |>
    mutate(
      sort_index = order(!!sym(order_column), decreasing = !order_run),
      tooltip = glue(
        "#{running_order} <b>{artist_country}</b>
        <i>{song}</i>
        by <b>{artist}</b>
        Poin: <b>{total_points}</b>
        "
      ),
      artist_country = artist_country |> sapply(function(.) {
        gsub(" ", "\n", .)
      })
    ) |>
    slice(sort_index) |>
    head(top_n) |>
    mutate(
      artist_country = forcats::fct_reorder(
        artist_country, !!sym(order_column),
        .desc = !order_run
      ),
    )

  eurovision_filtered |>
    ggplot(aes(x = artist_country, y = total_points, text = tooltip)) +
    geom_col(aes(fill = total_points), show.legend = FALSE) +
    theme_bw() +
    scale_fill_gradient2(
      low = "#fff7ec",
      mid = "#fe9929",
      high = "#8c2d04"
    ) +
    labs(
      x = "Negara",
      y = "Total Poin / Skor"
    )
}

section_tb <- eurovision |>
  select(year, section) |>
  distinct()

total_contestant_tb <- eurovision |>
  select(artist_country, year, section) |>
  group_by(year, section) |>
  summarise(total_contestant = n(), .groups = "keep") |>
  ungroup()

## TAB 1 ----

year_selected <- 2022

winner_info <- eurovision |>
  filter(
    year == year_selected,
    section == "grand-final", winner
  )




## PLOT HEATMAP ----

# https://stackoverflow.com/questions/9007741/

eurovision_xtabs_final <- xtabs(
  ~ artist_country + year, eurovision_grandfinal
) |>
  as_tibble() |>
  mutate(
    year_number = as.integer(year),
    # n = as.integer(n > 0)
  ) |>
  rename(participant = n)

list_country <- eurovision_xtabs_final$artist_country |> unique()
list_country_2022 <- eurovision |>
  filter(year == year_selected, section == "grand-final") |>
  select(artist_country) |>
  pull(artist_country) |>
  as.character()

eurovision_heatmap <- eurovision_xtabs_final |>
  left_join(
    eurovision |> mutate(year_number = year),
    by = c("year_number", "artist_country")
  ) |>
  mutate_at(
    c("artist", "song"), ~ replace_na(., "")
  ) |>
  mutate(
    tooltip = glue(
      "#{running_order}-{year.x} {artist_country}
      <i>{song}</i>
      <b>{artist}</b><extra></extra>"
    )
  )


plotheatmap <- function(selected_country, year_start,
                        year_end) {
  filtered_data <- eurovision_heatmap |>
    filter(
      artist_country %in% selected_country,
      year.x >= year_start & year.x <= year_end
    )

  plot_ly() |>
    add_heatmap(
      data = filtered_data,
      x = ~year.x,
      y = ~artist_country,
      z = ~participant,
      text = ~tooltip,
      showscale = FALSE,
      colorscale = "Blues",
      reversescale = TRUE,
      hovertemplate = "%{text}"
    ) |>
    layout(
      xaxis = list(
        title = "<b>Tahun</b>",
        spikemode = "across",
        spikethickness = 0.2
        # tickformat = "%{0}f"
      ),
      yaxis = list(
        title = "<b>Negara</b>"
      ),
      title = "<b>Partisipasi Negara di Babak Final <i>Eurovision Song Contest</i></b>"
    )
}



#
# eurovision_xtabs_winner <- xtabs(
#   winner ~ artist_country + year, eurovision_grandfinal
# ) |>
#   as.data.frame() |>
#   mutate(
#     year = as.numeric(year |> levels())[year],
#   )
#
# eurovision_xtabs_artist <- eurovision_xtabs_winner |>
#   left_join(eurovision,
#     by = c("year", "artist_country")
#   ) %>%
#   mutate_at(
#     c('artist', 'song'), ~replace_na(.,"")
#   ) |>
#   mutate(
#     tooltip = glue(
#       "{artist_country}
#       <i>{song}</i>
#       <b>{artist}</b><extra></extra>
#       "
#     )
#   )
#
# score_winner <- eurovision_xtabs_final$qualified +
#   eurovision_xtabs_winner$Freq + 100

#
# ggplot(
#   eurovision_xtabs_final, aes(x = year, y = artist_country, text = tooltip)
# ) +
#   geom_tile(aes(fill = Freq), show.legend = FALSE) +
#   theme_bw() +
#   # scale_fill_viridis_b() -> ewq
#   scale_fill_distiller(direction = 1) -> ewq
#
# ggplotly(ewq, tooltip = "text") |> layout(hovermode = "x unified")
