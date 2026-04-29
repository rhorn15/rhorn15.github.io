# Shared data pipeline for DANL 310 NFL storytelling project

NFL_STANDINGS_URL <- "https://github.com/nflverse/nfldata/raw/master/data/standings.csv"

load_nfl_standings <- function(path = "data/standings.csv", refresh = FALSE) {
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Package 'readr' is required.")
  }

  if (!file.exists(path) || isTRUE(refresh)) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    standings <- readr::read_csv(NFL_STANDINGS_URL, show_col_types = FALSE)
    readr::write_csv(standings, path)
  }

  readr::read_csv(path, show_col_types = FALSE)
}

prep_nfl_team_season <- function(standings_raw, min_season = 2002, max_season = NULL) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required.")
  }

  df <- standings_raw |>
    dplyr::mutate(
      seed_num = suppressWarnings(as.numeric(seed)),
      games_played = wins + losses + ties,
      # Some seasons have blank playoff text for teams that clearly made the
      # postseason, so use seed as the primary playoff indicator.
      playoff_flag = (!is.na(seed_num)) | (!is.na(playoff) & playoff != ""),
      win_pct = (wins + 0.5 * ties) / games_played,
      points_for_pg = scored / games_played,
      points_allowed_pg = allowed / games_played,
      point_diff_pg = net / games_played
    ) |>
    dplyr::filter(games_played >= 16, scored > 0, allowed > 0)

  if (is.null(max_season)) {
    max_season <- max(df$season, na.rm = TRUE)
  }

  df |>
    dplyr::filter(season >= min_season, season <= max_season) |>
    dplyr::mutate(
      era = dplyr::case_when(
        season <= 2010 ~ "2002-2010",
        season <= 2020 ~ "2011-2020",
        TRUE ~ "2021-present"
      )
    )
}

add_team_style <- function(team_season) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required.")
  }

  season_baseline <- team_season |>
    dplyr::group_by(season) |>
    dplyr::summarise(
      league_points_for_pg = mean(points_for_pg, na.rm = TRUE),
      league_points_allowed_pg = mean(points_allowed_pg, na.rm = TRUE),
      .groups = "drop"
    )

  team_season |>
    dplyr::left_join(season_baseline, by = "season") |>
    dplyr::mutate(
      offense_vs_league = points_for_pg - league_points_for_pg,
      defense_vs_league = league_points_allowed_pg - points_allowed_pg,
      style = dplyr::case_when(
        offense_vs_league >= 0 & defense_vs_league >= 0 ~ "Balanced contender",
        offense_vs_league >= 0 & defense_vs_league < 0 ~ "Offense-led",
        offense_vs_league < 0 & defense_vs_league >= 0 ~ "Defense-led",
        TRUE ~ "Needs both sides"
      )
    )
}

build_story_data <- function(team_season_styled) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required.")
  }

  list(
    league_trend = team_season_styled |>
      dplyr::group_by(season) |>
      dplyr::summarise(
        avg_points_for_pg = mean(points_for_pg, na.rm = TRUE),
        avg_points_allowed_pg = mean(points_allowed_pg, na.rm = TRUE),
        avg_point_diff_pg = mean(point_diff_pg, na.rm = TRUE),
        .groups = "drop"
      ),
    playoff_style = team_season_styled |>
      dplyr::group_by(era, style) |>
      dplyr::summarise(
        playoff_rate = mean(playoff_flag, na.rm = TRUE),
        n_team_seasons = dplyr::n(),
        .groups = "drop"
      ),
    era_summary = team_season_styled |>
      dplyr::group_by(era) |>
      dplyr::summarise(
        avg_points_for_pg = mean(points_for_pg, na.rm = TRUE),
        avg_points_allowed_pg = mean(points_allowed_pg, na.rm = TRUE),
        avg_win_pct = mean(win_pct, na.rm = TRUE),
        .groups = "drop"
      )
  )
}
