library(shiny)
library(dplyr)
library(ggplot2)
library(scales)

source("R/nfl_data_prep.R")

team_season <- load_nfl_standings() |>
  prep_nfl_team_season(min_season = 2002) |>
  add_team_style()

ui <- fluidPage(
  titlePanel("NFL Story Explorer: What Predicts Winning?"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "season_range",
        label = "Season range",
        min = min(team_season$season),
        max = max(team_season$season),
        value = c(min(team_season$season), max(team_season$season)),
        sep = ""
      ),
      selectInput(
        inputId = "conf_filter",
        label = "Conference",
        choices = c("All", sort(unique(team_season$conf))),
        selected = "All"
      ),
      selectInput(
        inputId = "team_input",
        label = "Team explorer",
        choices = sort(unique(team_season$team)),
        selected = "KC"
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "League Trends",
          plotOutput("scoring_trend_plot", height = "350px"),
          plotOutput("playoff_style_plot", height = "350px")
        ),
        tabPanel(
          title = "Team Explorer",
          plotOutput("team_trend_plot", height = "380px"),
          tableOutput("team_summary_table")
        ),
        tabPanel(
          title = "Season Snapshot",
          sliderInput(
            inputId = "season_snapshot",
            label = "Choose a season",
            min = min(team_season$season),
            max = max(team_season$season),
            value = max(team_season$season),
            sep = "",
            step = 1
          ),
          plotOutput("snapshot_plot", height = "380px"),
          tableOutput("snapshot_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  filtered_data <- reactive({
    df <- team_season |>
      filter(
        season >= input$season_range[1],
        season <= input$season_range[2]
      )

    if (input$conf_filter != "All") {
      df <- df |>
        filter(conf == input$conf_filter)
    }

    df
  })

  output$scoring_trend_plot <- renderPlot({
    filtered_data() |>
      group_by(season) |>
      summarise(avg_points_for_pg = mean(points_for_pg, na.rm = TRUE), .groups = "drop") |>
      ggplot(aes(x = season, y = avg_points_for_pg)) +
      geom_line(color = "steelblue", linewidth = 1) +
      geom_smooth(method = "loess", se = FALSE, color = "darkorange", linewidth = 1) +
      labs(
        title = "Average points per game over time",
        x = "Season",
        y = "Points per game"
      ) +
      theme_minimal(base_size = 12)
  })

  output$playoff_style_plot <- renderPlot({
    filtered_data() |>
      group_by(style) |>
      summarise(playoff_rate = mean(playoff_flag, na.rm = TRUE), n = n(), .groups = "drop") |>
      mutate(style = reorder(style, playoff_rate)) |>
      ggplot(aes(x = style, y = playoff_rate, fill = style)) +
      geom_col(width = 0.7, show.legend = FALSE) +
      geom_text(aes(label = percent(playoff_rate, accuracy = 0.1)), hjust = -0.1, size = 4) +
      coord_flip() +
      scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
      labs(
        title = "Playoff rate by team style",
        x = "Team style",
        y = "Playoff rate"
      ) +
      theme_minimal(base_size = 12)
  })

  output$team_trend_plot <- renderPlot({
    filtered_data() |>
      filter(team == input$team_input) |>
      select(season, win_pct, point_diff_pg) |>
      tidyr::pivot_longer(cols = c(win_pct, point_diff_pg), names_to = "metric", values_to = "value") |>
      mutate(metric = recode(metric, win_pct = "Winning percentage", point_diff_pg = "Point differential per game")) |>
      ggplot(aes(x = season, y = value, color = metric)) +
      geom_line(linewidth = 1, show.legend = FALSE) +
      geom_point(size = 2, show.legend = FALSE) +
      facet_wrap(~metric, ncol = 1, scales = "free_y") +
      labs(
        title = paste("Performance trend:", input$team_input),
        x = "Season",
        y = "Metric value"
      ) +
      theme_minimal(base_size = 12)
  })

  output$team_summary_table <- renderTable({
    filtered_data() |>
      filter(team == input$team_input) |>
      transmute(
        Season = season,
        Conference = conf,
        Wins = wins,
        Losses = losses,
        Ties = ties,
        `Win %` = percent(win_pct, accuracy = 0.1),
        `Pts/Game` = round(points_for_pg, 1),
        `Opp Pts/Game` = round(points_allowed_pg, 1),
        `Diff/Game` = round(point_diff_pg, 1),
        Playoffs = if_else(playoff_flag, "Yes", "No"),
        Style = style
      ) |>
      arrange(desc(Season))
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  output$snapshot_plot <- renderPlot({
    filtered_data() |>
      filter(season == input$season_snapshot) |>
      ggplot(aes(x = point_diff_pg, y = win_pct, color = playoff_flag, label = team)) +
      geom_point(size = 3, alpha = 0.85) +
      geom_text(check_overlap = TRUE, nudge_y = 0.02, size = 3) +
      geom_smooth(method = "lm", se = FALSE, color = "gray30") +
      scale_color_manual(values = c("TRUE" = "#1b9e77", "FALSE" = "#d95f02"), labels = c("No", "Yes")) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(
        title = paste("Season snapshot:", input$season_snapshot),
        x = "Point differential per game",
        y = "Winning percentage",
        color = "Made playoffs"
      ) +
      theme_minimal(base_size = 12)
  })

  output$snapshot_table <- renderTable({
    filtered_data() |>
      filter(season == input$season_snapshot) |>
      arrange(desc(win_pct), desc(point_diff_pg)) |>
      transmute(
        Team = team,
        `Win %` = percent(win_pct, accuracy = 0.1),
        `Diff/Game` = round(point_diff_pg, 1),
        `Pts/Game` = round(points_for_pg, 1),
        `Opp Pts/Game` = round(points_allowed_pg, 1),
        Style = style,
        Playoffs = if_else(playoff_flag, "Yes", "No")
      )
  }, striped = TRUE, bordered = TRUE, spacing = "xs")
}

shinyApp(ui = ui, server = server)
