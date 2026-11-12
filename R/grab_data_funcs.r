#' Get completed games for a season
#'
#' Wraps [baseballr::mlb_schedule()] to return the completed games for a
#' single MLB season, filtered by game type.
#'
#' @param season Integer season year, e.g. `2020`.
#' @param game_types Character vector of MLB game type codes to keep.
#'   Defaults to `"R"` (regular season only). Pass e.g. `c("R", "F", "D",
#'   "L", "W")` to include postseason rounds (wild card, division series,
#'   league championship series, world series) as well.
#' @param level_ids Sport level id passed to `mlb_schedule()`; `1` is MLB.
#'
#' @return A [data.table::data.table()] with one row per completed game:
#'   `game_pk`, `season`, `game_type`, `game_date`, `home_team`, `away_team`.
#' @export
get_season_schedule <- function(season, game_types = "R", level_ids = 1) {
  sch <- baseballr::mlb_schedule(season = season, level_ids = level_ids)
  sch <- data.table::as.data.table(sch)

  completed_states <- c("Final", "Completed Early")
  keep <- sch$game_type %in% game_types & sch$status_detailed_state %in% completed_states

  out <- sch[keep, .(
    game_pk = game_pk,
    season = season,
    game_type = game_type,
    game_date = as.Date(official_date),
    home_team = teams_home_team_name,
    away_team = teams_away_team_name
  )]

  data.table::setorder(out, game_date, game_pk)
  out[]
}

#' Fetch and cache one game's play-by-play data
#'
#' Downloads play-by-play data for a single game via [baseballr::mlb_pbp()],
#' caching the raw result to disk so repeated calls for the same game avoid
#' re-hitting the network.
#'
#' @param game_pk Integer MLB game primary key.
#' @param cache_dir Directory to store cached `.rds` files in. Created if it
#'   does not already exist.
#' @param overwrite If `TRUE`, re-download even if a cache file exists.
#'
#' @return A [data.table::data.table()] of play-by-play rows for the game, or
#'   `NULL` (with a warning) if the download fails.
#' @export
fetch_game_pbp_cached <- function(game_pk, cache_dir = "data-raw/pbp_cache", overwrite = FALSE) {
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  cache_file <- file.path(cache_dir, paste0("game_", game_pk, ".rds"))

  if (file.exists(cache_file) && !overwrite) {
    return(readRDS(cache_file))
  }

  pbp <- tryCatch(
    data.table::as.data.table(baseballr::mlb_pbp(game_pk)),
    error = function(e) {
      warning(sprintf("Failed to fetch pbp for game_pk %s: %s", game_pk, conditionMessage(e)))
      NULL
    }
  )

  if (!is.null(pbp) && nrow(pbp) > 0) {
    saveRDS(pbp, cache_file, compress = TRUE)
  }

  pbp
}

#' Download and cache play-by-play data for one or more seasons
#'
#' Orchestrates [get_season_schedule()] and [fetch_game_pbp_cached()] across
#' a vector of seasons: builds each season's completed-game schedule, then
#' fetches (or reuses the cached copy of) every game's play-by-play data.
#' Safe to interrupt and re-run — already-cached games are skipped unless
#' `overwrite = TRUE`.
#'
#' @param seasons Integer vector of season years, e.g. `c(2020, 2026)`.
#' @param game_types Character vector of game type codes; see
#'   [get_season_schedule()]. Defaults to `"R"` (regular season only).
#' @param cache_dir Directory for cached per-game `.rds` files.
#' @param overwrite If `TRUE`, re-download every game even if cached.
#' @param delay Seconds to sleep after each network download (not after
#'   cache hits), to avoid hammering the MLB Stats API.
#' @param verbose If `TRUE`, print progress messages.
#'
#' @return A [data.table::data.table()] summarizing the outcome for every
#'   game considered: `season`, `game_pk`, `game_date`, `status` (one of
#'   `"cache_hit"`, `"downloaded"`, `"failed"`).
#' @export
grab_season_data <- function(seasons, game_types = "R", cache_dir = "data-raw/pbp_cache",
                              overwrite = FALSE, delay = 0.5, verbose = TRUE) {
  summaries <- list()

  for (season in seasons) {
    if (verbose) message(sprintf("Building schedule for season %s...", season))
    schedule <- get_season_schedule(season, game_types = game_types)

    for (i in seq_len(nrow(schedule))) {
      game_pk <- schedule$game_pk[i]
      cache_file <- file.path(cache_dir, paste0("game_", game_pk, ".rds"))
      was_cached <- file.exists(cache_file) && !overwrite

      pbp <- fetch_game_pbp_cached(game_pk, cache_dir = cache_dir, overwrite = overwrite)

      status <- if (is.null(pbp)) {
        "failed"
      } else if (was_cached) {
        "cache_hit"
      } else {
        "downloaded"
      }

      if (!was_cached && status != "failed") {
        Sys.sleep(delay)
      }

      if (verbose && (i %% 50 == 0 || i == nrow(schedule))) {
        message(sprintf("  season %s: %d/%d games processed", season, i, nrow(schedule)))
      }

      summaries[[length(summaries) + 1]] <- data.table::data.table(
        season = season,
        game_pk = game_pk,
        game_date = schedule$game_date[i],
        status = status
      )
    }
  }

  data.table::rbindlist(summaries)
}
