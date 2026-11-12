#' Collapse raw play-by-play rows to one row per plate appearance
#'
#' Takes raw pitch-level play-by-play data (as returned by
#' [baseballr::mlb_pbp()] / [fetch_game_pbp_cached()]) and reduces it to one
#' row per completed plate appearance, with the fields needed to compute walk
#' rate and slugging percentage: whether the plate appearance was a walk,
#' whether it counts as an official at-bat, and how many total bases (if any)
#' it produced.
#'
#' Total-base and at-bat-exclusion rules follow the official scoring
#' definitions: walks, hit-by-pitch, sacrifice flies, sacrifice bunts, and
#' catcher's interference do not count as at-bats; only singles, doubles,
#' triples, and home runs count as hits and contribute total bases.
#'
#' @param pbp A data.table (or data.frame) of raw play-by-play rows, covering
#'   one or more games. Column names follow [baseballr::mlb_pbp()]'s
#'   dotted-name convention (e.g. `result.event`, `matchup.batter.fullName`).
#' @param keep_last_pitch_only If `TRUE` (default), keep only the row marked
#'   `last.pitch.of.ab == "true"` for each plate appearance — the mlb_pbp
#'   convention for the pitch that carries the finalized result of the at-bat.
#'
#' @return A [data.table::data.table()] with one row per plate appearance:
#'   `game_pk`, `game_date`, `batter_id`, `batter_name`, `team`,
#'   `at_bat_index`, `result_event`, `is_walk`, `is_official_ab`, `is_hit`,
#'   `total_bases`.
#' @export
build_pa_table <- function(pbp, keep_last_pitch_only = TRUE) {
  pbp <- data.table::as.data.table(pbp)

  if (keep_last_pitch_only) {
    pbp <- pbp[last.pitch.of.ab == "true"]
  }

  walk_events <- c("Walk", "Intent Walk")
  ab_exclusion_events <- c(
    "Walk", "Intent Walk", "Hit By Pitch",
    "Sac Fly", "Sac Fly Double Play", "Sac Bunt", "Sac Bunt Double Play",
    "Catcher Interference"
  )
  hit_events <- c("Single", "Double", "Triple", "Home Run")

  pa <- pbp[, .(
    game_pk = game_pk,
    game_date = as.Date(game_date),
    batter_id = matchup.batter.id,
    batter_name = matchup.batter.fullName,
    team = batting_team,
    at_bat_index = about.atBatIndex,
    result_event = result.event,
    is_walk = result.event %in% walk_events,
    is_official_ab = !(result.event %in% ab_exclusion_events),
    is_hit = result.event %in% hit_events,
    total_bases = data.table::fcase(
      result.event == "Single", 1L,
      result.event == "Double", 2L,
      result.event == "Triple", 3L,
      result.event == "Home Run", 4L,
      default = 0L
    )
  )]

  data.table::setorder(pa, game_date, game_pk, at_bat_index)
  pa[]
}

#' Aggregate plate-appearance data to player-game level
#'
#' @param pa_table A data.table as returned by [build_pa_table()].
#'
#' @return A [data.table::data.table()] with one row per player per game:
#'   `batter_id`, `batter_name`, `team`, `season`, `game_date`, `game_pk`,
#'   `PA` (plate appearances), `AB` (official at-bats), `BB` (walks),
#'   `H` (hits), `TB` (total bases). `season` is derived from the calendar
#'   year of `game_date`, which holds for regular-season and postseason MLB
#'   games alike (a season's postseason never crosses into the next
#'   calendar year).
#' @export
aggregate_player_game <- function(pa_table) {
  out <- pa_table[, .(
    PA = .N,
    AB = sum(is_official_ab),
    BB = sum(is_walk),
    H = sum(is_hit),
    TB = sum(total_bases)
  ), by = .(
    batter_id, batter_name, team,
    season = data.table::year(game_date),
    game_date, game_pk
  )]

  data.table::setorder(out, batter_id, game_date, game_pk)
  out[]
}

#' Add rolling-window walk rate and slugging percentage
#'
#' Computes a trailing moving-window average of walk rate (BB / PA) and
#' slugging percentage (TB / AB) for each player, within season, ordered by
#' game date. Supports windows defined either as a fixed number of trailing
#' games or a fixed number of trailing calendar days.
#'
#' @param player_game_dt A data.table as returned by [aggregate_player_game()].
#' @param window_length Size of the trailing window: a games count when
#'   `window_type = "games"`, or a number of calendar days when
#'   `window_type = "days"`.
#' @param window_type Either `"games"` (default) for a trailing-N-games
#'   window, or `"days"` for a trailing-N-calendar-days window.
#' @param min_pa Minimum plate appearances (for walk rate) or at-bats (for
#'   slugging) required within the window before a rate is reported;
#'   windows below this threshold return `NA` instead of a noisy rate.
#'
#' @return A copy of `player_game_dt` with added columns `roll_PA`, `roll_AB`,
#'   `roll_BB`, `roll_TB` (window sums) and `roll_bb_rate`, `roll_slg`
#'   (window rates).
#' @export
add_rolling_stats <- function(player_game_dt, window_length,
                               window_type = c("games", "days"), min_pa = 1) {
  window_type <- match.arg(window_type)
  dt <- data.table::copy(player_game_dt)
  data.table::setorder(dt, batter_id, season, game_date, game_pk)

  if (window_type == "games") {
    dt[, `:=`(
      roll_PA = data.table::frollsum(PA, window_length, align = "right"),
      roll_AB = data.table::frollsum(AB, window_length, align = "right"),
      roll_BB = data.table::frollsum(BB, window_length, align = "right"),
      roll_TB = data.table::frollsum(TB, window_length, align = "right")
    ), by = .(batter_id, season)]
  } else {
    dt[, c("roll_PA", "roll_AB", "roll_BB", "roll_TB") := {
      window_start <- game_date - window_length + 1
      idx <- lapply(seq_len(.N), function(i) {
        which(game_date >= window_start[i] & game_date <= game_date[i])
      })
      list(
        vapply(idx, function(ix) sum(PA[ix]), numeric(1)),
        vapply(idx, function(ix) sum(AB[ix]), numeric(1)),
        vapply(idx, function(ix) sum(BB[ix]), numeric(1)),
        vapply(idx, function(ix) sum(TB[ix]), numeric(1))
      )
    }, by = .(batter_id, season)]
  }

  dt[, roll_bb_rate := data.table::fifelse(roll_PA >= min_pa, roll_BB / roll_PA, NA_real_)]
  dt[, roll_slg := data.table::fifelse(roll_AB >= min_pa, roll_TB / roll_AB, NA_real_)]
  dt[]
}
