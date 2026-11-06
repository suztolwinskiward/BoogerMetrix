devtools::load_all(".")

## 1. Single game
schedule_2020 <- get_season_schedule(2020)
cat("2020 completed regular-season games:", nrow(schedule_2020), "\n")

one_game_pk <- schedule_2020$game_pk[1]
pbp1 <- fetch_game_pbp_cached(one_game_pk)
cat("game", one_game_pk, "pbp rows:", nrow(pbp1), "\n")

pa1 <- build_pa_table(pbp1)
cat("plate appearances:", nrow(pa1), "\n")
print(pa1[, .(batter_name, team, result_event, is_walk, is_official_ab, total_bases)][1:10])

pg1 <- aggregate_player_game(pa1)
cat("\nplayer-game rows for this one game:\n")
print(pg1)

## 2. Cache-hit re-run check
t0 <- Sys.time()
pbp1_again <- fetch_game_pbp_cached(one_game_pk)
t1 <- Sys.time()
cat("\nre-fetch (should be cache hit) took:", as.numeric(t1 - t0), "sec; identical:",
    identical(pbp1, pbp1_again), "\n")

## 3. Small multi-game aggregate sanity check (first 10 games of 2020)
sample_pks <- schedule_2020$game_pk[1:10]
pa_list <- list()
for (gp in sample_pks) {
  pbp <- fetch_game_pbp_cached(gp)
  if (!is.null(pbp)) pa_list[[as.character(gp)]] <- build_pa_table(pbp)
}
pa_all <- data.table::rbindlist(pa_list)
pg_all <- aggregate_player_game(pa_all)

cat("\n--- 10-game aggregate sanity check ---\n")
cat("total PA:", sum(pg_all$PA), " total AB:", sum(pg_all$AB),
    " total BB:", sum(pg_all$BB), " total TB:", sum(pg_all$TB), "\n")
cat("league BB rate (BB/PA):", sum(pg_all$BB) / sum(pg_all$PA), "\n")
cat("league SLG (TB/AB):", sum(pg_all$TB) / sum(pg_all$AB), "\n")

## 4. Rolling stats on the small sample, both window types
pg_all_multi <- data.table::copy(pg_all)
# duplicate as if across several dates isn't real here (small sample), just confirm function runs
roll_games <- add_rolling_stats(pg_all, window_length = 3, window_type = "games", min_pa = 1)
roll_days <- add_rolling_stats(pg_all, window_length = 7, window_type = "days", min_pa = 1)
cat("\nroll (games) sample:\n")
print(roll_games[1:5, .(batter_name, game_date, PA, roll_PA, roll_bb_rate, roll_slg)])
cat("\nroll (days) sample:\n")
print(roll_days[1:5, .(batter_name, game_date, PA, roll_PA, roll_bb_rate, roll_slg)])

cat("\nDONE\n")
