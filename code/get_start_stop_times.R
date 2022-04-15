pacman::p_load(
  tidyverse,
  here,
  beepr
)

# source(here("markdown", "get_streaks.r"))


# create start/stop tibbles function --------------------------------------


get_times <- function(streak_df, var1, var2) {
  var1 <- enquo(var1)
  var2 <- enquo(var2)

  start <-
    streak_df %>%
    filter(!!var1 == "yes") %>%
    select(start = idx_all)

  stop <-
    streak_df %>%
    filter(!!var2 == "yes") %>%
    select(stop = idx_all)

  times <-
    bind_cols(start, stop) %>%
    mutate(duration = ((stop - start + 1) * 10) / 60)
}




# thigh times -------------------------------------------------------------

criterion_thigh_times <-
  thigh_streaks %>%
  get_times(wear_criterion_streak_start, wear_criterion_streak_end)

tree_full_thigh_times <-
  thigh_streaks %>%
  get_times(tree_full_preds_streak_start, tree_full_preds_streak_end)

tree_imp6_thigh_times <-
  thigh_streaks %>%
  get_times(tree_imp6_preds_streak_start, tree_imp6_preds_streak_end)

tree_no_temp_thigh_times <-
  thigh_streaks %>%
  get_times(tree_no_temp_preds_streak_start, tree_no_temp_preds_streak_end)

cz_thigh_times <-
  thigh_streaks %>%
  get_times(wear_time_cz_streak_start, wear_time_cz_streak_end)

heuristic_thigh_times <-
  thigh_streaks %>%
  get_times(wear_heuristic_streak_start, wear_heuristic_streak_end)

sunda_thigh_times <-
  thigh_streaks %>%
  get_times(sunda_preds_streak_start, sunda_preds_streak_end) # TODO problem here!

syed_thigh_times <-
  thigh_streaks %>%
  get_times(syed_preds_streak_start, syed_preds_streak_end)



# hip ---------------------------------------------------------------------

criterion_hip_times <-
  hip_streaks %>%
  get_times(wear_criterion_streak_start, wear_criterion_streak_end)

tree_full_hip_times <-
  hip_streaks %>%
  get_times(tree_full_preds_streak_start, tree_full_preds_streak_end)

tree_imp6_hip_times <-
  hip_streaks %>%
  get_times(tree_imp6_preds_streak_start, tree_imp6_preds_streak_end)

tree_no_temp_hip_times <-
  hip_streaks %>%
  get_times(tree_no_temp_preds_streak_start, tree_no_temp_preds_streak_end)

cz_hip_times <-
  hip_streaks %>%
  get_times(wear_time_cz_streak_start, wear_time_cz_streak_end)

heuristic_hip_times <-
  hip_streaks %>%
  get_times(wear_heuristic_streak_start, wear_heuristic_streak_end)

sunda_hip_times <-
  hip_streaks %>%
  get_times(sunda_preds_streak_start, sunda_preds_streak_end)

syed_hip_times <-
  hip_streaks %>%
  get_times(syed_preds_streak_start, syed_preds_streak_end)


# wrist -------------------------------------------------------------------

criterion_wrist_times <-
  wrist_streaks %>%
  get_times(wear_criterion_streak_start, wear_criterion_streak_end)

tree_full_wrist_times <-
  wrist_streaks %>%
  get_times(tree_full_preds_streak_start, tree_full_preds_streak_end)

tree_imp6_wrist_times <-
  wrist_streaks %>%
  get_times(tree_imp6_preds_streak_start, tree_imp6_preds_streak_end)

tree_no_temp_wrist_times <-
  wrist_streaks %>%
  get_times(tree_no_temp_preds_streak_start, tree_no_temp_preds_streak_end)

cz_wrist_times <-
  wrist_streaks %>%
  get_times(wear_time_cz_streak_start, wear_time_cz_streak_end)

heuristic_wrist_times <-
  wrist_streaks %>%
  get_times(wear_heuristic_streak_start, wear_heuristic_streak_end)

sunda_wrist_times <-
  wrist_streaks %>%
  get_times(sunda_preds_streak_start, sunda_preds_streak_end)

syed_wrist_times <-
  wrist_streaks %>%
  get_times(syed_preds_streak_start, syed_preds_streak_end)

all_times <-
  tibble(
    name = c(
      "thigh_cri", "thigh_full", "thigh_imp6", "thigh_no_temp", "thigh_cz", "thigh_heu", "thigh_sunda", "thigh_syed",
      "hip_cri", "hip_full", "hip_imp6", "hip_no_temp", "hip_cz", "hip_heu", "hip_sunda", "hip_syed",
      "wrist_cri", "wrist_full", "wrist_imp6", "wrist_no_temp", "wrist_cz", "wrist_heu", "wrist_sunda", "wrist_syed"
    ),
    times = list(criterion_thigh_times, tree_full_thigh_times, tree_imp6_thigh_times, 
                 tree_no_temp_thigh_times, cz_thigh_times, heuristic_thigh_times, 
                 sunda_thigh_times, syed_thigh_times,
                 criterion_hip_times, tree_full_hip_times, tree_imp6_hip_times, 
                 tree_no_temp_hip_times, cz_hip_times, heuristic_hip_times, 
                 sunda_hip_times, syed_hip_times,
                 criterion_wrist_times, tree_full_wrist_times, tree_imp6_wrist_times, 
                 tree_no_temp_wrist_times, cz_wrist_times, heuristic_wrist_times, 
                 sunda_wrist_times, syed_wrist_times)
  )
