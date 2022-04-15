pacman::p_load(
  tidyverse,
  here,
  beepr
)

# source(here("markdown", "get_start_stop_times.r"))

# remove long nw ----------------------------------------------------------

remove_long_nw <-
  function(df, times) {
    df %>%
      rowwise() %>%
      filter(!any(idx_all >= times$start & idx_all <= times$stop)) %>% 
      ungroup()
  }


# thigh -------------------------------------------------------------------

tictoc::tic()
thigh_tree_full_no_long <-
  remove_long_nw(faser_thigh, tree_full_thigh_times)

thigh_imp6_no_long <-
  remove_long_nw(faser_thigh, tree_imp6_thigh_times)

thigh_no_temp_no_long <- 
  remove_long_nw(faser_thigh, tree_no_temp_thigh_times)

thigh_cz_no_long <- 
  remove_long_nw(faser_thigh, cz_thigh_times)

thigh_heu_no_long <- 
  remove_long_nw(faser_thigh, heuristic_thigh_times)

thigh_sunda_no_long <- 
  remove_long_nw(faser_thigh, sunda_hip_times)

thigh_syed_no_long <- 
  remove_long_nw(faser_thigh, syed_thigh_times)
tictoc::toc()
beep(4)

# hip ---------------------------------------------------------------------

tictoc::tic()
hip_tree_full_no_long <-
  remove_long_nw(faser_hip, tree_full_hip_times)

hip_imp6_no_long <-
  remove_long_nw(faser_hip, tree_imp6_hip_times)

hip_no_temp_no_long <- 
  remove_long_nw(faser_hip, tree_no_temp_hip_times)

hip_cz_no_long <- 
  remove_long_nw(faser_hip, cz_hip_times)

hip_heu_no_long <- 
  remove_long_nw(faser_hip, heuristic_hip_times)

hip_sunda_no_long <- 
  remove_long_nw(faser_hip, sunda_hip_times)

hip_syed_no_long <- 
  remove_long_nw(faser_hip, syed_hip_times)
tictoc::toc()
beep(4)

# wrist -------------------------------------------------------------------

tictoc::tic()
wrist_tree_full_no_long <-
  remove_long_nw(malthe_wrist, tree_full_wrist_times)

wrist_imp6_no_long <-
  remove_long_nw(malthe_wrist, tree_imp6_wrist_times)

wrist_no_temp_no_long <- 
  remove_long_nw(malthe_wrist, tree_no_temp_wrist_times)

wrist_cz_no_long <- 
  remove_long_nw(malthe_wrist, cz_wrist_times)

wrist_heu_no_long <- 
  remove_long_nw(malthe_wrist, heuristic_wrist_times)

wrist_sunda_no_long <- 
  remove_long_nw(malthe_wrist, sunda_wrist_times)

wrist_syed_no_long <- 
  remove_long_nw(malthe_wrist, syed_wrist_times)
tictoc::toc()
beep(9)
