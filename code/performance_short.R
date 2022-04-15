pacman::p_load(
  tidyverse,
  here,
  yardstick
)

source(here("markdown", "remove_long_nw.r"))
# Metrics on thigh data ---------------------------------------------------

thigh_metrics <-
  thigh_no_long %>%
  f_meas(wear_criterion, tree_full_pred, event_level = "second") %>%
  bind_rows(thigh_no_long %>% accuracy(wear_criterion, tree_full_pred, event_level = "second")) %>%
  bind_rows(thigh_no_long %>% sensitivity(wear_criterion, tree_full_pred, event_level = "second")) %>%
  bind_rows(thigh_no_long %>% precision(wear_criterion, tree_full_pred, event_level = "second")) %>%
  mutate(
    model = "tree_full",
    dataset = "FASER_thigh"
  ) %>%
  bind_rows(
    thigh_no_long %>%
      f_meas(wear_criterion, tree_imp6_pred, event_level = "second") %>%
      bind_rows(thigh_no_long %>% accuracy(wear_criterion, tree_imp6_pred, event_level = "second")) %>%
      bind_rows(thigh_no_long %>% sensitivity(wear_criterion, tree_imp6_pred, event_level = "second")) %>%
      bind_rows(thigh_no_long %>% precision(wear_criterion, tree_imp6_pred, event_level = "second")) %>%
      mutate(
        model = "tree_imp6",
        dataset = "FASER_thigh"
      )
  ) %>%
  bind_rows(
    thigh_no_long %>%
      f_meas(wear_criterion, tree_no_temp_pred, event_level = "second") %>%
      bind_rows(thigh_no_long %>% accuracy(wear_criterion, tree_no_temp_pred, event_level = "second")) %>%
      bind_rows(thigh_no_long %>% sensitivity(wear_criterion, tree_no_temp_pred, event_level = "second")) %>%
      bind_rows(thigh_no_long %>% precision(wear_criterion, tree_no_temp_pred, event_level = "second")) %>%
      mutate(
        model = "tree_no_temp",
        dataset = "FASER_thigh"
      )
  ) %>%
  bind_rows(
    thigh_no_long %>%
      f_meas(wear_criterion, tree_no_temp_imp5_pred, event_level = "second") %>%
      bind_rows(thigh_no_long %>% accuracy(wear_criterion, tree_no_temp_imp5_pred, event_level = "second")) %>%
      bind_rows(thigh_no_long %>% sensitivity(wear_criterion, tree_no_temp_imp5_pred, event_level = "second")) %>%
      bind_rows(thigh_no_long %>% precision(wear_criterion, tree_no_temp_imp5_pred, event_level = "second")) %>%
      mutate(
        model = "tree_no_temp_imp5",
        dataset = "FASER_thigh"
      )
  ) %>%
  bind_rows(
    thigh_no_long %>%
      f_meas(wear_criterion, syed_pred, event_level = "second") %>%
      bind_rows(thigh_no_long %>% accuracy(wear_criterion, syed_pred, event_level = "second")) %>%
      bind_rows(thigh_no_long %>% sensitivity(wear_criterion, syed_pred, event_level = "second")) %>%
      bind_rows(thigh_no_long %>% precision(wear_criterion, syed_pred, event_level = "second")) %>%
      mutate(
        model = "syed_CNN",
        dataset = "FASER_thigh"
      )
  ) %>%
  bind_rows(
    thigh_no_long %>%
      f_meas(wear_criterion, wear_heuristic, event_level = "second") %>%
      bind_rows(thigh_no_long %>% accuracy(wear_criterion, wear_heuristic, event_level = "second")) %>%
      bind_rows(thigh_no_long %>% sensitivity(wear_criterion, wear_heuristic, event_level = "second")) %>%
      bind_rows(thigh_no_long %>% precision(wear_criterion, wear_heuristic, event_level = "second")) %>%
      mutate(
        model = "heuristic",
        dataset = "FASER_thigh"
      )
  ) %>%
  bind_rows(
    thigh_no_long %>%
      f_meas(wear_criterion, wear_time_cz, event_level = "second") %>%
      bind_rows(thigh_no_long %>% accuracy(wear_criterion, wear_time_cz, event_level = "second")) %>%
      bind_rows(thigh_no_long %>% sensitivity(wear_criterion, wear_time_cz, event_level = "second")) %>%
      bind_rows(thigh_no_long %>% precision(wear_criterion, wear_time_cz, event_level = "second")) %>%
      mutate(
        model = "cz_60",
        dataset = "FASER_thigh"
      )
  ) %>%
  bind_rows(
    thigh_no_long %>%
      f_meas(wear_criterion, sunda_pred, event_level = "second") %>%
      bind_rows(thigh_no_long %>% accuracy(wear_criterion, sunda_pred, event_level = "second")) %>%
      bind_rows(thigh_no_long %>% sensitivity(wear_criterion, sunda_pred, event_level = "second")) %>%
      bind_rows(thigh_no_long %>% precision(wear_criterion, sunda_pred, event_level = "second")) %>%
      mutate(
        model = "sunda_RF",
        dataset = "FASER_thigh"
      )
  )

# Metrics on hip data -----------------------------------------------------

hip_metrics <-
  hip_no_long %>%
  f_meas(wear_criterion, tree_full_pred, event_level = "second") %>%
  bind_rows(hip_no_long %>% accuracy(wear_criterion, tree_full_pred, event_level = "second")) %>%
  bind_rows(hip_no_long %>% sensitivity(wear_criterion, tree_full_pred, event_level = "second")) %>%
  bind_rows(hip_no_long %>% precision(wear_criterion, tree_full_pred, event_level = "second")) %>%
  mutate(
    model = "tree_full",
    dataset = "FASER_hip"
  ) %>%
  bind_rows(
    hip_no_long %>%
      f_meas(wear_criterion, tree_imp6_pred, event_level = "second") %>%
      bind_rows(hip_no_long %>% accuracy(wear_criterion, tree_imp6_pred, event_level = "second")) %>%
      bind_rows(hip_no_long %>% sensitivity(wear_criterion, tree_imp6_pred, event_level = "second")) %>%
      bind_rows(hip_no_long %>% precision(wear_criterion, tree_imp6_pred, event_level = "second")) %>%
      mutate(
        model = "tree_imp6",
        dataset = "FASER_hip"
      )
  ) %>%
  bind_rows(
    hip_no_long %>%
      f_meas(wear_criterion, tree_no_temp_pred, event_level = "second") %>%
      bind_rows(hip_no_long %>% accuracy(wear_criterion, tree_no_temp_pred, event_level = "second")) %>%
      bind_rows(hip_no_long %>% sensitivity(wear_criterion, tree_no_temp_pred, event_level = "second")) %>%
      bind_rows(hip_no_long %>% precision(wear_criterion, tree_no_temp_pred, event_level = "second")) %>%
      mutate(
        model = "tree_no_temp",
        dataset = "FASER_hip"
      )
  ) %>%
  bind_rows(
    hip_no_long %>%
      f_meas(wear_criterion, tree_no_temp_imp5_pred, event_level = "second") %>%
      bind_rows(hip_no_long %>% accuracy(wear_criterion, tree_no_temp_imp5_pred, event_level = "second")) %>%
      bind_rows(hip_no_long %>% sensitivity(wear_criterion, tree_no_temp_imp5_pred, event_level = "second")) %>%
      bind_rows(hip_no_long %>% precision(wear_criterion, tree_no_temp_imp5_pred, event_level = "second")) %>%
      mutate(
        model = "tree_no_temp_imp5",
        dataset = "FASER_hip"
      )
  ) %>%
  bind_rows(
    hip_no_long %>%
      f_meas(wear_criterion, syed_pred, event_level = "second") %>%
      bind_rows(hip_no_long %>% accuracy(wear_criterion, syed_pred, event_level = "second")) %>%
      bind_rows(hip_no_long %>% sensitivity(wear_criterion, syed_pred, event_level = "second")) %>%
      bind_rows(hip_no_long %>% precision(wear_criterion, syed_pred, event_level = "second")) %>%
      mutate(
        model = "syed_CNN",
        dataset = "FASER_hip"
      )
  ) %>%
  bind_rows(
    hip_no_long %>%
      f_meas(wear_criterion, wear_heuristic, event_level = "second") %>%
      bind_rows(hip_no_long %>% accuracy(wear_criterion, wear_heuristic, event_level = "second")) %>%
      bind_rows(hip_no_long %>% sensitivity(wear_criterion, wear_heuristic, event_level = "second")) %>%
      bind_rows(hip_no_long %>% precision(wear_criterion, wear_heuristic, event_level = "second")) %>%
      mutate(
        model = "heuristic",
        dataset = "FASER_hip"
      )
  ) %>%
  bind_rows(
    hip_no_long %>%
      f_meas(wear_criterion, wear_time_cz, event_level = "second") %>%
      bind_rows(hip_no_long %>% accuracy(wear_criterion, wear_time_cz, event_level = "second")) %>%
      bind_rows(hip_no_long %>% sensitivity(wear_criterion, wear_time_cz, event_level = "second")) %>%
      bind_rows(hip_no_long %>% precision(wear_criterion, wear_time_cz, event_level = "second")) %>%
      mutate(
        model = "cz_60",
        dataset = "FASER_hip"
      )
  ) %>%
  bind_rows(
    hip_no_long %>%
      f_meas(wear_criterion, sunda_pred, event_level = "second") %>%
      bind_rows(hip_no_long %>% accuracy(wear_criterion, sunda_pred, event_level = "second")) %>%
      bind_rows(hip_no_long %>% sensitivity(wear_criterion, sunda_pred, event_level = "second")) %>%
      bind_rows(hip_no_long %>% precision(wear_criterion, sunda_pred, event_level = "second")) %>%
      mutate(
        model = "sunda_RF",
        dataset = "FASER_hip"
      )
  )


# Metrics on wrist data ---------------------------------------------------

wrist_metrics <-
  wrist_no_long %>%
  f_meas(wear_criterion, tree_full_pred, event_level = "second") %>%
  bind_rows(wrist_no_long %>% accuracy(wear_criterion, tree_full_pred, event_level = "second")) %>%
  bind_rows(wrist_no_long %>% sensitivity(wear_criterion, tree_full_pred, event_level = "second")) %>%
  bind_rows(wrist_no_long %>% precision(wear_criterion, tree_full_pred, event_level = "second")) %>%
  mutate(
    model = "tree_full",
    dataset = "Malthe_wrist"
  ) %>%
  bind_rows(
    wrist_no_long %>%
      f_meas(wear_criterion, tree_imp6_pred, event_level = "second") %>%
      bind_rows(wrist_no_long %>% accuracy(wear_criterion, tree_imp6_pred, event_level = "second")) %>%
      bind_rows(wrist_no_long %>% sensitivity(wear_criterion, tree_imp6_pred, event_level = "second")) %>%
      bind_rows(wrist_no_long %>% precision(wear_criterion, tree_imp6_pred, event_level = "second")) %>%
      mutate(
        model = "tree_imp6",
        dataset = "Malthe_wrist"
      )
  ) %>%
  bind_rows(
    wrist_no_long %>%
      f_meas(wear_criterion, tree_no_temp_pred, event_level = "second") %>%
      bind_rows(wrist_no_long %>% accuracy(wear_criterion, tree_no_temp_pred, event_level = "second")) %>%
      bind_rows(wrist_no_long %>% sensitivity(wear_criterion, tree_no_temp_pred, event_level = "second")) %>%
      bind_rows(wrist_no_long %>% precision(wear_criterion, tree_no_temp_pred, event_level = "second")) %>%
      mutate(
        model = "tree_no_temp",
        dataset = "Malthe_wrist"
      )
  ) %>%
  bind_rows(
    wrist_no_long %>%
      f_meas(wear_criterion, tree_no_temp_imp5_pred, event_level = "second") %>%
      bind_rows(wrist_no_long %>% accuracy(wear_criterion, tree_no_temp_imp5_pred, event_level = "second")) %>%
      bind_rows(wrist_no_long %>% sensitivity(wear_criterion, tree_no_temp_imp5_pred, event_level = "second")) %>%
      bind_rows(wrist_no_long %>% precision(wear_criterion, tree_no_temp_imp5_pred, event_level = "second")) %>%
      mutate(
        model = "tree_no_temp_imp5",
        dataset = "Malthe_wrist"
      )
  ) %>%
  bind_rows(
    wrist_no_long %>%
      f_meas(wear_criterion, sunda_pred, event_level = "second") %>%
      bind_rows(wrist_no_long %>% accuracy(wear_criterion, sunda_pred, event_level = "second")) %>%
      bind_rows(wrist_no_long %>% sensitivity(wear_criterion, sunda_pred, event_level = "second")) %>%
      bind_rows(wrist_no_long %>% precision(wear_criterion, sunda_pred, event_level = "second")) %>%
      mutate(
        model = "sunda_RF",
        dataset = "Malthe_wrist"
      )
  ) %>%
  bind_rows(
    wrist_no_long %>%
      f_meas(wear_criterion, wear_heuristic, event_level = "second") %>%
      bind_rows(wrist_no_long %>% accuracy(wear_criterion, wear_heuristic, event_level = "second")) %>%
      bind_rows(wrist_no_long %>% sensitivity(wear_criterion, wear_heuristic, event_level = "second")) %>%
      bind_rows(wrist_no_long %>% precision(wear_criterion, wear_heuristic, event_level = "second")) %>%
      mutate(
        model = "heuristic",
        dataset = "Malthe_wrist"
      )
  ) %>%
  bind_rows(
    wrist_no_long %>%
      f_meas(wear_criterion, wear_time_cz, event_level = "second") %>%
      bind_rows(wrist_no_long %>% accuracy(wear_criterion, wear_time_cz, event_level = "second")) %>%
      bind_rows(wrist_no_long %>% sensitivity(wear_criterion, wear_time_cz, event_level = "second")) %>%
      bind_rows(wrist_no_long %>% precision(wear_criterion, wear_time_cz, event_level = "second")) %>%
      mutate(
        model = "cz_60",
        dataset = "Malthe_wrist"
      )
  ) %>%
  bind_rows(
    wrist_no_long %>%
      f_meas(wear_criterion, syed_pred, event_level = "second") %>%
      bind_rows(wrist_no_long %>% accuracy(wear_criterion, syed_pred, event_level = "second")) %>%
      bind_rows(wrist_no_long %>% sensitivity(wear_criterion, syed_pred, event_level = "second")) %>%
      bind_rows(wrist_no_long %>% precision(wear_criterion, syed_pred, event_level = "second")) %>%
      mutate(
        model = "syed_CNN",
        dataset = "Malthe_wrist"
      )
  )


# Combine all metrics -----------------------------------------------------

all_metrics <-
  bind_rows(thigh_metrics, hip_metrics, wrist_metrics)
