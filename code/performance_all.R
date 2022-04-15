pacman::p_load(
  tidyverse,
  here,
  yardstick
)
# Load datasets including predictions --------------------------------------

# thigh_preds <-
#   read_rds(here("data", "faser_test_thigh_with_predictions.rds"))
# 
# thigh_preds <-
#   read_rds(here("data", "faser_test_hip_with_predictions.rds"))

new_test_faser_with_preds <- read_rds(here("data", "new_test_faser_with_preds.rds"))

malthe_nw_preds <-
  read_rds(here("data", "malthe_nw_with_predictions.rds"))

# split faser into thigh and hip
thigh_preds <- 
  new_test_faser_with_preds %>% 
  filter(location == 1) %>% 
  mutate(sunda_pred = factor(sunda_pred),
         wear_time_cz = if_else(wear_time_cz == 1, 0, 1) %>% 
           factor()) %>% 
  rename(sunda_pred = sunda_pred)
  

hip_preds <- 
  new_test_faser_with_preds %>% 
  filter(location == 0) %>% 
  mutate(sunda_pred = factor(sunda_pred),
         wear_time_cz = if_else(wear_time_cz == 1, 0, 1) %>% 
           factor()) %>% 
  rename(sunda_pred = sunda_pred)

# Metrics on thigh data ---------------------------------------------------

thigh_metrics <-
  thigh_preds %>%
  f_meas(wear_criterion, tree_full_preds, event_level = "second") %>%
  bind_rows(thigh_preds %>% accuracy(wear_criterion, tree_full_preds, event_level = "second")) %>%
  bind_rows(thigh_preds %>% sensitivity(wear_criterion, tree_full_preds, event_level = "second")) %>%
  bind_rows(thigh_preds %>% precision(wear_criterion, tree_full_preds, event_level = "second")) %>%
  mutate(
    model = "tree_full",
    dataset = "FASER_thigh"
  ) %>%
  bind_rows(
    thigh_preds %>%
      f_meas(wear_criterion, tree_imp6_preds, event_level = "second") %>%
      bind_rows(thigh_preds %>% accuracy(wear_criterion, tree_imp6_preds, event_level = "second")) %>%
      bind_rows(thigh_preds %>% sensitivity(wear_criterion, tree_imp6_preds, event_level = "second")) %>%
      bind_rows(thigh_preds %>% precision(wear_criterion, tree_imp6_preds, event_level = "second")) %>%
      mutate(
        model = "tree_imp6",
        dataset = "FASER_thigh"
      )
  ) %>%
  bind_rows(
    thigh_preds %>%
      f_meas(wear_criterion, tree_no_temp_preds, event_level = "second") %>%
      bind_rows(thigh_preds %>% accuracy(wear_criterion, tree_no_temp_preds, event_level = "second")) %>%
      bind_rows(thigh_preds %>% sensitivity(wear_criterion, tree_no_temp_preds, event_level = "second")) %>%
      bind_rows(thigh_preds %>% precision(wear_criterion, tree_no_temp_preds, event_level = "second")) %>%
      mutate(
        model = "tree_no_temp",
        dataset = "FASER_thigh"
      )
  ) %>%
  # bind_rows(
  #   thigh_preds %>%
  #     f_meas(wear_criterion, tree_no_temp_imp5_pred, event_level = "second") %>%
  #     bind_rows(thigh_preds %>% accuracy(wear_criterion, tree_no_temp_imp5_pred, event_level = "second")) %>%
  #     bind_rows(thigh_preds %>% sensitivity(wear_criterion, tree_no_temp_imp5_pred, event_level = "second")) %>%
  #     bind_rows(thigh_preds %>% precision(wear_criterion, tree_no_temp_imp5_pred, event_level = "second")) %>%
  #     mutate(
  #       model = "tree_no_temp_imp5",
  #       dataset = "FASER_thigh"
  #     )
  # ) %>%
  bind_rows(
    thigh_preds %>%
      f_meas(wear_criterion, syed_preds, event_level = "second") %>%
      bind_rows(thigh_preds %>% accuracy(wear_criterion, syed_preds, event_level = "second")) %>%
      bind_rows(thigh_preds %>% sensitivity(wear_criterion, syed_preds, event_level = "second")) %>%
      bind_rows(thigh_preds %>% precision(wear_criterion, syed_preds, event_level = "second")) %>%
      mutate(
        model = "syed_CNN",
        dataset = "FASER_thigh"
      )
  ) %>%
  bind_rows(
    thigh_preds %>%
      f_meas(wear_criterion, wear_heuristic, event_level = "second") %>%
      bind_rows(thigh_preds %>% accuracy(wear_criterion, wear_heuristic, event_level = "second")) %>%
      bind_rows(thigh_preds %>% sensitivity(wear_criterion, wear_heuristic, event_level = "second")) %>%
      bind_rows(thigh_preds %>% precision(wear_criterion, wear_heuristic, event_level = "second")) %>%
      mutate(
        model = "heuristic",
        dataset = "FASER_thigh"
      )
  ) %>%
  bind_rows(
    thigh_preds %>%
      f_meas(wear_criterion, wear_time_cz, event_level = "second") %>%
      bind_rows(thigh_preds %>% accuracy(wear_criterion, wear_time_cz, event_level = "second")) %>%
      bind_rows(thigh_preds %>% sensitivity(wear_criterion, wear_time_cz, event_level = "second")) %>%
      bind_rows(thigh_preds %>% precision(wear_criterion, wear_time_cz, event_level = "second")) %>%
      mutate(
        model = "cz_60",
        dataset = "FASER_thigh"
      )
  ) %>%
  bind_rows(
    thigh_preds %>%
      f_meas(wear_criterion, sunda_pred, event_level = "second") %>%
      bind_rows(thigh_preds %>% accuracy(wear_criterion, sunda_pred, event_level = "second")) %>%
      bind_rows(thigh_preds %>% sensitivity(wear_criterion, sunda_pred, event_level = "second")) %>%
      bind_rows(thigh_preds %>% precision(wear_criterion, sunda_pred, event_level = "second")) %>%
      mutate(
        model = "sunda_RF",
        dataset = "FASER_thigh"
      )
  )

# Metrics on hip data -----------------------------------------------------

hip_metrics <-
  thigh_preds %>%
  f_meas(wear_criterion, tree_full_preds, event_level = "second") %>%
  bind_rows(thigh_preds %>% accuracy(wear_criterion, tree_full_preds, event_level = "second")) %>%
  bind_rows(thigh_preds %>% sensitivity(wear_criterion, tree_full_preds, event_level = "second")) %>%
  bind_rows(thigh_preds %>% precision(wear_criterion, tree_full_preds, event_level = "second")) %>%
  mutate(
    model = "tree_full",
    dataset = "FASER_hip"
  ) %>%
  bind_rows(
    thigh_preds %>%
      f_meas(wear_criterion, tree_imp6_preds, event_level = "second") %>%
      bind_rows(thigh_preds %>% accuracy(wear_criterion, tree_imp6_preds, event_level = "second")) %>%
      bind_rows(thigh_preds %>% sensitivity(wear_criterion, tree_imp6_preds, event_level = "second")) %>%
      bind_rows(thigh_preds %>% precision(wear_criterion, tree_imp6_preds, event_level = "second")) %>%
      mutate(
        model = "tree_imp6",
        dataset = "FASER_hip"
      )
  ) %>%
  bind_rows(
    thigh_preds %>%
      f_meas(wear_criterion, tree_no_temp_preds, event_level = "second") %>%
      bind_rows(thigh_preds %>% accuracy(wear_criterion, tree_no_temp_preds, event_level = "second")) %>%
      bind_rows(thigh_preds %>% sensitivity(wear_criterion, tree_no_temp_preds, event_level = "second")) %>%
      bind_rows(thigh_preds %>% precision(wear_criterion, tree_no_temp_preds, event_level = "second")) %>%
      mutate(
        model = "tree_no_temp",
        dataset = "FASER_hip"
      )
  ) %>%
  # bind_rows(
  #   thigh_preds %>%
  #     f_meas(wear_criterion, tree_no_temp_imp5_pred, event_level = "second") %>%
  #     bind_rows(thigh_preds %>% accuracy(wear_criterion, tree_no_temp_imp5_pred, event_level = "second")) %>%
  #     bind_rows(thigh_preds %>% sensitivity(wear_criterion, tree_no_temp_imp5_pred, event_level = "second")) %>%
  #     bind_rows(thigh_preds %>% precision(wear_criterion, tree_no_temp_imp5_pred, event_level = "second")) %>%
  #     mutate(
  #       model = "tree_no_temp_imp5",
  #       dataset = "FASER_hip"
  #     )
  # ) %>%
  bind_rows(
    thigh_preds %>%
      f_meas(wear_criterion, syed_preds, event_level = "second") %>%
      bind_rows(thigh_preds %>% accuracy(wear_criterion, syed_preds, event_level = "second")) %>%
      bind_rows(thigh_preds %>% sensitivity(wear_criterion, syed_preds, event_level = "second")) %>%
      bind_rows(thigh_preds %>% precision(wear_criterion, syed_preds, event_level = "second")) %>%
      mutate(
        model = "syed_CNN",
        dataset = "FASER_hip"
      )
  ) %>%
  bind_rows(
    thigh_preds %>%
      f_meas(wear_criterion, wear_heuristic, event_level = "second") %>%
      bind_rows(thigh_preds %>% accuracy(wear_criterion, wear_heuristic, event_level = "second")) %>%
      bind_rows(thigh_preds %>% sensitivity(wear_criterion, wear_heuristic, event_level = "second")) %>%
      bind_rows(thigh_preds %>% precision(wear_criterion, wear_heuristic, event_level = "second")) %>%
      mutate(
        model = "heuristic",
        dataset = "FASER_hip"
      )
  ) %>%
  bind_rows(
    thigh_preds %>%
      f_meas(wear_criterion, wear_time_cz, event_level = "second") %>%
      bind_rows(thigh_preds %>% accuracy(wear_criterion, wear_time_cz, event_level = "second")) %>%
      bind_rows(thigh_preds %>% sensitivity(wear_criterion, wear_time_cz, event_level = "second")) %>%
      bind_rows(thigh_preds %>% precision(wear_criterion, wear_time_cz, event_level = "second")) %>%
      mutate(
        model = "cz_60",
        dataset = "FASER_hip"
      )
  ) %>%
  bind_rows(
    thigh_preds %>%
      f_meas(wear_criterion, sunda_pred, event_level = "second") %>%
      bind_rows(thigh_preds %>% accuracy(wear_criterion, sunda_pred, event_level = "second")) %>%
      bind_rows(thigh_preds %>% sensitivity(wear_criterion, sunda_pred, event_level = "second")) %>%
      bind_rows(thigh_preds %>% precision(wear_criterion, sunda_pred, event_level = "second")) %>%
      mutate(
        model = "sunda_RF",
        dataset = "FASER_hip"
      )
  )


# Metrics on wrist data ---------------------------------------------------

wrist_metrics <-
  malthe_nw_preds %>%
  f_meas(wear_criterion, tree_full_pred, event_level = "second") %>%
  bind_rows(malthe_nw_preds %>% accuracy(wear_criterion, tree_full_pred, event_level = "second")) %>%
  bind_rows(malthe_nw_preds %>% sensitivity(wear_criterion, tree_full_pred, event_level = "second")) %>%
  bind_rows(malthe_nw_preds %>% precision(wear_criterion, tree_full_pred, event_level = "second")) %>%
  mutate(
    model = "tree_full",
    dataset = "Malthe_wrist"
  ) %>%
  bind_rows(
    malthe_nw_preds %>%
      f_meas(wear_criterion, tree_imp6_pred, event_level = "second") %>%
      bind_rows(malthe_nw_preds %>% accuracy(wear_criterion, tree_imp6_pred, event_level = "second")) %>%
      bind_rows(malthe_nw_preds %>% sensitivity(wear_criterion, tree_imp6_pred, event_level = "second")) %>%
      bind_rows(malthe_nw_preds %>% precision(wear_criterion, tree_imp6_pred, event_level = "second")) %>%
      mutate(
        model = "tree_imp6",
        dataset = "Malthe_wrist"
      )
  ) %>%
  bind_rows(
    malthe_nw_preds %>%
      f_meas(wear_criterion, tree_no_temp_pred, event_level = "second") %>%
      bind_rows(malthe_nw_preds %>% accuracy(wear_criterion, tree_no_temp_pred, event_level = "second")) %>%
      bind_rows(malthe_nw_preds %>% sensitivity(wear_criterion, tree_no_temp_pred, event_level = "second")) %>%
      bind_rows(malthe_nw_preds %>% precision(wear_criterion, tree_no_temp_pred, event_level = "second")) %>%
      mutate(
        model = "tree_no_temp",
        dataset = "Malthe_wrist"
      )
  ) %>%
  # bind_rows(
  #   malthe_nw_preds %>%
  #     f_meas(wear_criterion, tree_no_temp_imp5_pred, event_level = "second") %>%
  #     bind_rows(malthe_nw_preds %>% accuracy(wear_criterion, tree_no_temp_imp5_pred, event_level = "second")) %>%
  #     bind_rows(malthe_nw_preds %>% sensitivity(wear_criterion, tree_no_temp_imp5_pred, event_level = "second")) %>%
  #     bind_rows(malthe_nw_preds %>% precision(wear_criterion, tree_no_temp_imp5_pred, event_level = "second")) %>%
  #     mutate(
  #       model = "tree_no_temp_imp5",
  #       dataset = "Malthe_wrist"
  #     )
  # ) %>%
  bind_rows(
    malthe_nw_preds %>%
      f_meas(wear_criterion, sunda_pred, event_level = "second") %>%
      bind_rows(malthe_nw_preds %>% accuracy(wear_criterion, sunda_pred, event_level = "second")) %>%
      bind_rows(malthe_nw_preds %>% sensitivity(wear_criterion, sunda_pred, event_level = "second")) %>%
      bind_rows(malthe_nw_preds %>% precision(wear_criterion, sunda_pred, event_level = "second")) %>%
      mutate(
        model = "sunda_RF",
        dataset = "Malthe_wrist"
      )
  ) %>%
  bind_rows(
    malthe_nw_preds %>%
      f_meas(wear_criterion, wear_heuristic, event_level = "second") %>%
      bind_rows(malthe_nw_preds %>% accuracy(wear_criterion, wear_heuristic, event_level = "second")) %>%
      bind_rows(malthe_nw_preds %>% sensitivity(wear_criterion, wear_heuristic, event_level = "second")) %>%
      bind_rows(malthe_nw_preds %>% precision(wear_criterion, wear_heuristic, event_level = "second")) %>%
      mutate(
        model = "heuristic",
        dataset = "Malthe_wrist"
      )
  ) %>%
  bind_rows(
    malthe_nw_preds %>%
      f_meas(wear_criterion, wear_time_cz, event_level = "second") %>%
      bind_rows(malthe_nw_preds %>% accuracy(wear_criterion, wear_time_cz, event_level = "second")) %>%
      bind_rows(malthe_nw_preds %>% sensitivity(wear_criterion, wear_time_cz, event_level = "second")) %>%
      bind_rows(malthe_nw_preds %>% precision(wear_criterion, wear_time_cz, event_level = "second")) %>%
      mutate(
        model = "cz_60",
        dataset = "Malthe_wrist"
      )
  ) %>%
  bind_rows(
    malthe_nw_preds %>%
      f_meas(wear_criterion, syed_pred, event_level = "second") %>%
      bind_rows(malthe_nw_preds %>% accuracy(wear_criterion, syed_pred, event_level = "second")) %>%
      bind_rows(malthe_nw_preds %>% sensitivity(wear_criterion, syed_pred, event_level = "second")) %>%
      bind_rows(malthe_nw_preds %>% precision(wear_criterion, syed_pred, event_level = "second")) %>%
      mutate(
        model = "syed_CNN",
        dataset = "Malthe_wrist"
      )
  )


# Combine all metrics -----------------------------------------------------

all_metrics <-
  bind_rows(thigh_metrics, hip_metrics, wrist_metrics)

write_rds(all_metrics, here("data", "all_metrics.rds"))