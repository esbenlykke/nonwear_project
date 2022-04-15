pacman::p_load(
  tidyverse,
  here,
  yardstick
)

# source(here("markdown", "remove_long_nw.r"))

my_metric <- metric_set(f_meas, accuracy, sensitivity, precision)


# thigh -------------------------------------------------------------------


thigh_no_long_metrics <-
  no_long %>%
  filter(data == "thigh_tree_full") %>% 
  unnest(file_contents) %>% 
  my_metric(truth = wear_criterion, estimate = tree_full_preds, event_level = "second") %>%
  mutate(
    model = "tree_full",
    dataset = "thigh"
  ) %>%
  bind_rows(
    no_long %>%
      filter(data == "thigh_imp6") %>% 
      unnest(file_contents) %>% 
    my_metric(truth = wear_criterion, estimate = tree_imp6_preds, event_level = "second") %>%
    mutate(
      model = "tree_imp6",
      dataset = "thigh"
    )) %>%
  bind_rows(
    no_long %>%
      filter(data == "thigh_no_temp") %>% 
      unnest(file_contents) %>% 
    my_metric(truth = wear_criterion, estimate = tree_no_temp_preds, event_level = "second") %>%
    mutate(
      model = "tree_no_temp",
      dataset = "thigh"
    )) %>%
  bind_rows(
    no_long %>%
      filter(data == "thigh_cz60") %>% 
      unnest(file_contents) %>% 
    my_metric(truth = wear_criterion, estimate = wear_time_cz, event_level = "second") %>%
    mutate(
      model = "cz_60",
      dataset = "thigh"
    )) %>%
  bind_rows(
    no_long %>%
      filter(data == "thigh_heu") %>% 
      unnest(file_contents) %>% 
    my_metric(truth = wear_criterion, estimate = wear_heuristic, event_level = "second") %>%
    mutate(
      model = "heuristic",
      dataset = "thigh"
    )) %>%
  bind_rows(
    no_long %>%
      filter(data == "thigh_sunda") %>% 
      unnest(file_contents) %>%     
      my_metric(truth = wear_criterion, estimate = sunda_preds, event_level = "second") %>%
    mutate(
      model = "sunda_rf",
      dataset = "thigh"
    )) %>%
  bind_rows(
    no_long %>%
      filter(data == "thigh_syed") %>% 
      unnest(file_contents) %>% 
    my_metric(truth = wear_criterion, estimate = syed_preds, event_level = "second") %>%
    mutate(
      model = "syed_cnn",
      dataset = "thigh"
    ))


# hip ---------------------------------------------------------------------

hip_no_long_metrics <-
  no_long %>%
  filter(data == "hip_tree_full") %>% 
  unnest(file_contents) %>% 
  my_metric(truth = wear_criterion, estimate = tree_full_preds, event_level = "second") %>%
  mutate(
    model = "tree_full",
    dataset = "hip"
  ) %>%
  bind_rows(
    no_long %>%
      filter(data == "hip_imp6") %>% 
      unnest(file_contents) %>% 
      my_metric(truth = wear_criterion, estimate = tree_imp6_preds, event_level = "second") %>%
      mutate(
        model = "tree_imp6",
        dataset = "hip"
      )) %>%
  bind_rows(
    no_long %>%
      filter(data == "hip_no_temp") %>% 
      unnest(file_contents) %>% 
      my_metric(truth = wear_criterion, estimate = tree_no_temp_preds, event_level = "second") %>%
      mutate(
        model = "tree_no_temp",
        dataset = "hip"
      )) %>%
  bind_rows(
    no_long %>%
      filter(data == "hip_cz60") %>% 
      unnest(file_contents) %>% 
      my_metric(truth = wear_criterion, estimate = wear_time_cz, event_level = "second") %>%
      mutate(
        model = "cz_60",
        dataset = "hip"
      )) %>%
  bind_rows(
    no_long %>%
      filter(data == "hip_heu") %>% 
      unnest(file_contents) %>% 
      my_metric(truth = wear_criterion, estimate = wear_heuristic, event_level = "second") %>%
      mutate(
        model = "heuristic",
        dataset = "hip"
      )) %>%
  bind_rows(
    no_long %>%
      filter(data == "hip_sunda") %>% 
      unnest(file_contents) %>%     
      my_metric(truth = wear_criterion, estimate = sunda_preds, event_level = "second") %>%
      mutate(
        model = "sunda_rf",
        dataset = "hip"
      )) %>%
  bind_rows(
    no_long %>%
      filter(data == "hip_syed") %>% 
      unnest(file_contents) %>% 
      my_metric(truth = wear_criterion, estimate = syed_preds, event_level = "second") %>%
      mutate(
        model = "syed_cnn",
        dataset = "hip"
      ))


# wrist -------------------------------------------------------------------

wrist_no_long_metrics <-
  no_long %>%
  filter(data == "wrist_tree_full") %>% 
  unnest(file_contents) %>% 
  my_metric(truth = wear_criterion, estimate = tree_full_preds, event_level = "second") %>%
  mutate(
    model = "tree_full",
    dataset = "wrist"
  ) %>%
  bind_rows(
    no_long %>%
      filter(data == "wrist_imp6") %>% 
      unnest(file_contents) %>% 
      my_metric(truth = wear_criterion, estimate = tree_imp6_preds, event_level = "second") %>%
      mutate(
        model = "tree_imp6",
        dataset = "wrist"
      )) %>%
  bind_rows(
    no_long %>%
      filter(data == "wrist_no_temp") %>% 
      unnest(file_contents) %>% 
      my_metric(truth = wear_criterion, estimate = tree_no_temp_preds, event_level = "second") %>%
      mutate(
        model = "tree_no_temp",
        dataset = "wrist"
      )) %>%
  bind_rows(
    no_long %>%
      filter(data == "wrist_cz60") %>% 
      unnest(file_contents) %>% 
      my_metric(truth = wear_criterion, estimate = wear_time_cz, event_level = "second") %>%
      mutate(
        model = "cz_60",
        dataset = "wrist"
      )) %>%
  bind_rows(
    no_long %>%
      filter(data == "wrist_heu") %>% 
      unnest(file_contents) %>% 
      my_metric(truth = wear_criterion, estimate = wear_heuristic, event_level = "second") %>%
      mutate(
        model = "heuristic",
        dataset = "wrist"
      )) %>%
  bind_rows(
    no_long %>%
      filter(data == "wrist_sunda") %>% 
      unnest(file_contents) %>%     
      my_metric(truth = wear_criterion, estimate = sunda_preds, event_level = "second") %>%
      mutate(
        model = "sunda_rf",
        dataset = "wrist"
      )) %>%
  bind_rows(
    no_long %>%
      filter(data == "wrist_syed") %>% 
      unnest(file_contents) %>% 
      my_metric(truth = wear_criterion, estimate = syed_preds, event_level = "second") %>%
      mutate(
        model = "syed_cnn",
        dataset = "wrist"
      ))



# all metrics -------------------------------------------------------------

all_metrics <- 
  bind_rows(thigh_no_long_metrics, hip_no_long_metrics, wrist_no_long_metrics)

# write_rds(all_metrics, here("data", "all_metrics_60.rds"))