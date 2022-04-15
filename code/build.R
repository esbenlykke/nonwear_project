pacman::p_load(
  tidyverse,
  tidymodels,
  here,
  parallel,
  doParallel,
  finetune
)

tidymodels_prefer()
options(tidymodels.dark = TRUE)

ctrl <-
  control_race(
    save_workflow = TRUE,
    verbose = TRUE,
    verbose_elim = TRUE,
    allow_par = TRUE,
    parallel_over = "everything"
  )

tree_grid <-
  grid_latin_hypercube(
    cost_complexity(),
    tree_depth(),
    min_n(),
    size = 5
  )

my_metrics <-
  metric_set(f_meas, accuracy, precision, sensitivity)


## -----------------------------------------------------------------------------------------------------------------------------------
faser_nw <-
  read_rds(here("data", "faser_full.rds"))

# for testing
# faser_small <- faser_nw %>% slice_sample(n = 1000)
## -----------------------------------------------------------------------------------------------------------------------------------
set.seed(123)
spl <-
  faser_nw %>%
  initial_time_split(prop = .2) # TODO make sure to include complete recordings for each subject

train <-
  training(spl)

test <-
  testing(spl)

folds <-
  vfold_cv(train, strata = wear_criterion, v = 5)

## -----------------------------------------------------------------------------------------------------------------------------------
tree_mod <-
  decision_tree(
    cost_complexity = tune(),
    tree_depth = tune(),
    min_n = tune()
  ) %>%
  set_engine("rpart") %>%
  set_mode("classification")


## -----------------------------------------------------------------------------------------------------------------------------------
tree_full_rec <-
  recipe(wear_criterion ~ wear_criterion ~ id + sdmax + sdacc_x + sdacc_y + sdacc_z + macc_z +
           macc_y + macc_x + incl + time_day + location + weekday + temp, data = train) %>%
  update_role(id, new_role = "id") %>%
  # TODO consider SMOTE to address sligth class imbalance.
  # themis::step_smote(all_outcomes()) %>%
  step_naomit(all_predictors(), skip = TRUE)

tree_full_wf <-
  workflow(tree_full_rec, tree_mod)


## -----------------------------------------------------------------------------------------------------------------------------------
tree_imp6_rec <-
  recipe(wear_criterion ~ id + temp + sdmax + sdacc_x +
    sdacc_y + sdacc_z + macc_z,
  data = train
  ) %>%
  update_role(id, new_role = "id") %>%
  # TODO consider SMOTE to address sligth class imbalance.
  # themis::step_smote(all_outcomes()) %>%
  step_naomit(all_predictors(), skip = TRUE)

tree_imp6_wf <-
  workflow(tree_imp6_rec, tree_mod)

## -----------------------------------------------------------------------------------------------------------------------------------
tree_no_temp_rec <-
  recipe(wear_criterion ~ id + sdmax + sdacc_x + sdacc_y + sdacc_z + macc_z +
    macc_y + macc_x + incl + time_day + location + weekday,
  data = train
  ) %>%
  update_role(id, new_role = "id") %>%
  # TODO consider SMOTE to address sligth class imbalance.
  # themis::step_smote(all_outcomes()) %>%
  step_naomit(all_predictors(), skip = TRUE)

tree_no_temp_wf <-
  workflow(tree_no_temp_rec, tree_mod)

# gather to workflow set --------------------------------------------------

tree_models <-
  workflow_set(
    preproc = list(
      full = tree_full_rec,
      imp6 = tree_imp6_rec,
      no_temp = tree_no_temp_rec
    ),
    models = list(tree = tree_mod),
    cross = FALSE
  )



# train all models --------------------------------------------------------

all_cores <- detectCores(logical = FALSE)
cl <- makePSOCKcluster(all_cores)
registerDoParallel(cl)

tree_models_res <-
  workflow_map("tune_race_anova",
    verbose = TRUE,
    seed = 123,
    resamples = folds,
    control = ctrl,
    grid = tree_grid,
    object = tree_models,
    metrics = my_metrics
  )


beepr::beep(4)

# write_rds(tree_models_res, here("models", "tree_models_res.rds"))


# Finalize workflows and fit last time ------------------------------------

best_full_tree <-
  tree_models_res %>%
  extract_workflow_set_result("full_tree") %>%
  select_best(metric = "f_meas")

tree_full_test_res <-
  tree_models_res %>%
  extract_workflow("full_tree") %>%
  finalize_workflow(best_full_tree) %>%
  last_fit(split = spl)

best_imp6_tree <-
  tree_models_res %>%
  extract_workflow_set_result("imp6_tree") %>%
  select_best(metric = "f_meas")

tree_imp6_test_res <-
  tree_models_res %>%
  extract_workflow("imp6_tree") %>%
  finalize_workflow(best_imp6_tree) %>%
  last_fit(split = spl)

best_no_temp_tree <-
  tree_models_res %>%
  extract_workflow_set_result("no_temp_tree") %>%
  select_best(metric = "f_meas")

Tree_no_temp_test_res <-
  tree_models_res %>%
  extract_workflow("no_temp_tree") %>%
  finalize_workflow(best_no_temp_tree) %>%
  last_fit(split = spl)


# Join predictions --------------------------------------------------------

tree_preds <-
  tree_full_test_res %>%
  unnest(.predictions) %>%
  select(tree_full_preds = .pred_class) %>%
  bind_cols(
    tree_imp6_test_res %>%
      unnest(.predictions) %>%
      select(tree_imp6_preds = .pred_class) %>%
      bind_cols(
        Tree_no_temp_test_res %>%
          unnest(.predictions) %>%
          select(tree_no_temp_preds = .pred_class)
      )
  )



# fit and write_rds -------------------------------------------------------

tree_full_fit <-
  tree_models_res %>%
  extract_workflow("full_tree") %>%
  finalize_workflow(best_full_tree) %>%
  fit(train)

tree_imp6_fit <-
  tree_models_res %>%
  extract_workflow("imp6_tree") %>%
  finalize_workflow(best_imp6_tree) %>%
  fit(train)

tree_no_temp_fit <-
  tree_models_res %>%
  extract_workflow("no_temp_tree") %>%
  finalize_workflow(best_no_temp_tree) %>%
  fit(train)

# predict on wrist --------------------------------------------------------

wrist_full_pred <- 
  tree_full_fit %>% 
  predict(malthe_nw)

wrist_imp6_pred <-
  tree_imp6_fit %>%
  predict(malthe_nw)

# TODO continue this shit...

beepr::beep(4)
