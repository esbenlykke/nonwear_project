# Load packages -----------------------------------------------------------
pacman::p_load(
  tidyverse,
  here,
  readxl
)

# Import data -------------------------------------------------------------

list_malthe <-
  list.files(
    path = here("data", "misc", "malthe_nw_preprocessed"),
    pattern = ".xlsx",
    recursive = TRUE,
    full.names = TRUE
  )

malthe_nw <-
  tibble(filename = list_malthe) %>%
  mutate(file_contents = map(
    list_malthe,
    ~ read_excel(here(.), col_names = TRUE)
  ))


# Unnest and write_rds ----------------------------------------------------

malthe_nw <-
  malthe_nw %>%
  unnest(file_contents) %>%
  janitor::clean_names() %>%
  select(-filename, -wear_ml_pred)

# write_rds(malthe_nw_with_predictions, here("data", "malthe_nw_with_predictions.rds"))
