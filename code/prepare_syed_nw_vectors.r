pacman::p_load(
  tidyverse,
  tidymodels,
  here
)



# Preds on Faser ----------------------------------------------------------


list_nw_vec_faser <-
  list.files(path = here("data", "CNN_faser", "raw"), pattern = "nw_vector.csv", recursive = TRUE, full.names = TRUE)

(all_nw_vectors <-
  tibble(filename = list_nw_vec_faser) %>%
  mutate(file_contents = map(
    list_nw_vec,
    ~ read_csv(here(.), col_names = "nw")
  )))

(nw_vecs <- all_nw_vectors %>%
  unnest(file_contents) %>%
  mutate(
    filename = str_extract(filename, "\\d{10}"),
    filename = str_remove(filename, "000"),
    id = as.numeric(filename)
  ) %>%
  select(-filename))

# add two rows of zeros to nw_vecs to make them of equal length compared to wear_criterion
nw_vecs <-
  nw_vecs %>%
  # arrange(id) %>%
  group_by(id) %>%
  group_modify(~ add_row(.x, nw = c(0, 0))) %>%
  select(id, nw)

write_rds(nw_vecs, here("data", "nw_vecs.rds"))


# Preds on malthe_nw ------------------------------------------------------


list_nw_vec_malthe <-
  list.files(path = here("data", "misc", "nw_vecs_malthe_csv"), pattern = "nw_vector.csv", recursive = TRUE, full.names = TRUE)

(all_nw_vectors_malthe <-
  tibble(filename = list_nw_vec_malthe) %>%
  mutate(file_contents = map(
    list_nw_vec_malthe,
    ~ read_csv(here(.), col_names = "syed_pred")
  )))

(nw_vecs_malthe_full <- all_nw_vectors_malthe %>%
  unnest(file_contents) %>%
  mutate(
    id = parse_number(filename)
  ) %>%
  select(-filename))

# remove one row of zeros from nw_vecs to make them of equal length compared to wear_criterion
nw_vecs_malthe_full <-
nw_vecs_malthe_full %>%
  group_by(id) %>%
  group_modify(~ add_row(.x, syed_pred = 0)) %>%
  mutate(idx = 1:n(),
         syed_pred = factor(syed_pred)) %>%
  select(idx, id, syed_pred) %>% 
  ungroup()

write_rds(nw_vecs_malthe_full, here("data", "nw_vecs_malthe.rds"))
