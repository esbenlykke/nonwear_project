pacman::p_load(
  tidyverse,
  here
)


# Import sunda predictions on malthe_nw (wrist) ---------------------------

list_sunda_malthe <-
  list.files(
    path = here("data", "misc", "output_wav_sunda_malthe", "meta", "basic"),
    pattern = "RData",
    recursive = TRUE,
    full.names = TRUE
  )


sunda_preds_nested_malthe <-
  tibble(filename = list_sunda_malthe) %>%
  mutate(file_contents = map(
    list_sunda_malthe,
    function(i) {
      load(i)
      M$metashort %>%
        mutate(
          nonwear = as.numeric(factor(wake_sleep)),
          nonwear = if_else(nonwear %in% c(2, 3), 0, 1),
          nonwear10 = ceiling(slider::slide_dbl(nonwear, .after = 1L, .f = mean, .step = 2))
        ) %>%
        filter(nonwear10 %in% c(0, 1))
    }
  ))

# write_rds(sunda_preds_nested, here("data", "sunda_preds_nested.rds"))



# Clean and extract only preds --------------------------------------------

sunda_preds_malthe <-
  sunda_preds_nested_malthe %>%
  unnest(file_contents) %>%
  select(filename, nonwear10) %>%
  group_by(filename) %>%
  group_modify(~ add_row(., nonwear10 = rep(0, 540))) %>%
  mutate(
    filename = str_extract(filename, "\\d{10}"),
    filename = str_remove(filename, "^0+"),
    id = as.numeric(filename),
    nonwear10 = factor(nonwear10),
    idx = seq_along(filename)
  ) %>%
  ungroup() %>%
  select(idx, id, sunda_pred = nonwear10) %>%
  arrange(id)

# malthe_nw_with_predictions %>%
#   left_join(sunda_preds_malthe, by = c("idx", "id")) %>%
#   write_rds(., here("data", "malthe_nw_with_predictions.rds"))


# Import sunda predictions on faser (hip/thigh) ---------------------------

list_sunda_faser <-
  list.files(
    path = here("data", "misc", "output_wav_sunda_faser", "meta", "basic"),
    pattern = "RData",
    recursive = TRUE,
    full.names = TRUE
  )


sunda_preds_nested_faser <-
  tibble(filename = list_sunda_faser) %>%
  mutate(file_contents = map(
    list_sunda_faser,
    function(i) {
      load(i)
      M$metashort %>%
        mutate(
          nonwear = as.numeric(factor(wake_sleep)),
          nonwear = if_else(nonwear %in% c(2, 3), 0, 1)
        )
      # nonwear10 = ceiling(slider::slide_dbl(nonwear, .after = 1L, .f = mean, .step = 2))
      # ) %>%
      # filter(nonwear10 %in% c(0, 1))
    }
  ))

# write_rds(sunda_preds_nested_faser, here("data", "sunda_preds_nested_faser.rds"))



# Clean and extract only preds --------------------------------------------

sunda_preds_faser <-
  sunda_preds_nested_faser %>%
  unnest(file_contents) %>%
  select(filename, nonwear) %>%
  group_by(filename) %>%
  # group_modify(~ add_row(., nonwear = rep(0, 186))) %>% # not working right
  mutate(
    filename = str_extract(filename, "\\d{10}"),
    filename = str_remove(filename, "^0+"),
    id = as.numeric(filename),
    idx = seq_along(filename)
  ) %>%
  ungroup() %>%
  select(idx, id, sunda_preds = nonwear) %>%
  arrange(id)
