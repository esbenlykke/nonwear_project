pacman::p_load(
  tidyverse,
  here,
  runner,
  beepr
)



faser_thigh <-
  read_rds(here("data", "preds_thigh.rds")) %>% 
  replace_na(list(sunda_preds = 1))

faser_hip <-
  read_rds(here("data", "preds_hip.rds"))

malthe_wrist <-
  read_rds(here("data", "preds_wrist.rds"))


# generate streaks --------------------------------------------------------

get_streaks <- function(df) {
  df %>%
    mutate(
      across(wear_criterion:syed_preds,
        ~ if_else(.x == 1, streak_run(.x), 0L),
        .names = "{.col}_streak"
      ),
      across(contains("streak"),
        ~ if_else(lag(.x) == 0 & .x == 1, "yes", "no"),
        .names = "{.col}_start"
      ),
      across(wear_criterion_streak:syed_preds_streak,
        ~ if_else(lead(.x) == 0 & .x > 0 | lag(.x) == 0 & lead(.x) == 0 & .x > 0, "yes", "no"),
        .names = "{.col}_end"
      )
    ) %>%
    mutate(across(contains("streak"), ~ replace_na(.x, "yes"))) 
}

# get_start_stop <- function(df) {
#   df %>%
#     mutate(
#       across(wear_criterion:syed_preds,
#         ~ if_else(.x == 1, runner::streak_run(.x), 0L),
#         .names = "{.col}_streak"
#       ),
#       across(contains("streak"),
#         ~ case_when(lead(.x, n = 360) > 0 & .x > 0 ~ "long",
#                     lag(.x) > 0 & .x > 0 ~ "short",
#                     lead(.x) == 0 & .x > 0 | lag(.x) == 0 & lead(.x) == 0 & .x > 0 ~ "short",
#                     TRUE ~ "wear")
#       )
#     ) %>%
#     mutate(across(contains("streak"), ~ replace_na(.x, "yes")))
# }
#
# test <- get_start_stop(faser_thigh %>% slice(1:5000))

thigh_streaks <-
  get_streaks(faser_thigh)

hip_streaks <-
  get_streaks(faser_hip)

wrist_streaks <-
  get_streaks(malthe_wrist)

beep(4)

