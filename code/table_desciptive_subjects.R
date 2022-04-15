pacman::p_load(
  tidyverse,
  haven,
  here,
  lubridate,
  anytime,
  gt,
  gtsummary,
  naniar
)

test <-
  read_dta(here("data", "misc", "faser_natascha.dta"))

test %>%
  mutate(
    birth_date = anydate(birth_date),
    StartDate = anydate(StartDate),
    age = time_length(StartDate - birth_date, unit = "year")
  ) %>%
  select(gender, age) %>%
  tbl_summary(
    # by = "gender",
    missing = "no",
    statistic = list(all_continuous() ~ "{mean} ({sd})")
  ) %>%
  # add_overall() %>%
  add_n() %>% 
  as_gt() %>% 
  tab_header("Mean age of PHASAR participants") 

# test %>%
#   mutate(
#     birth_date = anydate(birth_date),
#     StartDate = anydate(StartDate),
#     age = time_length(StartDate - birth_date, unit = "year")
#   ) %>% write_dta(here("data", "misc", "faser_natascha.dta"))

range(test$age, na.rm = T)