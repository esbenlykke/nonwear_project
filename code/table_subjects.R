pacman::p_load(tidyverse,
               here,
               gt,
               gtsummary)

df <- haven::read_dta(here("data", "misc", "faser_natascha.dta"))

df %>% 
  select(gender, age) %>% 
  mutate(gender = as.numeric(gender),
         gender = if_else(gender == 1, "Males", "Females")) %>% 
  tbl_summary(by = "gender",
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>% 
  add_overall()
