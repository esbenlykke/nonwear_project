pacman::p_load(
  tidyverse,
  tidymodels,
  here,
  gt,
  gtsummary,
  lubridate
)

# faser_full <-
#   read_rds(here("data", "faser_full.rds"))
# 
# malthe_nw <-
#   read_rds(here("data", "malthe_nw.rds"))
# 
# # Calculate number and length of nw periods -------------------------------
# 
# 
# faser_full_nw_periods <-
#   faser_full %>%
#   mutate(
#     wear_criterion = as.numeric(wear_criterion) - 1,
#     location = if_else(location == 1, "thigh", "hip") %>%
#       factor()
#   ) %>%
#   group_by(id, group = cumsum(wear_criterion == 0)) %>%
#   mutate(cumulative = cumsum(wear_criterion)) %>%
#   ungroup() %>%
#   # group_by(nw_counter = cumsum(cumulative == 1)) %>%
#   # ungroup() %>%
#   filter(wear_criterion == 1) %>%
#   count(id, group, location)
# 
# malthe_nw_periods <-
#   malthe_nw %>%
#   mutate(wear_criterion = as.numeric(wear_criterion) - 1) %>%
#   group_by(id, group = cumsum(wear_criterion == 0)) %>%
#   mutate(
#     cumulative = cumsum(wear_criterion),
#     location = "wrist"
#   ) %>%
#   ungroup() %>%
#   # group_by(nw_counter = cumsum(cumulative == 1)) %>%
#   # ungroup() %>%
#   filter(wear_criterion == 1) %>%
#   count(id, group, location)
# 
# 
# # gtsummary table ---------------------------------------------------------
# 
# my_ttest3 <- function(data, variable, by, ...) {
#   t.test(data[[variable]] ~ as.factor(data[[by]])) %>%
#     broom::tidy() %>%
#     select(statistic, p.value)
# }
# 
# prop_dur <- function(data, var, by, ...) {
#   data[[var]] / sum(data[[var]]) ~ as.factor(data[[by]])
# }
# 
# 
# # summary_tbl <-
# faser_full_nw_periods %>%
#   bind_rows(malthe_nw_periods) %>%
#   mutate(
#     n = (n * 10),
#     duration = time_length(n, unit = "minute"),
#     group_duration = if_else(duration >= 60, "\u226560 minutes", "<60 minutes")
#   ) %>%
#   select(-id, -group, -n) %>%
#   tbl_summary(
#     by = c(group_duration),
#     type = all_continuous() ~ "continuous2",
#     statistic = all_continuous() ~ "{mean} ({sd})"
#   ) %>% # TODO experiment with summary functions!
#   modify_header(label = "") %>%
#   add_overall() %>%
#   as_gt() %>%
#   tab_header("Non-wear episodes overview")

# gtsave(summary_tbl, here("figures", "summary_tbl.rtf"))


# gt table ----------------------------------------------------------------
nw_thigh_hip <- read_rds(here("data", "nw_episodes_thigh_hip.rds"))
nw_wrist <- read_rds(here("data", "nw_episodes_wrist.rds"))

tbl_summary <- nw_thigh_hip %>%
  bind_rows(nw_wrist) %>%
  mutate(
    n = (n * 10),
    duration = time_length(n, unit = "minute"),
    group_duration = if_else(duration >= 60, "\u226560 minutes", "<60 minutes")
  ) %>%
  group_by(location, group_duration) %>%
  summarise(
    mean = mean(duration),
    duration_total = sum(duration)
  ) %>%
  mutate(duration_prop = duration_total / sum(duration_total)) %>%
  group_by(group_duration) %>%
  gt() %>%
  fmt_number(columns = mean:duration_total, decimals = 0) %>%
  fmt_percent(duration_prop, decimals = 1) %>%
  # tab_header(
  #   title = "Overview of non-wear episodes",
  #   subtitle = "Grouped in short and long non-wear episodes"
  # ) %>%
  tab_footnote(
    footnote = "Aggregated in minutes",
    locations = cells_column_labels(columns = 3:4)
  ) %>%
  tab_footnote(footnote = "Proportion of total non-wear time by wear location",
               locations = cells_column_labels(5)) %>% 
  cols_label(
    location = "Wear location",
    mean = "Mean",
    duration_total = "Cumulated",
    duration_prop = "Proportion") %>%
  tab_options(
    heading.subtitle.font.size = 12,
    heading.align = "left",
    # table.border.top.color = "black",
    # column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width = px(3),
  ) 


# in-text summary numbers in results

# nw_thigh_hip %>%
#   bind_rows(nw_wrist) %>%
#   mutate(
#     n = (n * 10),
#     nw = time_length(n, unit = "minute"),
#     group_duration = if_else(nw >= 60, "long", "short")
#   ) %>%
#   group_by(group_duration) %>%
#   summarise(
#     n = n(),
#     mean = mean(nw),
#     sd = sd(nw)
#   ) %>%
#   mutate(
#     freq = n / sum(n),
#     freq_dur = sum(freq)
#   )
