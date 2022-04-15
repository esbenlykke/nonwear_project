pacman::p_load(
  tidyverse,
  here,
  patchwork,
  lubridate,
  gt
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
# faser_thigh <-
#   faser_full %>%
#   mutate(
#     wear_criterion = as.numeric(wear_criterion) - 1,
#     location = if_else(location == 1, "thigh", "hip") %>%
#       factor()
#   ) %>%
#   filter(location == "thigh") %>%
#   group_by(id, group = cumsum(wear_criterion == 0)) %>%
#   mutate(cumulative = cumsum(wear_criterion)) %>%
#   ungroup() %>%
#   # group_by(nw_counter = cumsum(cumulative == 1)) %>%
#   # ungroup() %>%
#   filter(wear_criterion == 1) %>%
#   count(id, group, location)
# 
# faser_hip <-
#   faser_full %>%
#   mutate(
#     wear_criterion = as.numeric(wear_criterion) - 1,
#     location = if_else(location == 1, "thigh", "hip") %>%
#       factor()
#   ) %>%
#   filter(location == "hip") %>%
#   group_by(id, group = cumsum(wear_criterion == 0)) %>%
#   mutate(cumulative = cumsum(wear_criterion)) %>%
#   ungroup() %>%
#   # group_by(nw_counter = cumsum(cumulative == 1)) %>%
#   # ungroup() %>%
#   filter(wear_criterion == 1) %>%
#   count(id, group, location)
# 
# # write_rds(faser_full_nw_periods, here("data", "nw_episodes_thigh_hip.rds"))
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
# # write_rds(malthe_nw_periods, here("data", "nw_episodes_wrist.rds"))


# plot distribution of faser nw periods -----------------------------------

wrist_nw <- read_rds(here("data", "nw_episodes_wrist.rds"))
hip_thigh_nw <- read_rds(here("data", "nw_episodes_thigh_hip.rds"))

all_short <-
  hip_thigh_nw %>%
  bind_rows(wrist_nw) %>%
  mutate(
    n = (n * 10),
    nw = time_length(n, unit = "minute")
  ) %>%
  filter(nw < 60) %>%
  ggplot(aes(nw, fill = location)) +
  geom_histogram(color = "white", binwidth = 5, alpha = .8) +
  labs(
    title = "<60 minutes",
    x = "Non-wear episode duration (minutes)",
    y = NULL,
    fill = NULL
  ) +
  scale_fill_manual(values = c("#E8C15F", "lightblue", "#7CB07C"),
                    labels = c("Hip", "Thigh", "Wrist")) +
  theme_bw() +
  facet_wrap(~location, ncol = 1) +
  theme(
    strip.background = element_rect(fill = NA, color = NA),
    strip.text = element_blank(),
    plot.title = element_text(hjust = .5)
  )

all_long <-
  hip_thigh_nw %>%
  bind_rows(wrist_nw) %>%
  mutate(
    n = (n * 10),
    nw = time_length(n, unit = "hour")
  ) %>%
  filter(nw >= 2) %>%
  ggplot(aes(nw, fill = location)) +
  geom_histogram(color = "white", binwidth = 5, alpha = .8) +
  labs(
    title = "\u2265 60 minutes",
    x = "Non-wear episode duration (hours)",
    y = NULL,
    fill = NULL
  ) +
  scale_fill_manual(values = c("#E8C15F", "lightblue", "#7CB07C"),
                    labels = c("Hip", "Thigh", "Wrist")) +
  theme_bw() +
  facet_wrap(~location, ncol = 1, scales = "free_y") +
  theme(
    strip.background = element_blank(),
    strip.text = element_blank(),
    plot.title = element_text(hjust = .5)
  ) +
  xlim(0, 80)

nw_dists <-
  all_short + all_long +
    plot_layout(guides = "collect") &
    theme(
      legend.position = "bottom",
      panel.border = element_rect(linetype = 1, size = .3)
    )

# ggsave(plot = all_color_sep, here("figures", "dist_plots_all_color_sep.tiff"), dpi = 600, width = 30, height = 15, units = "cm")