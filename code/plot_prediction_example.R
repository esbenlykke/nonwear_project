pacman::p_load(
  tidyverse,
  here,
  patchwork
)

preds_wrist <- readRDS("C:/Users/eskovgaard/Desktop/nonwear/data/preds_wrist.rds")
preds_thigh <- readRDS("C:/Users/eskovgaard/Desktop/nonwear/data/preds_thigh.rds")
preds_hip <- readRDS("C:/Users/eskovgaard/Desktop/nonwear/data/preds_hip.rds")

times_all <-
  read_rds(here("data", "all_nw_start_stop_times.rds"))

times <-
  times_all %>%
  filter(name == "thigh_cri") %>%
  unnest(times) %>%
  filter(start > 481682 & stop < 481682 + 66960) # samples from id = 1043206

ml_plots <-
  preds_thigh %>%
  filter(id == 2029012) %>%
  mutate(across(wear_criterion:syed_preds, as.numeric)) %>%
  select(idx_all, wear_criterion:syed_preds) %>%
  pivot_longer(-idx_all) %>%
  filter(name %in% c("syed_preds", "sunda_preds")) %>%
  mutate(value = if_else(name == "syed_preds", value + .05, value)) %>%
  ggplot(aes(idx_all, value, color = name)) +
  geom_rect(times,
    mapping = aes(
      xmin = start,
      xmax = stop,
      ymin = .8,
      ymax = 2.2
    ),
    fill = "lightblue",
    alpha = .4,
    inherit.aes = FALSE
  ) +
  geom_line(size = .05) +
  labs(
    x = NULL,
    y = NULL,
    color = NULL
  ) +
  scale_y_continuous(n.breaks = 2, labels = c("Wear", "Non-wear")) +
  scale_color_manual(labels = c("Sunda_RF", "Syed_CNN"), values = c("darkorange", "darkgreen")) +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    panel.grid = element_blank()
  )

heu_plots <-
  preds_thigh %>%
  filter(id == 2029012) %>%
  mutate(across(wear_criterion:syed_preds, as.numeric)) %>%
  select(idx_all, wear_criterion:syed_preds) %>%
  pivot_longer(-idx_all) %>%
  filter(name %in% c("wear_heuristic", "wear_time_cz")) %>%
  mutate(value = if_else(name == "wear_heuristic", value + .05, value)) %>%
  ggplot(aes(idx_all, value, color = name)) +
  geom_rect(times,
    mapping = aes(
      xmin = start,
      xmax = stop,
      ymin = .8,
      ymax = 2.2
    ),
    fill = "lightblue",
    alpha = .4,
    inherit.aes = FALSE
  ) +
  geom_line(size = .05) +
  labs(
    x = NULL,
    y = NULL,
    color = NULL
  ) +
  scale_y_continuous(n.breaks = 2, labels = c("Wear", "Non-wear")) +
  scale_color_manual(labels = c("CZ60", "Heuristic"), values = c("darkorange", "darkgreen")) +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    panel.grid = element_blank()
  )

tree_plots <-
preds_thigh %>%
  filter(id == 2029012) %>%
  mutate(across(wear_criterion:syed_preds, as.numeric)) %>%
  select(idx_all, wear_criterion:syed_preds) %>%
  pivot_longer(-idx_all) %>%
  filter(name %in% c("tree_full_preds", "tree_imp6_preds")) %>%
  mutate(value = if_else(name == "tree_full_preds", value + .05, value)) %>%
  # value = if_else(name == "tree_no_temp_preds", value - .05, value)) %>%
  ggplot(aes(idx_all, value, color = name)) +
  geom_rect(times,
    mapping = aes(
      xmin = start,
      xmax = stop,
      ymin = .8,
      ymax = 2.2
    ),
    fill = "lightblue",
    alpha = .4,
    inherit.aes = FALSE
  ) +
  geom_line(size = .05) +
  labs(
    x = NULL,
    y = NULL,
    color = NULL
  ) +
  scale_y_continuous(n.breaks = 2, labels = c("Wear", "Non-wear")) +
  scale_color_manual(labels = c("full", "imp6"), values = c("darkorange", "darkgreen")) +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    panel.grid = element_blank()
  )


plot_thigh <-
  ml_plots / heu_plots / tree_plots



# ggsave(plot = plots, here("figures", "predict_example_thigh.tiff"), dpi = 600, height = 6, width = 15)


# testing -----------------------------------------------------------------



# times <-
#   times_all %>%
#   filter(name == "thigh_cri") %>%
#   unnest(times) %>%
#   filter(start > 481682 & stop < 481682 + 66960) %>% 
#   mutate(start = lubridate::as_datetime(start - 481682),
#          stop = lubridate::as_datetime(stop - 481682))
# 
# preds_thigh %>%
#   filter(id == 2029012) %>%
#   mutate(across(wear_criterion:syed_preds, as.numeric)) %>%
#   select(idx_all, wear_criterion:syed_preds) %>%
#   pivot_longer(-idx_all) %>%
#   filter(name %in% c("tree_full_preds", "tree_imp6_preds")) %>%
#   mutate(
#     value = if_else(name == "tree_full_preds", value + .05, value),
#     idx_all = lubridate::as_datetime(idx_all - 481682)
#   ) %>%
#   # value = if_else(name == "tree_no_temp_preds", value - .05, value)) %>%
#   ggplot(aes(idx_all, value, color = name)) +
#   geom_rect(times,
#     mapping = aes(
#       xmin = start,
#       xmax = stop,
#       ymin = .8,
#       ymax = 2.2
#     ),
#     fill = "lightblue",
#     alpha = .4,
#     inherit.aes = FALSE
#   ) +
#   geom_line(size = .05) +
#   labs(
#     x = NULL,
#     y = NULL,
#     color = NULL
#   ) +
#   scale_y_continuous(n.breaks = 2, labels = c("Wear", "Non-wear")) +
#   scale_color_manual(labels = c("full", "imp6"), values = c("darkorange", "darkgreen")) +
#   theme_bw() +
#   theme(
#     # axis.text.x = element_blank(),
#     panel.grid = element_blank()
#   )
