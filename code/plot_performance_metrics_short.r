pacman::p_load(
  tidyverse,
  tidymodels,
  here,
  ggthemes,
  lykkelib
)

# plot performance from short episodes
# source(here("markdown", "performance_short.r"))

# plot performance from all episodes
# source(here("markdown", "performance_all.r"))

all_metrics <- read_rds(here("data", "all_metrics_60.rds"))

# performance on all episode lengths --------------------------------------


plot_short <-
  all_metrics %>%
  filter(.metric != "accuracy") %>% 
  replace_na(replace = list(.estimate = 0)) %>%
  mutate(
    model = fct_relevel(model, c("syed_cnn", "sunda_rf", "tree_no_temp", "tree_imp6", "tree_full", "cz_60", "heuristic")),
    .metric = fct_relevel(.metric, c("precision", "sensitivity", "f_meas")),
    model = fct_reorder(model, .estimate)
  ) %>%
  ggplot(aes(model, .estimate, fill = dataset)) +
  geom_col(
    width = .75,
    alpha = .8,
    # color = "grey50",
    position = position_dodge(width = .8)
  ) +
  geom_text(aes(model, .estimate + .05,
                label = round(.estimate, 2)
  ),
  size = 3,
  position = position_dodge(width = .8)
  ) +
  expand_limits(y = c(0, 1.2)) +
  scale_fill_manual(
    values = c("#E8C15F", "lightblue", "#7CB07C"),
    labels = c("Hip", "Thigh", "Wrist")
  ) +
  # lykkelib::scale_fill_lykke() +
  labs(
    title = NULL,
    x = NULL,
    y = NULL,
    fill = NULL,
  ) +
  facet_wrap(~.metric,
             labeller = labeller(.metric = c(
               precision = "Precision",
               sensitivity = "Sensitivity",
               accuracy = "Accuracy",
               f_meas = "F1 score"
             )),
             nrow = 4
  ) +
  theme_bw() +
  theme(
    panel.border = element_rect(linetype = 1, size = .3),
    # axis.text.x = element_text(angle = 45, vjust = .65),
    legend.position = "bottom",
    # panel.spacing = unit(1, "lines"),
    strip.text.x = element_text(size = 12),
    strip.text.y = element_blank(),
    strip.background = element_blank()
  ) 


# ggsave(filename = here("figures", "performance_all_episodes.tiff"), plot = p1, dpi = 600, height = 8, width = 12)
# ggsave(filename = here("figures", "performance_short_episodes.tiff"), plot = p1, dpi = 600, height = 8, width = 12)
