pacman::p_load(
  tidyverse,
  vip,
  here
  # DALEX,
  # DALEXtra
)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# tree_full <- read_rds(here("models", "tree_fit_full_butch.rds"))
# write_rds(vi_scores, here("data", "vi_scores.rds"))

vi_scores <- 
  read_rds(here("data", "vi_scores.rds"))

## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# tree_imp_plot <-
# vi_scores <-
#   tree_full %>%
#   extract_fit_parsnip() %>%
#   vi()

# top_6 <-
#   c("temp", "sdmax", "sdacc_x", "sdacc_z", "sdacc_y", "macc_z")
# 
# vi_scores %>%
#   mutate(Variable = fct_reorder(Variable, Importance)) %>%
#   ggplot(aes(Importance, Variable)) +
#   geom_segment(aes(xend = 0, yend = Variable),
#     color = if_else(vi_scores$Variable %in% top_6, "darkorange", "grey70"),
#     size = if_else(vi_scores$Variable %in% top_6, 1, .5)
#   ) +
#   geom_point(
#     color = if_else(vi_scores$Variable %in% top_6, "darkorange", "grey70"),
#     size = if_else(vi_scores$Variable %in% top_6, 5, 2)
#   ) +
#   # geom_text(aes(Importance, Variable, label = if_else(Variable %in% top_6, Importance, NULL))) +
#   theme_bw() +
#   scale_x_continuous(labels = scales::scientific) +
#   labs(y = "Feature")



# ggsave(plot = plot_full_vip,
# filename = here("figures", "bar_plot_full_vip.tiff"),
# device = "tiff",
# width = 20,
# height = 10,
# units = "cm",
# dpi = 600)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
vi_plot <-
  vi_scores %>% 
  mutate(Variable = fct_reorder(Variable, Importance)) %>%
  ggplot(aes(Importance, Variable, fill = Importance)) +
  geom_col(color = "grey50", show.legend = FALSE, size = .08) +
  scale_fill_gradient2(low = "blue", high = "#c94848") +
  scale_x_continuous(labels = scales::scientific) +
  labs(y = "Feature") +
  theme_bw() +
  theme(axis.text.x = element_blank())
  

# ggsave(
#   plot = red_vip,
#   filename = here("figures", "gradient_vip.tiff"),
#   device = "tiff",
#   width = 7,
#   height = 3,
#   dpi = 600
# )


# ## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# faser_train <-
#   read_rds(here("data", "faser_train.rds"))
# 
# tree_explainer <-
#   DALEXtra::explain_tidymodels(
#     tree_full,
#     faser_train,
#     y = faser_train$wear_criterion,
#     label = "full tree",
#     verbose = TRUE
#   )
# 
# breakdown <-
#   model_parts(
#     tree_explainer,
#     loss_function = loss_root_mean_square
#   )

