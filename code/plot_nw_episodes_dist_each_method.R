pacman::p_load(
  tidyverse,
  here,
  beepr,
  lykkelib,
  patchwork
)

# source(here("data", "get_start_stop_times.R"))

times_all <-
  read_rds(here("data", "all_nw_start_stop_times.rds"))

theme_set(theme_bw())

dist_plot <- function(tbl, title) {
    tbl %>%
    filter(duration > 5 & duration < 60) %>%
    ggplot(aes(duration)) +
    geom_histogram(fill = "firebrick", color = "black", alpha = .7, size = .3)
}


plots_all <-
  times_all %>%
  mutate(plots = map(times, ~ dist_plot(tbl = .x, title = name))) %>% 
  pull(plots) %>% 
  wrap_plots(ncol = 8)

# ggsave(plot = plots_all, here("figures", "all_nw_dists.tiff"), dpi = 600, height = 6, width = 15)
