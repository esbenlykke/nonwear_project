pacman::p_load(
  tidyverse,
  here,
  gt
)

feat <-
tribble(
  ~Feature,   ~Description,
  "weekday",  "day of week ([1:7])",
  "time_day", "time of day (miliseconds)",
  "location", "device wear location: 0 = thigh, 1 = hip",
  "macc_x",   "moving average of the z axis acceleration",
  "macc_y",   "moving average of the y axis acceleration",
  "macc_z",   "moving average of the z axis acceleration",
  "sdacc_x",  "moving average of the standard deviation on the x axis acceleration",
  "sdacc_y",  "moving average of the standard deviation of the y axis acceleration",
  "sdacc_z",  "moving average of the standard deviation of the z axis acceleration",
  "sdmax",    "maximum standard deviation",
  "incl",     "inclination angle of the device in relation to the direction of the gravitational force",
  "temp",     "surface skin temperature (degrees Celcius)"
) %>%
  mutate(Description = str_to_sentence(Description)) %>%
  gt() %>%
  tab_style(
    style = cell_fill(color = "grey90"),
    locations = cells_body(
      columns = 1:2,
      rows = seq(2, 13, 2)
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold"),
      cell_fill(color = "grey60")
    ),
    locations = cells_column_labels(1:2)
  ) 


#   gtsave(here("figures", "feature_description.tex"))

feat