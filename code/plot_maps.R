# Preharvest Drop: Data sheets
# author: Jimmy Larson
# created: 8/25/20
# last edited: 8/25/20

## packages----
library(tidyverse)
## load in plot map----
map <- read_csv("plot_map/pfd_plot_map_fall_2020.csv")
## data sheet for drop counts----
### select center tree of each plot
map %>%
  group_by(plot) %>%
  filter(tree == mean(tree)) -> DropDataSheet
### save data sheet
write_csv(DropDataSheet, file.path("plot_map/pfd_fruit_drop_counts_data_sheet.csv"))
## ethylene and fruit quality----
map %>%
  group_by(plot) %>%
  filter(tree == mean(tree)) %>%
  select(row, plot, treatment, rep, flag) -> quality
quality <- quality[rep(seq_len(nrow(quality)), each = 12),]
quality %>%
  mutate(fruit = seq(1,12)) -> quality
write_csv(quality, file.path("plot_map/pfd_fruit_quality_data_sheet.csv"))
