# Preharvest Drop: Data sheets
# author: Jimmy Larson
# created: 8/25/20
# last edited: 9/11/20

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
treatmentCat <- c("ctrl", "avg", "naa", "ethephon") 
### fruit quality
map %>%
  group_by(plot) %>%
  filter(tree == mean(tree)) %>%
  select(row, plot, treatment, rep, flag) -> quality
quality <- quality[rep(seq_len(nrow(quality)), each = 12),]
quality %>%
  mutate(fruit = seq(1,12)) %>%
  arrange(rep, match(treatment, treatmentCat))-> quality
write_csv(quality, file.path("plot_map/pfd_fruit_quality_data_sheet.csv"))
### internal ethylene content
ethylene <- quality[rep(seq_len(nrow(quality)), each = 10),]
ethylene %>%
  mutate(fruit = seq(1,10)) %>%
  arrange(rep, match(treatment, treatmentCat))-> ethylene
write_csv(ethylene, file.path("plot_map/pfd_ethylene_data_sheet.csv"))
### brix
map %>%
  group_by(plot) %>%
  filter(tree == mean(tree)) %>%
  select(row, plot, treatment, rep, flag) %>%
  arrange(rep, match(treatment, treatmentCat)) -> brix
write_csv(brix, file.path("plot_map/pfd_brix_data_sheet.csv"))
