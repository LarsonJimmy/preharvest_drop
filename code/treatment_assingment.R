# Preharvest Drop: Treatment Selection
# author: Jimmy Larson
# created: 8.10.20
# last edited: 8.10.20

## packages----
library(tidyverse)
#install.packages("randomizr")
library(randomizr)
## set seed----
set.seed(222)
## read in data----
trees <- read_csv("data/plot_map_tree_selection.csv")
## get plots----
plots <- as.data.frame(seq(1:(nrow(trees)/3)))
N = nrow(plots)
## create random treatment groups----
Z <- as.data.frame(complete_ra(N = N, num_arms = 4, conditions = c("ctrl", "avg", "naa", "ethephon")))
table(Z)
print(Z)
## assign treatment groups to trees----
plots <- cbind(plots, Z)
plots %>%
  rename(plot = `seq(1:(nrow(trees)/3))`,
         treatment = `complete_ra(N = N, num_arms = 4, conditions = c("ctrl", "avg", "naa", "ethephon"))`) -> plots

treatments <- left_join(trees, plots, by = "plot")
## assign reps to trees----
treatments %>%
  group_by(treatment) %>%
  mutate(rep = rep(1:5, each = 3)) -> treatments
## add flag colors----
treatments %>%
  mutate(flag = case_when(treatment == "naa" ~ "OrangeWhite",
                          treatment == "ethephon" ~ "BlackWhite",
                          treatment == "avg" ~ "BlueBlack",
                          treatment == "ctrl" ~ "Pink")) -> treatments
## write new map out to .csv file----
write_csv(treatments, file.path("plot_map/pfd_plot_map_fall_2020.csv"))
