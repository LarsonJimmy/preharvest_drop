# Preharvest Drop: Xylem Function Data Analysis
# author: Jimmy Larson
# created: 11/2/20
# last edited: 11/2/20
## load packages----
library(tidyverse)
library(ggpubr)
library(car)
library(lubridate)
library(RColorBrewer)
library(ggrepel)
library(hrbrthemes)
library(latex2exp)
## plotting theme----
theme_jl <- theme_ipsum_ps(grid="XY", axis="xy", axis_text_size = 10, axis_title_size = 11, axis_col = "black")
## read in data----
dyeAug27 <- read_csv("data/2020/acid_fucshin/pfd_dye_data_aug27.csv")
dyeSep2 <- read_csv("data/2020/acid_fucshin/pfd_dye_data_sep2.csv")
dyeSep9 <- read_csv("data/2020/acid_fucshin/pfd_dye_data_sep9.csv")
dyeSep16 <- read_csv("data/2020/acid_fucshin/pfd_dye_data_sep16.csv")
dyeSep23 <- read_csv("data/2020/acid_fucshin/pfd_dye_data_sep23.csv")
dyeSep30 <- read_csv("data/2020/acid_fucshin/pfd_dye_data_sep30.csv")
dyeOct14 <- read_csv("data/2020/acid_fucshin/pfd_dye_data_Oct14.csv")
## join data----
dyeAug27 %>%
  rename(primaryAug27 = primary,
         dorsalAug27 = dorsal) %>%
  left_join(dyeSep2) %>%
  rename(primarySep2 = primary,
         dorsalSep2 = dorsal) %>%
  left_join(dyeSep9) %>%
  rename(primarySep9 = primary,
         dorsalSep9 = dorsal) %>%
  left_join(dyeSep16) %>%
  rename(primarySep16 = primary,
         dorsalSep16 = dorsal) %>%
  left_join(dyeSep23) %>%
  rename(primarySep23 = primary,
         dorsalSep23 = dorsal) %>%
  left_join(dyeSep30) %>%
  rename(primarySep30 = primary,
         dorsalSep30 = dorsal) %>%
  left_join(dyeOct14) %>%
  rename(primaryOct14 = primary,
         dorsalOct14 = dorsal) -> dye
## long form data----
dye %>%
  select(treatment, rep, fruit, primaryAug27, primarySep2, primarySep9, primarySep16, primarySep23, primarySep30,
         primaryOct14) %>%
  gather('primaryAug27', 'primarySep2', 'primarySep9', 'primarySep16', 'primarySep23', 'primarySep30',
         'primaryOct14', key = "measDate", value = "primary") -> dyePrimary
dye %>%
  select(treatment, rep, fruit, dorsalAug27, dorsalSep2, dorsalSep9, dorsalSep16, dorsalSep23, dorsalSep30,
         dorsalOct14) %>%
  gather('dorsalAug27', 'dorsalSep2', 'dorsalSep9', 'dorsalSep16', 'dorsalSep23', 'dorsalSep30',
         'dorsalOct14', key = "measDate", value = "dorsal") %>%
  mutate(date = case_when(measDate == 'dorsalAug27' ~ mdy("8/27/2020"),
                          measDate == 'dorsalSep2' ~ mdy("9/2/2020"),
                          measDate == 'dorsalSep9' ~ mdy("9/9/2020"),
                          measDate == 'dorsalSep16' ~ mdy("9/16/2020"),
                          measDate == 'dorsalSep23' ~ mdy("9/23/2020"),
                          measDate == 'dorsalSep30' ~ mdy("9/30/2020"),
                          measDate == 'dorsalOct14' ~ mdy("10/14/2020"))) %>%
  select(date, treatment, rep, fruit, dorsal) %>%
  mutate(primary = dyePrimary$primary) -> dyeLong

## explore data----
### dorsal bundles
ggplot(dyeLong, aes(x = treatment, y = dorsal, fill = treatment)) +
  geom_boxplot() +
  facet_wrap(~date, scales = "free") +
  scale_fill_brewer(palette = "RdYlBu", labels = c("AVG", "Control", "Ethephon", "NAA")) +
  labs(y = "Number of Dorsal Bundles Stained",
       x = "Treatment",
       fill = "Treatment") +
  theme_jl
### primary bundles
ggplot(dyeLong, aes(x = treatment, y = primary, fill = treatment)) +
  geom_boxplot() +
  facet_wrap(~date, scales = "free") +
  scale_fill_brewer(palette = "RdYlBu", labels = c("AVG", "Control", "Ethephon", "NAA")) +
  labs(y = "Number of Primary Bundles Stained",
       x = "Treatment",
       fill = "Treatment") +
  theme_jl
## AMOVA----