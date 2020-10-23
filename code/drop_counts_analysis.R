# Preharvest Drop: Drop Count Data Analysis
# author: Jimmy Larson
# created: 10/23/20
# last edited: 10/23/20

## load packages----
library(tidyverse)
library(ggpubr)
library(car)
library(lubridate)
## load data----
dropAug25 <- read_csv("data/2020/dropCounts/pfd_fruit_drop_counts_aug_25.csv")
dropSep1 <- read_csv("data/2020/dropCounts/pfd_fruit_drop_counts_sep_1.csv")
dropSep8 <- read_csv("data/2020/dropCounts/pfd_fruit_drop_counts_sep_8.csv")
dropSep15 <- read_csv("data/2020/dropCounts/pfd_fruit_drop_counts_sep_15.csv")
dropSep22 <- read_csv("data/2020/dropCounts/pfd_fruit_drop_counts_sep_22.csv")
dropSep29 <- read_csv("data/2020/dropCounts/pfd_fruit_drop_counts_sep_29.csv")
dropOct7 <- read_csv("data/2020/dropCounts/pfd_fruit_drop_counts_oct_7.csv")
dropOct13 <- read_csv("data/2020/dropCounts/pfd_fruit_drop_counts_oct_13.csv")
dropOct21 <- read_csv("data/2020/dropCounts/pfd_fruit_drop_counts_oct_21.csv")
treeCounts <- read_csv("data/2020/dropCounts/pfd_fruit_tree_counts_oct_21.csv")
## calculate remaining fruit on treee----
treeCounts %>%
  rowwise() %>%
  mutate(remainingFruit = sum(count1,count2,count3,count4,count5,count6,count7, na.rm = TRUE)) %>%
  select(row, tree, plot, treatment, rep, flag, remainingFruit) %>%
  left_join(dropAug25) %>%
  rename(Aug25Drop = dropped) %>%
  left_join(dropSep1) %>%
  rename(Sep1Drop = dropped)%>%
  mutate(cumDropSep1 = Aug25Drop + Sep1Drop) %>%
  left_join(dropSep8) %>%
  rename(Sep8Drop = dropped)%>%
  mutate(cumDropSep8 = Sep8Drop + cumDropSep1) %>%
  left_join(dropSep15) %>%
  rename(Sep15Drop = dropped)%>%
  mutate(cumDropSep15 = Sep15Drop + cumDropSep8) %>%
  left_join(dropSep22) %>%
  rename(Sep22Drop = dropped)%>%
  mutate(cumDropSep22 = Sep22Drop + cumDropSep15) %>%
  left_join(dropSep29) %>%
  rename(Sep29Drop = dropped)%>%
  mutate(cumDropSep29 = Sep29Drop + cumDropSep22) %>%
  left_join(dropOct7) %>%
  rename(Oct7Drop = dropped)%>%
  mutate(cumDropOct7 = Oct7Drop + cumDropSep29) %>%
  left_join(dropOct13) %>%
  rename(Oct13Drop = dropped)%>%
  mutate(cumDropOct13 = Oct13Drop + cumDropOct7) %>%
  left_join(dropOct21) %>%
  rename(Oct21Drop = dropped) %>%
  mutate(cumDropOct21 = Oct21Drop + cumDropOct13) %>%
  mutate(totalFruit = cumDropOct13 + remainingFruit) %>%
  mutate(perDropAug25 = ((Aug25Drop / totalFruit)*100),
         perDropSep1 = ((cumDropSep1 / totalFruit)*100),
         perDropSep8 = ((cumDropSep8 / totalFruit)*100),
         perDropSep15 = ((cumDropSep15 / totalFruit)*100),
         perDropSep22 = ((cumDropSep22 / totalFruit)*100),
         perDropSep29 = ((cumDropSep29 / totalFruit)*100),
         perDropOct7 = ((cumDropOct7 / totalFruit)*100),
         perDropOct13 = ((cumDropOct13 / totalFruit)*100),
         perDropOct21 = ((cumDropOct21 / totalFruit)*100))%>%
  select(row, tree, plot, treatment, rep, perDropAug25, perDropSep1, perDropSep8, perDropSep15, perDropSep22,
         perDropSep29, perDropOct7, perDropOct13, perDropOct21)-> drops
## create long form data ----
drops %>%
  gather('perDropAug25', 'perDropSep1', 'perDropSep8', 'perDropSep15', 'perDropSep22', 'perDropSep29',
         'perDropOct7', 'perDropOct13', 'perDropOct21', key = "measDate", value = "drops") %>%
  mutate(date = case_when(measDate == 'perDropAug25' ~ mdy("8/25/2020"), ### add date
                   measDate == 'perDropSep1' ~ mdy("9/1/2020"),
                   measDate == 'perDropSep8' ~ mdy("9/8/2020"),
                   measDate == 'perDropSep15' ~ mdy("9/15/2020"),
                   measDate == 'perDropSep22' ~ mdy("9/22/2020"),
                   measDate == 'perDropSep29' ~ mdy("9/29/2020"),
                   measDate == 'perDropOct7' ~ mdy("10/7/2020"),
                   measDate == 'perDropOct13' ~ mdy("10/13/2020"),
                   measDate == 'perDropOct21' ~ mdy("10/21/2020"))) %>%
  select(row, tree, plot, treatment, rep, drops, date) -> dropsLong
## explore data ----
ggplot(dropsLong, aes(x = treatment, y = drops)) +
  geom_boxplot() +
  facet_wrap(~ date, scales = "free") +
  labs(y = "Percent Fruit Drop (%)",
       x = "Treatment") +
  theme_bw()
## Analyze and plot drop percent----
### anova test
drops.aov <- aov(drops ~ treatment*date, data = dropsLong)
summary.aov(drops.aov) # treatment*date interaction is significant, analyze within dates
### anova for each date
#### Aug 25----
dropAug25.aov <- aov(perDropAug25 ~ treatment, data = drops)
summary.aov(dropAug25.aov)
#### test normality
plot(dropAug25.aov, 2)
dropAug25.resid <- residuals(object = dropAug25.aov)
shapiro.test(x = dropAug25.resid) ## p < 0.05
##### test homogeniety of variance
plot(dropAug25.aov, 1)
leveneTest(perDropAug25 ~ treatment, data = drops) ## p > 0.05
#### Sep 1----
dropSep1.aov <- aov(perDropSep1 ~ treatment, data = drops)
summary.aov(dropSep1.aov)
##### test normality
plot(dropSep1.aov, 2)
dropSep1.resid <- residuals(object = dropSep1.aov)
shapiro.test(x = dropSep1.resid) ## p > 0.05
##### test homogeniety of variance
plot(dropSep1.aov, 1)
leveneTest(perDropSep1 ~ treatment, data = drops) ## p > 0.05
#### Sep 8----
dropSep8.aov <- aov(perDropSep8 ~ treatment, data = drops)
summary.aov(dropSep8.aov)
#### test normality
plot(dropSep8.aov, 2)
dropSep8.resid <- residuals(object = dropSep8.aov)
shapiro.test(x = dropSep8.resid) ## p > 0.05
##### test homogeniety of variance
plot(dropSep8.aov, 1)
leveneTest(perDropSep8 ~ treatment, data = drops) ## p > 0.05
#### Sep 15----
dropSep15.aov <- aov(perDropSep15 ~ treatment, data = drops)
summary.aov(dropSep15.aov)
#### test normality
plot(dropSep15.aov, 2)
dropSep15.resid <- residuals(object = dropSep15.aov)
shapiro.test(x = dropSep15.resid)## p > 0.05
##### test homogeniety of variance
plot(dropSep15.aov, 1)
leveneTest(perDropSep15 ~ treatment, data = drops) ## p > 0.05
#### Sep 22----
dropSep22.aov <- aov(perDropSep22 ~ treatment, data = drops)
summary.aov(dropSep22.aov)
#### test normality
plot(dropSep22.aov, 2)
dropSep22.resid <- residuals(object = dropSep22.aov)
shapiro.test(x = dropSep22.resid) ## p<0.05
##### test homogeniety of variance
plot(dropSep22.aov, 1)
leveneTest(perDropSep22 ~ treatment, data = drops) ## p > 0.05
#### Sep 29----
dropSep29.aov <- aov(perDropSep29 ~ treatment, data = drops)
summary.aov(dropSep29.aov)
#### test normality
plot(dropSep29.aov, 2)
dropSep29.resid <- residuals(object = dropSep29.aov)
shapiro.test(x = dropSep29.resid) ## p>0.05
##### test homogeniety of variance
plot(dropSep29.aov, 1)
leveneTest(perDropSep29 ~ treatment, data = drops) ## p > 0.05
#### Oct 7----
dropOct7.aov <- aov(perDropOct7 ~ treatment, data = drops)
summary.aov(dropOct7.aov)
#### test normality
plot(dropOct7.aov, 2)
dropOct7.resid <- residuals(object = dropOct7.aov)
shapiro.test(x = dropOct7.resid) ## p>0.05
##### test homogeniety of variance
plot(dropOct7.aov, 1)
leveneTest(perDropOct7 ~ treatment, data = drops) ## p > 0.05
#### Oct 13----
dropOct13.aov <- aov(perDropOct13 ~ treatment, data = drops)
summary.aov(dropOct13.aov)
#### test normality
plot(dropOct13.aov, 2)
dropOct13.resid <- residuals(object = dropOct13.aov)
shapiro.test(x = dropOct13.resid) ## p>0.05
##### test homogeniety of variance
plot(dropOct13.aov, 1)
leveneTest(perDropOct13 ~ treatment, data = drops) ## p > 0.05
#### Oct 21----
dropOct21.aov <- aov(perDropOct21 ~ treatment, data = drops)
summary.aov(dropOct21.aov)
#### test normality
plot(dropOct21.aov, 2)
dropOct21.resid <- residuals(object = dropOct21.aov)
shapiro.test(x = dropOct21.resid) ## p>0.05
##### test homogeniety of variance
plot(dropOct21.aov, 1)
leveneTest(perDropOct21 ~ treatment, data = drops) ## p > 0.05
