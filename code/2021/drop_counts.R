#Preharvest Drop: Drop Counts 2021
#authot: Jimmy Larson
#created: 11.3.21
#last edited:11.3.21

## load packages----
library(tidyverse)
library(ggpubr)
library(car)
library(lubridate)
library(RColorBrewer)
library(ggrepel)
library(hrbrthemes)
## plotting theme----
theme_jl <- theme_ipsum_ps(grid="XY", axis="xy", axis_text_size = 10, axis_title_size = 11, axis_col = "black")
## load in data----
dropsAug29 <- read_csv("data/2021/drops/drops_21JLPFD_aug29.csv")
dropsSep7 <- read_csv("data/2021/drops/drops_21JLPFD_sep7.csv")
dropsSep9 <- read_csv("data/2021/drops/drops_21JLPFD_sep9.csv")
dropsSep15 <- read_csv("data/2021/drops/drops_21JLPFD_sep15.csv")
dropsSep23 <- read_csv("data/2021/drops/drops_21JLPFD_sep23.csv")
dropsSep28 <- read_csv("data/2021/drops/drops_21JLPFD_sep28.csv")
dropsOct11 <- read_csv("data/2021/drops/drops_21JLPFD_oct11.csv")
dropsOct20 <- read_csv("data/2021/drops/drops_21JLPFD_oct20.csv")
dropsOct25 <- read_csv("data/2021/drops/drops_21JLPFD_oct25.csv")
harvest <- read_csv("data/2021/drops/harvest_21JLPFD_oct25.csv")
## calculate total fruit on tree----
harvest %>%
  rowwise() %>%
  mutate(remainingFruit = sum(harvest1, harvest2, harvest3, harvest4, na.rm = TRUE)) %>%
  select(plot, treatment, rep, remainingFruit) %>%
  left_join(dropsAug29) %>%
  rename(Aug29drops = drops) %>%
  left_join(dropsSep7) %>%
  rename(Sep7drops = drops) %>%
  mutate(cumDropSep7 = Aug29drops + Sep7drops) %>%
  left_join(dropsSep9) %>%
  rename(Sep9drops = drops) %>%
  left_join(dropsSep15) %>%
  rename(Sep15drops = drops) %>%
  mutate(cumDropSep15 = cumDropSep7 + Sep9drops + Sep15drops) %>%
  left_join(dropsSep23) %>%
  rename(Sep23drops = drops) %>%
  mutate(cumDropSep23 = cumDropSep15 + Sep23drops) %>%
  left_join(dropsSep28) %>%
  rename(Sep28drops = drops) %>%
  mutate(cumDropSep28 = cumDropSep23 + Sep28drops) %>%
  left_join(dropsOct11) %>%
  rename(Oct11drops = drops) %>%
  mutate(cumDropOct11 = cumDropSep28 + Oct11drops)%>%
  left_join(dropsOct20) %>%
  rename(Oct20drops = drops) %>%
  mutate(cumDropOct20 = cumDropOct11 + Oct20drops) %>%
  left_join(dropsOct25) %>%
  rename(Oct25drops = drops) %>%
  mutate(cumDropOct25 = cumDropOct20 + Oct25drops) %>%
  mutate(totalFruit = cumDropOct25 + remainingFruit) %>%
  mutate(perDropAug29 = ((Aug29drops/totalFruit)*100),
         perDropSep7 = ((cumDropSep7/totalFruit)*100), 
         perDropSep15 = ((cumDropSep15/totalFruit)*100),
         perDropSep23 = ((cumDropSep23/totalFruit)*100),
         perDropSep28 = ((cumDropSep28/totalFruit)*100),
         perDropOct11 = ((cumDropOct11/totalFruit)*100),
         perDropOct20 = ((cumDropOct20/totalFruit)*100),
         perDropOct25 = ((cumDropOct25/totalFruit)*100)) %>%
  select(plot, treatment, rep, perDropAug29, perDropSep7, perDropSep15, perDropSep23, perDropSep28, perDropOct11, perDropOct20, perDropOct25) -> drops
## long form data----
drops %>%
  gather('perDropAug29', 'perDropSep7', 'perDropSep15', 'perDropSep23', 'perDropSep28', 'perDropOct11', 'perDropOct20', 'perDropOct25',
         key = "measDate", value = "drops") %>%
  mutate(date = case_when(measDate == 'perDropAug29' ~ ymd(20210829),
                          measDate == 'perDropSep7' ~ ymd(20210907),
                          measDate == 'perDropSep15' ~ ymd(20210915),
                          measDate == 'perDropSep23' ~ ymd(20210923),
                          measDate == 'perDropSep28' ~ ymd(20210928),
                          measDate == 'perDropOct11' ~ ymd(20211011),
                          measDate == 'perDropOct20' ~ ymd(20211020),
                          measDate == 'perDropOct25' ~ ymd(20211025))) -> dropsLong
## exploratory data viz----
dropsLong %>%
  group_by(date, treatment) %>%
  summarise(meanDrops = mean(drops)) %>%
  ggplot(aes(x = date, y = meanDrops, color = treatment)) +
  geom_point()+
  geom_line()+
  theme_bw()
## treatment separation----
### Aug 29
#### anova
aug29.aov <- aov(perDropAug29 ~ treatment, data = drops)
#### normality
aug29.resid <- residuals(object = aug29.aov)
shapiro.test(x = aug29.resid)
### had to transform data with sqrt to normalize
sqrtAug29.aov <- aov(sqrt(perDropAug29) ~ treatment, data = drops)
#### normality
sqrtAug29.resid <- residuals(object = sqrtAug29.aov)
shapiro.test(x = sqrtAug29.resid)
#### homogeneity of variance
leveneTest(perDropAug29 ~ treatment, data = drops)
#### separation
TukeyHSD(sqrtAug29.aov)
### Sep 7
#### anova
sep7.aov <- aov(perDropSep7 ~ treatment, data = drops)
#### normality
sep7.resid <- residuals(object = sep7.aov)
shapiro.test(x = sep7.resid)
#### homogeneity of variance
leveneTest(perDropSep7 ~ treatment, data = drops)
#### separation
TukeyHSD(sep7.aov)
### Sep 15
#### anova
sep15.aov <- aov(perDropSep15 ~ treatment, data = drops)
#### normality
sep15.resid <- residuals(object = sep15.aov)
shapiro.test(x = sep15.resid)
#### homogeneity of variance
leveneTest(perDropSep15 ~ treatment, data = drops)
#### separation
TukeyHSD(sep15.aov)
### Sep 23
#### anova
sep23.aov <- aov(perDropSep23 ~ treatment, data = drops)
#### normality
sep23.resid <- residuals(object = sep23.aov)
shapiro.test(x = sep23.resid)
#### homogeneity of variance
leveneTest(perDropSep23 ~ treatment, data = drops)
#### separation
TukeyHSD(sep23.aov)
### Sep 28
#### anova
sep28.aov <- aov(perDropSep28 ~ treatment, data = drops)
#### normality
sep28.resid <- residuals(object = sep28.aov)
shapiro.test(x = sep28.resid)
#### homogeneity of variance
leveneTest(perDropSep28 ~ treatment, data = drops)
#### separation
TukeyHSD(sep28.aov)
### Oct11
#### anova
oct11.aov <- aov(perDropOct11 ~ treatment, data = drops)
#### normality
oct11.resid <- residuals(object = oct11.aov)
shapiro.test(x = oct11.resid)
#### homogeneity of variance
leveneTest(perDropOct11 ~ treatment, data = drops)
#### separation
TukeyHSD(oct11.aov)
## plotting----
treatmentCat <- c("ctrl", "avg", "naa", "ethephon")
dropsLong %>%
  filter(!(date == "2021-10-20"|date=="2021-10-25")) %>%
  group_by(date, treatment) %>%
  summarise(meanDrops = mean(drops)) %>%
  arrange(date, match(treatment, treatmentCat)) %>%
  ggplot(aes(x = date, y = meanDrops, color = treatment)) +
  geom_point()+
  geom_line()+
  geom_vline(xintercept = mdy("8/25/2021"), colour = "pink", linetype = "dashed") +
  annotate("text", x = mdy("8/25/2021") - 1, y = 30, label = "Treatment Application", angle = 90, size = 3) +
  geom_text_repel(aes(x = date, y= meanDrops, label = c(rep(NA, times = 16), "a", "b", "a", "ab","a", "b", "a", "a")), vjust=1, color="black",
                  position = position_dodge(2), size= 3,parse = T, min.segment.length = 1.9) +
  scale_color_brewer(palette = "Set2", labels = c("AVG", "Control", "Ethephon", "NAA")) +
  labs(y = "Cumulative Percent Drop (%)",
       x = "Date",
       color = "Treatment",
       title = "Cumulative Fruit Drop - 2021")+
  theme_jl
ggsave("figs/2021/drops.png")
