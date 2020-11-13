# Preharvest Drop: Drop Count Data Analysis
# author: Jimmy Larson
# created: 10/23/20
# last edited: 10/26/20

## load packages----
library(tidyverse)
library(ggpubr)
library(car)
library(lubridate)
library(RColorBrewer)
#install.packages("ggrepel")
library(ggrepel)
#install.packages("hrbrthemes")
library(hrbrthemes)
## plotting theme----
theme_jl <- theme_ipsum_ps(grid="XY", axis="xy", axis_text_size = 10, axis_title_size = 11, axis_col = "black")
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
  mutate(totalFruit = cumDropOct21 + remainingFruit) %>%
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
ggplot(dropsLong, aes(x = treatment, y = drops, fill = treatment)) +
  geom_boxplot() +
  facet_wrap(~ date, scales = "free") +
  scale_fill_brewer(palette = "RdYlBu") +
  labs(y = "Percent Fruit Drop (%)",
       x = "Treatment") +
  theme_jl
## Analyze and plot drop percent----
### anova test
drops.aov <- aov(drops ~ treatment*date, data = dropsLong)
summary.aov(drops.aov) # treatment*date interaction is significant, analyze within dates
### anova for each date
#### Aug 25----
dropAug25.aov <- aov(perDropAug25 ~ treatment, data = drops)
summary.aov(dropAug25.aov)
Aug25Model <- data.frame(fit = fitted(dropAug25.aov),
                         res = resid(dropAug25.aov),
                         treat = drops$treatment)
#### test normality
Aug25QQ <- ggplot(drops, aes(sample = perDropAug25)) + 
  stat_qq() +
  stat_qq_line() +
  labs(y = "Sample",
       x = "Theoretical Quantiles",
       title = "QQ Plot",
       subtitle = "Aug. 25") +
  theme_jl
dropAug25.resid <- residuals(object = dropAug25.aov)
#shapiro.test(x = dropAug25.resid)## p < 0.05
shapiro.test(x = sqrt(dropAug25.resid)) ## p > 0.05
##### test homogeniety of variance
Aug25Resid <- ggplot(Aug25Model, aes(x = fit, y = res))+
  geom_point()+
  geom_hline(yintercept = 0, colour = "pink")+
  scale_color_brewer(palette = "RdYlBu")+
  labs(y = "Residuals",
       x = "Fitted Values", 
       title = "Residuals vs. Fitted",
       subtitle = "Aug. 25")+
  theme_jl
leveneTest(perDropAug25 ~ treatment, data = drops) ## p > 0.05
#### mean separation
dropAug25.norm.aov <- aov(sqrt(perDropAug25) ~ treatment, data = drops) # had to transform data with sqrt to normalize
TukeyHSD(dropAug25.norm.aov)
#### Sep 1----
dropSep1.aov <- aov(perDropSep1 ~ treatment, data = drops)
summary.aov(dropSep1.aov)
Sep1Model <- data.frame(fit = fitted(dropSep1.aov),
                         res = resid(dropSep1.aov),
                         treat = drops$treatment)
##### test normality
Sep1QQ <- ggplot(drops, aes(sample = perDropSep1)) + 
  stat_qq() +
  stat_qq_line() +
  labs(y = "Sample",
       x = "Theoretical Quantiles",
       title = "QQ Plot",
       subtitle = "Sep. 1") +
  theme_jl
dropSep1.resid <- residuals(object = dropSep1.aov)
shapiro.test(x = dropSep1.resid) ## p > 0.05
##### test homogeniety of variance
Sep1Resid <- ggplot(Sep1Model, aes(x = fit, y = res))+
  geom_point()+
  geom_hline(yintercept = 0, colour = "pink")+
  scale_color_brewer(palette = "RdYlBu")+
  labs(y = "Residuals",
       x = "Fitted Values",
       title = "Residuals vs. Fitted",
       subtitle = "Sep. 1")+
  theme_jl
leveneTest(perDropSep1 ~ treatment, data = drops) ## p > 0.05
#### mean separation
TukeyHSD(dropSep1.aov)
#### Sep 8----
dropSep8.aov <- aov(perDropSep8 ~ treatment, data = drops)
summary.aov(dropSep8.aov)
Sep8Model <- data.frame(fit = fitted(dropSep8.aov),
                        res = resid(dropSep8.aov),
                        treat = drops$treatment)
#### test normality
Sep8QQ <- ggplot(drops, aes(sample = perDropSep8)) + 
  stat_qq() +
  stat_qq_line() +
  labs(y = "Sample",
       x = "Theoretical Quantiles",
       title = "QQ Plot",
       subtitle = "Sep. 8") +
  theme_jl
dropSep8.resid <- residuals(object = dropSep8.aov)
shapiro.test(x = dropSep8.resid) ## p > 0.05
##### test homogeniety of variance
Sep8Resid <- ggplot(Sep8Model, aes(x = fit, y = res))+
  geom_point()+
  geom_hline(yintercept = 0, colour = "pink")+
  scale_color_brewer(palette = "RdYlBu")+
  labs(y = "Residuals",
       x = "Fitted Values", 
       title = "Residuals vs. Fitted",
       subtitle = "Sep. 8")+
  theme_jl
leveneTest(perDropSep8 ~ treatment, data = drops) ## p > 0.05
#### mean separation
TukeyHSD(dropSep8.aov)
#### Sep 15----
dropSep15.aov <- aov(perDropSep15 ~ treatment, data = drops)
summary.aov(dropSep15.aov)
Sep15Model <- data.frame(fit = fitted(dropSep15.aov),
                        res = resid(dropSep15.aov),
                        treat = drops$treatment)
#### test normality
Sep15QQ <- ggplot(drops, aes(sample = perDropSep15)) + 
  stat_qq() +
  stat_qq_line() +
  labs(y = "Sample",
       x = "Theoretical Quantiles",
       title = "QQ Plot",
       subtitle = "Sep. 15") +
  theme_jl
dropSep15.resid <- residuals(object = dropSep15.aov)
shapiro.test(x = dropSep15.resid)## p > 0.05
##### test homogeniety of variance
Sep15Resid <- ggplot(Sep15Model, aes(x = fit, y = res))+
  geom_point()+
  geom_hline(yintercept = 0, colour = "pink")+
  scale_color_brewer(palette = "RdYlBu")+
  labs(y = "Residuals",
       x = "Fitted Values", 
       title = "Residuals vs. Fitted",
       subtitle = "Sep. 15")+
  theme_jl
leveneTest(perDropSep15 ~ treatment, data = drops) ## p > 0.05
#### mean separation
TukeyHSD(dropSep15.aov)
#### Sep 22----
dropSep22.aov <- aov(perDropSep22 ~ treatment, data = drops)
summary.aov(dropSep22.aov)
Sep22Model <- data.frame(fit = fitted(dropSep22.aov),
                        res = resid(dropSep22.aov),
                        treat = drops$treatment)
#### test normality
Sep22QQ <- ggplot(drops, aes(sample = perDropSep22)) + 
  stat_qq() +
  stat_qq_line() +
  labs(y = "Sample",
       x = "Theoretical Quantiles",
       title = "QQ Plot",
       subtitle = "Sep. 22") +
  theme_jl
dropSep22.resid <- residuals(object = dropSep22.aov)
#shapiro.test(x = dropSep22.resid) ## p<0.05
shapiro.test(x = (dropSep22.resid)^(-.5)) ## p>0.05
##### test homogeniety of variance
Sep22Resid <- ggplot(Sep22Model, aes(x = fit, y = res))+
  geom_point()+
  geom_hline(yintercept = 0, colour = "pink")+
  scale_color_brewer(palette = "RdYlBu")+
  labs(y = "Residuals",
       x = "Fitted Values", 
       title = "Residuals vs. Fitted",
       subtitle = "Sep. 22")+
  theme_jl
leveneTest(perDropSep22 ~ treatment, data = drops) ## p > 0.05
#### mean separation
dropSep22.norm.aov <- aov((perDropSep22)^(-.5) ~ treatment, data = drops) ## used 1/sqrt() to normalize
TukeyHSD(dropSep22.norm.aov)
#### Sep 29----
dropSep29.aov <- aov(perDropSep29 ~ treatment, data = drops)
summary.aov(dropSep29.aov)
Sep29Model <- data.frame(fit = fitted(dropSep29.aov),
                        res = resid(dropSep29.aov),
                        treat = drops$treatment)
#### test normality
Sep29QQ <- ggplot(drops, aes(sample = perDropSep29)) + 
  stat_qq() +
  stat_qq_line() +
  labs(y = "Sample",
       x = "Theoretical Quantiles",
       title = "QQ Plot",
       subtitle = "Sep. 29") +
  theme_jl
dropSep29.resid <- residuals(object = dropSep29.aov)
shapiro.test(x = dropSep29.resid) ## p>0.05
##### test homogeniety of variance
Sep29Resid <- ggplot(Sep29Model, aes(x = fit, y = res))+
  geom_point()+
  geom_hline(yintercept = 0, colour = "pink")+
  scale_color_brewer(palette = "RdYlBu")+
  labs(y = "Residuals",
       x = "Fitted Values", 
       title = "Residuals vs. Fitted",
       subtitle = "Sep. 29")+
  theme_jl
leveneTest(perDropSep29 ~ treatment, data = drops) ## p > 0.05
#### mean separation
TukeyHSD(dropSep29.aov)
#### Oct 7----
dropOct7.aov <- aov(perDropOct7 ~ treatment, data = drops)
summary.aov(dropOct7.aov)
Oct7Model <- data.frame(fit = fitted(dropOct7.aov),
                         res = resid(dropOct7.aov),
                         treat = drops$treatment)
#### test normality
Oct7QQ <- ggplot(drops, aes(sample = perDropOct7)) + 
  stat_qq() +
  stat_qq_line() +
  labs(y = "Sample",
       x = "Theoretical Quantiles",
       title = "QQ Plot",
       subtitle = "Oct. 7") +
  theme_jl
dropOct7.resid <- residuals(object = dropOct7.aov)
shapiro.test(x = dropOct7.resid) ## p>0.05
##### test homogeniety of variance
Oct7Resid <- ggplot(Oct7Model, aes(x = fit, y = res))+
  geom_point()+
  geom_hline(yintercept = 0, colour = "pink")+
  scale_color_brewer(palette = "RdYlBu")+
  labs(y = "Residuals",
       x = "Fitted Values", 
       title = "Residuals vs. Fitted",
       subtitle = "Oct. 7")+
  theme_jl
leveneTest(perDropOct7 ~ treatment, data = drops) ## p > 0.05
#### mean separation
TukeyHSD(dropOct7.aov)
#### Oct 13----
dropOct13.aov <- aov(perDropOct13 ~ treatment, data = drops)
summary.aov(dropOct13.aov)
Oct13Model <- data.frame(fit = fitted(dropOct13.aov),
                         res = resid(dropOct13.aov),
                         treat = drops$treatment)
#### test normality
Oct13QQ <- ggplot(drops, aes(sample = perDropOct13)) + 
  stat_qq() +
  stat_qq_line() +
  labs(y = "Sample",
       x = "Theoretical Quantiles",
       title = "QQ Plot",
       subtitle = "Oct. 13") +
  theme_jl
dropOct13.resid <- residuals(object = dropOct13.aov)
shapiro.test(x = dropOct13.resid) ## p>0.05
##### test homogeniety of variance
Oct13Resid <- ggplot(Oct13Model, aes(x = fit, y = res))+
  geom_point()+
  geom_hline(yintercept = 0, colour = "pink")+
  scale_color_brewer(palette = "RdYlBu")+
  labs(y = "Residuals",
       x = "Fitted Values", 
       title = "Residuals vs. Fitted",
       subtitle = "Oct. 13")+
  theme_jl
leveneTest(perDropOct13 ~ treatment, data = drops) ## p > 0.05
#### mean separation
TukeyHSD(dropOct13.aov)
#### Oct 21----
dropOct21.aov <- aov(perDropOct21 ~ treatment, data = drops)
summary.aov(dropOct21.aov)
Oct21Model <- data.frame(fit = fitted(dropOct21.aov),
                         res = resid(dropOct21.aov),
                         treat = drops$treatment)
#### test normality
Oct21QQ <- ggplot(drops, aes(sample = perDropOct21)) + 
  stat_qq() +
  stat_qq_line() +
  labs(y = "Sample",
       x = "Theoretical Quantiles",
       title = "QQ Plot",
       subtitle = "Oct. 21") +
  theme_jl
dropOct21.resid <- residuals(object = dropOct21.aov)
shapiro.test(x = dropOct21.resid) ## p>0.05
##### test homogeniety of variance
Oct21Resid <- ggplot(Oct21Model, aes(x = fit, y = res))+
  geom_point()+
  geom_hline(yintercept = 0, colour = "pink")+
  scale_color_brewer(palette = "RdYlBu")+
  labs(y = "Residuals",
       x = "Fitted Values",
       title = "Residuals vs. Fitted",
       subtitle = "Oct. 21")+
  theme_jl
leveneTest(perDropOct21 ~ treatment, data = drops) ## p > 0.05
#### mean separation
TukeyHSD(dropOct21.aov)

ggarrange(Aug25QQ, Aug25Resid, Sep1QQ, Sep1Resid, Sep8QQ, Sep8Resid, ncol = 2, nrow = 3)
ggarrange(Sep15QQ, Sep15Resid, Sep22QQ, Sep22Resid, Sep29QQ, Sep29Resid, ncol = 2, nrow = 3)

ggarrange(Oct7QQ, Oct7Resid, Oct13QQ, Oct13Resid, Oct21QQ, Oct21Resid, ncol = 2, nrow = 3)

## plot data----
treatmentCat <- c("ctrl", "avg", "naa", "ethephon")
sep <- c(rep("a", times = 20), rep(c("a", "b", "b", "a"), times = 4))
dropsLong %>%
  group_by(date, treatment) %>%
  summarise(avg.drop = mean(drops)) %>%
  arrange(date, match(treatment, treatmentCat)) %>%
  add_column(sep = sep) %>%
  ggplot(aes(x = date, y = avg.drop, color = treatment)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = mdy("8/26/2020"), colour = "pink", linetype = "dashed") +
  geom_vline(xintercept = mdy("9/16/2020"), colour = "green") +
  annotate("text", x = mdy("9/16/2020") - 1, y = 30, label = "Harvest Date", angle = 90, size = 3) +
  annotate("text", x = mdy("8/26/2020") - 1, y = 30, label = "Treatment Application", angle = 90, size = 3) +
  geom_text_repel(aes(x = date, y= avg.drop, label = c(rep(NA, times = 16), "a", "a", "a", "a",
                                                       rep(c("a", "b", "b", "a"), times = 4))), vjust=1, color="black",
            position = position_dodge(2), size= 3,parse = T, min.segment.length = 1.9) +
  scale_color_brewer(palette = "Set2", labels = c("AVG", "Control", "Ethephon", "NAA")) +
  labs(y = "Cumulative Percent Drop (%)",
       x = "Date",
       color = "Treatment",
       title = "Cumulative Fruit Drop")+
  theme_jl
ggsave("figs/drops_plot2.png")
