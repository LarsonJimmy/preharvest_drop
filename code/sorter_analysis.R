# Preharvest Drop: Sorter Data Analysis
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
sorterAug28 <- read_csv("data/2020/sorter/sorter_data_aug28.csv")
sorterSep1 <- read_csv("data/2020/sorter/sorter_data_sep1.csv")
sorterSep7 <- read_csv("data/2020/sorter/sorter_data_sep7.csv")
sorterSep14 <- read_csv("data/2020/sorter/sorter_data_sep14.csv")
sorterSep21 <- read_csv("data/2020/sorter/sorter_data_sep21.csv")
sorterSep28 <- read_csv("data/2020/sorter/sorter_data_sep28.csv")
sorterOct12 <- read_csv("data/2020/sorter/sorter_data_oct12.csv")
## rename treatments----
sorterAug28 %>%
  mutate(treatment2 = case_when(treatment == "AVG" ~ "avg",
                                treatment == "Control" ~ "ctrl",
                                treatment == "NAA" ~ "naa",
                                treatment == "Ethephon" ~ "ethephon")) %>%
  select(X1, treatment2, rep, BlushPercentage) %>%
  rename(treatment = treatment2) -> sorterAug28
sorterSep1 %>%
  mutate(treatment2 = case_when(treatment == "AVG" ~ "avg",
                                treatment == "Control" ~ "ctrl",
                                treatment == "NAA" ~ "naa",
                                treatment == "Ethephon" ~ "ethephon")) %>%
  select(X1, treatment2, rep, BlushPercentage) %>%
  rename(treatment = treatment2) -> sorterSep1
sorterSep7 %>%
  mutate(treatment2 = case_when(treatment == "AVG" ~ "avg",
                                treatment == "Ctrl" ~ "ctrl",
                                treatment == "NAA" ~ "naa",
                                treatment == "Ethephon" ~ "ethephon")) %>%
  select(X1, treatment2, rep, BlushPercentage) %>%
  rename(treatment = treatment2) -> sorterSep7
sorterSep14 %>%
  mutate(treatment2 = case_when(treatment == "AVG" ~ "avg",
                                treatment == "Ctrl" ~ "ctrl",
                                treatment == "NAA" ~ "naa",
                                treatment == "Ethephon" ~ "ethephon")) %>%
  select(X1, treatment2, rep, BlushPercentage) %>%
  rename(treatment = treatment2) -> sorterSep14
sorterSep21 %>%
  mutate(treatment2 = case_when(treatment == "AVG" ~ "avg",
                                treatment == "Ctrl" ~ "ctrl",
                                treatment == "NAA" ~ "naa",
                                treatment == "Ethephon" ~ "ethephon")) %>%
  select(X1, treatment2, rep, BlushPercentage) %>%
  rename(treatment = treatment2) -> sorterSep21
sorterSep28 %>%
  mutate(treatment2 = case_when(treatment == "AVG" ~ "avg",
                                treatment == "Ctrl" ~ "ctrl",
                                treatment == "NAA" ~ "naa",
                                treatment == "Ethephon" ~ "ethephon")) %>%
  select(X1, treatment2, rep, BlushPercentage) %>%
  rename(treatment = treatment2) -> sorterSep28
sorterOct12 %>%
  mutate(treatment2 = case_when(treatment == "AVG" ~ "avg",
                                treatment == "Ctrl" ~ "ctrl",
                                treatment == "NAA" ~ "naa",
                                treatment == "Ethephon" ~ "ethephon")) %>%
  select(X1, treatment2, rep, BlushPercentage) %>%
  rename(treatment = treatment2) -> sorterOct12
## Join data----
sorterAug28 %>%
  rename(blushAug28 = BlushPercentage) %>%
  left_join(sorterSep1) %>%
  rename(blushSep1 = BlushPercentage) %>%
  select(X1,treatment, rep, blushAug28, blushSep1) %>%
  left_join(sorterSep7) %>%
  rename(blushSep7 = BlushPercentage) %>%
  select(X1,treatment, rep, blushAug28, blushSep1, blushSep7) %>%
  left_join(sorterSep14) %>%
  rename(blushSep14 = BlushPercentage) %>%
  select(X1,treatment, rep, blushAug28, blushSep1, blushSep7, blushSep14) %>%
  left_join(sorterSep21) %>%
  rename(blushSep21 = BlushPercentage) %>%
  select(X1,treatment, rep, blushAug28, blushSep1, blushSep7, blushSep14, blushSep21) %>%
  left_join(sorterSep28) %>%
  rename(blushSep28 = BlushPercentage) %>%
  select(X1,treatment, rep, blushAug28, blushSep1, blushSep7, blushSep14, blushSep21, blushSep28) %>%
  left_join(sorterOct12) %>%
  rename(blushOct12 = BlushPercentage) %>%
  select(X1,treatment, rep, blushAug28, blushSep1, blushSep7, blushSep14, blushSep21, blushSep28, blushOct12) -> blush

## Long form data----
blush %>%
  gather('blushAug28', 'blushSep1', 'blushSep7', 'blushSep14', 'blushSep21', 'blushSep28',
         'blushOct12', key = "measDate", value = "blush") %>%
  mutate(date = case_when(measDate == 'blushAug28' ~ mdy("8/28/2020"),
                          measDate == 'blushSep1' ~ mdy("9/1/2020"),
                          measDate == 'blushSep7' ~ mdy("9/7/2020"),
                          measDate == 'blushSep14' ~ mdy("9/14/2020"),
                          measDate == 'blushSep21' ~ mdy("9/21/2020"),
                          measDate == 'blushSep28' ~ mdy("9/28/2020"),
                          measDate == 'blushOct12' ~ mdy("10/12/2020"))) %>%
  select(treatment, rep, date, blush) -> blushLong

## explore data----
ggplot(blushLong, aes(x = treatment, y = blush, fill = treatment)) +
  geom_boxplot() +
  facet_wrap(~date, scales = "free") +
  scale_fill_brewer(palette = "RdYlBu", labels = c("AVG", "Control", "Ethephon", "NAA")) +
  labs(y = "Red Fruit Color (% Blush)",
       x = "Treatment",
       fill = "Treatment") +
  theme_jl
## ANOVA----
### Aug. 28----
#### ANOVA model
blushAug28.aov <- aov(blushAug28 ~ treatment, data = blush)
Aug28Model <- data.frame(fit = fitted(blushAug28.aov),
                         res = resid(blushAug28.aov))
#### normality check
Aug28QQ <- ggplot(blush, aes(sample = blushAug28)) + 
  stat_qq() +
  stat_qq_line() +
  labs(y = "Sample",
       x = "Theoretical Quantiles",
       subtitle = "QQ Plot-Aug. 28") +
  theme_jl
blushAug28.resid <- residuals(object = blushAug28.aov)
shapiro.test(x = blushAug28.resid) 
#### homogeneity of variance check
Aug28Resid <- ggplot(Aug28Model, aes(x = fit, y = res))+
  geom_point()+
  geom_hline(yintercept = 0, colour = "pink")+
  scale_color_brewer(palette = "RdYlBu")+
  labs(y = "Residuals",
       x = "Fitted Values", 
       subtitle = "Residuals-Aug. 28")+
  theme_jl
leveneTest(blushAug28 ~ treatment, data = blush)
#### treatment separation
kruskal.test(blushAug28~ treatment, data = blush) #p><0.05
pairwise.wilcox.test(blush$blushAug28, blush$treatment, p.adjust.method = "BH")
### Sep. 1----
#### ANOVA model
blushSep1.aov <- aov(blushSep1 ~ treatment, data = blush)
Sep1Model <- data.frame(fit = fitted(blushSep1.aov),
                         res = resid(blushSep1.aov))
#### normality check
Sep1QQ <- ggplot(blush, aes(sample = blushSep1)) + 
  stat_qq() +
  stat_qq_line() +
  labs(y = "Sample",
       x = "Theoretical Quantiles",
       subtitle = "QQ Plot-Sep. 1") +
  theme_jl
blushSep1.resid <- residuals(object = blushSep1.aov)
shapiro.test(x = blushSep1.resid) 
#### homogeneity of variance check
Sep1Resid <- ggplot(Sep1Model, aes(x = fit, y = res))+
  geom_point()+
  geom_hline(yintercept = 0, colour = "pink")+
  scale_color_brewer(palette = "RdYlBu")+
  labs(y = "Residuals",
       x = "Fitted Values", 
       subtitle = "Residuals-Sep. 1")+
  theme_jl
leveneTest(blushSep1 ~ treatment, data = blush)
#### treatment separation
kruskal.test(blushSep1~ treatment, data = blush) #p><0.05
pairwise.wilcox.test(blush$blushSep1, blush$treatment, p.adjust.method = "BH")
### Sep. 7----
#### ANOVA model
blushSep7.aov <- aov(blushSep7 ~ treatment, data = blush)
Sep7Model <- data.frame(fit = fitted(blushSep7.aov),
                        res = resid(blushSep7.aov))
#### normality check
Sep7QQ <- ggplot(blush, aes(sample = blushSep7)) + 
  stat_qq() +
  stat_qq_line() +
  labs(y = "Sample",
       x = "Theoretical Quantiles",
       subtitle = "QQ Plot-Sep. 7") +
  theme_jl
blushSep7.resid <- residuals(object = blushSep7.aov)
shapiro.test(x = blushSep7.resid) 
#### homogeneity of variance check
Sep7Resid <- ggplot(Sep7Model, aes(x = fit, y = res))+
  geom_point()+
  geom_hline(yintercept = 0, colour = "pink")+
  scale_color_brewer(palette = "RdYlBu")+
  labs(y = "Residuals",
       x = "Fitted Values", 
       subtitle = "Residuals-Sep. 7")+
  theme_jl
leveneTest(blushSep7 ~ treatment, data = blush)
#### treatment separation
kruskal.test(blushSep7~ treatment, data = blush) #p><0.05
pairwise.wilcox.test(blush$blushSep7, blush$treatment, p.adjust.method = "BH")
### Sep. 14----
#### ANOVA model
blushSep14.aov <- aov(blushSep14 ~ treatment, data = blush)
Sep14Model <- data.frame(fit = fitted(blushSep14.aov),
                        res = resid(blushSep14.aov))
#### normality check
Sep14QQ <- ggplot(blush, aes(sample = blushSep14)) + 
  stat_qq() +
  stat_qq_line() +
  labs(y = "Sample",
       x = "Theoretical Quantiles",
       subtitle = "QQ Plot-Sep. 14") +
  theme_jl
blushSep14.resid <- residuals(object = blushSep14.aov)
shapiro.test(x = blushSep14.resid) 
#### homogeneity of variance check
Sep14Resid <- ggplot(Sep14Model, aes(x = fit, y = res))+
  geom_point()+
  geom_hline(yintercept = 0, colour = "pink")+
  scale_color_brewer(palette = "RdYlBu")+
  labs(y = "Residuals",
       x = "Fitted Values", 
       subtitle = "Residuals-Sep. 14")+
  theme_jl
leveneTest(blushSep14 ~ treatment, data = blush)
#### treatment separation
kruskal.test(blushSep14~ treatment, data = blush) #p><0.05
pairwise.wilcox.test(blush$blushSep14, blush$treatment, p.adjust.method = "BH")
### Sep. 21----
#### ANOVA model
blushSep21.aov <- aov(blushSep21 ~ treatment, data = blush)
Sep21Model <- data.frame(fit = fitted(blushSep21.aov),
                        res = resid(blushSep21.aov))
#### normality check
Sep21QQ <- ggplot(blush, aes(sample = blushSep21)) + 
  stat_qq() +
  stat_qq_line() +
  labs(y = "Sample",
       x = "Theoretical Quantiles",
       subtitle = "QQ Plot-Sep. 21") +
  theme_jl
blushSep21.resid <- residuals(object = blushSep21.aov)
shapiro.test(x = blushSep21.resid) 
#### homogeneity of variance check
Sep21Resid <- ggplot(Sep21Model, aes(x = fit, y = res))+
  geom_point()+
  geom_hline(yintercept = 0, colour = "pink")+
  scale_color_brewer(palette = "RdYlBu")+
  labs(y = "Residuals",
       x = "Fitted Values", 
       subtitle = "Residuals-Sep. 21")+
  theme_jl
leveneTest(blushSep21 ~ treatment, data = blush)
#### treatment separation
kruskal.test(blushSep21~ treatment, data = blush) #p><0.05
pairwise.wilcox.test(blush$blushSep1, blush$treatment, p.adjust.method = "BH")
### Sep. 28----
#### ANOVA model
blushSep28.aov <- aov(blushSep28 ~ treatment, data = blush)
Sep28Model <- data.frame(fit = fitted(blushSep28.aov),
                         res = resid(blushSep28.aov))
#### normality check
Sep28QQ <- ggplot(blush, aes(sample = blushSep28)) + 
  stat_qq() +
  stat_qq_line() +
  labs(y = "Sample",
       x = "Theoretical Quantiles",
       subtitle = "QQ Plot-Sep. 28") +
  theme_jl
blushSep28.resid <- residuals(object = blushSep28.aov)
shapiro.test(x = blushSep28.resid) 
#### homogeneity of variance check
Sep28Resid <- ggplot(Sep28Model, aes(x = fit, y = res))+
  geom_point()+
  geom_hline(yintercept = 0, colour = "pink")+
  scale_color_brewer(palette = "RdYlBu")+
  labs(y = "Residuals",
       x = "Fitted Values", 
       subtitle = "Residuals-Sep. 28")+
  theme_jl
leveneTest(blushSep28 ~ treatment, data = blush)
#### treatment separation
kruskal.test(blushSep28~ treatment, data = blush) #p><0.05
pairwise.wilcox.test(blush$blushSep28, blush$treatment, p.adjust.method = "BH")
### Oct. 12----
#### ANOVA model
blushOct12.aov <- aov(blushOct12 ~ treatment, data = blush)
Oct12Model <- data.frame(fit = fitted(blushOct12.aov),
                        res = resid(blushOct12.aov))
#### normality check
Oct12QQ <- ggplot(blush, aes(sample = blushOct12)) + 
  stat_qq() +
  stat_qq_line() +
  labs(y = "Sample",
       x = "Theoretical Quantiles",
       subtitle = "QQ Plot-Oct. 12") +
  theme_jl
blushOct12.resid <- residuals(object = blushOct12.aov)
shapiro.test(x = blushOct12.resid) 
#### homogeneity of variance check
Oct12Resid <- ggplot(Oct12Model, aes(x = fit, y = res))+
  geom_point()+
  geom_hline(yintercept = 0, colour = "pink")+
  scale_color_brewer(palette = "RdYlBu")+
  labs(y = "Residuals",
       x = "Fitted Values", 
       subtitle = "Residuals-Oct. 12")+
  theme_jl
leveneTest(blushOct12 ~ treatment, data = blush)
#### treatment separation
kruskal.test(blushOct12~ treatment, data = blush) #p><0.05
pairwise.wilcox.test(blush$blushOct12, blush$treatment, p.adjust.method = "BH")
## fig----
treatmentCat <- c("ctrl", "avg", "naa", "ethephon")
blushLong %>%
  drop_na(blush) %>%
  group_by(date, treatment) %>%
  summarise(med.blush = median(blush)) %>%
  arrange(date, match(treatment, treatmentCat)) -> blush.med
ggplot(blush.med, aes(x = date, y = med.blush, color = treatment)) +
  geom_point() +
  geom_line() +
  geom_text_repel(aes(x = date, y= med.blush, label = TeX(c(rep("a*", times = 28)), output = "character")), vjust=1,
                  color="black", position = position_dodge(2), size= 3,parse = T, min.segment.length = 1.9) +
  scale_color_brewer(palette = "Set2", labels = c("AVG", "Control", "Ethephon", "NAA")) +
  labs(y = "Blush (%)",
       x = "Date",
       color = "Treatment",
       title = "Red Fruit Color",
       subtitle = "Median Values of Each Treatment by Date",
       caption = "*Kruskal-Wallis test")+
  theme_jl
ggsave("figs/blush_plot.png")  
