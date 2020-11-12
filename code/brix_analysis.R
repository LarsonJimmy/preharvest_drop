# Preharvest Drop: Brix Data Analysis
# author: Jimmy Larson
# created: 10/28/20
# last edited: 10/28/20

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
## load data----
brixAug28 <- read_csv("data/2020/brix/pfd_brix_aug_28.csv")
brixSep1 <- read_csv("data/2020/brix/pfd_brix_sep_1.csv")
brixSep7 <- read_csv("data/2020/brix/pfd_brix_sep_7.csv")
brixSep14 <- read_csv("data/2020/brix/pfd_brix_sep_14.csv")
brixSep21 <- read_csv("data/2020/brix/pfd_brix_sep_21.csv")
brixSep29 <- read_csv("data/2020/brix/pfd_brix_sep_29.csv")
brixOct12 <- read_csv("data/2020/brix/pfd_brix_oct_12.csv")
## join data----
brixAug28 %>%
  rename(brixAug28 = brix) %>%
  left_join(brixSep1) %>%
  rename(brixSep1 = brix) %>%
  left_join(brixSep7) %>%
  rename(brixSep7 = brix) %>%
  left_join(brixSep14) %>%
  rename(brixSep14 = brix) %>%
  left_join(brixSep21) %>%
  rename(brixSep21 = brix) %>%
  left_join(brixSep29) %>%
  rename(brixSep29 = brix) %>%
  left_join(brixOct12) %>%
  rename(brixOct12 = brix) -> brix
## long form data----
brix %>%
  gather('brixAug28', 'brixSep1', 'brixSep7', 'brixSep14', 'brixSep21', 'brixSep29', 'brixOct12',
         key = "measDate", value = "brix") %>%
  mutate(date = case_when(measDate == 'brixAug28' ~ mdy("8/28/2020"),
                          measDate == 'brixSep1' ~ mdy("9/1/2020"),
                          measDate == 'brixSep7' ~ mdy("9/7/2020"),
                          measDate == 'brixSep14' ~ mdy("9/14/2020"),
                          measDate == 'brixSep21' ~ mdy("9/21/2020"),
                          measDate == 'brixSep29' ~ mdy("9/29/2020"),
                          measDate == 'brixOct12' ~ mdy("10/12/2020"))) %>%
  select(treatment, rep, date, brix)-> brixLong
## explore data----
treatmentCat <- c("ctrl", "avg", "naa", "ethephon")
brixLong %>%
  arrange(date, match(treatment, treatmentCat)) -> brixLong
ggplot(brixLong, aes(x = treatment, y = brix, fill = treatment)) +
  geom_boxplot() +
  facet_wrap(~date, scales = "free") +
  scale_fill_brewer(palette = "RdYlBu", labels = c("AVG", "Control", "Ethephon", "NAA")) +
  labs(y = "Soluble Sugar Content (%)",
       x = "Treatment",
       fill = "Treatment") +
  theme_jl +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
## ANOVA----
### Aug. 28----
#### ANOVA model
brixAug28.aov <- aov(brixAug28 ~ treatment, data = brix)
Aug28Model <- data.frame(fit = fitted(brixAug28.aov),
                         res = resid(brixAug28.aov))
#### normality check
Aug28QQ <- ggplot(brix, aes(sample = brixAug28)) + 
  stat_qq() +
  stat_qq_line() +
  labs(y = "Sample",
       x = "Theoretical Quantiles",
       subtitle = "QQ Plot-Aug. 28") +
  theme_jl
brixAug28.resid <- residuals(object = brixAug28.aov)
shapiro.test(x = brixAug28.resid)#p>0.05
#### homogeneity of variance check
Aug28Resid <- ggplot(Aug28Model, aes(x = fit, y = res))+
  geom_point()+
  geom_hline(yintercept = 0, colour = "pink")+
  scale_color_brewer(palette = "RdYlBu")+
  labs(y = "Residuals",
       x = "Fitted Values", 
      subtitle = "Residuals-Aug. 28")+
  theme_jl
leveneTest(brixAug28 ~ treatment, data = brix) #p>0.05
#### treatment separation
TukeyHSD(brixAug28.aov)
### Sep. 1----
#### ANOVA model
brixSep1.aov <- aov(brixSep1 ~ treatment, data = brix)
Sep1Model <- data.frame(fit = fitted(brixSep1.aov),
                         res = resid(brixSep1.aov))
#### normality check
Sep1QQ <- ggplot(brix, aes(sample = brixSep1)) + 
  stat_qq() +
  stat_qq_line() +
  labs(y = "Sample",
       x = "Theoretical Quantiles",
       subtitle = "QQ Plot-Sep. 1") +
  theme_jl
brixSep1.resid <- residuals(object = brixSep1.aov)
shapiro.test(x = brixSep1.resid) #p>0.05
#### homogeneity of variance check
Sep1Resid <- ggplot(Sep1Model, aes(x = fit, y = res))+
  geom_point()+
  geom_hline(yintercept = 0, colour = "pink")+
  scale_color_brewer(palette = "RdYlBu")+
  labs(y = "Residuals",
       x = "Fitted Values", 
       subtitle = "Residuals-Sep. 1")+
  theme_jl
leveneTest(brixSep1 ~ treatment, data = brix)#p>0.05
#### treatment separation
TukeyHSD(brixSep1.aov)
### Sep. 7----
#### ANOVA model
brixSep7.aov <- aov(brixSep7 ~ treatment, data = brix)
Sep7Model <- data.frame(fit = fitted(brixSep7.aov),
                        res = resid(brixSep7.aov))
#### normality check
Sep7QQ <- ggplot(brix, aes(sample = brixSep7)) + 
  stat_qq() +
  stat_qq_line() +
  labs(y = "Sample",
       x = "Theoretical Quantiles",
       subtitle = "QQ Plot-Sep. 7") +
  theme_jl
brixSep7.resid <- residuals(object = brixSep7.aov)
shapiro.test(x = brixSep7.resid)#p>0.05
#### homogeneity of variance check
Sep7Resid <- ggplot(Sep7Model, aes(x = fit, y = res))+
  geom_point()+
  geom_hline(yintercept = 0, colour = "pink")+
  scale_color_brewer(palette = "RdYlBu")+
  labs(y = "Residuals",
       x = "Fitted Values", 
       subtitle = "Residuals-Sep. 7")+
  theme_jl
leveneTest(brixSep7 ~ treatment, data = brix)#p>0.05
#### treatment separation
TukeyHSD(brixSep7.aov)
### Sep. 14----
#### ANOVA model
brixSep14.aov <- aov(brixSep14 ~ treatment, data = brix)
Sep14Model <- data.frame(fit = fitted(brixSep14.aov),
                        res = resid(brixSep14.aov))
#### normality check
Sep14QQ <- ggplot(brix, aes(sample = brixSep14)) + 
  stat_qq() +
  stat_qq_line() +
  labs(y = "Sample",
       x = "Theoretical Quantiles",
       subtitle = "QQ Plot-Sep. 14") +
  theme_jl
brixSep14.resid <- residuals(object = brixSep14.aov)
shapiro.test(x = brixSep14.resid)#p>0.05
#### homogeneity of variance check
Sep14Resid <- ggplot(Sep14Model, aes(x = fit, y = res))+
  geom_point()+
  geom_hline(yintercept = 0, colour = "pink")+
  scale_color_brewer(palette = "RdYlBu")+
  labs(y = "Residuals",
       x = "Fitted Values", 
       subtitle = "Residuals-Sep. 14")+
  theme_jl
leveneTest(brixSep14 ~ treatment, data = brix)#p>0.05
#### treatment separation
TukeyHSD(brixSep14.aov)
### Sep. 21----
#### ANOVA model
brixSep21.aov <- aov(brixSep21 ~ treatment, data = brix)
Sep21Model <- data.frame(fit = fitted(brixSep21.aov),
                        res = resid(brixSep21.aov))
#### normality check
Sep21QQ <- ggplot(brix, aes(sample = brixSep21)) + 
  stat_qq() +
  stat_qq_line() +
  labs(y = "Sample",
       x = "Theoretical Quantiles",
       subtitle = "QQ Plot-Sep. 21") +
  theme_jl
brixSep21.resid <- residuals(object = brixSep21.aov)
shapiro.test(x = brixSep21.resid)#p>0.05
#### homogeneity of variance check
Sep21Resid <- ggplot(Sep21Model, aes(x = fit, y = res))+
  geom_point()+
  geom_hline(yintercept = 0, colour = "pink")+
  scale_color_brewer(palette = "RdYlBu")+
  labs(y = "Residuals",
       x = "Fitted Values", 
       subtitle = "Residuals-Sep. 21")+
  theme_jl
leveneTest(brixSep21 ~ treatment, data = brix)
#### treatment separation
TukeyHSD(brixSep21.aov)
### Sep. 29----
#### ANOVA model
brixSep29.aov <- aov(brixSep29 ~ treatment, data = brix)
Sep29Model <- data.frame(fit = fitted(brixSep29.aov),
                        res = resid(brixSep29.aov))
#### normality check
Sep29QQ <- ggplot(brix, aes(sample = brixSep29)) + 
  stat_qq() +
  stat_qq_line() +
  labs(y = "Sample",
       x = "Theoretical Quantiles",
       subtitle = "QQ Plot-Sep. 29") +
  theme_jl
brixSep29.resid <- residuals(object = brixSep29.aov)
shapiro.test(x = brixSep29.resid) #p<0.05
shapiro.test(x = sqrt(brixSep29.resid)) #p>0.05
#### homogeneity of variance check
Sep29Resid <- ggplot(Sep29Model, aes(x = fit, y = res))+
  geom_point()+
  geom_hline(yintercept = 0, colour = "pink")+
  scale_color_brewer(palette = "RdYlBu")+
  labs(y = "Residuals",
       x = "Fitted Values", 
       subtitle = "Residuals-Sep. 29")+
  theme_jl
leveneTest(sqrt(brixSep29) ~ treatment, data = brix)#p>0.05
#### treatment separation
brixSep29.sqrt.aov <- aov(sqrt(brixSep29) ~ treatment, data = brix)
TukeyHSD(brixSep29.sqrt.aov)
### Oct. 12----
#### ANOVA model
brixOct12.aov <- aov(brixOct12 ~ treatment, data = brix)
Oct12Model <- data.frame(fit = fitted(brixOct12.aov),
                        res = resid(brixOct12.aov))
#### normality check
Oct12QQ <- ggplot(brix, aes(sample = brixOct12)) + 
  stat_qq() +
  stat_qq_line() +
  labs(y = "Sample",
       x = "Theoretical Quantiles",
       subtitle = "QQ Plot-Oct. 12") +
  theme_jl
brixOct12.resid <- residuals(object = brixOct12.aov)
shapiro.test(x = brixOct12.resid)#p>0.05
#### homogeneity of variance check
Oct12Resid <- ggplot(Oct12Model, aes(x = fit, y = res))+
  geom_point()+
  geom_hline(yintercept = 0, colour = "pink")+
  scale_color_brewer(palette = "RdYlBu")+
  labs(y = "Residuals",
       x = "Fitted Values", 
       subtitle = "Residuals-Oct. 12")+
  theme_jl
leveneTest(brixOct12 ~ treatment, data = brix)#p>0.05
#### treatment separation
TukeyHSD(brixOct12.aov)
## Fig----
brixLong %>%
  group_by(date, treatment) %>%
  summarise(avg.brix = mean(brix)) %>%
  arrange(date, match(treatment, treatmentCat)) %>%
  ggplot(aes(x = date, y = avg.brix, color = treatment))  +
  geom_point() +
  geom_line() +
  geom_text_repel(aes(x = date, y = avg.brix, label = c("a", "a", "a", "a", rep(c("b", "b", "b", "a"), times = 2), "a",
                                                        "a", "a", "a", "ab", "b", "ab", "a",
                                                        "a", "a", "a", "a", "a", "a", "a", "a")),
                  vjust=1, color="black", position = position_dodge(2), size= 3,parse = T,
                  min.segment.length = 1.9) +
  scale_color_brewer(palette = "Set2", labels = c("AVG", "Control", "Ethephon", "NAA")) +
  labs(y = "Soluble Solids Content (%)",
       x = "Date",
       color = "Treatment",
       title = "Soluble Solids Content")+
  theme_jl
ggsave("brix_plot.png")
