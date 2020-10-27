# Preharvest Drop: IEC Data Analysis
# author: Jimmy Larson
# created: 10/23/20
# last edited: 10/26/20

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
theme_jl45 <- theme_ipsum_ps(grid="XY", axis="xy", axis_text_size = 10, axis_title_size = 11, axis_col = "black",
                             axis.text.x = element_text(angle = 90))
## load data----
iecAug28 <- read_csv("data/2020/IEC/pfd_ethylene_aug_28.csv")
iecSep1 <- read_csv("data/2020/IEC/pfd_ethylene_sep_1.csv")
iecSep7 <- read_csv("data/2020/IEC/pfd_ethylene_sep_7.csv")
iecSep14 <- read_csv("data/2020/IEC/pfd_ethylene_sep_14.csv")
iecSep21 <- read_csv("data/2020/IEC/pfd_ethylene_sep_21.csv")
iecSep29 <- read_csv("data/2020/IEC/pfd_ethylene_sep_29.csv")
iecOct12 <- read_csv("data/2020/IEC/pfd_ethylene_oct_12.csv")
## join data ----
iecAug28 %>%
  rename(iec.Aug.28 = IEC) %>%
  left_join(iecSep1) %>%
  rename(iec.Sep.1 = IEC) %>%
  left_join(iecSep7) %>%
  rename(iec.Sep.7 = IEC) %>%
  left_join(iecSep14) %>%
  rename(iec.Sep.14 = IEC) %>%
  left_join(iecSep21) %>%
  rename(iec.Sep.21 = IEC) %>%
  left_join(iecSep29) %>%
  rename(iec.Sep.29 = IEC) %>%
  left_join(iecOct12) %>%
  rename(iec.Oct.12 = IEC) %>%
  select(row, treatment, rep, fruit, iec.Aug.28, iec.Sep.1, iec.Sep.7, iec.Sep.14,
         iec.Sep.21, iec.Sep.29, iec.Oct.12) -> iec
## long form data----
iec %>%
  gather('iec.Aug.28', 'iec.Sep.1', 'iec.Sep.7', 'iec.Sep.14', 'iec.Sep.21', 'iec.Sep.29', 'iec.Oct.12',
         key = "measDate", value = "iec") %>%
  mutate(date = case_when(measDate == 'iec.Aug.28' ~ mdy("8/28/2020"),
                          measDate == 'iec.Sep.1' ~ mdy("9/1/2020"),
                          measDate == 'iec.Sep.7' ~ mdy("9/7/2020"),
                          measDate == 'iec.Sep.14' ~ mdy("9/14/2020"),
                          measDate == 'iec.Sep.21' ~ mdy("9/21/2020"),
                          measDate == 'iec.Sep.29' ~ mdy("9/29/2020"),
                          measDate == 'iec.Oct.12' ~ mdy("10/12/2020"))) -> iecLong
## explore data----
ggplot(iecLong, aes(x = treatment, y = iec, fill = treatment)) +
  geom_boxplot() +
  facet_wrap(~date, scales = "free") +
  scale_fill_brewer(palette = "RdYlBu") +
  labs(y = "Internal Ethylene Conc. (ppm)",
       x = "Treatment",
       subtitle = "Internal Ethylene Concentrations") +
  theme_jl +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
### log scale
ggplot(iecLong, aes(x = treatment, y = log(iec), fill = treatment)) +
  geom_boxplot() +
  facet_wrap(~date, scales = "free") +
  scale_fill_brewer(palette = "RdYlBu") +
  labs(y = "Internal Ethylene Conc. (ppm)",
       x = "Treatment",
       subtitle = "Natural Log of Internal Ethylene Concentrations") +
  theme_jl +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
## density plots
iecLong %>%
  drop_na(iec) %>%
  droplevels() %>%
  ggplot(aes(x = iec, group = treatment, fill = treatment))+
  geom_density(adjust=1.5, alpha=.4) +
  facet_wrap(~date, scales = "free") +
  scale_fill_brewer(palette = "RdYlBu") +
  theme_jl

iecLong %>%
  drop_na(iec) %>%
  droplevels() %>%
  ggplot(aes(x = log(iec), group = treatment, fill = treatment))+
  geom_density(adjust=1.5, alpha=.4) +
  facet_wrap(~date, scales = "free") +
  scale_fill_brewer(palette = "RdYlBu") +
  theme_jl
## ANOVA assumptions untransformed----
### Aug. 28----
#### ANOVA model
iecAug28.aov <- aov(iec.Aug.28 ~ treatment, data = iec)
summary(iecAug28.aov)
Aug28Model <- data.frame(fit = fitted(iecAug28.aov),
                         res = resid(iecAug28.aov))
#### Normality
Aug28QQ <- ggplot(iec, aes(sample = iec.Aug.28)) + 
  stat_qq() +
  stat_qq_line() +
  labs(y = "Sample",
       x = "Theoretical Quantiles",
       title = "QQ Plot",
       subtitle = "Aug. 28") +
  theme_jl
iecAug28.resid <- residuals(object = iecAug28.aov)
shapiro.test(x = iecAug28.resid) #<0.05
#### Homogeneity of Variance
Aug28Resid <- ggplot(Aug28Model, aes(x = fit, y = res))+
  geom_point()+
  geom_hline(yintercept = 0, colour = "pink")+
  scale_color_brewer(palette = "RdYlBu")+
  labs(y = "Residuals",
       x = "Fitted Values", 
       title = "Residuals vs. Fitted",
       subtitle = "Aug. 28")+
  theme_jl
leveneTest(iec.Aug.28 ~ treatment, data = iec) #p<0.05
### Sep. 1----
#### ANOVA model
iecSep1.aov <- aov(iec.Sep.1 ~ treatment, data = iec)
summary(iecSep1.aov)
Sep1Model <- data.frame(fit = fitted(iecSep1.aov),
                         res = resid(iecSep1.aov))
#### Normality
Sep1QQ <- ggplot(iec, aes(sample = iec.Sep.1)) + 
  stat_qq() +
  stat_qq_line() +
  labs(y = "Sample",
       x = "Theoretical Quantiles",
       title = "QQ Plot",
       subtitle = "Sep. 1") +
  theme_jl
iecSep1.resid <- residuals(object = iecSep1.aov)
shapiro.test(x = iecSep1.resid) #p<0.05
#### Homogeneity of Variance
Sep1Resid <- ggplot(Sep1Model, aes(x = fit, y = res))+
  geom_point()+
  geom_hline(yintercept = 0, colour = "pink")+
  scale_color_brewer(palette = "RdYlBu")+
  labs(y = "Residuals",
       x = "Fitted Values", 
       title = "Residuals vs. Fitted",
       subtitle = "Sep. 1")+
  theme_jl
leveneTest(iec.Sep.1 ~ treatment, data = iec) #p<0.05
### Sep. 7----
#### ANOVA model
iecSep7.aov <- aov(iec.Sep.7 ~ treatment, data = iec)
summary(iecSep7.aov)
Sep7Model <- data.frame(fit = fitted(iecSep7.aov),
                        res = resid(iecSep7.aov))
#### Normality
Sep7QQ <- ggplot(iec, aes(sample = iec.Sep.7)) + 
  stat_qq() +
  stat_qq_line() +
  labs(y = "Sample",
       x = "Theoretical Quantiles",
       title = "QQ Plot",
       subtitle = "Sep. 7") +
  theme_jl
iecSep7.resid <- residuals(object = iecSep7.aov)
shapiro.test(x = iecSep7.resid) #p<0.05
#### Homogeneity of Variance
Sep7Resid <- ggplot(Sep7Model, aes(x = fit, y = res))+
  geom_point()+
  geom_hline(yintercept = 0, colour = "pink")+
  scale_color_brewer(palette = "RdYlBu")+
  labs(y = "Residuals",
       x = "Fitted Values", 
       title = "Residuals vs. Fitted",
       subtitle = "Sep. 7")+
  theme_jl
leveneTest(iec.Sep.7 ~ treatment, data = iec) #p>0.05
### Sep. 14----
#### ANOVA model
iecSep14.aov <- aov(iec.Sep.14 ~ treatment, data = iec)
summary(iecSep14.aov)
Sep14Model <- data.frame(fit = fitted(iecSep14.aov),
                        res = resid(iecSep14.aov))
#### Normality
Sep14QQ <- ggplot(iec, aes(sample = iec.Sep.14)) + 
  stat_qq() +
  stat_qq_line() +
  labs(y = "Sample",
       x = "Theoretical Quantiles",
       title = "QQ Plot",
       subtitle = "Sep. 14") +
  theme_jl
iecSep14.resid <- residuals(object = iecSep14.aov)
shapiro.test(x = iecSep14.resid) #p<0.05
#### Homogeneity of Variance
Sep14Resid <- ggplot(Sep14Model, aes(x = fit, y = res))+
  geom_point()+
  geom_hline(yintercept = 0, colour = "pink")+
  scale_color_brewer(palette = "RdYlBu")+
  labs(y = "Residuals",
       x = "Fitted Values", 
       title = "Residuals vs. Fitted",
       subtitle = "Sep. 14")+
  theme_jl
leveneTest(iec.Sep.14 ~ treatment, data = iec) #p>0.05
### Sep. 21----
#### ANOVA model
iecSep21.aov <- aov(iec.Sep.21 ~ treatment, data = iec)
summary(iecSep21.aov)
Sep21Model <- data.frame(fit = fitted(iecSep21.aov),
                        res = resid(iecSep21.aov))
#### Normality
Sep21QQ <- ggplot(iec, aes(sample = iec.Sep.21)) + 
  stat_qq() +
  stat_qq_line() +
  labs(y = "Sample",
       x = "Theoretical Quantiles",
       title = "QQ Plot",
       subtitle = "Sep. 21") +
  theme_jl
iecSep21.resid <- residuals(object = iecSep21.aov)
shapiro.test(x = iecSep21.resid) #p<0.05
#### Homogeneity of Variance
Sep21Resid <- ggplot(Sep21Model, aes(x = fit, y = res))+
  geom_point()+
  geom_hline(yintercept = 0, colour = "pink")+
  scale_color_brewer(palette = "RdYlBu")+
  labs(y = "Residuals",
       x = "Fitted Values", 
       title = "Residuals vs. Fitted",
       subtitle = "Sep. 21")+
  theme_jl
leveneTest(iec.Sep.21 ~ treatment, data = iec) #p>0.05
### Sep. 29----
#### ANOVA model
iecSep29.aov <- aov(iec.Sep.29 ~ treatment, data = iec)
summary(iecSep29.aov)
Sep29Model <- data.frame(fit = fitted(iecSep29.aov),
                        res = resid(iecSep29.aov))
#### Normality
Sep29QQ <- ggplot(iec, aes(sample = iec.Sep.29)) + 
  stat_qq() +
  stat_qq_line() +
  labs(y = "Sample",
       x = "Theoretical Quantiles",
       title = "QQ Plot",
       subtitle = "Sep. 29") +
  theme_jl
iecSep29.resid <- residuals(object = iecSep29.aov)
shapiro.test(x = iecSep29.resid) #p<0.05
#### Homogeneity of Variance
Sep29Resid <- ggplot(Sep29Model, aes(x = fit, y = res))+
  geom_point()+
  geom_hline(yintercept = 0, colour = "pink")+
  scale_color_brewer(palette = "RdYlBu")+
  labs(y = "Residuals",
       x = "Fitted Values", 
       title = "Residuals vs. Fitted",
       subtitle = "Sep. 29")+
  theme_jl
leveneTest(iec.Sep.29 ~ treatment, data = iec) #p<0.05
### Oct. 12----
#### ANOVA model
iecOct12.aov <- aov(iec.Oct.12 ~ treatment, data = iec)
summary(iecOct12.aov)
Oct12Model <- data.frame(fit = fitted(iecOct12.aov),
                        res = resid(iecOct12.aov))
#### Normality
Oct12QQ <- ggplot(iec, aes(sample = iec.Oct.12)) + 
  stat_qq() +
  stat_qq_line() +
  labs(y = "Sample",
       x = "Theoretical Quantiles",
       title = "QQ Plot",
       subtitle = "Oct. 12") +
  theme_jl
iecOct12.resid <- residuals(object = iecOct12.aov)
shapiro.test(x = iecOct12.resid) #p<0.05
#### Homogeneity of Variance
Oct12Resid <- ggplot(Oct12Model, aes(x = fit, y = res))+
  geom_point()+
  geom_hline(yintercept = 0, colour = "pink")+
  scale_color_brewer(palette = "RdYlBu")+
  labs(y = "Residuals",
       x = "Fitted Values", 
       title = "Residuals vs. Fitted",
       subtitle = "Oct. 12")+
  theme_jl
leveneTest(iec.Oct.12 ~ treatment, data = iec)#p<0.05
  
## Log transform data----
iec %>%
  mutate(iec.Aug.28.log = log(iec.Aug.28),
         iec.Sep.1.log = log(iec.Sep.1),
         iec.Sep.7.log = log(iec.Sep.7),
         iec.Sep.14.log = log(iec.Sep.14),
         iec.Sep.21.log = log(iec.Sep.21),
         iec.Sep.29.log = log(iec.Sep.29),
         iec.Oct.12.log = log(iec.Oct.12)) %>%
  select(row, treatment, rep, fruit, iec.Aug.28.log, iec.Sep.1.log , iec.Sep.7.log, iec.Sep.14.log,
         iec.Sep.21.log, iec.Sep.29.log, iec.Oct.12.log) %>%
  rename(iec.Aug.28 = iec.Aug.28.log,
         iec.Sep.1 = iec.Sep.1.log,
         iec.Sep.7 = iec.Sep.7.log,
         iec.Sep.14 = iec.Sep.14.log,
         iec.Sep.21 = iec.Sep.21.log,
         iec.Sep.29 = iec.Sep.29.log,
         iec.Oct.12 = iec.Oct.12.log) ->iec.log
iec.log <- do.call(data.frame, lapply(iec.log,
                                      function(x) replace(x, is.infinite(x), NA)))
## ANOVA assumptions natural log transformed----
### Aug. 28----
#### ANOVA model
iecAug28.aov <- aov(iec.Aug.28 ~ treatment, data = iec.log)
summary(iecAug28.aov)
Aug28Model <- data.frame(fit = fitted(iecAug28.aov),
                         res = resid(iecAug28.aov))
#### Normality
Aug28QQ <- ggplot(iec.log, aes(sample = iec.Aug.28)) + 
  stat_qq() +
  stat_qq_line() +
  labs(y = "Sample",
       x = "Theoretical Quantiles",
       subtitle = "QQ Plot - Aug. 28") +
  theme_jl
iecAug28.resid <- residuals(object = iecAug28.aov)
shapiro.test(x = iecAug28.resid) #p>0.05
#### Homogeneity of Variance
Aug28Resid <- ggplot(Aug28Model, aes(x = fit, y = res))+
  geom_point()+
  geom_hline(yintercept = 0, colour = "pink")+
  scale_color_brewer(palette = "RdYlBu")+
  labs(y = "Residuals",
       x = "Fitted Values", 
       title = "Residuals vs. Fitted",
       subtitle = "Aug. 28")+
  theme_jl
leveneTest(iec.Aug.28 ~ treatment, data = iec.log) #p>0.05
#### mean separation
TukeyHSD(iecAug28.aov )
### Sep. 1----
#### ANOVA model
iecSep1.aov <- aov(iec.Sep.1 ~ treatment, data = iec.log)
summary(iecSep1.aov)
Sep1Model <- data.frame(fit = fitted(iecSep1.aov),
                        res = resid(iecSep1.aov))
#### Normality
Sep1QQ <- ggplot(iec.log, aes(sample = iec.Sep.1)) + 
  stat_qq() +
  stat_qq_line() +
  labs(y = "Sample",
       x = "Theoretical Quantiles",
       title = "QQ Plot",
       subtitle = "Sep. 1") +
  theme_jl
iecSep1.resid <- residuals(object = iecSep1.aov)
shapiro.test(x = iecSep1.resid) # p<0.05
#### Homogeneity of Variance
Sep1Resid <- ggplot(Sep1Model, aes(x = fit, y = res))+
  geom_point()+
  geom_hline(yintercept = 0, colour = "pink")+
  scale_color_brewer(palette = "RdYlBu")+
  labs(y = "Residuals",
       x = "Fitted Values", 
       title = "Residuals vs. Fitted",
       subtitle = "Sep. 1")+
  theme_jl
leveneTest(iec.Sep.1 ~ treatment, data = iec.log) #p>0.05
#### mean separation
kruskal.test(iec.Sep.1 ~ treatment, data = iec.log) #p<0.05
pairwise.wilcox.test(iec.log$iec.Sep.1, iec.log$treatment, p.adjust.method = "BH")
### Sep. 7----
#### ANOVA model
iecSep7.aov <- aov(iec.Sep.7 ~ treatment, data = iec.log)
summary(iecSep7.aov)
Sep7Model <- data.frame(fit = fitted(iecSep7.aov),
                        res = resid(iecSep7.aov))
#### Normality
Sep7QQ <- ggplot(iec.log, aes(sample = iec.Sep.7)) + 
  stat_qq() +
  stat_qq_line() +
  labs(y = "Sample",
       x = "Theoretical Quantiles",
       title = "QQ Plot",
       subtitle = "Sep. 7") +
  theme_jl
iecSep7.resid <- residuals(object = iecSep7.aov)
shapiro.test(x = iecSep7.resid) #p<0.05
#### Homogeneity of Variance
Sep7Resid <- ggplot(Sep7Model, aes(x = fit, y = res))+
  geom_point()+
  geom_hline(yintercept = 0, colour = "pink")+
  scale_color_brewer(palette = "RdYlBu")+
  labs(y = "Residuals",
       x = "Fitted Values", 
       title = "Residuals vs. Fitted",
       subtitle = "Sep. 7")+
  theme_jl
leveneTest(iec.Sep.7 ~ treatment, data = iec.log) #p>0.05
#### mean separation
kruskal.test(iec.Sep.7~ treatment, data = iec.log) #p<0.05
pairwise.wilcox.test(iec.log$iec.Sep.7, iec.log$treatment, p.adjust.method = "BH")
### Sep. 14----
#### ANOVA model
iecSep14.aov <- aov(iec.Sep.14 ~ treatment, data = iec.log)
summary(iecSep14.aov)
Sep14Model <- data.frame(fit = fitted(iecSep14.aov),
                         res = resid(iecSep14.aov))
#### Normality
Sep14QQ <- ggplot(iec.log, aes(sample = iec.Sep.14)) + 
  stat_qq() +
  stat_qq_line() +
  labs(y = "Sample",
       x = "Theoretical Quantiles",
       title = "QQ Plot",
       subtitle = "Sep. 14") +
  theme_jl
iecSep14.resid <- residuals(object = iecSep14.aov)
shapiro.test(x = iecSep14.resid) #p<0.05
#### Homogeneity of Variance
Sep14Resid <- ggplot(Sep14Model, aes(x = fit, y = res))+
  geom_point()+
  geom_hline(yintercept = 0, colour = "pink")+
  scale_color_brewer(palette = "RdYlBu")+
  labs(y = "Residuals",
       x = "Fitted Values", 
       title = "Residuals vs. Fitted",
       subtitle = "Sep. 14")+
  theme_jl
leveneTest(iec.Sep.14 ~ treatment, data = iec.log) #p>0.05
#### mean separation
kruskal.test(iec.Sep.14~ treatment, data = iec.log) #p<0.05
pairwise.wilcox.test(iec.log$iec.Sep.14, iec.log$treatment, p.adjust.method = "BH")
### Sep. 21----
#### ANOVA model
iecSep21.aov <- aov(iec.Sep.21 ~ treatment, data = iec.log)
summary(iecSep21.aov)
Sep21Model <- data.frame(fit = fitted(iecSep21.aov),
                         res = resid(iecSep21.aov))
#### Normality
Sep21QQ <- ggplot(iec.log, aes(sample = iec.Sep.21)) + 
  stat_qq() +
  stat_qq_line() +
  labs(y = "Sample",
       x = "Theoretical Quantiles",
       title = "QQ Plot",
       subtitle = "Sep. 21") +
  theme_jl
iecSep21.resid <- residuals(object = iecSep21.aov)
shapiro.test(x = iecSep21.resid) #p>0.05
#### Homogeneity of Variance
Sep21Resid <- ggplot(Sep21Model, aes(x = fit, y = res))+
  geom_point()+
  geom_hline(yintercept = 0, colour = "pink")+
  scale_color_brewer(palette = "RdYlBu")+
  labs(y = "Residuals",
       x = "Fitted Values", 
       title = "Residuals vs. Fitted",
       subtitle = "Sep. 21")+
  theme_jl
leveneTest(iec.Sep.21 ~ treatment, data = iec.log) #p<0.05
#### mean separation
kruskal.test(iec.Sep.21~ treatment, data = iec.log) #p<0.05
pairwise.wilcox.test(iec.log$iec.Sep.21, iec.log$treatment, p.adjust.method = "BH")
### Sep. 29----
#### ANOVA model
iecSep29.aov <- aov(iec.Sep.29 ~ treatment, data = iec.log)
summary(iecSep29.aov)
Sep29Model <- data.frame(fit = fitted(iecSep29.aov),
                         res = resid(iecSep29.aov))
#### Normality
Sep29QQ <- ggplot(iec.log, aes(sample = iec.Sep.29)) + 
  stat_qq() +
  stat_qq_line() +
  labs(y = "Sample",
       x = "Theoretical Quantiles",
       title = "QQ Plot",
       subtitle = "Sep. 29") +
  theme_jl
iecSep29.resid <- residuals(object = iecSep29.aov)
shapiro.test(x = iecSep29.resid) #p<0.05
#### Homogeneity of Variance
Sep29Resid <- ggplot(Sep29Model, aes(x = fit, y = res))+
  geom_point()+
  geom_hline(yintercept = 0, colour = "pink")+
  scale_color_brewer(palette = "RdYlBu")+
  labs(y = "Residuals",
       x = "Fitted Values", 
       title = "Residuals vs. Fitted",
       subtitle = "Sep. 29")+
  theme_jl
leveneTest(iec.Sep.29 ~ treatment, data = iec.log) #p<0.05
#### mean separation
kruskal.test(iec.Sep.29~ treatment, data = iec.log) #p<0.05
pairwise.wilcox.test(iec.log$iec.Sep.29, iec.log$treatment, p.adjust.method = "BH")
### Oct. 12----
#### ANOVA model
iecOct12.aov <- aov(iec.Oct.12 ~ treatment, data = iec.log)
summary(iecOct12.aov)
Oct12Model <- data.frame(fit = fitted(iecOct12.aov),
                         res = resid(iecOct12.aov))
#### Normality
Oct12QQ <- ggplot(iec.log, aes(sample = iec.Oct.12)) + 
  stat_qq() +
  stat_qq_line() +
  labs(y = "Sample",
       x = "Theoretical Quantiles",
       title = "QQ Plot",
       subtitle = "Oct. 12") +
  theme_jl
iecOct12.resid <- residuals(object = iecOct12.aov)
shapiro.test(x = iecOct12.resid) #p<0.05
#### Homogeneity of Variance
Oct12Resid <- ggplot(Oct12Model, aes(x = fit, y = res))+
  geom_point()+
  geom_hline(yintercept = 0, colour = "pink")+
  scale_color_brewer(palette = "RdYlBu")+
  labs(y = "Residuals",
       x = "Fitted Values", 
       title = "Residuals vs. Fitted",
       subtitle = "Oct. 12")+
  theme_jl
leveneTest(iec.Oct.12 ~ treatment, data = iec.log)#p>0.05
#### mean separation
kruskal.test(iec.Oct.12~ treatment, data = iec.log) #p<0.05
pairwise.wilcox.test(iec.log$iec.Oct.12, iec.log$treatment, p.adjust.method = "BH")
### fig----
treatmentCat <- c("ctrl", "avg", "naa", "ethephon")
iecLong %>%
  mutate(log.iec = log(iec)) %>%
  drop_na(log.iec) %>%
  group_by(date, treatment) %>%
  summarise(med.iec = median(log.iec)) %>%
  arrange(date, match(treatment, treatmentCat)) -> log.iec.med
ggplot(log.iec.med, aes(x = date, y = med.iec, color = treatment)) +
  geom_point() +
  geom_line() +
  geom_text_repel(aes(x = date, y= med.iec, label = TeX(c("b", "b", "b", "a", "b*", "b*", "b*", "a*",
                      rep(c("a*", "b*", "a*", "a*"), times = 5)), output = "character")), vjust=1, color="black",
                  position = position_dodge(2), size= 3,parse = T, min.segment.length = 1.9) +
  scale_color_brewer(palette = "RdBu", labels = c("AVG", "Control", "Ethephon", "NAA")) +
  labs(y = "LN of IEC (ppm)",
       x = "Date",
       color = "Treatment",
       title = "Internal Ethylene Concentration",
       subtitle = "Median Values of Each Treatment by Date",
       caption = "*Kruskal-Wallis test")+
  theme_jl
ggsave("figs/iec_plot.png")  
