# Preharvest Drop: Firmness Data Analysis
# author: Jimmy Larson
# created: 10/28/20
# last edited: 10/29/20

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
firmAug28 <- read_csv("data/2020/firmness/pfd_firmness_aug_28.csv")
firmSep1 <- read_csv("data/2020/firmness/pfd_firmness_sep_1.csv")
firmSep8 <- read_csv("data/2020/firmness/pfd_firmness_sep_8.csv")
firmSep14 <- read_csv("data/2020/firmness/pfd_firmness_sep_14.csv")
firmSep21 <- read_csv("data/2020/firmness/pfd_firmness_sep_21.csv")
firmSep29 <- read_csv("data/2020/firmness/pfd_firmness_sep_29.csv")
firmOct12 <- read_csv("data/2020/firmness/pfd_firmness_oct_12.csv")
## average firmness measurements ----
firmAug28 %>%
  mutate(firm = ((firm.1 + firm.2)/2)) %>%
  select(row, treatment, rep, fruit, firm)-> firmAug28
firmSep1 %>%
  mutate(firm = ((firm.1 + firm.2)/2)) %>%
  select(row, treatment, rep, fruit, firm) -> firmSep1
firmSep8 %>%
  mutate(firm = ((firm.1 + firm.2)/2)) %>%
  select(row, treatment, rep, fruit, firm) -> firmSep8
firmSep14 %>%
  mutate(firm = ((firm.1 + firm.2)/2)) %>%
  select(row, treatment, rep, fruit, firm) -> firmSep14
firmSep21 %>%
  mutate(firm = ((firm.1 + firm.2)/2)) %>%
  select(row, treatment, rep, fruit, firm)-> firmSep21
firmSep29 %>%
  mutate(firm = ((firm.1 + firm.2)/2)) %>%
  select(row, treatment, rep, fruit, firm)-> firmSep29
firmOct12 %>%
  mutate(firm = ((firm.1 + firm.2)/2)) %>%
  select(row, treatment, rep, fruit, firm)-> firmOct12
## join data----
firmAug28 %>%
  rename(firmAug28 = firm) %>%
  left_join(firmSep1) %>%
  rename(firmSep1 = firm) %>%
  left_join(firmSep8) %>%
  rename(firmSep8 = firm) %>%
  left_join(firmSep14) %>%
  rename(firmSep14 = firm) %>%
  left_join(firmSep21) %>%
  rename(firmSep21 = firm) %>%
  left_join(firmSep29) %>%
  rename(firmSep29 = firm) %>%
  left_join(firmOct12) %>%
  rename(firmOct12 = firm) -> firmness
## long form data----
firmness %>%
  gather('firmAug28', 'firmSep1', 'firmSep8', 'firmSep14', 'firmSep21', 'firmSep29',
         'firmOct12', key = "measDate", value = "firmness") %>%
  mutate(date = case_when(measDate == 'firmAug28' ~ mdy("8/28/2020"),
                          measDate == 'firmSep1' ~ mdy("9/1/2020"),
                          measDate == 'firmSep8' ~ mdy("9/8/2020"),
                          measDate == 'firmSep14' ~ mdy("9/14/2020"),
                          measDate == 'firmSep21' ~ mdy("9/21/2020"),
                          measDate == 'firmSep29' ~ mdy("9/29/2020"),
                          measDate == 'firmOct12' ~ mdy("10/12/2020"))) %>%
  select(treatment, rep, date, firmness)-> firmnessLong
## explore data----
ggplot(firmnessLong, aes(x = treatment, y = firmness, fill = treatment)) +
  geom_boxplot() +
  facet_wrap(~date, scales = "free") +
  scale_fill_brewer(palette = "RdYlBu", labels = c("AVG", "Control", "Ethephon", "NAA")) +
  labs(y = "Firmness",
       x = "Treatment",
       fill = "Treatment") +
  theme_jl
## ANOVA----
### Aug. 28----
#### ANOVA model
firmAug28.aov <- aov(firmAug28 ~ treatment, data = firmness)
Aug28Model <- data.frame(fit = fitted(firmAug28.aov),
                         res = resid(firmAug28.aov))
#### normality check
Aug28QQ <- ggplot(firmness, aes(sample = firmAug28)) + 
  stat_qq() +
  stat_qq_line() +
  labs(y = "Sample",
       x = "Theoretical Quantiles",
       subtitle = "QQ Plot-Aug. 28") +
  theme_jl
firmAug28.resid <- residuals(object = firmAug28.aov)
shapiro.test(x = firmAug28.resid) #p<0.05
#### homogeneity of variance check
Aug28Resid <- ggplot(Aug28Model, aes(x = fit, y = res))+
  geom_point()+
  geom_hline(yintercept = 0, colour = "pink")+
  scale_color_brewer(palette = "RdYlBu")+
  labs(y = "Residuals",
       x = "Fitted Values", 
       subtitle = "Residuals-Aug. 28")+
  theme_jl
leveneTest(firmAug28 ~ treatment, data = firmness)
#### treatment separation
kruskal.test(firmAug28~ treatment, data = firmness) #p><0.05
pairwise.wilcox.test(firmness$firmAug28, firmness$treatment, p.adjust.method = "BH")
### Sep. 1----
#### ANOVA model
firmSep1.aov <- aov(firmSep1 ~ treatment, data = firmness)
Sep1Model <- data.frame(fit = fitted(firmSep1.aov),
                         res = resid(firmSep1.aov))
#### normality check
Sep1QQ <- ggplot(firmness, aes(sample = firmSep1)) + 
  stat_qq() +
  stat_qq_line() +
  labs(y = "Sample",
       x = "Theoretical Quantiles",
       subtitle = "QQ Plot-Sep. 1") +
  theme_jl
firmSep1.resid <- residuals(object = firmSep1.aov)
shapiro.test(x = firmSep1.resid)
#### homogeneity of variance check
Sep1Resid <- ggplot(Sep1Model, aes(x = fit, y = res))+
  geom_point()+
  geom_hline(yintercept = 0, colour = "pink")+
  scale_color_brewer(palette = "RdYlBu")+
  labs(y = "Residuals",
       x = "Fitted Values", 
       subtitle = "Residuals-Sep. 1")+
  theme_jl
leveneTest(firmSep1 ~ treatment, data = firmness)
#### treatment separation
kruskal.test(firmSep1~ treatment, data = firmness) #p<0.05
pairwise.wilcox.test(firmness$firmSep1, firmness$treatment, p.adjust.method = "BH")
### Sep. 8----
#### ANOVA model
firmSep8.aov <- aov(firmSep8 ~ treatment, data = firmness)
Sep8Model <- data.frame(fit = fitted(firmSep8.aov),
                         res = resid(firmSep8.aov))
#### normality check
Sep8QQ <- ggplot(firmness, aes(sample = firmSep8)) + 
  stat_qq() +
  stat_qq_line() +
  labs(y = "Sample",
       x = "Theoretical Quantiles",
       subtitle = "QQ Plot-Sep. 8") +
  theme_jl
firmSep8.resid <- residuals(object = firmSep8.aov)
shapiro.test(x = firmSep8.resid)
#### homogeneity of variance check
Sep8Resid <- ggplot(Sep8Model, aes(x = fit, y = res))+
  geom_point()+
  geom_hline(yintercept = 0, colour = "pink")+
  scale_color_brewer(palette = "RdYlBu")+
  labs(y = "Residuals",
       x = "Fitted Values", 
       subtitle = "Residuals-Sep. 8")+
  theme_jl
leveneTest(firmSep8 ~ treatment, data = firmness)
#### treatment separation
kruskal.test(firmSep8~ treatment, data = firmness) #p<0.05
pairwise.wilcox.test(firmness$firmSep8, firmness$treatment, p.adjust.method = "BH")
### Sep. 14----
#### ANOVA model
firmSep14.aov <- aov(firmSep14 ~ treatment, data = firmness)
Sep14Model <- data.frame(fit = fitted(firmSep14.aov),
                         res = resid(firmSep14.aov))
#### normality check
Sep14QQ <- ggplot(firmness, aes(sample = firmSep14)) + 
  stat_qq() +
  stat_qq_line() +
  labs(y = "Sample",
       x = "Theoretical Quantiles",
       subtitle = "QQ Plot-Sep. 14") +
  theme_jl
firmSep14.resid <- residuals(object = firmSep14.aov)
shapiro.test(x = firmSep14.resid)
#### homogeneity of variance check
Sep14Resid <- ggplot(Sep14Model, aes(x = fit, y = res))+
  geom_point()+
  geom_hline(yintercept = 0, colour = "pink")+
  scale_color_brewer(palette = "RdYlBu")+
  labs(y = "Residuals",
       x = "Fitted Values", 
       subtitle = "Residuals-Sep. 14")+
  theme_jl
leveneTest(firmSep14 ~ treatment, data = firmness)
#### treatment separation
kruskal.test(firmSep14~ treatment, data = firmness) #p<0.05
pairwise.wilcox.test(firmness$firmSep14, firmness$treatment, p.adjust.method = "BH")
### Sep. 21----
#### ANOVA model
firmSep21.aov <- aov(firmSep21 ~ treatment, data = firmness)
Sep21Model <- data.frame(fit = fitted(firmSep21.aov),
                         res = resid(firmSep21.aov))
#### normality check
Sep21QQ <- ggplot(firmness, aes(sample = firmSep21)) + 
  stat_qq() +
  stat_qq_line() +
  labs(y = "Sample",
       x = "Theoretical Quantiles",
       subtitle = "QQ Plot-Sep. 21") +
  theme_jl
firmSep21.resid <- residuals(object = firmSep21.aov)
shapiro.test(x = firmSep21.resid)
#### homogeneity of variance check
Sep21Resid <- ggplot(Sep21Model, aes(x = fit, y = res))+
  geom_point()+
  geom_hline(yintercept = 0, colour = "pink")+
  scale_color_brewer(palette = "RdYlBu")+
  labs(y = "Residuals",
       x = "Fitted Values", 
       subtitle = "Residuals-Sep. 21")+
  theme_jl
leveneTest(firmSep21 ~ treatment, data = firmness)
#### treatment separation
kruskal.test(firmSep21~ treatment, data = firmness) #p<0.05
pairwise.wilcox.test(firmness$firmSep21, firmness$treatment, p.adjust.method = "BH")
### Sep. 29----
#### ANOVA model
firmSep29.aov <- aov(firmSep29 ~ treatment, data = firmness)
Sep29Model <- data.frame(fit = fitted(firmSep29.aov),
                         res = resid(firmSep29.aov))
#### normality check
Sep29QQ <- ggplot(firmness, aes(sample = firmSep29)) + 
  stat_qq() +
  stat_qq_line() +
  labs(y = "Sample",
       x = "Theoretical Quantiles",
       subtitle = "QQ Plot-Sep. 29") +
  theme_jl
firmSep29.resid <- residuals(object = firmSep29.aov)
shapiro.test(x = firmSep29.resid)
#### homogeneity of variance check
Sep29Resid <- ggplot(Sep29Model, aes(x = fit, y = res))+
  geom_point()+
  geom_hline(yintercept = 0, colour = "pink")+
  scale_color_brewer(palette = "RdYlBu")+
  labs(y = "Residuals",
       x = "Fitted Values", 
       subtitle = "Residuals-Sep. 29")+
  theme_jl
leveneTest(firmSep29 ~ treatment, data = firmness)
#### treatment separation
kruskal.test(firmSep29~ treatment, data = firmness) #p<0.05
pairwise.wilcox.test(firmness$firmSep29, firmness$treatment, p.adjust.method = "BH")
### Oct. 12----
#### ANOVA model
firmOct12.aov <- aov(firmOct12 ~ treatment, data = firmness)
Oct12Model <- data.frame(fit = fitted(firmOct12.aov),
                         res = resid(firmOct12.aov))
#### normality check
Oct12QQ <- ggplot(firmness, aes(sample = firmOct12)) + 
  stat_qq() +
  stat_qq_line() +
  labs(y = "Sample",
       x = "Theoretical Quantiles",
       subtitle = "QQ Plot-Oct. 12") +
  theme_jl
firmOct12.resid <- residuals(object = firmOct12.aov)
shapiro.test(x = firmOct12.resid)
#### homogeneity of variance check
Oct12Resid <- ggplot(Oct12Model, aes(x = fit, y = res))+
  geom_point()+
  geom_hline(yintercept = 0, colour = "pink")+
  scale_color_brewer(palette = "RdYlBu")+
  labs(y = "Residuals",
       x = "Fitted Values", 
       subtitle = "Residuals-Oct. 12")+
  theme_jl
leveneTest(firmOct12 ~ treatment, data = firmness)
#### treatment separation
kruskal.test(firmOct12~ treatment, data = firmness) #p<0.05
pairwise.wilcox.test(firmness$firmOct12, firmness$treatment, p.adjust.method = "BH")
## Fig----
treatmentCat <- c("ctrl", "avg", "naa", "ethephon")
firmnessLong %>%
  drop_na(firmness) %>%
  group_by(date, treatment) %>%
  summarise(med.firmness = median(firmness)) %>%
  arrange(date, match(treatment, treatmentCat)) -> firm.med
ggplot(firm.med, aes(x = date, y = med.firmness, color = treatment)) +
  geom_point() +
  geom_line() +
  geom_text_repel(aes(x = date, y= med.firmness, label = TeX(c(rep("a*", times = 12), "a*", "a*", "b*", "a*", "a*",
                                                          "a*", "b*", "ab*", "ab*", "a*", "c*", "b*", "b*", "a*",
                                                          "c*", "b*"), output = "character")), vjust=1,
                  color="black", position = position_dodge(2), size= 3,parse = T, min.segment.length = 1.9) +
  scale_color_brewer(palette = "RdBu", labels = c("AVG", "Control", "Ethephon", "NAA")) +
  labs(y = "Firmness",
       x = "Date",
       color = "Treatment",
       title = "Fruit Firmness",
       subtitle = "Median Values of Each Treatment by Date",
       caption = "*Kruskal-Wallis test")+
  theme_jl
ggsave("figs/firmness_plot.png")  


