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
#install.packages("mvnormtest")
library(mvnormtest)
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
### Aug. 27----
#### Normality test
#### Primary Bundles
aug27.primary.aov <- aov(primaryAug27 ~ treatment, data = dye)
primaryAug27.resid <- residuals(object = aug27.primary.aov)
shapiro.test(x = primaryAug27.resid)# p<0.05
#### Dorsal Bundles
aug27.dorsal.aov <- aov(dorsalAug27 ~ treatment, data = dye)
dorsalAug27.resid <- residuals(object = aug27.dorsal.aov)
shapiro.test(x = dorsalAug27.resid) # p<0.05
#### Levene's test
#### Primary
leveneTest(primaryAug27 ~ treatment, data = dye)#p>0.05
#### Dorsal
leveneTest(dorsalAug27 ~ treatment, data = dye)#p>0.05
#### Treatment Separation
#### Primary
kruskal.test(primaryAug27 ~ treatment, data = dye) #p>0.05
#### Dorsal
kruskal.test(dorsalAug27 ~ treatment, data = dye) # p<0.05
pairwise.wilcox.test(dye$dorsalAug27, dye$treatment, p.adjust.method = "BH")
### Sep. 2 ----
#### Normality test
#### Primary Bundles
Sep2.primary.aov <- aov(primarySep2 ~ treatment, data = dye)
primarySep2.resid <- residuals(object = Sep2.primary.aov)
shapiro.test(x = primarySep2.resid) # p<0.05
#### Dorsal Blundles
Sep2.dorsal.aov <- aov(dorsalSep2 ~ treatment, data = dye)
dorsalSep2.resid <- residuals(object = Sep2.dorsal.aov)
shapiro.test(x = dorsalSep2.resid) # p<0.05
#### Levene's test
#### Primary
leveneTest(primarySep2 ~ treatment, data = dye)#p>0.05
#### Dorsal
leveneTest(dorsalSep2 ~ treatment, data = dye)#p>0.05
#### Treatment separation
#### Primary
kruskal.test(primarySep2 ~ treatment, data = dye)#p>0.05
#### Dorsal
kruskal.test(dorsalSep2 ~ treatment, data = dye)#p>0.05
### Sep. 9 ----
#### Normality test
#### Primary Bundles
Sep9.primary.aov <- aov(primarySep9 ~ treatment, data = dye)
primarySep9.resid <- residuals(object = Sep9.primary.aov)
shapiro.test(x = primarySep9.resid) # p<0.05
#### Dorsal Blundles
Sep9.dorsal.aov <- aov(dorsalSep9 ~ treatment, data = dye)
dorsalSep9.resid <- residuals(object = Sep9.dorsal.aov)
shapiro.test(x = dorsalSep9.resid) # p<0.05
#### Levene's test
#### Primary
leveneTest(primarySep9 ~ treatment, data = dye)#p<0.05
#### Dorsal
leveneTest(dorsalSep9 ~ treatment, data = dye)#p>0.05
#### Treatment separation
#### Primary
kruskal.test(primarySep9 ~ treatment, data = dye)#p>0.05
#### Dorsal
kruskal.test(dorsalSep9 ~ treatment, data = dye)#p>0.05
### Sep. 16 ----
#### Normality test
#### Primary Bundles
Sep16.primary.aov <- aov(primarySep16 ~ treatment, data = dye)
primarySep16.resid <- residuals(object = Sep16.primary.aov)
shapiro.test(x = primarySep16.resid) # p<0.05
#### Dorsal Blundles
Sep16.dorsal.aov <- aov(dorsalSep16 ~ treatment, data = dye)
dorsalSep16.resid <- residuals(object = Sep16.dorsal.aov)
shapiro.test(x = dorsalSep16.resid) # p<0.05
#### Levene's test
#### Primary
leveneTest(primarySep16 ~ treatment, data = dye)#p>0.05
#### Dorsal
leveneTest(dorsalSep16 ~ treatment, data = dye)#p>0.05
#### Treatment separation
#### Primary
kruskal.test(primarySep16 ~ treatment, data = dye)#p>0.05
#### Dorsal
kruskal.test(dorsalSep16 ~ treatment, data = dye)#p>0.05
### Sep. 23 ----
#### Normality test
#### Primary Bundles
Sep23.primary.aov <- aov(primarySep23 ~ treatment, data = dye)
primarySep23.resid <- residuals(object = Sep23.primary.aov)
shapiro.test(x = primarySep23.resid) # p<0.05
#### Dorsal Blundles
Sep23.dorsal.aov <- aov(dorsalSep23 ~ treatment, data = dye)
dorsalSep23.resid <- residuals(object = Sep23.dorsal.aov)
shapiro.test(x = dorsalSep23.resid) # p<0.05
#### Levene's test
#### Primary
leveneTest(primarySep23 ~ treatment, data = dye)#p<0.05
#### Dorsal
leveneTest(dorsalSep23 ~ treatment, data = dye)#p>0.05
#### Treatment separation
#### Primary
kruskal.test(primarySep23 ~ treatment, data = dye)#p<0.05
pairwise.wilcox.test(dye$primarySep23, dye$treatment, p.adjust.method = "BH")
#### Dorsal
kruskal.test(dorsalSep23 ~ treatment, data = dye)#p<0.05
pairwise.wilcox.test(dye$dorsalSep23, dye$treatment, p.adjust.method = "BH")
### Sep. 30 ----
#### Normality test
#### Primary Bundles
Sep30.primary.aov <- aov(primarySep30 ~ treatment, data = dye)
primarySep30.resid <- residuals(object = Sep30.primary.aov)
shapiro.test(x = primarySep30.resid) # p<0.05
#### Dorsal Blundles
Sep30.dorsal.aov <- aov(dorsalSep30 ~ treatment, data = dye)
dorsalSep30.resid <- residuals(object = Sep30.dorsal.aov)
shapiro.test(x = dorsalSep30.resid) # p<0.05
#### Levene's test
#### Primary
leveneTest(primarySep30 ~ treatment, data = dye)#p<0.05
#### Dorsal
leveneTest(dorsalSep30 ~ treatment, data = dye)#p>0.05
#### Treatment separation
#### Primary
kruskal.test(primarySep30 ~ treatment, data = dye)#p<0.05
pairwise.wilcox.test(dye$primarySep30, dye$treatment, p.adjust.method = "BH")
#### Dorsal
kruskal.test(dorsalSep30 ~ treatment, data = dye)#p>0.05
### Oct. 14 ----
#### Normality test
#### Primary Bundles
Oct14.primary.aov <- aov(primaryOct14 ~ treatment, data = dye)
primaryOct14.resid <- residuals(object = Oct14.primary.aov)
shapiro.test(x = primaryOct14.resid) # p<0.05
#### Dorsal Blundles
Oct14.dorsal.aov <- aov(dorsalOct14 ~ treatment, data = dye)
dorsalOct14.resid <- residuals(object = Oct14.dorsal.aov)
shapiro.test(x = dorsalOct14.resid) # p<0.05
#### Levene's test
#### Primary
leveneTest(primaryOct14 ~ treatment, data = dye)#p<0.05
#### Dorsal
leveneTest(dorsalOct14 ~ treatment, data = dye)#p<0.05
#### Treatment separation
#### Primary
kruskal.test(primaryOct14 ~ treatment, data = dye)#p<0.05
pairwise.wilcox.test(dye$primaryOct14, dye$treatment, p.adjust.method = "BH")
#### Dorsal
kruskal.test(dorsalOct14 ~ treatment, data = dye)#p>0.05
pairwise.wilcox.test(dye$dorsalOct14, dye$treatment, p.adjust.method = "BH")

## figs----
treatmentCat <- c("ctrl", "avg", "naa", "ethephon")
primary <- c(rep("a*", times = 16), "bc*", "a*", "b*", "c*", "b*", "a*", "a*", "c*",
             rep("a*", times = 4))
dorsal <- c(rep("a*", times = 16),  "ab*", "a*", "b*", "b*", rep("a*", times = 4), "b*", "a*", "b*", "b*")
dyeLong %>%
  drop_na(primary) %>%
  group_by(date, treatment) %>%
  summarise(avg.primary = mean(primary),
            avg.dorsal = mean(dorsal)) %>%
  arrange(date, match(treatment, treatmentCat)) -> dye.med
### primary----
ggplot(dye.med, aes(x = date, y = avg.primary, color = treatment)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = mdy("9/29/2020"), colour = "red") +
  annotate("text", x = mdy("9/29/2020") - 1, y = 3.25, label = "Begin drop", angle = 90, size = 3) +
  geom_text_repel(aes(x = date, y= avg.primary, label = TeX(c(rep("a*", times = 16), "bc*", "a*", "b*", "c*", "b*", "a*", "a*", "c*",
                                                            rep("a*", times = 4)), output = "character")), vjust=1,
                  color="black", position = position_dodge(2), size= 3,parse = T, min.segment.length = 1.9) +
  scale_color_brewer(palette = "Set2", labels = c("AVG", "Control", "Ethephon", "NAA")) +
  labs(y = "No. of Stained Primary Vessels",
       x = "Date",
       color = "Treatment",
       title = "Primary Bundles",
       subtitle = "Mean Values of Each Treatment by Date",
       caption = "*Kruskal-Wallis test")+
  theme_jl
ggsave("figs/primary_bundles_plot2.png")  

### dorsal----
ggplot(dye.med, aes(x = date, y = avg.dorsal, color = treatment)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = mdy("9/29/2020"), colour = "red") +
  annotate("text", x = mdy("9/29/2020") - 1, y = 3.25, label = "Begin drop", angle = 90, size = 3) +
  geom_text_repel(aes(x = date, y= avg.dorsal, label = TeX(c(rep("a*", times = 16),  "ab*", "a*", "b*", "b*",
                                                             rep("a*", times = 4), "b*", "a*", "b*", "b*"), output = "character")), vjust=1,
                  color="black", position = position_dodge(2), size= 3,parse = T, min.segment.length = 1.9) +
  scale_color_brewer(palette = "Set2", labels = c("AVG", "Control", "Ethephon", "NAA")) +
  labs(y = "No. of Stained Dorsal Vessels",
       x = "Date",
       color = "Treatment",
       title = "Dorsal Bundles",
       subtitle = "Mean Values of Each Treatment by Date",
       caption = "*Kruskal-Wallis test")+
  theme_jl
ggsave("figs/dorsal_bundles_plot2.png")  
