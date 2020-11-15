# Preharvest Drop: Temperature Data Analysis
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
## load data----
temp <- read_csv("data/2020/weather_data.csv")
## separate date----
temp$date <- mdy(temp$date)
temp %>%
  separate(date, sep = "-", into = c("year", "month", "day")) -> temp
## 2020 data----
temp %>%
  filter(year == "2020") -> temp2020
## previous 5 years----
temp %>%
  filter(!year == "2020") -> tempAvg
## calculate montly averages ----
temp2020 %>%
  group_by(month) %>%
  summarise(temp = mean(avgTemp),
            sd = sd(avgTemp)) %>%
  mutate(year = "2020") -> meanTemps
tempAvg %>%
  drop_na(avgTemp) %>%
  group_by(month) %>%
  summarise(temp = mean(avgTemp),
            sd = sd(avgTemp)) %>%
  mutate(year = "5 year avg.") -> mean5temps
meanTemps <- rbind(meanTemps, mean5temps)
## change month----
meanTemps %>%
  mutate(month2 = case_when(month == "07" ~ "July",
                            month == "08" ~ "August",
                            month == "09" ~ "September",
                            month == "10" ~ "October")) %>%
  ggplot(aes(x = month, y = temp, color = year)) +
  geom_point() +
  geom_line() +
  scale_color_brewer(palette = "Dark2") +
  scale_x_discrete(breaks = c("07", "08", "09", "10"),
                   labels = c("July", "Aug.", "Sep.", "Oct.")) +
  labs(y = "Avg. Temp. (°F)",
       x = "Month",
       color = "Measurement Period",
       title = "Average Monthly Temperature")+
  theme_jl
ggsave("figs/temp_plot.png")  
