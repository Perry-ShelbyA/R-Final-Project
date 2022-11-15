library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggpubr)
library(rstatix)

fiveglumps<- read.csv("10g Overall Percent Growth.csv", stringsAsFactors = TRUE, header = TRUE)
fiveglumps<- fiveglumps %>%
  mutate(SD=as.factor(SD))
fiveglumps<- fiveglumps %>%
  mutate(Rep=as.factor(NewRep))


str(fiveglumps)



stats<- fiveglumps%>%
  group_by(Light, SD) %>%
  get_summary_stats(Totalgrowth_percent, type="mean_sd")
view(stats)

bxp<-ggboxplot(fiveglumps, x="Light", y="Totalgrowth_percent", color="SD", palette = "jco", short.panel.labs = FALSE)
bxp

#testing assumptions for Outliers and Normality
fiveglumps%>%
  group_by(Light, SD) %>%
  identify_outliers(Totalgrowth_percent)
fiveglumps%>%
  group_by(Light, SD) %>%
  shapiro_test(Totalgrowth_percent)

model<- lm(Totalgrowth_percent ~ Light*SD,
           data=fiveglumps)
ggqqplot(residuals(model))

shapiro_test(residuals(model))

#QQ plot
ggqqplot(fiveglumps, "Totalgrowth_percent", ggtheme = theme_bw()) +
  facet_grid(Light ~ SD)

#Homogneity of variance assumption
fiveglumps%>%
  levene_test(Totalgrowth_percent ~ Light*SD)

res.aov <- fiveglumps %>% anova_test(Totalgrowth_percent ~ Light * SD)
res.aov

