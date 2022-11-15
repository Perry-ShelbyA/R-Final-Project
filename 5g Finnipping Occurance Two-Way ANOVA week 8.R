library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggpubr)
library(rstatix)

fiveglumps<- read.csv("5g by Treatment week8.csv", stringsAsFactors = TRUE, header = TRUE)
fiveglumps<- fiveglumps %>%
  mutate(SD=as.factor(SD))
fiveglumps<- fiveglumps %>%
  mutate(Rep=as.factor(NewRep))
fiveglumps<- fiveglumps %>%
  mutate(Time=as.factor(Time))

fiveglumps$Transformed<-asin(sqrt(fiveglumps$Presence))

str(fiveglumps)

stats<-fiveglumps%>%
  group_by(Light, SD) %>%
  get_summary_stats(Presence, type="mean_sd")
view(stats)
bxp<-ggboxplot(fiveglumps, x="Light", y="Transformed", color="SD", palette = "jco", short.panel.labs = FALSE)
bxp

#testing assumptions for Outliers and Normality
fiveglumps%>%
  group_by(Light, SD) %>%
  identify_outliers(Transformed)
fiveglumps%>%
  group_by(Light, SD) %>%
  shapiro_test(Transformed)

model<- lm(Transformed ~ Light*SD,
             data=fiveglumps)
ggqqplot(residuals(model))

shapiro_test(residuals(model))

#QQ plot
ggqqplot(fiveglumps, "Transformed", ggtheme = theme_bw()) +
  facet_grid(Light ~ SD)

#Homogneity of variance assumption
fiveglumps%>%
  levene_test(Transformed ~ Light*SD)

res.aov <- fiveglumps %>% anova_test(Transformed ~ Light * SD)
res.aov
