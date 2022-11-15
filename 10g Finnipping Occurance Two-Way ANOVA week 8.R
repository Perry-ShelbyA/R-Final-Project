library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggpubr)
library(rstatix)

tenglumps<- read.csv("10g by Treatment week8.csv", stringsAsFactors = TRUE, header = TRUE)
tenglumps<- tenglumps %>%
  mutate(SD=as.factor(SD))
tenglumps<- tenglumps %>%
  mutate(Rep=as.factor(NewRep))
tenglumps<- tenglumps %>%
  mutate(Time=as.factor(Time))

tenglumps$Transformed<-asin(sqrt(tenglumps$Presence))
tenglumps$Transformed2<- (tenglumps$Transformed)^(2)

str(tenglumps)

stats<-tenglumps%>%
  group_by(Light, SD) %>%
  get_summary_stats(Presence, type="mean_sd")
view(stats)
bxp<-ggboxplot(tenglumps, x="Light", y="Transformed2", color="SD", palette = "jco", short.panel.labs = FALSE)
bxp

#testing assumptions for Outliers and Normality
tenglumps%>%
  group_by(Light, SD) %>%
  identify_outliers(Transformed2)
tenglumps%>%
  group_by(Light, SD) %>%
  shapiro_test(Transformed2)

model<- lm(Transformed2 ~ Light*SD,
           data=tenglumps)
ggqqplot(residuals(model))

shapiro_test(residuals(model))

#QQ plot
ggqqplot(tenglumps, "Transformed2", ggtheme = theme_bw()) +
  facet_grid(Light ~ SD)

#Homogneity of variance assumption
tenglumps%>%
  levene_test(Transformed2 ~ Light*SD)

res.aov <- tenglumps %>% anova_test(Transformed2 ~ Light * SD)
res.aov

