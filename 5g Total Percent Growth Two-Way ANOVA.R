library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggpubr)
library(rstatix)

fiveglumps<- read.csv("5g Overall Percent Growth.csv", stringsAsFactors = TRUE, header = TRUE)
fiveglumps<- fiveglumps %>%
  mutate(SD=as.factor(SD))
fiveglumps<- fiveglumps %>%
  mutate(Rep=as.factor(NewRep))

str(fiveglumps)

stats<- fiveglumps%>%
  group_by(Light, SD) %>%
  get_summary_stats(Totalgrowth_percent, type="mean_sd")
view(stats2)

stats2<- fiveglumps%>%
  group_by(Light) %>%
  get_summary_stats(Totalgrowth_percent, type="mean_sd")

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

# Post Hoc

aov(Totalgrowth_percent~SD, data = fiveglumps) %>% tukey_hsd()
aov(Totalgrowth_percent~Light, data = fiveglumps) %>% tukey_hsd()


library(emmeans)
pwc <- fiveglumps %>% 
  group_by(Light) %>%
  emmeans_test(Totalgrowth_percent ~ SD, p.adjust.method = "bonferroni") 
pwc

library(DescTools)
DunnettTest(x=fiveglumps$Totalgrowth_percent, g=fiveglumps$SD)


model2 <- lm(Totalgrowth_percent ~ SD * Light, data = fiveglumps)
fiveglumps %>%
  group_by(Light) %>%
  anova_test(Totalgrowth_percent ~ SD, error = model2)

library(emmeans)
pwc2 <- fiveglumps %>% 
  group_by(Light) %>%
  emmeans_test(Totalgrowth_percent ~ SD, p.adjust.method = "bonferroni") 
pwc2