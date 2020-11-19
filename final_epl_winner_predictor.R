rm(list = ls())
library(arm)
library(pROC)
library(e1071)
library(caret)
library(ggplot2)
require(gridExtra)
library(grid)
library(rms)
library(stargazer)
library(lme4)
library(stringr)
require(dplyr)
library(dplyr)
library(lme4)
require(tab)
library(sjPlot)

epl <- read.csv("Data/final_dataset.csv",header=T, na.strings = c("NA","."))

epl <- subset(epl, select=-c(defenceDefenderLineClass))

epl$date <- factor(epl$date)
epl$buildUpPlaySpeedClass <- factor(epl$buildUpPlaySpeedClass)
epl$buildUpPlayDribblingClass <- factor(epl$buildUpPlayDribblingClass)
epl$buildUpPlayPassingClass <- factor(epl$buildUpPlayPassingClass)
epl$buildUpPlayPositioningClass <- factor(epl$buildUpPlayPositioningClass)
epl$chanceCreationPassingClass <- factor(epl$chanceCreationPassingClass)
epl$chanceCreationCrossingClass <- factor(epl$chanceCreationCrossingClass)
epl$chanceCreationShootingClass <- factor(epl$chanceCreationShootingClass)
epl$chanceCreationPositioningClass <- factor(epl$chanceCreationPositioningClass)
epl$defencePressureClass <- factor(epl$defencePressureClass)
epl$defenceAggressionClass <- factor(epl$defenceAggressionClass)
epl$defenceTeamWidthClass <- factor(epl$defenceTeamWidthClass)
epl$team <- factor(epl$team)
epl$season <- factor(epl$season)
epl$log_overall_win_perc <- log(epl$overall_win_perc)

str(epl)

ggplot(epl,aes(log_overall_win_perc)) +
  geom_histogram(aes(y=..density..),color="black",linetype="dashed",
                 fill=rainbow(15),bins=15) + theme(legend.position="none") +
  geom_density(alpha=.25, fill="lightblue") + scale_fill_brewer(palette="Blues") +
  labs(title="Distribution of Log Overall Win Percentage",y="log_overall_win_perc") + theme_classic()

ggplot(epl,aes(overall_win_perc)) +
  geom_histogram(aes(y=..density..),color="black",linetype="dashed",
                 fill=rainbow(15),bins=15) + theme(legend.position="none") +
  geom_density(alpha=.25, fill="lightblue") + scale_fill_brewer(palette="Blues") +
  labs(title="Distribution of Overall Win Percentage",y="overall_win_perc") + theme_classic()

ggplot(epl,
       aes(x=buildUpPlaySpeedClass, y=log_overall_win_perc, fill=buildUpPlaySpeedClass)) +
  geom_boxplot() +
  labs(title="log_overall_win_perc by buildUpPlaySpeedClass",
       x="buildUpPlaySpeedClass",y="log_overall_win_perc") + theme_classic() +
  theme(legend.position="none",axis.text.x = element_text(angle = 90))

ggplot(epl,
       aes(x=buildUpPlayDribblingClass, y=log_overall_win_perc, fill=buildUpPlayDribblingClass)) +
  geom_boxplot() +
  labs(title="log_overall_win_perc by buildUpPlayDribblingClass",
       x="buildUpPlayDribblingClass",y="log_overall_win_perc") + theme_classic() +
  theme(legend.position="none",axis.text.x = element_text(angle = 90))

ggplot(epl,
       aes(x=buildUpPlayPassingClass, y=log_overall_win_perc, fill=buildUpPlayPassingClass)) +
  geom_boxplot() +
  labs(title="log_overall_win_perc by buildUpPlayPassingClass",
       x="buildUpPlayPassingClass",y="log_overall_win_perc") + theme_classic() +
  theme(legend.position="none",axis.text.x = element_text(angle = 90))

ggplot(epl,
       aes(x=buildUpPlayPositioningClass, y=log_overall_win_perc, fill=buildUpPlayPositioningClass)) +
  geom_boxplot() +
  labs(title="log_overall_win_perc by buildUpPlayPositioningClass",
       x="buildUpPlayPositioningClass",y="log_overall_win_perc") + theme_classic() +
  theme(legend.position="none",axis.text.x = element_text(angle = 90))

ggplot(epl,
       aes(x=chanceCreationPassingClass, y=log_overall_win_perc, fill=chanceCreationPassingClass)) +
  geom_boxplot() +
  labs(title="log_overall_win_perc by chanceCreationPassingClass",
       x="chanceCreationPassingClass",y="log_overall_win_perc") + theme_classic() +
  theme(legend.position="none",axis.text.x = element_text(angle = 90))

ggplot(epl,
       aes(x=chanceCreationCrossingClass, y=log_overall_win_perc, fill=chanceCreationCrossingClass)) +
  geom_boxplot() +
  labs(title="log_overall_win_perc by chanceCreationCrossingClass",
       x="chanceCreationCrossingClass",y="log_overall_win_perc") + theme_classic() +
  theme(legend.position="none",axis.text.x = element_text(angle = 90))

ggplot(epl,
       aes(x=chanceCreationShootingClass, y=log_overall_win_perc, fill=chanceCreationShootingClass)) +
  geom_boxplot() +
  labs(title="log_overall_win_perc by chanceCreationShootingClass",
       x="chanceCreationShootingClass",y="log_overall_win_perc") + theme_classic() +
  theme(legend.position="none",axis.text.x = element_text(angle = 90))

ggplot(epl,
       aes(x=chanceCreationPositioningClass, y=log_overall_win_perc, fill=chanceCreationPositioningClass)) +
  geom_boxplot() +
  labs(title="log_overall_win_perc by chanceCreationPositioningClass",
       x="chanceCreationPositioningClass",y="log_overall_win_perc") + theme_classic() +
  theme(legend.position="none",axis.text.x = element_text(angle = 90))

ggplot(epl,
       aes(x=defencePressureClass, y=log_overall_win_perc, fill=defencePressureClass)) +
  geom_boxplot() +
  labs(title="log_overall_win_perc by defencePressureClass",
       x="defencePressureClass",y="log_overall_win_perc") + theme_classic() +
  theme(legend.position="none",axis.text.x = element_text(angle = 90))

ggplot(epl,
       aes(x=defenceAggressionClass, y=log_overall_win_perc, fill=defenceAggressionClass)) +
  geom_boxplot() +
  labs(title="log_overall_win_perc by defenceAggressionClass",
       x="defenceAggressionClass",y="log_overall_win_perc") + theme_classic() +
  theme(legend.position="none",axis.text.x = element_text(angle = 90))

ggplot(epl,
       aes(x=defenceTeamWidthClass, y=log_overall_win_perc, fill=defenceTeamWidthClass)) +
  geom_boxplot() +
  labs(title="log_overall_win_perc by defenceTeamWidthClass",
       x="defenceTeamWidthClass",y="log_overall_win_perc") + theme_classic() +
  theme(legend.position="none",axis.text.x = element_text(angle = 90))

ggplot(epl,aes(x=chanceCreationPassing, y=log_overall_win_perc)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="log_overall_win_perc vs chanceCreationPassing:defenceAggressionClass",x="chanceCreationPassing",y="log_overall_win_perc") +
  facet_wrap( ~ defenceAggressionClass,ncol=4)

ggplot(epl,aes(x=buildUpPlayPassing, y=log_overall_win_perc)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="log_overall_win_perc vs buildUpPlayPassing:buildUpPlayDribblingClass",x="buildUpPlayPassing",y="log_overall_win_perc") +
  facet_wrap( ~ buildUpPlayDribblingClass,ncol=4)

ggplot(epl,aes(x=chanceCreationCrossing, y=log_overall_win_perc)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="log_overall_win_perc vs chanceCreationCrossing:buildUpPlayPassingClass",x="chanceCreationCrossing",y="log_overall_win_perc") +
  facet_wrap( ~ buildUpPlayPassingClass,ncol=4)

ggplot(epl,aes(x=chanceCreationShooting, y=log_overall_win_perc)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="log_overall_win_perc vs chanceCreationShooting:buildUpPlaySpeedClass",x="chanceCreationShooting",y="log_overall_win_perc") +
  facet_wrap( ~ buildUpPlaySpeedClass,ncol=4)

ggplot(epl,aes(x=defencePressure, y=log_overall_win_perc)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="lm",col="red3") + theme_classic() +
  labs(title="log_overall_win_perc vs defencePressure:buildUpPlaySpeedClass",x="defencePressure",y="log_overall_win_perc") +
  facet_wrap( ~ buildUpPlaySpeedClass,ncol=4)

str(epl)

Null_Model <- lmer(log_overall_win_perc ~ buildUpPlaySpeedClass + buildUpPlayDribblingClass 
               + chanceCreationCrossingClass + chanceCreationShootingClass + defenceAggressionClass
               + (1 | team), data = epl)

summary(Null_Model)

confint(Null_Model)

Full_Model <- lmer(log_overall_win_perc ~ buildUpPlaySpeedClass + buildUpPlayDribblingClass
               + buildUpPlayPassingClass + buildUpPlayPositioningClass #+ chanceCreationPassingClass
               + chanceCreationCrossingClass + chanceCreationShootingClass + chanceCreationPositioningClass 
               + defencePressureClass + defenceAggressionClass #+ defenceTeamWidthClass #+ season
               #+ chanceCreationPassing*defenceAggressionClass
               + (1 | team), data = epl)

summary(Full_Model)

anova(Null_Model, Full_Model)

confint(Full_Model)



Model1 <- lmer(log_overall_win_perc ~ buildUpPlaySpeedClass + buildUpPlayDribblingClass 
               + buildUpPlayPassingClass + buildUpPlayPositioningClass #+ chanceCreationPassingClass
               + chanceCreationCrossingClass + chanceCreationShootingClass + chanceCreationPositioningClass 
               + defencePressureClass + defenceAggressionClass #+ defenceTeamWidthClass #+ season
               + chanceCreationPassing*defenceAggressionClass
               + (1 | team), data = epl)

summary(Model1)

confint(Model1)

anova(Full_Model, Model1, test='Chisq')

Model2 <- lmer(log_overall_win_perc ~ buildUpPlaySpeedClass + buildUpPlayDribblingClass 
               #+ buildUpPlayPassingClass 
               + buildUpPlayPositioningClass + chanceCreationPassingClass
               #+ chanceCreationCrossingClass 
               + chanceCreationShootingClass + chanceCreationPositioningClass 
               + defencePressureClass + defenceAggressionClass #+ defenceTeamWidthClass #+ season
               + chanceCreationCrossing*buildUpPlayPassingClass
               + (1 | team), data = epl)

summary(Model2)

confint(Model2)

anova(Model1, Model2)

base_line_model <- Model1

#tab_model(base_line_model)
confint(base_line_model)

summary(base_line_model)

pred <- predict(base_line_model)
epl$y_pred <- pred

tmp1 <- epl %>%
    select(season, log_overall_win_perc) %>%
    group_by(season) %>%
    summarise(max = max(log_overall_win_perc))

tmp2 <- inner_join(epl, tmp1, by=c("season" = "season", "log_overall_win_perc" = "max"))
tmp3 <- subset(tmp2, select=c(season, team, log_overall_win_perc, y_pred))
arrange(tmp3, season, log_overall_win_perc)

tmp1 <- epl %>%
    select(season, y_pred) %>%
    group_by(season) %>%
    summarise(max = max(y_pred))

tmp2 <- inner_join(epl, tmp1, by=c("season" = "season", "y_pred" = "max"))
tmp3 <- subset(tmp2, select=c(season, team, log_overall_win_perc, y_pred))
arrange(tmp3, season, y_pred)

str(epl)

res <- residuals(base_line_model)

# Linearity
# chanceCreationPassing

ggplot(epl, aes(chanceCreationPassing, y=res)) +
  geom_point(alpha = .5,colour="blue3") +
  geom_line(y=0, col="red3") +
  geom_smooth(method = "lm", col = "red3") +
  xlab("chanceCreationPassing") +
  ylab("Residuals") +
  labs(caption="Residual vs chanceCreationPassing") +
  theme(plot.caption = element_text(hjust = 0.5, size = 20)) +
  facet_wrap(~team,ncol = 5)

# Independence and Equality of Variance
pred <- predict(base_line_model)
pred_res <- data.frame(pred, res)
ggplot(pred_res, aes(pred, y=res)) +
  geom_point(alpha = .5,colour="blue3") +
  #geom_line(y = 0, col = "red3") +
  geom_smooth(method="lm",col="red3") +
  xlab("Fitted values") +
  ylab("Residuals") +
  labs(caption="Residuals vs Fitted values") +
  theme(plot.caption = element_text(hjust = 0.5, size = 20))

# Normality
std_res <- (res - mean(res)) / sd(res)
std_res_df <- data.frame(std_res)
qplot(sample = std_res, data = std_res_df, color=I("blue3"), alpha=.5) +
  geom_abline(intercept = 0, slope = 1, col="red3") +
  xlab("Theoretical Quantiles") +
  ylab("Standardized Residuals") +
  labs(caption="Normal Q-Q") +
  theme(plot.caption = element_text(hjust = 0.5, size = 20), legend.position = "none")

mock_data <- read.csv("Data/mockup_2015-2016_dataset.csv",header=T, na.strings = c("NA","."))

mock_data <- subset(mock_data, select=-c(defenceDefenderLineClass))

mock_data$date <- factor(mock_data$date)
mock_data$buildUpPlaySpeedClass <- factor(mock_data$buildUpPlaySpeedClass)
mock_data$buildUpPlayDribblingClass <- factor(mock_data$buildUpPlayDribblingClass)
mock_data$buildUpPlayPassingClass <- factor(mock_data$buildUpPlayPassingClass)
mock_data$buildUpPlayPositioningClass <- factor(mock_data$buildUpPlayPositioningClass)
mock_data$chanceCreationPassingClass <- factor(mock_data$chanceCreationPassingClass)
mock_data$chanceCreationCrossingClass <- factor(mock_data$chanceCreationCrossingClass)
mock_data$chanceCreationShootingClass <- factor(mock_data$chanceCreationShootingClass)
mock_data$chanceCreationPositioningClass <- factor(mock_data$chanceCreationPositioningClass)
mock_data$defencePressureClass <- factor(mock_data$defencePressureClass)
mock_data$defenceAggressionClass <- factor(mock_data$defenceAggressionClass)
mock_data$defenceTeamWidthClass <- factor(mock_data$defenceTeamWidthClass)
mock_data$team <- factor(mock_data$team)
mock_data$season <- factor(mock_data$season)
mock_data$log_overall_win_perc <- log(mock_data$overall_win_perc)

y_pred <- predict(base_line_model, mock_data, allow.new.levels=TRUE)
mock_data$y_pred <- y_pred

tmp1 <- mock_data %>%
    select(season, y_pred) %>%
    group_by(season) %>%
    summarise(max = max(y_pred))

tmp2 <- inner_join(mock_data, tmp1, by=c("season" = "season", "y_pred" = "max"))
tmp3 <- subset(tmp2, select=c(season, team, log_overall_win_perc, y_pred))
arrange(tmp3, season, y_pred)

mock_test <- subset(mock_data, select=c(season, team, log_overall_win_perc, y_pred))
arrange(mock_test, season, y_pred)

#print(base_line_model, correlation=TRUE)

#vcov(base_line_model) 

dotplot(ranef(base_line_model, condVar=TRUE))

tab_model(base_line_model)


