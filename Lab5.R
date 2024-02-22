#### Libraries and Prelab ####

library(lme4)
library(ggplot2)
library(lattice)
data(sleepstudy)
head(sleepstudy)

hist(sleepstudy$Reaction)

m0 <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)

summary(m0)

m0.ranef <- ranef(m0)
m0.ranef

pred.Reaction <- predict(m0, newdata = sleepstudy, type = "response") # create a vector of predicted values for Subjects, just using the original data
pred.df <- data.frame(sleepstudy, pred.Reaction) # bind those predictions to the original data frame
ggplot(data = pred.df, aes(Days, Reaction)) + 
  geom_point() + 
  geom_line(aes(Days, pred.Reaction), col = "dodgerblue") + # set new aesthetic for the predicted Y
  facet_wrap(vars(Subject)) #wrap by groups


#### This Week's Assignment ####

# 1 #

library(datasets)
data(ChickWeight)
hist(ChickWeight$weight) # Visualize the data distribution 

plot(ChickWeight) # Visualize the trends per-chick

m_chick <- lmer(weight ~ poly(Time, 2)*Diet + (Time | Chick), data = ChickWeight)

capture.output(summary(m_chick), file = "Chick_Model.txt")

# 2 #

m0.ranef <- ranef(m_chick)
m0.ranef

dotplot(m0.ranef) 

# 3 #

m_chick_1 <- lmer(weight ~ poly(Time, 2)*Diet + (0 + Time | Chick), data = ChickWeight)

capture.output(summary(m_chick_1), file = "Chick_Model_2.txt")

# 4 #

chick_anova <- anova(m_chick_1 ,m_chick)

capture.output(chick_anova, file = "chick_anova.txt")

# 5 #

pred.Reaction <- predict(m_chick, newdata = ChickWeight, type = "response") # create a vector of predicted values for Subjects, just using the original data
pred.df <- data.frame(ChickWeight, pred.Reaction) # bind those predictions to the original data frame
ggplot(data = pred.df, aes(Time, weight)) + 
  geom_point(size = 0.5) + 
  geom_line(aes(Time, pred.Reaction), col = "red") + # set new aesthetic for the predicted Y
  facet_wrap(vars(Chick)) + #wrap by groups
  labs(title = "Chick-Specific Summary of Chick Growth Models", y = "Weight")
