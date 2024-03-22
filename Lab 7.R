#### Library ####
library(mgcv)
library(tidyverse)
library(ggplot2)

#### Explaining ####
X = seq(0, 7.5, by = 0.05)
Y = jitter(sin(X), amount = 1) # compute sin(X), but add random noise
plot(x = X, y = Y)

test.mod <- lm(Y ~ X) # Construct a simple linear model
plot(x = X, y = Y) # Plot the raw data
abline(test.mod, col = 'red4', lwd = 3, lty = 2) # Plot the model fit

summary(test.mod) 

par(mfrow = c(2,2))
plot(m0)

par(mfrow = c(1,1))
g0 <- gam(Y ~ s(X, k = 10), method = 'REML') # More on k = 10 later...
plot(x = X, y = Y) # the observed data
lines(X, g0$fitted.values, lwd = 4, col = 'grey80') # Extract and plot fitted GAM values against original predictor

par(mfrow=c(2,2))
gam.check(g0, col = "orange3")

summary(g0)

# Run models for small k
g.k3 <- gam(Y ~ s(X, k = 3), method = 'REML')
g.k4 <- gam(Y ~ s(X, k = 4), method = 'REML')
g.k5 <- gam(Y ~ s(X, k = 5), method = 'REML')
g.k25 <- gam(Y ~ s(X, k = 25), method = 'REML') # kinda large k
g.k50 <- gam(Y ~ s(X, k = 50), method = 'REML') # very large k

# Plot data and our original GAM
plot(x = X, y = Y)
lines(X, g0$fitted.values, lwd = 3, col = 'gray50')

# Plot the different-k GAMs
lines(X, g.k3$fitted.values, lwd = 3, col = 'yellow', lty = 2)
lines(X, g.k4$fitted.values, lwd = 3, col = 'gold', lty = 2)
lines(X, g.k5$fitted.values, lwd = 3, col = 'orange2', lty = 2)
lines(X, g.k25$fitted.values, lwd = 3, col = 'firebrick', lty = 2)

# Plot how quickly optimal "k" is reached vs. different starting values
k.set <- c(3, 4, 5, 10, 25, 50)
k.optimal <- c(
  g.k3$df.null-g.k3$df.resid, 
  g.k4$df.null-g.k4$df.resid, 
  g.k5$df.null-g.k5$df.resid,
  g0$df.null-g0$df.resid,
  g.k25$df.null-g.k25$df.resid,
  g.k50$df.null-g.k50$df.resid
)

plot(k.set, k.optimal, type = "b", col = "orange3", pch = 16)

# Polynomial terms are specified with the poly() argument the following way:
g.poly2 <- glm(Y ~ poly(X, degree = 2, raw = T), family = gaussian()) # the degree is the basis expansion
g.poly3 <- glm(Y ~ poly(X, degree = 3, raw = T), family = gaussian())
g.poly5 <- glm(Y ~ poly(X, degree = 5, raw = T), family = gaussian())

# Plot data and original GAM
plot(x = X, y = Y)
lines(X, g0$fitted.values, lwd = 3, col = 'gray50')

# Plot the polynomial GLMs
lines(X, g.poly2$fitted.values, lwd = 3, col = 'blue', lty = 2)
lines(X, g.poly3$fitted.values, lwd = 3, col = 'navyblue', lty = 2)
lines(X, g.poly5$fitted.values, lwd = 3, col = 'purple3', lty = 2)

#### Doing the Lab ####
beav <- read.csv("beaver1_day346.csv")
head(beav)

plot(beav$time, beav$temp, pch = 15, col = "turquoise") # quick look at data distribution

# 1 #
gam1 <- gam(temp ~ s(time, k = 10), method = 'REML', data = beav)
capture.output(summary(gam1), file = "Bunch_Lab7_Model1.txt")

# 2 #
preds <- predict(gam1, newdata = beav, type = "response", se.fit = T)


## plot
my_data <- data.frame(mu=preds$fit, low =(preds$fit - 1.96 * preds$se.fit), high = (preds$fit + 1.96 * preds$se.fit))

ggplot() +
  geom_point(data = beav, aes(time,temp)) +
  geom_line(data = my_data, aes(x=beav$time, y=mu), size=1, col="blue")+
  geom_smooth(data=my_data,aes(ymin = low, ymax = high, x=beav$time, y = mu), stat = "identity", col="green") +
  labs(x= "Temperature", y = "Time")

# 3 #

# Fit GAM model with activity pattern variable as a parametric term
model <- gam(temp ~ s(time, k = 10) + activ, data = beav, method = "REML")

# Save the full model summary as a .txt file
capture.output(summary(model), file = "Bunch_Lab7_Model2.txt")

# 4 #

# Construct linear models with polynomial terms on time
lm_degree3 <- lm(temp ~ poly(time, 3) + activ, data = beav)
lm_degree5 <- lm(temp ~ poly(time, 5) + activ, data = beav)
lm_degree7 <- lm(temp ~ poly(time, 7) + activ, data = beav)

# Compare AIC scores of linear models
aic_scores <- c(AIC(lm_degree3), AIC(lm_degree5), AIC(lm_degree7))
aic_scores

#7 is best

# Save the full model summary of the best fit linear model as a .txt file
capture.output(summary(lm_degree7), file = "Bunch_Lab7_Model3.txt")


# 5 #

# Generate predictions for the GAM model
gam_pred <- predict(gam1, newdata = data.frame(time = beav$time, activ = 0), type = "response", se.fit = T)

# Generate predictions for the top-ranked linear model
lm_pred <- predict(lm_degree7, newdata = data.frame(time = beav$time, activ = 0), se.fit = T)


# Plot empirical data
plot(beav$time, beav$temp, type = "n", xlab = "Time", ylab = "Temperature", main = "Temperature vs. Time")

# Plot GAM fit with standard error
lines(beav$time, gam_pred$fit, col = "turquoise3", lwd = 2)
lines(beav$time, gam_pred$fit + 1.96 * gam_pred$se.fit, col = "turquoise", lty = 2)
lines(beav$time, gam_pred$fit - 1.96 * gam_pred$se.fit, col = "turquoise", lty = 2)

# Plot LM fit with standard error
lines(beav$time, lm_pred$fit, col = "orange3", lwd = 2)
lines(beav$time, lm_pred$fit + 1.96 * lm_pred$se.fit, col = "orange", lty = 2)
lines(beav$time, lm_pred$fit - 1.96 * lm_pred$se.fit, col = "orange", lty = 2)

# Add legend
legend("topright", legend = c("GAM", "Linear Model"), col = c("turquoise3", "orange3"), lwd = 2, lty = 1)

# Add axis labels
axis(1, at = pretty(beav$time), labels = pretty(beav$time))
axis(2, at = pretty(beav$temp), labels = pretty(beav$temp))


# Create data frames for GAM and linear model predictions
gam_df <- data.frame(time = beav$time, temp = gam_pred$fit, se = gam_pred$se.fit)
lm_df <- data.frame(time = beav$time, temp = lm_pred$fit, se = lm_pred$se.fit)

# ggplot :)
ggplot(beav, aes(x = time, y = temp)) +
  geom_point() +
  geom_line(data = gam_df, aes(y = temp, color = "GAM"), size = 1.5) +
  geom_ribbon(data = gam_df, aes(ymin = temp - 1.96 * se, ymax = temp + 1.96 * se, fill = "GAM"), alpha = 0.3) +
  geom_line(data = lm_df, aes(y = temp, color = "Linear"), size = 1.5) +
  geom_ribbon(data = lm_df, aes(ymin = temp - 1.96 * se, ymax = temp + 1.96 * se, fill = "Linear"), alpha = 0.3) +
  labs(x = "Time", y = "Temperature", title = "Temperature vs. Time") +
  scale_color_manual(name = "Model", values = c("GAM" = "turquoise3", "Linear" = "orange3")) +
  scale_fill_manual(name = "Model", values = c("GAM" = "turquoise", "Linear" = "orange"), guide = FALSE) +
  theme_bw()



