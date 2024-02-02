
load("mlb11.rdata")

nrow(mlb11) 

ncol(mlb11) - length(which(colnames(mlb11) == 'team'))  # print number of stats

head(mlb11)

summary(mlb11)

par(mar = c(2, 2, 2, 2))
plot(mlb11$at_bats, mlb11$runs)

m1 <- lm(runs ~ at_bats, data = mlb11)

summary(m1)

attributes(m1)

plot(mlb11$at_bats, mlb11$runs)
abline(m1, col = "dodgerblue")      # plotting the model
segments(
  mlb11$at_bats,         # X1 values
  m1$fitted.values,      # predicted Y values (the model fit)
  mlb11$at_bats,         # X2 values
  mlb11$runs,            # real Y values (the actual data)
  col = 'dodgerblue',
  lty = 2
)


par(mfrow = c(1,2))
plot(m1 $residuals ~ m1$fitted.values, col = 'dodgerblue')  # plot of residuals vs. fitted
abline(h = 0, lty = 3)  # add a horizontal dashed line at y = 0
hist(m1$residuals, col = 'white', border = 'dodgerblue')  # histogram of residuals

par(mfrow = c(1,1))
plot(m1, which = 1, col = 'dodgerblue')  # a residual plot using built-in R functionality

par(mfrow = c(1,1))
plot(m1, which = 2, col = 'dodgerblue')  # the q-q plot

#### The lab

#Make a txt file

m2 <- lm(strikeouts ~ at_bats, data = mlb11)

capture.output(summary(m2), file = "text")

#make 4x

par(mfrow=c(2,2))


# Scatter plot 1


plot(mlb11$at_bats, mlb11$strikeouts) 
abline(m2, col = "blue")      # plotting the mode

# QQ Plot

plot(m2$residuals ~ m2$fitted.values, col = 'blue2')   # plot of residuals vs. fitted


plot(m2, which = 2, col = 'blueviolet')  # the q-q plot

# Scatter plot 2 + 3 


hist(m2$residuals, col = 'blue3', border = 'blue4')  # histogram of residuals




# Wrap em


