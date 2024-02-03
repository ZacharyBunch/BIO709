library("statmod")
library(tidyverse)
wells <- read.delim("wells.txt", sep = " ") # reading in a space-delimited data file
head(wells)
hist(wells$switch)
table(wells$switch)

par()
plot(wells, col = "seagreen")

# Set up the model using the glm() function
m1 <- glm(switch ~ arsenic, wells, family = binomial(link = "logit"))
summary(m1) # Print summary and note similarity with output of e.g. lm()...

# Set up the model using the standard lm() function
m0 <- glm(switch ~ arsenic, data = wells, family = gaussian(link = "identity"))
# the above is identical to calling lm(switch ~ arsenic, data = wells)
summary(m0)

par(mfrow = c(2,2))

# plotting residuals versus fitted values to examine violation of linear assumptions
plot(predict(m0, type = 'response'), qresid(m0), col = 'seagreen')
abline(a = 0, b = 0, lty = 2, lwd = 2)
plot(predict(m1, type = 'response'), qresid(m1), col = 'red')
abline(a = 0, b = 0, lty = 2, lwd = 2)

# Q-Q plots to assess specification of wrong error distribution
qqnorm(qresid(m0), col = 'seagreen'); qqline(qresid(m0), lwd = 2)
 qqnorm(qresid(m1), col = 'red'); qqline(qresid(m1), lwd = 2)


# First, make a new vector of fake X values (here, arsenic) over which to apply the model. Usually, you will want to create a vector that spans at least the entire range of the empirical data.

fake.arsenic <- seq(min(wells$arsenic), max(wells$arsenic), by = 0.1) # New X, spanning from min to max of empirical data
fake.switches <- predict(m1, list(arsenic = fake.arsenic), type = 'link') # Predicted Y
plot(wells$arsenic, wells$switch, xlim = c(0,10))
points(fake.arsenic, fake.switches, col = 'red', type = 'l')

fake.switches <- predict(m1, list(arsenic = fake.arsenic), type = 'response')
plot(wells$arsenic, wells$switch, xlim = c(0,10))
points(fake.arsenic, fake.switches, col = 'red', type = 'l')
par()

gala <- read.delim("gala.txt", sep = ' ')
head(gala)
plot(gala, col = "seagreen3")
hist(gala$Species, breaks = 15, col = "seagreen3") # Possibly Poisson-distributed, but likely **overdispersed** (note a few very large values)

# Set up the model using the glm() function
m2 <- glm(Species ~ Area * Elevation, data = gala, family = quasipoisson(link = "log"))
summary(m2)

par(mfrow = c(1,2))

# plotting residuals versus fitted values to examine violation of linear assumptions
plot(predict(m2, type = 'response'), qresid(m2), col = 'seagreen', main = "Residuals vs. Fitted")
abline(a = 0, b = 0, lty = 2, lwd = 2)

# Q-Q plot to assess specification of wrong error distribution
qqnorm(qresid(m2), col = 'seagreen'); qqline(qresid(m2), lwd = 2)

par()

#### Lab 3 ####

#1

m_gala <- glm(Species ~ Area + Elevation + Nearest + Adjacent + Area * Elevation +  Nearest * Adjacent, data = gala, family = quasipoisson(link = "log"))
summary(m_gala) 

#2
capture.output(summary(m_gala) , file = "Bunch_m_gala.txt")

#3

fake.Elevation <- seq(min(gala$Elevation), max(gala$Elevation), by = 1)# New X, spanning from min to max of empirical data

Mean_Nearest <- rep(mean(gala$Nearest), 1683)
Mean_Adjacent <- rep(mean(gala$Adjacent), 1683)
Mean_Area <- rep(mean(gala$Area), 1683)

data.new <- data.frame(cbind(fake.Elevation, Mean_Nearest, Mean_Adjacent, Mean_Area))


fake.Y <- predict(m_gala, list(Elevation = fake.Elevation, Nearest = Mean_Nearest, Adjacent = Mean_Adjacent, Area = Mean_Area), type = 'response') # Predicted Y

# Create the 2-panel plot
#Clear old graphs - causing interactions?
dev.off()

par(mfrow = c(1, 2))

#### Graphs ####

## Create the Residuals vs. Fitted Values plot

plot(m_gala, which = 1, col = "blue")

# Panel 2: QQ Plot
plot(m_gala, which = 2, col = "red")

# Set a custom color scheme
plot_color <- rgb(0.2, 0.4, 0.8, alpha = 0.7)  # Adjust RGB values as needed

# Create the bivariate plot using ggplot
ggplot(gala, aes(x = Elevation, y = Species)) +
  geom_point(color = plot_color, shape = 16) +
  geom_line(data = data.frame(Elevation = fake.Elevation, Species = fake.Y), aes(x = Elevation, y = Species), color = 'red') +
  labs(title = "Elevation vs. Species Count", x = "Elevation", y = "Species Count") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))




