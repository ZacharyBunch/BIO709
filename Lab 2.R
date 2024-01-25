load("evals.rdata")
head(evals)

dim(evals)

eval.vars <- read.delim("eval-variables.txt")
eval.vars


par(mar = c(2, 2, 2, 2)) # Set the margin on all sides to 2
hist(evals$score)

summary(evals$score) # provides values of all four *quartiles*, plus the min and max

boxplot(score ~ gender, data = evals) # NOTE: the call to boxplot() can take the form of a formula, as in lm()

boxplot(score ~ rank, data = evals)

boxplot(score ~ gender + rank, evals)

# Use the subset() argument within boxplot()
boxplot(score ~ gender + rank, data = evals,
        subset = 
          gender == c('female','male'), 
        col = c('red','blue')
)

m1 <- aov(score ~ gender * rank, data = evals) 
# NOTE: this formula is simpler than, but identical to, this: (gender + rank + (gender * rank))
summary(m1)

model.tables(m1)

m2 <- lm(score ~ age + rank + gender + bty_avg, data = evals)
summary(m2)

par(mfrow = c(2,2))
plot(m2, col = "tomato")


par(mfrow = c(1,3))
hist(evals$score, ylim = c(0,90)) # Use a ylim range to make sure all plots are on identical axes.
hist(evals$age, ylim = c(0,90))
hist(evals$bty_avg, ylim = c(0,90))


# Laboratory 

par(mfrow = c(1,3), col.lab="red") # No sufficient simple transformations
hist(evals$score, col = "blue", main = "No Transformation Evaluation Score")
hist(evals$age, col = "green", main = "No transformation Age Score")
hist(sqrt(evals$bty_avg), col = "orange", main = "Square Root No Bty Avg Score")

m2_new <- lm(score ~ age + rank + gender + bty_avg + age * rank + age * gender + gender * rank, data = evals)
summary(m2_new)


capture.output(summary(m2_new), file = "Bunch_Lab2_Model.txt")

# Step 4

par(mfrow = c(2,2), col.lab="red",mar = c(2, 2, 2, 2))

plot(m2_new, which = 1, col = "purple")
plot(m2_new, which = 2, col ="blue")
plot(m2_new, which = 5, col ="green")
plot(m2_new, which = 3, col ="orange")


