#### Read Libraries ####
library(lme4)
library(ggplot2)
library(lattice)

#### CSV LOAD ####
embs <- read.csv("all-species-v6-reducedForGLMMs.csv")
head(embs)
table(embs$binomial)

aggregate(embryocount ~ binomial, embs, FUN = mean)

ggplot(
  data = embs, 
  mapping = aes(x = sqrt(embryocount)) # this tells ggplot what the x and y vars are
) + 
  geom_histogram(binwidth = .25) # anything added on here describes the *type* of plot, here a histogram

par(mfrow = c(2,2))
hist(embs$CONT.10yrmean, col = "firebrick", main = "Continentality") # a temperature var
hist(embs$RH.10yrmean, col = "navy", main = "Relative Humidity") # a precip var
hist(embs$EVAP.10yrmean, col = "firebrick", main = "Evapotranspiration Index") # a temperature var
hist(embs$MAP.10yrmean, col = "navy", main = "Mean Annual Precipitation") # a precip var

hist(sqrt(embs$EVAP.10yrmean), col = "firebrick", main = "Evapotranspiration Index (transformed)")
hist(log10(embs$MAP.10yrmean), col = "navy", main = "Mean Annual Precipitation (transformed)")

m0 <- lm(
  sqrt(embryocount) ~
    (CONT.10yrmean + 
       RH.10yrmean +
       log10(MAP.10yrmean) + 
       sqrt(EVAP.10yrmean))^2 + # superscript notation to model all 2-way interactions
    log10(headbodylength.speciesAVG),
  data = embs
)
summary(m0)

ggplot(
  data = embs, 
  mapping = aes(x = embryocount) # raw data, not square-root transformed
) + 
  geom_histogram() + 
  facet_wrap(~ binomial, scales = "free_y", nrow = 8)

ggplot(
  data = embs, 
  mapping = aes(x = CONT.10yrmean, y = sqrt(embryocount))
) + 
  geom_point() + 
  geom_smooth(method = "lm") # compute and plot relationship

ggplot(
  data = embs, 
  mapping = aes(x = CONT.10yrmean, y = sqrt(embryocount))) + 
  geom_point() +
  geom_smooth(method = "lm") + 
  facet_wrap(~ binomial, nrow = 8, ncol = 5) # facetting by species identity

#### This week's lab ####

# 1 #

m2 <- lmer(sqrt(embryocount) ~
        (CONT.10yrmean + 
           RH.10yrmean +
           log10(MAP.10yrmean) + 
           sqrt(EVAP.10yrmean))^2 + 
        log10(headbodylength.speciesAVG) + (1 | binomial) + (0 + CONT.10yrmean | binomial) + (0 + log10(MAP.10yrmean) | binomial)   + (0 + sqrt(EVAP.10yrmean) | binomial) + (0 + RH.10yrmean| binomial),
      data = embs)

# control = lmerControl(optimizer = "bobyqa")

summary(m2)

capture.output(m2, file = "Bunch_Lab_5_Model1.txt")

# 2 #

m0.ranef <- ranef(m2)
m0.ranef

dotplot(m0.ranef) 

# 3 #
m3 <- lmer(sqrt(embryocount) ~
             (CONT.10yrmean + 
                RH.10yrmean +
                log10(MAP.10yrmean) + 
                sqrt(EVAP.10yrmean))^2 + 
             log10(headbodylength.speciesAVG) + (1 | binomial) + (0 + CONT.10yrmean | binomial) + (0 + log10(MAP.10yrmean) | binomial)   + (0 + sqrt(EVAP.10yrmean) | binomial) + (0 + RH.10yrmean| binomial) + (0 + headbodylength | binomial),
           data = embs, 
           control = lmerControl(optimizer = "bobyqa"))


summary(m3)

# 4 #
# Extract species-specific random effects
random_effects <- ranef(m3)$binomial

# Extract species names
species_names <- rownames(random_effects)

# Convert random effects to data frame
random_effects_df <- data.frame(species = species_names, intercept = random_effects[, 1], slope = random_effects$headbodylength)


# Create the plot
ggplot(embs, aes(x = headbodylength, y = embryocount)) +
  geom_point() +  # Scatter plot
  geom_abline(data = random_effects_df, aes(intercept = intercept, slope = slope, color = species)) +  # Species-specific lines
  scale_color_manual(values = rainbow(length(species_names))) +  # Species-specific colors
  labs(x = "Individual body size (headbodylength)", y = "Litter size (embryocount)") +  # Axis labels
  theme_minimal()  # Minimal theme


