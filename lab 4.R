library(vegan)
library(ggfortify)

birdhab <- read.csv("birdhab.csv", header = TRUE)

head(birdhab)

rip.bird <- subset(birdhab, select = AMGO:WWPE)
head(rip.bird) # explore the data structure visually

rip.hab <- subset(birdhab, select = VAREA:CONEDGE)
head(rip.hab) # explore the data structure visually

rip.hab.pca <- prcomp(rip.hab, scale = TRUE) # conduct PCA and set scale=TRUE to use correlation matrix
# Plot how much of the variance is explained by each PCA component
screeplot(rip.hab.pca) # npcs specifies how many axes to consider

# The first two axes are what is typically plotted in a PCA plot. 
head(rip.hab.pca$x)

rip.hab.rda <- rda(rip.hab, scale = TRUE)

ordiplot(rip.hab.rda, choices = c(1, 2), type = 'text', scaling = 2)

counts <- colSums(rip.hab) # compute 
ordiplot(rip.hab.rda, choices = c(1, 2), type = 'none') # blank plot space
orditorp(rip.hab.rda, display = 'sites', col = 'blue', pch = 19)
orditorp(rip.hab.rda, display='species', priority = counts, col = 'red', pch = 19)

p <- ordiplot(rip.hab.rda, choices = c(1,2), type = 'points')
identify(p, 'species') #for variables (species)

## integer(0)

# Repeat, but this time for the sites:
p <- ordiplot(rip.hab.rda, choices = c(1,2), type = 'points')
identify(p, 'sites') #for samples (sites)

cols = c(sample(colours(), 10)) # create a vector of 10 random colors

p <- ordiplot(rip.hab.rda, choices = c(1, 2), display = 'sites')
ordispider(p, groups = as.factor(birdhab$SUB), col = cols)

dissim <- vegdist(rip.hab, method = "euclidian")
head(dissim)

y.anosim <- anosim(dissim, birdhab$SUB) # argument 2 is the group IDs
summary(y.anosim)

y.adonis <- adonis2(rip.hab ~ SUB, data = birdhab, permutations = 1000, method = 'euclidian')
y.adonis

rip.bird.nmds <- metaMDS(rip.bird, distance = 'bray', k = 3, trymax = 50, autotransform = FALSE)
rip.bird.nmds

head(rip.bird.nmds$points)

head(rip.bird.nmds$species)

cols = c(sample(colours(), 10)) # create a vector of 10 random colors

p <- ordiplot(rip.bird.nmds, choices = c(1, 2), display = 'sites')
ordispider(p, groups = as.factor(birdhab$SUB), col = cols) 

p <- ordiplot(rip.bird.nmds, choices = c(1, 2), display = 'sites')
ordiellipse(p, groups = as.factor(birdhab$SUB), conf = 0.95, col = cols)

# The ordihull() function is useful for producing familiar convex hulls.
p <- ordiplot(rip.bird.nmds, choices = c(1, 2), display = 'sites')
ordihull(p, groups = as.factor(birdhab$SUB), col = cols)

dissim <- vegdist(rip.bird, method = "bray") # get the dissimilarity matrix
y.anosim <- anosim(dissim, birdhab$SUB) # ANOSIM
summary(y.anosim)

dissim <- vegdist(rip.bird, method = "bray") # get the same dissimilarity matrix
y.adonis <- adonis2(rip.bird ~ SUB, data = birdhab, permutations = 1000, method = 'bray') # PERMANOVA
y.adonis

#### Lab 4 ####

turtle <- read.csv("byturtle.csv", header=TRUE)
hist(turtle$turtle) # Visualize the distribution of presences (1) and absences (0)

# Subset the data to include only environmental predictor variables
env_data <- turtle[, 4:28]

# Perform Principal Component Analysis (PCA)
pca_result <- prcomp(env_data, scale. = TRUE)



# Perform Redundancy Analysis (RDA)
rda_result <- rda(env_data)

### 1 ####

# p <- ordiplot(rda_result, choices = c(1, 2), display = 'sites')
# ordiellipse(p, groups = as.factor(turtle$turtle), conf = 0.9, col = cols) # polygons will be 90% confidence interva

p <- ordiplot(rda_result, choices = c(1, 2), display = 'sites')
ordispider(p, groups = as.factor(turtle$turtle), conf = 0.9, col = cols) # polygons will be 90% confidence interva

#### 2 ####

# Plot the ordination using ordiplot
# p_2 <- scores(rda_result, choices = c(1, 2), display = "sites")
# 
# # Add points for each sample
# points(p_2[turtle$turtle == 1, ], col = "red", pch = 19)
# points(p_2[turtle$turtle == 0, ], col = "blue", pch = 19)
# 
# # Add identifiers around the two groups using ordiellipse()
# cols <- c("red", "blue")  # Custom color scheme
# ordiellipse(p_2[turtle$turtle == 1, ], conf = 0.95, draw = "polygon", col = adjustcolor(cols[1], 0.3), lty = 2, groups = turtle$turtle[turtle$turtle == 1])
# ordiellipse(p_2[turtle$turtle == 0, ], conf = 0.95, draw = "polygon", col = adjustcolor(cols[2], 0.3), lty = 2, groups = turtle$turtle[turtle$turtle == 0])

# Compute the PCoA scores
p_2 <- scores(rda_result, choices = c(1, 2), display = "sites")

# Add points for each sample
points(p_2[turtle$turtle == 1, ], col = "red", pch = 19)
points(p_2[turtle$turtle == 0, ], col = "blue", pch = 19)

# Add identifiers around the two groups using ordispider()
cols <- c("red", "blue")  # Custom color scheme
ordispider(p_2[turtle$turtle == 1, ], groups = turtle$turtle[turtle$turtle == 1], col = adjustcolor(cols[1], 0.3))
ordispider(p_2[turtle$turtle == 0, ], groups = turtle$turtle[turtle$turtle == 0], col = adjustcolor(cols[2], 0.3))

# Add legend
legend('topright', c('Wood Turtle', 'No Turtle'), col = cols, pch = 19, cex = 0.5)


# Add axis labels
title <- "PCA Ordination of Wood Turtle Habitat Data"
title(main = title)

### 3 ###

# Plot the scree plot to visualize the variance explained by each principal component
screeplot(pca_result, type = "bar", main = "Scree Plot: Variation Explained by Principal Components", npcs = 5)


### 4 ###

# Calculate dissimilarity matrix using Bray-Curtis dissimilarity
dissimilarity <- vegdist(env_data, method = "euclidian")

# Conduct ANOSIM
anosim_result <- anosim(dissimilarity, turtle$turtle)
anosim_result

# Capture ANOSIM summary
capture.output(summary(anosim_result), file = "anosim.txt")

# Conduct PERMANOVA
permanova_result <- adonis2(env_data ~ turtle$turtle, permutations = 999, method = "euclidian")
permanova_result

# Capture PERMANOVA summary
capture.output(permanova_result, file = "permaova.txt")


