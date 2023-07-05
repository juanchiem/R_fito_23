# Install and load necessary packages
install.packages("MASS")
library(MASS)

# Load the Wine Quality dataset
winequality <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv", sep = ";")

head(winequality)
names(winequality)

winequality$quality = factor(winequality$quality)

# Fit a proportional odds model
model <- polr(quality ~ fixed.acidity, data = winequality)

# View the summary of the model
summary(model)
predict(model, newdata = data.frame(fixed.acidity=8),type="p")

# https://data.library.virginia.edu/fitting-and-interpreting-a-proportional-odds-model/