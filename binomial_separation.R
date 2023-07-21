# https://doi.org/10.1016/j.rmal.2023.100044

# Complete separation
#https://stats.stackexchange.com/questions/11109/how-to-deal-with-perfect-separation-in-logistic-regression

# Quasi-complete separate_wider_position
# https://stats.stackexchange.com/questions/38493/how-to-deal-with-quasi-complete-separation-in-a-logistic-glmm

library(tidyverse)
library(sjPlot)
library(emmeans)

df <- mtcars %>% 
    mutate(bin = as.numeric(mtcars$mpg > 20), 
           cyl_factor = factor(cyl)) # create a binary variable

with(df, table(bin, cyl))

# Regular logsitic regression

fit1 <- glm(bin ~ cyl_factor, data=df, family="binomial")
summary(fit1)
emmeans(fit1, ~ cyl_factor, type="response")
#noten IC en cyl=4 y cyl=8
plot_model(fit1)

# Bayesian logsitic regression
library(arm)

fit2 <- bayesglm(bin ~ cyl_factor, data=df, family="binomial")
summary(fit2)
emmeans(fit2, ~ cyl_factor, type="response")
plot_model(fit2)

# logistic regression with Firth's correction
library(logistf)

fit3 <- logistf(bin ~ cyl_factor, data=df)
summary(fit3)
emmeans(fit3, ~ cyl_factor, type="response")
plot_model(fit3)

