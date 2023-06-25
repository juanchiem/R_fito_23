library(brms)
library(nnet)
library(emmeans)
library(tidyverse)

# read in data
raw <- foreign::read.dta("https://stats.idre.ucla.edu/stat/data/hsbdemo.dta")
# The data set contains variables on 200 students. 
# The outcome variable is prog, program type, a three-level 
# categorical variable (general, academic, vocation). 
# The predictor variable is social economic status, ses, a three-level categorical variable. 
# Now to conduct the analysis via the nnet package nnet

# first relevel so 'academic' is the reference level

dat <- raw %>% 
  # mutate_at(vars(prog), as.factor) %>% 
  mutate(prog = fct_relevel(factor(prog), "academic"))

# str(dat$prog)

# run test in nnet
test_nnet <- multinom(prog ~ ses,  data = dat)

# Now to create an emmeans object that will allow us to conduct pariwise tests

# pass into emmeans
rg_nnet <- ref_grid(test_nnet)
emm <- emmeans(rg_nnet, specs = ~prog|ses, mode = "prob")
contrast(emm, "pairwise", adjust = "mvt")

