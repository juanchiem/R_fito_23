# https://argoshare.is.ed.ac.uk/healthyr_book/binary-logistic-regression.html
library(tidyverse)

# create data
raw <- tribble(
  ~trt,     ~disease, ~no_disease,
  "check_mz",       50,          50,
  "girasol",        30,          70,
  "cobertura",      70,          30
) 

df <- raw %>% 
  mutate(total = no_disease + disease,
         inc = disease / total) 
df

dm <- as.matrix(raw %>% column_to_rownames("trt"))
dm

# install.packages('epitools')
library(epitools)
# calculate odds ratio
oddsratio(dm, rev="c")
# res

# install.packages('epiDisplay')
library(epiDisplay)

mod <- glm(inc ~ trt,  
           weights = total,
           family = binomial("logit"),  
           data = df)

logistic.display(mod)
# install.packages('OddsPlotty')

library(OddsPlotty)
odds_plot(mod)
