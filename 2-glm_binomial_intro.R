# Introduccion a GLM binomial

library(tidyverse) # tareas generales
library(sjPlot)    # visualizar modelos outputs
library(emmeans)   # estimacion de probabilidades estimadas


## Normal data

set.seed(123)
df <- tibble(check=rnorm(10, 70, 5),
             trat=rnorm(10, 30, 5))

## Visualizamos 

df %>% 
  pivot_longer(cols=everything()) %>% 
  ggplot()+
  aes(x=name, y=value)+  
  geom_jitter(width = .01, height = 0.01, alpha=.2) + 
  stat_summary(fun="mean", pch=18, col="red") + 
  stat_summary(fun="mean", geom="text", aes(label = after_stat(y)))

## Pivoteamos datos de wide a long:

df %>% 
  pivot_longer(cols=everything(), 
               names_to = "tratamiento", 
               values_to= "status") -> long_df 

## Ajustamos un ML
mod_lin <- lm(status ~ tratamiento, data=long_df)
summary(mod_lin)
70.373 + (-39.33)

## LM es una de las posibilidades de los GLMs
mod_gauss <- glm(status ~ tratamiento,
                 family=gaussian(link = "identity"), 
                 data=long_df)
summary(mod_gauss)

# GLM 

## Recreamos el ejemplo de la presentacion

dat <- tribble(
  ~check, ~fungicida,
       1,          1,
       1,          1,
       1,          1,
       1,          1,
       1,          0,
       1,          0,
       1,          0,
       0,          0,
       0,          0,
       0,          0
)

## Visualizamos las medias

dat %>% 
  pivot_longer(cols=everything()) %>% 
  ggplot()+
  aes(x=name, y=value)+  
  geom_jitter(width = .01, height = 0.01, alpha=.2) + 
  stat_summary(fun="mean", pch=18, col="red") + 
  stat_summary(fun="mean", geom="text", aes(label = after_stat(y))) 

## Pivoteamos datos de wide a long para modelar

dat %>% 
  pivot_longer(cols=everything(), 
               names_to = "tratamiento", 
               values_to= "status") %>% 
  arrange(tratamiento)-> dat_long 

dat_long 

## Ajustamos el modelo con family binomial

mod_bin1 <- glm(status ~ tratamiento, 
               family=binomial(link = "logit"), 
               data=dat_long)

summary(mod_bin1)
parameters::parameters(mod_bin1)

tab_model(mod_bin1)
# exp(mod_bin1$coef)

emmeans(mod_bin1, ~ tratamiento, type="response")
# predict(mod_bin1, newdata=data.frame(tratamiento="check"), type="response")
# predict(mod_bin,data.frame(tratamiento="fungicida"),type="response")

# GLM Opcion 2 - n_total y n_exitos

dat_sum <- dat_long %>% 
  group_by(tratamiento) %>% 
  summarise(
    n = n(),                # Total count
    enfermas = sum(status)  # Success count (sum of 1's)
  )
dat_sum

# cbind(dat_sum$enfermas, dat_sum$n-dat_sum$enfermas)

mod_bin2 <- glm( 
  cbind(enfermas, n-enfermas) ~ tratamiento, 
  family=binomial(link = "logit"), 
  data=dat_sum)

summary(mod_bin2)
emmeans(mod_bin2, ~ tratamiento, type="response")

# GLM Opcion 3 - proporciones

dat_prop <- dat_sum %>% 
  group_by(tratamiento) %>% 
  mutate(prop_enfermas = enfermas/n)

dat_prop

mod_bin3 <- glm( 
  prop_enfermas ~ tratamiento, 
  weights = n,
  family=binomial(link = "logit"), 
  data=dat_prop)

summary(mod_bin3)
emmeans(mod_bin3, ~ tratamiento, type="response")
emmeans(mod_bin2, ~ tratamiento, type="response")
emmeans(mod_bin1, ~ tratamiento, type="response")
