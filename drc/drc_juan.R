# carrega pacotes
pacman::p_load(drc, readxl, tidyverse, magrittr, ggplot2)

# carrega os dados
tm <- read_excel("~/Dropbox/EPI-Tools/EC50/tm.xlsx")

# cria um vetor de 1 a 17 para cada isolado
tm <- tm %>% mutate(isolado  = rep(1:3, each=48))
head(tm)

# Explore data
xtabs(~ isolado + rep, data = tm)
ggplot(tm, aes(dos, tm)) + geom_point() + geom_smooth(span = 0.5) + 
  facet_wrap(~isolado)

# ajusta modelo com 3 parametros para cada isolado
mod_iso <- drm(tm ~ dos,  curveid = isolado, data = tm, fct = LL.3())
summary(mod_iso)
plot(mod_iso)

# média geral
mod_overall <- drm(tm ~ dos,  data = tm, fct = LL.3())
summary(mod_overall)

AIC(mod_iso, mod_overall)

# Output 
ED(mod_iso, c(50), interval = "delta")
ec50s = data.frame(ED(mod_iso, c(50), interval = "delta"))
ec50s$iso = factor(1:3)

ggplot(ec50s, aes(x = iso, y = Estimate)) +  
  geom_pointrange(aes(ymin = Lower, ymax = Upper)) +
  coord_flip() + labs(x= "Isolado", y="CE50")

# Multilpe comparisons 
EDcomp(mod_iso, c(50,50,50)) # First 3 rows

# gera o gráfico para cada isolado mudando em level
plot(mod_iso, broken = F, level = c(1), type = "all",
     xlab = "TM (ppm)", ylab = "S. sclerotiroum growth (mm)")
