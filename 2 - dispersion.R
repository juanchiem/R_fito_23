pacman::p_load(tidyverse,  # manipulacion general de datos
               lme4,       # ajuste de modelos mixtos
               DHARMa, hnp,    # diagnosticos de GLM
# https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html#recognizing-overunderdispersion
               performance,# diagnosticos de LM 
               sjPlot,     # visualizacion de outputs
               emmeans,    # medias predichas por los modelos y comparaciones
               multcomp)   # letras de test de comparaciones multiples

# Over/underdispersion
# 
# La varianza residual es mayor/menor de lo esperado con el modelo ajustado
# Más común para familias GLM con dispersión constante (fija), en particular para modelos de Poisson y binomiales, pero también puede ocurrir en familias GLM que ajustan la varianza (como la beta o binomial negativa) cuando se violan los supuestos de distribución.
# 
# Algunas reglas generales sobre el manejo de problemas de dispersión:
# - La dispersión es una propiedad de los residuos, es decir, puede detectar problemas de dispersión solo DESPUÉS de ajustar el modelo. No tiene sentido mirar la dispersión de su variable de respuesta!
# - La sobredispersión es más común que la subdispersión
# - Si hay sobredispersión, el efecto principal es que los intervalos de confianza tienden a ser demasiado estrechos y los valores de p demasiado pequeños, lo que lleva a un error de tipo I inflado. 
# - Lo contrario es cierto para la sub-dispersión, es decir, es que pierde potencia.
# - Una razón común para la sobredispersión es un modelo mal especificado. Cuando se detecta sobredispersión, primero se deben buscar problemas en la especificación del modelo (por ejemplo, graficando residuales contra predictores con DHARMa), y solo si esto no resuelve, las correcciones de sobredispersión tales como efectos aleatorios a nivel individual o cambios en la distribución se deben aplicar. 

# daphnia <- rio::import("http://www.bio.ic.ac.uk/research/mjcraw/therbook/data/Daphnia.txt")

# Veamos este ejemplo extraido de 

# Proportion Data - Libro Crawley: http://cmq.esalq.usp.br/BIE5781/lib/exe/fetch.php?media=leituras:crawley_cap16.pdf

# This next example concerns the germination of seeds of two genotypes of the parasitic
# plant Orobanche and two extracts from host plants (bean and cucumber) that were used to
# stimulate germination. It is a two-way factorial analysis of deviance.

germi <- rio::import("http://www.bio.ic.ac.uk/research/mjcraw/therbook/data/germination.txt")
germi

model<-glm(cbind(count, sample-count) ~ Orobanche * extract, binomial, germi)
summary(model)
1-pchisq(model$deviance,model$df.residual)
# el ajuste obtenido no es bueno, dado que es inferior a 0.05, no significativo.
model$deviance/model$df.residual
# 33.278/17
# The overdispersion factor is almost 2. The simplest way to take this into account is to
# use what is called an ‘empirical scale parameter’ to reflect the fact that the errors are not
# binomial as we assumed, but were larger than this (overdispersed) by a factor of 1.9576.

# Diagonstico con DHARma
testDispersion(model, type = "PearsonChisq")
# p-value = 0.03324, se rechaza H0 de no dispersion

# Diagonstico con hnp
plot(hnp(model, print.on=TRUE)) 
# muchos puntos fuera del intervalo, mal ajuste.

# We refit the model using quasibinomial to account for the overdispersion:
# The quasi-binomial isn't necessarily a particular distribution; it describes a model for the relationship between variance and mean in generalized linear models which is ϕ times the variance for a binomial in terms of the mean for a binomial.
# There is a distribution that fits such a specification (the obvious one - a scaled binomial), but that's not necessarily the aim when a quasi-binomial model is fitted; if you're fitting to data that's still 0-1 it can't be scaled binomial.
# So the quasi-binomial variance model, via the ϕ parameter, can better deal with data for which the variance is larger (or, perhaps, smaller) than you'd get with binomial data, while not necessarily being an actual distribution at all.
# https://stats.stackexchange.com/questions/91724/what-is-quasi-binomial-distribution-in-the-context-of-glm

model2 <- glm(cbind(count, sample-count) ~ Orobanche * extract, 
            family=quasibinomial, data=germi)
anova(model2,test="F")


# vemos que podemos prescindir de la interaccion en model2, ya que P>0.05  
model3 <- update(model2, ~ . - Orobanche:extract)

# comparamos ambos modelos 
anova(model2,model3,test="F")
# vemos que no difieren P=0.08099, nos quedamos con el menos predictoras
anova(model3,test="F")
plot(hnp(model3, print.on=TRUE)) 

# de nuevo vemos que Orobanche esta demas en el modelo
model4 <- update(model3, ~ . - Orobanche)
anova(model4,model3,test="F")
# de nuevo se confirma que el modelo mas parsimonioso es model4
plot(hnp(model4, print.on=TRUE)) 

# Avanzamos hacia la estimacion de media de los tratamientos y sus comparaciones 
em4 <- emmeans(model4, ~ extract, type="response")
res4 <- cld(em4, Letters = letters, alpha = .05, type = "response")
res4
