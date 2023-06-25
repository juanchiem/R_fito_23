# Session setup ####

pacman::p_load(tidyverse, drc, ec50estimator, ggridges)

dat <- ec50estimator::multi_isolate 
dat

# Dataset containing simulated data of mycelial growth under 
# increasing fungicide doses 
# for: 
# two types of field (conventional and organic), 
# two different fungicides
# 25 fungal isolates
# 5 rep
# at 6 doses 

# importante chequear el data type ya que ajustaremos regresiones, 
# por lo tanto necesitamos 
# x e y,  dosis y diametro de colonia respectivamente, como numericos, 
# mientras que los activos y otras variables predictoras como en este caso

dat %>% str

# Ahora chequeamos la estrutura del dataset, ubicando el nivel jerarquico o de 
# anidamiento de las variables
ftable(xtabs(complete.cases(growth)~ field + fungicida + dose , data=dat))
# vemos que tenemos 125 datos de diametro por combinacion 

ftable(xtabs(complete.cases(growth)~ field + fungicida + isolate, data=dat))

# Entendemos que cada aislamiento fue muestreado de uno de los sistemas de produccion y fue utilizado para los dos fungicidas
dat %>% 
  filter(isolate==1) %>% 
  mutate(dose_decimal = format(dose, scientific = FALSE))

# Caso unico #####
iso1_org_A <- dat %>% 
  filter(field=="Organic", fungicida=="Fungicide A", isolate==1)%>% 
  mutate(dose_decimal = format(dose, scientific = FALSE))

# podemos apreciar las 5 reps
iso1_org_A %>% filter(dose==0)

# visualizamos en escala natural 

iso1_org_A %>% 
  ggplot()+
  aes(dose, growth)+
  geom_point(alpha=.22)+
  theme_bw()
  
# visualizamos en escala logaritmica
iso1_org_A %>% 
  ggplot()+
  aes(dose, growth)+
  geom_jitter(width = 0.1, alpha=.22) +
  scale_x_log10()
  
# ajuste de modelo con drc para un dataset individual
model.LL3 <- drm(growth ~ dose, 
                 fct = LL.3(names = c("Slope", "Upper Limit", "ED50")),
                 # fct = LL.3(),
                 data = iso1_org_A 
                 )
# getMeanFunctions()
plot(model.LL3)

model.LL4 <- drm(growth ~ dose, 
                 data = iso1_org_A, 
                 fct=LL.4(names = c("Slope", "Lower Limit", "Upper Limit", "ED50")))
plot(model.LL4)

# cual mejor modelo
all_models <- list(LL.2(), LL.3(), LL.3u(),LL.4(),LL.5(),W1.2(), W1.3(),W1.3u(), W1.4(),W2.4(), W2.3(), W2.2(), BC.5(),LL2.2(), LL2.3(), LL2.3u(),LL2.4(),LL2.5(), AR.2(), AR.3(), MM.2(), MM.3())
mselect(model.LL4, fctList = all_models) 

# Symmetric dose-response curves, like log-logistic curves, 
# show symmetry about an inflection point (i.e., the relative EC50). 
# C, Asymmetric dose-response curves, like Weibull curves, 
# do not show symmetry about an inflection point. 

# avanzamos con LL.3
summary(model.LL3)
ED(model.LL3, 
   respLev = c(50), 
   interval="delta", type = "relative")

ED(model.LL3, 
   respLev = c(90), 
   interval="delta", type = "relative")

# The ED function will calculate its 
# ED values based on the relative distance 
# between the upper and lower asymptotes. 
# This can lead to some confusion. 
# For example, if we have a model starting at 100 with a 
# lower asymptote at 20, our calculated ED50 value 
# will actually be the midpoint of 20 and 100, an ED60.

#model average
maED(model.LL3, 
     list(W2.4(),LL.4(), W1.4()),
     respLev = c(50), 
     interval="kang")

iso1_org_A <- iso1_org_A %>% 
  mutate(percent_response = growth/(mean(iso1_org_A$growth[iso1_org_A$dose==0]))*100)

#when fixing model parameters, we use "NA" to indicate that it needs to be calculated 
model_fixed <- drm(percent_response ~ dose, 
                   data = iso1_org_A, 
                   fct=LL.4(fixed=c(NA, 0, 100, NA),
                            names = c("Slope", "Lower Limit", "Upper Limit", "ED50")))
model_fixed

# We care about the “IC” (which defaults to the Akaike’s 
# Information Criterion, AIC). 
# The rule of thumb is that the smaller the better and differences of 
# more than 10 indicates “significantly” better model fits

# Dos fungicidas ####

dat_conv <- dat %>% 
  filter(field=="Conventional") %>% 
  filter(isolate<7)

dat_conv %>% janitor::tabyl(fungicida, dose)

mod_fungi <- drm(growth ~ dose,   
                 curveid = fungicida, 
                 fct = LL.3(names = c("Slope", "Upper Limit", "ED50")), 
                 data = dat_conv)
summary(mod_fungi)
plot(mod_fungi)

EDcomp(mod_fungi, c(10,50)) 

# Output 
ec50s <- ED(mod_fungi, c(50), interval = "delta") %>% 
  data.frame() %>% 
  mutate(Fungicida = c("A", "B"))
ec50s


# Comparar aislados
dat_conv_A <- dat %>% 
  filter(field=="Conventional") %>% 
  filter(fungicida=="Fungicida A") %>% 
  filter(isolate<7)

dat_conv %>% janitor::tabyl(fungicida, dose)

mod_fungi_A <- drm(growth ~ dose,   
                 curveid = isolate, 
                 fct = LL.3(names = c("Slope", "Upper Limit", "ED50")), 
                 data = dat_conv)
summary(mod_fungi)
plot(mod_fungi_A)

EDcomp(mod_fungi_A, c(50,50, 50)) 

# Estudio multi-nivel ####

# visualizamos todo el estudio
dat %>% 
  ggplot()+
  aes(dose, growth)+
  geom_jitter(width = 0.1, alpha=.22)+
  facet_grid(field~fungicida)+
  scale_x_log10()

df_ec50 <- estimate_EC50(growth~dose,
                         data =dat,
                         isolate_col = "isolate", 
                         strata_col =  c("field","fungicida"),
                         interval = "delta",
                         fct = drc::LL.3())

# podrian testearse el ajuste de 3 aislmientos de cada sistema - fungicida 

head(df_ec50)

df_ec50 %>% 
  tibble() %>% 
  mutate(ID = as.numeric(ID)) %>% 
  ggplot(aes(ID, Estimate, ymin=Lower , ymax=Upper, color = field))+
  geom_pointrange()+
  facet_wrap(~fungicida, scales = "free_x", ncol = 2)+
  scale_y_log10()+
  labs(x = "Isolates", y = "EC50")+
  theme_minimal()+
  coord_flip()+
  theme(axis.text.x = element_text(size=10),
        legend.position = "bottom")

as.data.frame(df_ec50) %>% 
  ggplot(aes(Estimate, field, fill = stat(x)))+
  geom_density_ridges_gradient(alpha = 0.3)+
  scale_x_log10()+
  scale_fill_viridis_c(option = "C")+
  facet_wrap(~fungicida, nrow = 2)+
  theme_minimal()+
  labs(x = "EC50", y = "Field")+
  theme(legend.position = "none")

# Referencias
# https://alvesks.github.io/ec50estimator/articles/how_to_use.html
# http://www.darrenkoppel.com/2020/09/04/dose-response-modelling-and-model-selection-in-r/
# Noel, Z. A., Wang, J., and Chilvers, M. I. 2018. Significant Influence of EC 50 Estimation by Model Choice and EC 50 Type. Plant Disease. 102:708–714.