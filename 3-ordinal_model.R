library(tidyverse)
library(ordinal)
library(emmeans)

# read_csv("https://github.com/juanchiem/agro_data/raw/master/SummerInoculationFinal.csv") %>%
#   filter(Year==2019) %>% #str
#   select(score_factor, Treatment, bloc, Plot, pruned, apogee, apogeeBino) %>%
#   rio::export("data/ordinal_vincent.csv")

dat <- rio::import("data/ordinal_vincent.csv") %>% 
  janitor::clean_names() %>%   
  # mutate(score_factor = score_factor+1) %>% 
  mutate_at(vars(score_factor, apogee, pruned, bloc), as.factor)  %>% 
  mutate(apogee = fct_relevel(apogee, "None")) 
  
str(dat)

ftable(xtabs(~ bloc + pruned + apogee, dat))

dat %>% 
  count(pruned, apogee, bloc, score_factor) %>% 
  arrange(score_factor) %>% 
  pivot_wider(
    names_from =  score_factor,
    values_from = n) %>% 
  mutate(n_sample = rowSums(select(., `0`: `4`), na.rm=TRUE)) %>%
  print(n=Inf)  

# sample_n(dat, 30)

dat %>% 
  ggplot()+
  aes(x = pruned, fill = score_factor)+
  geom_bar(position = position_fill(reverse = TRUE))+
  facet_grid(cols=vars(apogee))+
  xlab("") + ylab("Proportion of shoots")+
  scale_fill_grey(name = 'Score',start =.9, end = 0 )+
  scale_x_discrete(labels = c("pruned", "Control"))


# Full Model

mod_full <- clmm(score_factor ~  pruned * apogee + (1|bloc), data=dat)
# mod_full <- clmm(score_factor ~  pruned + apogee + pruned : apogee (1|bloc), data=dat)
joint_tests(mod_full)

# Removemos interaccion
mod_adit <- clmm(score_factor ~  pruned + apogee + (1|bloc),  data=dat) 
joint_tests(mod_adit)
anova(mod_full, mod_adit)

emmeans(mod_adit, list(pairwise ~ apogee,
                       pairwise ~ pruned))


# Modelo con apogee binaria

mod_apo_bin <- clmm(score_factor ~  pruned + apogee_bino + (1|bloc), 
                          data=dat) 

anova(mod_apo_bin, mod_adit)

joint_tests(mod_apo_bin)

emmeans(mod_apo_bin, list(pairwise ~ apogee_bino, 
                          pairwise ~ pruned))

dat %>% 
  ggplot()+
  aes(x = pruned, fill = score_factor)+
  geom_bar(position = position_fill(reverse = TRUE))+
  facet_wrap("apogee_bino", labeller = label_both)+
  xlab("") + ylab("Proportion of shoots")+
  scale_fill_grey(name = 'Score',start =.9, end = 0 )+
  scale_x_discrete(labels = c("pruned", "Control"))
