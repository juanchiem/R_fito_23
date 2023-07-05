pacman::p_load(epiR, irr, correlation, lme4, purrr, broom, car, emmeans, multcomp)

sbr <- tibble::tribble(
  ~leaf, ~actual, ~R1, ~R2,  ~R3, ~R4,
  1L,    0.25, 0.6, 0.6,  0.7, 0.6,
  2L,     2.5,   2, 0.7,    5,   1,
  3L,    7.24,   5,   5,    8,   5,
  4L,    7.31,   2,   4,    6,   2,
  5L,    9.07,   6,  14,   10,   7,
  6L,    11.6,   5,   6,   10,   5,
  7L,   12.46,  10,  18, 12.5,  12,
  8L,    13.1,  15,  30,   22,  10,
  9L,   14.61,   7,   2,   12,   8,
  10L,   16.06,   6,   9, 11.5,   8,
  11L,    16.7,   7,   7,   20,   9,
  12L,    19.5,   6,  23,   22,  14,
  13L,   20.75,  10,  35, 18.5,  20,
  14L,   23.56,  19,  10,    9,  10,
  15L,   23.77,  15,  20,   19,  20,
  16L,   24.45,  17,  30,   18,  13,
  17L,   25.78,  19,  53,   33,  38,
  18L,   26.03,  17, 6.8,   15,   9,
  19L,   26.42,  15,  20,   18,  16,
  20L,   28.89,  18,  22,   24,  15
)
# set the global theme
theme_set(theme_bw())

# Viz
sbr %>% 
  pivot_longer(2:6, 
               names_to = "rater",
               values_to = "estimate") %>% 
  ggplot()+
  aes(x=leaf,y=estimate, 
      group = rater, 
      col=rater)+
  geom_line()+
  geom_point(size = 2)+
  labs(y = "Severity estimate (%)",
       x = "Leaf number")

# Precision ----
  
sbr_long <- sbr %>% 
  pivot_longer(cols = contains("R"), 
               names_to = "rater",
               values_to = "estimate") 
sbr_long %>%    
  ggplot()+
  aes(x=actual,y=estimate)+
  geom_point(alpha = 0.7)+
  facet_wrap(~rater)+
  ylim(0,45)+
  xlim(0,45)+
  geom_abline(intercept = 0, slope =1)+
  labs(x = "Actual severity (%)",
       y = "Estimate severity (%)") +
  coord_fixed(ratio = 1 / 1)

precision <- sbr_long %>% 
  select(-leaf) %>% 
  group_by(rater) %>% 
  correlation() 

precision

precision %>%
  summarise(mean(r), sd(r))

# Exactitud  ----

sbr_long %>% 
  ggplot(aes(actual, estimate-actual))+
  geom_point(alpha = 0.7)+
  facet_wrap(~rater)+
  geom_hline(yintercept = 0)+
  labs(x = "Actual severity (%)",
       y = "Error (Estimate - Actual)")

library(epiR)

sbr2 <- sbr_long %>% 
  filter(rater == "R2")

ccc2 <- epi.ccc(sbr2$actual, sbr2$estimate)
ccc2

# Concordance coefficient
rho <- ccc$rho.c[,1]
rho

# Bias coefficient
Cb <- ccc$C.b
Cb
0.58 * Cb

# Scale-shift
ss <- ccc$s.shift
ss

# Location-shift
ls <- ccc$l.shift
ls

# Confiabilidad ----  
# Reliability 

# Solo evaluadores

raters <- sbr %>% 
  select(3:6)
raters

library(irr)

icc(raters, "twoway")
# twoway: both raters and leaves are viewed as random effects

library(epiR)
epi.occc(raters, na.rm = FALSE, pairs = TRUE)
