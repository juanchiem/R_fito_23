https://doseresponse.github.io/medrc/index.html

## first installing drc and drcData
devtools::install_github("DoseResponse/drcData")
devtools::install_github("DoseResponse/drc")

## then installing the development version of medrc
devtools::install_github("DoseResponse/medrc")
pacman::p_load(tidyverse, googlesheets4, medrc)

url <- "https://docs.google.com/spreadsheets/d/1cyn8WLKgaQWpOkc9YG_YpSEOgGTmKIg8W7GQwsLBnK8/edit?usp=sharing"

dat <- read_sheet(url, sheet="sensib-fungic", 
                  skip=5, 
                  col_types="ccdcd")

head(dat)

with(dat, ftable(xtabs(complete.cases(resp)~ 
               ai + strain + dose, 
               drop.unused.levels = FALSE)))

dat %>% janitor::tabyl(strain, dose, ai)

dat %>% 
ggplot(aes(dose, resp, col = ai)) +
  geom_point() +
  geom_smooth() 

dat %>% 
  ggplot(aes(log(dose), resp, col = ai)) +
  geom_point() +
  geom_smooth() 

### nlme
sm2 <- medrm(resp ~ dose, 
             curveid = b + d + e ~ ai, 
             data=dat, na.action = na.omit,
             fct=LL.3(), 
             random = b + d + e ~ 1|strain,
             start=c(0.5, 1, 1.5, 1.5, 1.5, 0.3))

data.frame(ED(sm2, c(50), interval = "delta"))->ec50
log(ec50)
# class(ec50)
EDcomp(sm2, 
       percVec=50,
       percMat=rbind(c(1, 1)),
       interval="delta")

# El diuron es 8 veces mas potente que el bentazon para inhibir el 50% la actividad 
# O, el bentazon necesita 8.8601 la dosis de diuron... 
0.20855 * 8.86013

# comparing effective dose levels for nlme

cmat <- rbind(c(1, 1), 
              c(2, 2), 
              c(3, 3))
EDcomp(sm2, 
       # percVec=c(50), 
       percVec=c(15, 50, 85),
       percMat=cmat,
       interval="delta")

pdata <- spinach %>%
  group_by(CURVE, HERBICIDE) %>%
  expand(DOSE=exp(seq(-5, 5, length=50)))

pdata$SLOPEind <- predict(sm2, newdata=pdata)
pdata$SLOPE <- predict(sm2, newdata=pdata, level=0)

spinach %>% 
  ggplot(aes(x=log(DOSE), y=SLOPE, label = CURVE, 
             colour=HERBICIDE, group=CURVE, 
             shape=HERBICIDE)) +
  geom_point(size=1) +
  # geom_text(size=3)+
  geom_segment(aes(x=log(ec50[1,3]),xend=log(ec50[1,4]),
                   y=0, yend=0), col ="#CD5C5C", size = 3)+
  geom_segment(aes(x=log(ec50[2,3]),xend=log(ec50[2,4]),
                   y=0, yend=0), col ="#00C5CD",size = 3)+
  
  geom_line(data=pdata) +
  geom_line(data=pdata, aes(y=SLOPEind), linetype=2) +
  theme_bw() +
  scale_x_continuous("DOSE", 
                     breaks=log(c(0.01, 0.1, 1, 10, 100)), 
                     labels=c(0.01, 0.1, 1, 10, 100))                              
