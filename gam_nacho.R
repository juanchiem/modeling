# https://fromthebottomoftheheap.net/2021/02/02/random-effects-in-gams/
# https://environmentalcomputing.net/statistics/gams/
# https://noamross.github.io/gams-in-r-course/chapter1
# https://cran.r-project.org/web/packages/tidymv/vignettes/predict-gam.html
# https://people.bath.ac.uk/man54/SAMBa/ITTs/ITT2/EDF/WoodGAM.pdf
# https://drmowinckels.io/blog/2019-11-16-plotting-gamm-interactions-with-ggplot2/
# https://peerj.com/articles/6876/

load(here::here("data/nacho.Rdata"))

pacman::p_load(tidyverse, mgcv, gratia, tidymv, mgcv)

theme_set(theme_bw())

dat <- x %>% 
  janitor::clean_names() %>% 
  drop_na(fs_mes) %>% 
  mutate(fs_mes=fct_rev(fs_mes), 
         campana_id=interaction(fs_mes, campana))

dat %>% str

dat %>% 
  ggplot(aes(x = prof, y = rendimiento))+
  geom_line(aes(col=campana), alpha=.5)+ 
  facet_wrap(~fs_mes)+ 
  geom_smooth() 


# efectos simples de prof (continua) y fs_mes (factor)
mod.gam <- gam(
  rendimiento ~ 
    s(prof, k=3) + 
    fs_mes, 
  data=dat
)

rsd1 <- residuals(mod.gam)
qq.gam(mod.gam,rep=100); 
plot(fitted(mod.gam),rsd1)
gam.check(mod.gam)

# incluye la interaccion 
mod.gam2 <- gam(
  rendimiento ~ 
    fs_mes +
    s(prof, k=4, by = fs_mes),
  data = dat)

rsd2 <- residuals(mod.gam2)
qq.gam(mod.gam2, rep=100); 
plot(fitted(mod.gam2),rsd2)


# random effect of season (not working)

mod.gam3 <- gam(
  rendimiento ~ 
    fs_mes +
    s(prof, k=3) +
    s(campana, fs_mes, bs="re"),
  data = dat)

# rsd3 <- residuals(mod.gam3)
# qq.gam(mod.gam3, rep=100); 
# plot(fitted(mod.gam3),rsd3)

broom::tidy(mod.gam2)

draw(mod.gam)
acf(residuals(mod.gam))

draw(mod.gam2)

AIC(mod.gam, mod.gam2)
gam.check(mod.gam) 

model_p <- predict_gam(mod.gam)
model_p %>%
  ggplot(aes(prof, fit)) +
  geom_smooth_ci(fs_mes)

# model_p2 <- predict_gam(mod.gam2)
# model_p2 %>%
#   ggplot(aes(prof, fit)) +
#   geom_smooth_ci(fs_mes)

# me parece que falta ajustar algo mas en el modelo... 