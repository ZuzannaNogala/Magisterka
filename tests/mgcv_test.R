# TEST

library(mgcv)
library(ggplot2)

data <- mgcv::gamSim()

ggplot(data) +
  #geom_line(aes(x = x0, y = f0)) # kwadratowa
  #geom_line(aes(x = x1, y = f1)) # wykładnicza
  #geom_line(aes(x = x2, y = f2)) # sinusoidalna
  geom_line(aes(x = x3, y = f3)) # liniowa (= 0)


gam_model <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = data)
plot(gam_model)

gam_summ <- summary(gam_model)
gam_summ$

plot(gam_model)



gam.check(gam_model)
