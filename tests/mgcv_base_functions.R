library(mgcv) ## load the package
data(trees)

# base splines
ct1 <- gam(Volume ~ s(Height) + s(Girth),
           family=Gamma(link=log),data=trees)

s_ct1 <- summary(ct1)
print.gam(ct1)

plot(ct1,residuals=TRUE)


# cubic splines
ct2 <- gam(Volume ~ s(Height,bs="cr") + s(Girth,bs="cr"),
           family=Gamma(link=log),data=trees)
ct2

s_ct2 <- summary(ct2)
s_ct2$edf
s_ct1$edf

# more knots
ct3 <- gam(Volume ~ s(Height) + s(Girth,bs="cr",k=20),
           family=Gamma(link=log),data=trees)
ct3

s_ct3 <- summary(ct3)
s_ct3$edf


# Adding gamma parameter 
# default: gamma =1 - GCV overfitts
# arround gamma = 1.5 better

ct4 <- gam(Volume ~ s(Height) + s(Girth),
           family=Gamma(link=log),data=trees,gamma=1.4)

s_ct4 <- summary(ct4)

plot(ct4,residuals=TRUE)

# double variables
ct5 <- gam(Volume ~ s(Height,Girth,k=25),
           family=Gamma(link=log),data=trees)

s_ct5 <- summary(ct5)

plot(ct5,residuals=TRUE)
plot(ct5,too.far=0.15)


ct6 <- gam(Volume ~ te(Height,Girth,k=5),
           family=Gamma(link=log),data=trees)

plot(ct6,too.far=0.15)

