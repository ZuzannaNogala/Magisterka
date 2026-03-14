require(gamair)
data(engine)
attach(engine) 
plot(size,wear,xlab="Engine capacity",ylab="Wear index")

## Piecewise linear regression fit (continuous line) 
## to data (◦) on engine wear index versus engine capacity (size)
## for 19 Volvo car engines.

tf <- function(x,xj,j) {
  ## generate jth tent function from set defined by knots xj
  dj <- xj*0;dj[j] <- 1
  approx(xj,dj,x)$y
}


tf.X <- function(x,xj) {
  ## tent function basis matrix given data x ## and knot sequence xj
  nk <- length(xj)
  n <- length(x)
  X <- matrix(NA,n,nk)
  for (j in 1:nk) X[,j] <- tf(x,xj,j)
  X
}

sj <- seq(min(size),max(size),length=6) ## generate knots
X <- tf.X(size,sj)                      ## get model matrix 
b <- lm(wear ~ X - 1)                   ## fit model
s <- seq(min(size),max(size),length=200)## prediction data
Xp <- tf.X(s,sj)                        ## prediction matrix
plot(size,wear)
lines(s,Xp %*% coef(b))


### Penalized PieceWise 

prs.fit <- function(y,x,xj,sp) {
  X <- tf.X(x,xj) ## model matrix
  D <- diff(diag(length(xj)),differences=2) ## sqrt penalty 
  X <- rbind(X,sqrt(sp)*D) ## augmented model matrix
  y <- c(y,rep(0,nrow(D))) ## augmented data
  lm(y ~ X - 1) ## penalized least squares fit
}


sj <- seq(min(size),max(size),length=20) ## knots 
b <- prs.fit(wear,size,sj,2) ## penalized fit plot(size,wear) ## plot data
Xp <- tf.X(s,sj) ## prediction matrix
lines(s,Xp %*% coef(b), col = "red") ## plot the smooth


### GCV 

rho = seq(-9,11,length=90)
n <- length(wear)
V <- rep(NA,90)

for (i in 1:90) { ## loop through smoothing params
  b <- prs.fit(wear,size,sj,exp(rho[i])) ## fit model
  trF <- sum(influence(b)$hat[1:n])
  rss <- sum((wear-fitted(b)[1:n])^2)
  V[i] <- n*rss/(n-trF)^2
  ## extract EDF
  ## residual SS
  ## GCV score
}

plot(rho,V,type="l",xlab=expression(log(lambda)),
     main="GCV score")
sp <- exp(rho[V==min(V)]) ## extract optimal sp 
b <- prs.fit(wear,size,sj,sp) ## re-fit plot(size,wear,main="GCV optimal fit")
plot(size,wear,xlab="Engine capacity",ylab="Wear index")
lines(s,Xp %*% coef(b))
