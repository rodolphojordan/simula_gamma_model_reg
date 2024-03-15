library("mgcv")

set.seed(123)
n <- 100
minx <- 0
maxx <- 1
x <- runif(n, minx, maxx)
t = runif(n, minx, 2*maxx)
meang <- exp(1.2+0.6*x + 3*t)
newxs <- seq(minx, maxx, length=200)    
newts <- seq(minx, 2*maxx, length=200)    


# Primeiro cenário
# define a dispersion
disp <- 0.8
y <- rgamma(n, shape=1/disp, scale=meang*disp)
df<-data.frame(y,x,t)
glmA1 <- glm(y ~ x+t,family=Gamma(link="log"),data = df)
prdys1 <- predict(glmA1, newdata=data.frame(x=newxs,t=newts), type="response")

#Segundo cenário
var <- 2.3
y <- rgamma(n, shape=(meang^2)/var, scale=var/meang)
df<-data.frame(y,x,t)
glmA2 <- glm(y ~ x+t,family=Gamma(link="log"),data = df)
prdys2 <- predict(glmA2, newdata=data.frame(x=newxs,t=newts), type="response")

# plot data and fits
par(mfrow=c(1, 2))
plot(x, y)
lines(newxs, prdys1, col="blue", lty=2, lwd=3)
plot(xs, ys2)
lines(newxs, prdys2, col="blue", lty=2, lwd=3)


# GLM Gamma
# parametrization 1 
set.seed(12345)
n <- 1000
minx <- -3.2
maxx <- 3.2
xs <- runif(n, minx, maxx)
meang <- exp(1.2+0.6*xs)
# define a variance
var <- 2.3
# define a dispersion
disp <- 0.8
# generate data defining the mean and the (constant) variance
ys1 <- rgamma(n, shape=(meang^2)/var, scale=var/meang)

glmA1 <- glm(ys1 ~ xs, family=Gamma(link="log"))
summary(glmA1)
newxs <- seq(minx, maxx, length=200)    
prdys1 <- predict(glmA1, newdata=data.frame(xs=newxs), type="response")

# parametrization 2 
# next parametrization is motivated by the statement of
# https://stats.stackexchange.com/questions/247624/dispersion-parameter-for-gamma-family
# "In R GLM assumes shape to be a constant"
# generate data defining the mean and fixing the shape (= fixing dispersion)
# here induces an overall increase variance with mean
ys2 <- rgamma(n, shape=1/disp, scale=meang*disp)

glmA2 <- glm(ys2 ~ xs,family=Gamma(link="log"))
summary(glmA2)
prdys2 <- predict(glmA2, newdata=data.frame(xs=newxs), type="response")

# plot data and fits
par(mfrow=c(1, 2))
plot(xs, ys1)
lines(newxs, prdys1, col="blue", lty=2, lwd=3)
plot(xs, ys2)
lines(newxs, prdys2, col="blue", lty=2, lwd=3)
