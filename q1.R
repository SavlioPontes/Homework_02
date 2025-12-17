rm(list=ls())
options(digits=2)
library(latex2exp)

# 1. Determine a função de distribuição de X
n <- 50
p <- 0.7
x <- 0:n

# 2. Gráficos da  PMF e da CDF

PMF <- dbinom(x, size = n, prob = 0.7)
plot(x, PMF,
     type = "h",
     lwd = 2,
     xlab = "Número de clientes que pedem sobremesa",
     ylab = "P(X = x)",
     main = "PMF - Distribuição Binomial (n = 50, p = 0.7)")

cdf = c(0,cumsum(PMF))
cdf.plot = stepfun(x,cdf,f=0)
par(mar=c(6,5,4,2))
plot.stepfun(cdf.plot,col='blue',
             xlab=TeX('x'),ylab=TeX('F_X(x)'),
             verticals=FALSE,main='',
             do.points=TRUE,
             pch=19, lwd=2,
             cex.lab=1.3, 
             cex.axis=1.2)
abline(h=0)

#3. Valor esperado, variância e desvio padrão de X.

valor_esperado <- n*p
variância <-n * p * (1 - p)
desvio <- sqrt(variância)

data.frame(valor_esperado, variância, desvio)


