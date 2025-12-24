rm(list=ls())
options(digits=7)
library(latex2exp)
#QUESTÃO 1

# 1. Função de distribuição de X - Binomial(x,n,p)
n <- 50
p <- 0.7
x <- 0:n

# 2. Gráficos da  PMF e da CDF

PMF <- dbinom(x, size = n, prob = p)
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
             verticals=FALSE,main='Função de Distribuição Acumulada (CDF)',
             do.points=TRUE,
             pch=19, lwd=2,
             cex.lab=1.3, 
             cex.axis=1.2)
abline(h=0)

#3. Valor esperado, variância e desvio padrão de X.

valor_esperado <- n*p
variância <-n * p * (1 - p)
desvio <- sqrt(variância)

#display dos dados
data.frame(valor_esperado, variância, desvio)

#4. a) P(X ≥ 20)
respostaA <- 1-pbinom(q = 19, size = 50, prob = 0.7)
# b) P(30 < X < 43)
respostaB <- sum(dbinom(31:42, size = 50, prob = 0.7))
# c)P(X = 31) 
respostaC <- dbinom(31, 50, 0.7)

cat("a) P(X ≥ 20) =", respostaA, "\n")
cat("b) P(30 < X < 43) =", respostaB, "\n")
cat("c) P(X = 31) =", respostaC, "\n")
