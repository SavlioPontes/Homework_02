rm(list=ls())
options(digits=7)
library(latex2exp)
#QUESTÃO 1

# 1. Função de distribuição de X - Binomial(x,n,p)
n <- 50
p <- 0.7
x <- 0:n

# 2. Gráficos
#PMF
PMF <- dbinom(x, size = n, prob = p)
plot(x, PMF,
     type = "h",
     lwd = 2,
     xlab = "Número de clientes que pedem sobremesa",
     ylab = "P(X = x)",
     main = "")
# Curva Normal de aproximação
curve(dnorm(x, mean = valor_esperado, sd = desvio), add = TRUE, col = "red", lwd = 2)
legend("topleft", legend = c("Binomial", "Normal"), col = c("black", "red"), lwd = 2)


#CDF
CDF = c(0,cumsum(PMF))
CDF.plot = stepfun(x,CDF,f=0)
par(mar=c(6,5,4,2))
plot.stepfun(CDF.plot,col='blue',
             xlab=TeX('x (valores que X pode assumir)'),ylab=TeX('F_X(x)'),
             verticals=FALSE,
             do.points=TRUE,
             pch=19, lwd=2,
             cex.lab=1.3, 
             cex.axis=1.2,
             main="")
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

#6 p = 0.8
PMF2 <- dbinom(x, size = n, prob = 0.8)
plot(x, PMF2,
     type = "h",
     lwd = 2,
     xlab = "Número de clientes que pedem sobremesa",
     ylab = "P(X = x)")
curve(dnorm(x, mean = 40, sd = sqrt(0.8*0.2*50)), add = TRUE, col = "red", lwd = 2)
legend("topleft", legend = c("Binomial", "Normal"), col = c("black", "red"), lwd = 2)

# n=100
y <- 0:100
PMF3 <- dbinom(y, size = 100, prob = 0.7)
plot(y, PMF3,
     type = "h",
     lwd = 2,
     xlab = "Número de clientes que pedem sobremesa",
     ylab = "P(X = x)",
     main = "")
curve(dnorm(x, mean = 70, sd = sqrt(0.7*0.3*100)), add = TRUE, col = "red", lwd = 2)
legend("topleft", legend = c("Binomial", "Normal"), col = c("black", "red"), lwd = 2)
