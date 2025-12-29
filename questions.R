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

#--------------- QUESTÃO 2.1 ---------------#
n <- 10*10^6 # Visitantes diários do site
p <- 10^-7 # Probabilidade de ganhar a recompensa

# Por ser uma probabilidade de grande amostra,usaremos a distribuição de poisson
#como aproximação
taxa <- n * p
# Calculamos a taxa com a probabilidade individual e a "população"

# PMF da distribuição de Poisson = dpois(k,taxa)
aproximacao <- function(n,p,k){
  lambda <- n * p
  pmf <- (exp(-lambda) * lambda^k)/factorial(k) 
  
  return(pmf) # Retorna a aproximação da probabilidade
}


#--------------- QUESTÃO 2.2 ---------------#

#Distribuição Binomial (exata)
exp_bin <- n * p
var_bin <- n * p * (1-p)

#Distribuição de Poisson (aproximada)
exp_pois <- taxa
var_pois <- taxa

data.frame(
  Distribuição = c("Binomial (exata)", "Poisson (aproximada)"),
  Valor_Esperado = c(exp_bin, exp_pois),
  Variância = c(var_bin, var_pois)
)


#--------------- QUESTÃO 2.3 ---------------#

# Como a probabilidade é a PMF de Poisson e um sorteio entre os ganhadores,
# para isso pegaremos cada possivel k para calcular a prob total:
# P(ganho) = sum(Pw(k) * 1/(k+1))
k_ganhadores <- 0:30 #30 pois ja e o suficiente para uma prob prox de zero.
prob_W <- aproximacao(n,p,k_ganhadores)
prob_ganho <- sum(prob_W * (1/(k_ganhadores+1)))

#--------------- QUESTÃO 2.4   ---------------#
set.seed(90) # Setamos uma semente para analisar as mesmas simulações aleatorias.
n_simulacoes <- 100000 # Quantidade de simulações a serem realizadas
simulacoes <- rpois(n_simulacoes,lambda = taxa) #função que irá simular a distribuição.

#Análise do resultado das simulações;
freq_empirica <- table(simulacoes) / n_simulacoes

#Análise do resultado esperado(Teórico):
k_teorico <- as.numeric(names(freq_empirica))
freq_teorica <- aproximacao(n, p, k_teorico)
names(freq_teorica) <- names(freq_empirica)
print(round(freq_teorica,4))

print("--EMPÍRICO--")
print(round(freq_empirica,4))
print("--TEÓRICO--")
print(round(freq_teorica,4))

# Gráfico de comparação: PMF teórica vs empírica
plot(k_teorico, freq_teorica, type = "h",
     lwd = 3,
     main = "",
     xlab = "Número de Vencedores (k)",
     ylab = "Probabilidade",
     ylim = c(0, max(freq_teorica, freq_empirica) * 1.1))

points(k_teorico, freq_empirica,
       pch = 16, col = "blue")

legend("topright",
       legend = c("Teoria (Poisson)", "Simulação"),
       lwd = c(3, NA),
       pch = c(NA, 16),
       col = c("black", "blue"))
