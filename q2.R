n <- 10*10^6 # Visitantes diários do site
p <- 10^-7 # Probabilidade de ganhar a recompensa

#--------------- QUESTÃO 2.1 ---------------#

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
