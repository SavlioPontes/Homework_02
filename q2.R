setwd("C:/Users/savli/Documents/GitHub/Homework_02")

n <- 10*10^6 # Visitantes diários do site
p <- 10^-7 # Probabilidade de ganhar a recompensa

#--------------- QUESTÃO 2.1 ---------------#

# Por ser uma probabilidade de grande amostra,usaremos a distribuição de poisson
#como aproximação
taxa <- n * p
# Calculamos a taxa com a probabilidade individual e a "população"

aproximacao <- function(n,p,k){
  pmf <- (exp(-taxa) * taxa^k)/factorial(k) 
  # PMF da distribuição de Poisson = dpois(k,taxa)
  return(pmf) # Retorna a aproximação da probabilidade
}


#--------------- QUESTÃO 2.2 ---------------#

# Como as probabilidades são independentes,a distribuição exata é a Binomial
exp_bin <- n * p
var_bin <- n * p * (1-p)

# Como foi apresentado anteriormente, a distribuição aproximada é a Poisson
exp_pois <- taxa
var_pois <- taxa

#--------------- QUESTÃO 2.3 ---------------#
