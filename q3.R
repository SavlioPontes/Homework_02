#Questão 3
#--------------- QUESTÃO 3.1 ---------------#

gerar_temp <- function(n){ # Função que vai gerar os valores empíricos
  n_pares <- n/2
  
  u1 = runif(n_pares)
  u2 = runif(n_pares)
  
  z1 = sqrt(-2 * log(u1)) * cos(2 * pi * u2) 
  z2 = sqrt(-2 * log(u1)) * sin(2 * pi * u2)
  
  z <- c(z1,z2) # Concatenação dos vetores
  temp <- 62 + 3.5*z
  return(temp)
}

#--------------- QUESTÃO 3.2 ---------------#

#Dados gerados a partir da Função Box-Muller
dados_box <- gerar_temp(1000)
head(dados_box)

#Dados gerados a partir da Função rnorm
dados_r <- rnorm(n = 1000,mean = 62,sd = 3.5)
head(dados_r)

#--------------- QUESTÃO 3.3 ---------------#

# Estatísticas para o seu método (Box-Muller) - EMPÍRICO
# Estatísticas para o método do R (rnorm) - TEÓRICO

# a)Mean: 
media_empirica <- mean(dados_box)
media_teorico <-mean(dados_r)

# b)Desvio Padrão:
desvio_empirico <- sd(dados_box)
desvio_teorico <- sd(dados_r)

# c)Max:
min_empirico <- min(dados_box)
min_teorico <- min(dados_r)

#c)Min:
max_empirico <- max(dados_box)
max_teorico <- max(dados_r)

# d)P(T > 68):
pd_empirica <- mean(dados_box > 68) #Média dos valores maiores que 68
pd_teorico <- 1 - pnorm(q = 68,mean = 62,sd = 3.5) # P(T > 68) = 1 - P(T <= 68)


# e)P(60 < T < 65):
pe_empirica <- mean(dados_box > 60 & dados_box < 65) #Média dos valores no intervalo
pe_teorico <- pnorm(q = 65,mean = 62,sd = 3.5) - pnorm(q = 60,mean = 62,sd = 3.5)
#P(60 < T < 65) = P(T <= 65) - P(T <= 60)

# f)Existe T > 75 ?
pf_empirica <- sum(dados_box > 75) #Número de valores maiores que 75 (igual a zero.)
# f)P(T > 75):
pf_teorico <- 1 - pnorm(q = 75,mean = 62,sd = 3.5) ## P(T > 75) = 1 - P(T <= 75)

#--------------- QUESTÃO 3.4 ---------------#

# (a) Histograma apenas com os dados simulados
hist(dados_box, 
     breaks = 30, 
     col = "lightgray", 
     border = "white",
     main = "", 
     xlab = "Temperatura (°C)", 
     ylab = "Frequência")

# Gráfico da normal isolada (Curva Clássica)
x_curva <- seq(50, 75, length = 200)
y_curva <- dnorm(x_curva, mean = 62, sd = 3.5)
plot(x_curva, y_curva, type = "l", col = "red", lwd = 2,
     main = "", xlab = "Temp", ylab = "Densidade")

# (b) PDF Teórica sobreposta ao Histograma
hist(dados_box, 
     breaks = 30, 
     probability = TRUE, 
     col = "lightblue", 
     main = "", 
     xlab = "Temperatura (°C)", ylab = "Densidade")

lines(x_curva, y_curva, col = "red", lwd = 3) # Linhas da Normal
