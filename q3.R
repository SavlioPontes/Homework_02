    setwd("C:/Users/savli/Documents/GitHub/Homework_02")
    #Questão 3
    #--------------- QUESTÃO 3.1 ---------------#
    gerar_temp <- function(n){
      n_pares <- n/2
      
      u1 = runif(n_pares)
      u2 = runif(n_pares)
    
      z1 = sqrt(-2 * log(u1)) * cos(2 * pi * u2)
      z2 = sqrt(-2 * log(u1)) * sin(2 * pi * u2)
      
      z <- c(z1,z2)
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
    # Estatísticas para o seu método (Box-Muller)
      mean(dados_box)
      sd(dados_box)
      min(dados_box)
      max(dados_box)
      
      # Estatísticas para o método do R (rnorm)
      mean(dados_r)
      sd(dados_r)
      min(dados_r)
      max(dados_r)
    