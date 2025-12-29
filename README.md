# Homework_02
Trabalho de Estatística com o Software R
## DESCRIÇÃO DO PROJETO (resolução das seguintes questões)
- Questão 1: Análise de quantidade de clientes que pedem sobremesa em um restaurante
- Questão 2: Análise da probabilidade de um usuário ser selecionadopara ganhar uma recompensa em um site
- Questão 3: Monitoramento da temperatura de uma CPU multicore em uma unidade de processamento embarcada


### ESTRUTURA
Homework_01/
- q1.R                        # Análise dos clientes que pedem sobremesa
- q2.R                        # Seleção aleatória de um usuário
- q3.R                        # Monitoramento da CPU
- questions.R                 # Arquivo com todas as questões
- RelatórioHW1.pdf            # Relatório do trabalho (PDF)
- README.md                   # Este arquivo

### PROCESSO
Nesse projeto, foram trabalhadas as competências em R relacionadas à estatística probabilística por meio de questões contextualizadas, nas quais foram calculados e analisados dados como o valor esperado, a variância, o desvio padrão e as diferentes distribuições possíveis para as variáveis aleatórias em cada contexto. A partir desses valores e das visualizações gráficas, foi possível fazer interpretações acerca do comportamento dos dados e como ele se traduziu nos contextos apresentados. Além disso, também foram realizados os cálculos teóricos das questões de modo a possibilitar uma comparação com os resultados obtidos pelos métodos utilizados pelo R.

Para isso, o projeto foi separado em três arquivos de código em R (q1.R, q2.R e q3.R), um para cada questão, os quais foram criados e programados pela plataforma RStudio conectada ao Git. Desse modo, os integrantes faziam suas partes simultaneamente, enviando as alterações feitas para o repositório remoto do gitHub, no qual conseguiam visualizar todo o trabalho.  
Ademais, o relatório do projeto foi feito na linguagem LATEX na plataforma Overleaf, que também permite uma edição simultânea dos textos. 

## USO DA IA
Para esse trabalho, a inteligência artifical foi utilizada para otimizar processos, como gerar modelos de código para o LATEX, gerar as tabelas simples de exposição dos dados, encontrar erros nos programas deesenvolvidos e tirar dúvidas sobre a linguagem. A plataforma mais utilizada foi o ChatGPT. A seguir, o link para a conversa referente ao projeto:
- https://chatgpt.com/g/g-p-6928cb70feb081918dfadeeb2be7d45d-homework-2/project

## INSTRUÇÕES PARA EXECUÇÃO
Para rodar o código, basta:
1. Clone o repositório:
- git clone https://github.com/SavlioPontes/Homework_01.git
cd Homework_01

2. Instale as Dependências #Execute no R/RStudio:
- dependencies <- c(
  "latex2exp"  # Utilizado para renderização de expressões LaTeX nos gráficos
)

install.packages(dependencies, dependencies = TRUE)


3. Para rodar o código execute no console do R:
- source("questions.R") 
