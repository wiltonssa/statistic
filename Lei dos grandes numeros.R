# clear all
rm(list = ls())

# biblioteca
library(ggplot2)

# quantidade de lancamentos de um dado de seis lados
n=500

# define semente de reprodução
set.seed(25742)

# extrai os lancamentos
Dados_sample = sample(x=1:6, size = n, replace = TRUE)

# Cria uma tabela com contagem das faces 1 e 2
dados = data.frame(lancamento = 1:n, resultado=Dados_sample, D1=0, D2=0)

head(dados)

# faz a contagem cumultativa dos resultados
dados$D1 = cumsum(dados$resultado == 1)
dados$D2 = cumsum(dados$resultado == 2)

head(dados)


# Plota os resultados de convergencia
ggplot(dados, aes(x= lancamento)) +
  geom_line(aes(y=D1/lancamento), color="red") +
  geom_line(aes(y=D2/lancamento), color="blue") +
  geom_hline(yintercept = 1/6)


