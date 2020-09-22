# clear all
rm(list = ls())

# bibliotecas utilizadas
library(ggplot2)

# define semente aleatoria para reprodução
set.seed(987123)

# quantidade de amostras
qtd = 10000

# Tamanho da amostra aleatoria
n=1000


# Tabela que guarda o resultado de cada amostra
data = data.frame(amostra = 1:qtd, media.uni=0, media.csq=0)

head(data)

# para cara ensaio (desiginada pela linha da tabela)
# Extrai uma amostra da qui quadrado
# Extrai uma amostra da Uniforme
# calcula a media aritimetica das amostras
for(i in data$amostra){
  # retira amostra aleatoria
  sample.chi = rchisq(n, df = 1)
  sample.uni = runif(n)
  
  data$media.uni[i] = mean(sample.uni)
  data$media.csq[i] = mean(sample.chi)
}


# Plota o histograma das MEDIAS da distribuicao uniforme
# lembre: var(uniforme) =  (b-1)^2 * 1/12
ggplot(data) +
  geom_histogram(aes(x=media.uni, y = ..density..), bins = 50) +
  geom_line(aes(x,y), data = data.frame(x=data$media.uni,
                                        y=dnorm(data$media.uni,
                                                mean = 0.5,
                                                sd = ((1/12)/n)^0.5)), color = "red") +
  labs(title = "Distribuicao da media da Uniforme")

# Plota o histograma das MEDIAS da distribuicao qui-quadrado
# lembre: Mean(Csq) = df
#         var(df) =  2*df
ggplot(data) +
  geom_histogram(aes(x=media.csq, y = ..density..), bins = 50) +
  geom_line(aes(x,y), data = data.frame(x=data$media.csq,
                                        y=dnorm(data$media.csq,
                                                mean = 1,
                                                sd = (2/n)^0.5)), color = "red") +
  labs(title = "Distribuicao da media da Qui quadrado")
