# Clear All
rm(list=ls())

# bibliotecas
library(ggplot2)

# Tamanho da amostra
M=1000

# para repruducao
set.seed(68563)

# Retirada de uma amostra aleatoria de uma uniforme [0, 1]
x = runif(M)

# Funcao que calcula a media geometrica
geoMean = function(x){
  # return(prod(x)^(1/length(x)))
  return(exp(mean(log(x))))
}

# Quantidade de ensaios que vamos fazer
data = data.frame(n = 1:M, Aritimetica = 0, Geometrica = 0)

head(data)

for(n in data$n) {
  sample = x[1:n]
  
  data[n, "Aritimetica"] = mean(sample)
  data[n, "Geometrica"] = geoMean(sample)
}

# plota os dados
ggplot(data, aes(x=n)) +
  geom_line(aes(y=Aritimetica), color="blue") +
  geom_line(aes(y=Geometrica)) +
  geom_hline(yintercept = 0.5, color="red")
