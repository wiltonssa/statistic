# Clear all
rm(list=ls())

# biliotecas utilizadas
library(ggplot2)

# Caracteristicas da populacao que vamos tirar a amostra
pop.Media = 5
pop.Sd = 1.5

# Tamanho de elementos de uma amostra
n=10

# Quantidade de amostras
M=15


# Tabela com pontos da curva chi-quadrado
data.ChiSquare = data.frame(x=seq(from=0, to=25, by=0.1), y=0)
data.ChiSquare$y = dchisq(data.ChiSquare$x, df=(n-1))

# Plotagem dos pontos da curva Normal com linhas na media e 1,2,3 s.d.
ggplot(data.ChiSquare) +
  geom_point(aes(x, y), colour = "red", alpha=0.4) +
  geom_line(aes(x, y)) + 
  geom_vline(xintercept = pop.Sd^2)  

# Tabela com as amostras e intervalos
dados = data.frame(id = 1:M, Var = 0, upper = 0, lower = 0)

#  mostra as primeiras linhas da tabela dados
head(dados)

# fixa semente de aleatoriedade para reprodução
set.seed(1234)

for(i in 1:M){
  # Extrai uma amostra de tamanho n
  amostra = rnorm(n, mean = pop.Media, sd=pop.Sd)
  
  # calcula a media e os intervalos de conficanca
  dados[i, "Var"] = var(amostra)
  dados[i, "upper"] = (n-1)*var(amostra)/qchisq(0.025, df=(n-1))
  dados[i, "lower"] = (n-1)*var(amostra)/qchisq(0.975, df=(n-1))
}

# determino se o intervalo contem ou nao a media
# TRUE = Contem a media
# FALSE = Não contem a media
dados$HasTheVar = dados$upper >= pop.Sd^2 &  dados$lower <= pop.Sd^2


grafico = 
  ggplot(dados) +
  # ggplot(dados[!dados$HasTheVar,]) +
  geom_line(aes(x, y), data=data.ChiSquare) +
  geom_vline(xintercept = pop.Sd^2, linetype="dashed") +
  geom_errorbar(aes(y=runif(n=length(upper), min = -0.02, max=0.02),
                    xmin=lower, xmax=upper,
                    colour=HasTheVar),
                width=0.01, alpha=0.5) +
  scale_color_manual("Contem a média",
                     values = c("TRUE"="#336666", "FALSE"="#FF3300"),
                     breaks = c("TRUE", "FALSE"),
                     labels = c("Sim", "Não")) +
  labs(title = "Intervalo de confiança da Variância",
       subtitle = "Curva Qui-Quadrado",
       x=NULL,
       y=NULL,
       caption = sprintf("População ~ N(%.1f, %.1f)", pop.Media, pop.Sd^2)) + 
  guides(colour = guide_legend(override.aes = list(alpha = 1)))


print(grafico)


cat(sprintf("\n%6s [%6.3f, %6.3f] - %.2f", dados$HasTheVar, dados$lower, dados$upper, pop.Sd^2))

    