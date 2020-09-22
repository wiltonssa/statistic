# Clear all
rm(list=ls())

library(ggplot2)

# Caracteristicas da populacao que vamos tirar a amostra
pop.Media = 5
pop.Sd = 1.5

# Tabela com pontos da curva normal
data.Normal = data.frame(x=seq(from=1, to=9, by=0.1), y=0)
data.Normal$y = dnorm(data.Normal$x, mean = pop.Media, sd = pop.Sd)

# Plotagem dos pontos da curva Normal com linhas na media e 1,2,3 s.d.
# ggplot(data.Normal) +
#   geom_point(aes(x, y), colour = "red", alpha=0.4) +
#   geom_line(aes(x, y)) +
#   geom_vline(xintercept = pop.Media, linetype="dashed") +
#   geom_vline(xintercept = pop.Media+pop.Sd, linetype="dashed", colour = "blue") +
#   geom_vline(xintercept = pop.Media-pop.Sd, linetype="dashed", colour = "blue") +
#   geom_vline(xintercept = pop.Media+2*pop.Sd, linetype="dashed", colour = "purple") +
#   geom_vline(xintercept = pop.Media-2*pop.Sd, linetype="dashed", colour = "purple") +
#   geom_vline(xintercept = pop.Media+3*pop.Sd, linetype="dashed", colour = "orange") +
#   geom_vline(xintercept = pop.Media-3*pop.Sd, linetype="dashed", colour = "orange") 

# Tamanho de elementos de uma amostra
n=10
# n=50

# Quantidade de amostras
M=5
# M=50
# M=100

# Tabela com as amostras e intervalos
dados = data.frame(id = 1:M, media = 0, upper = 0, lower = 0)

#  mostra as primeiras linhas da tabela dados
head(dados)

# fixa semente de aleatoriedade para reprodução
set.seed(1234)

for(i in 1:M){
  # Extrai uma amostra de tamanho n
  amostra = rnorm(n, mean = pop.Media, sd=pop.Sd)
  
  # calcula a media e os intervalos de conficanca
  dados[i, "media"] = mean(amostra)
  dados[i, "upper"] = mean(amostra) + qnorm(0.975) * pop.Sd/n^0.5
  dados[i, "lower"] = mean(amostra) - qnorm(0.975) * pop.Sd/n^0.5
}

# determino se o intervalo contem ou nao a media
# TRUE = Contem a media
# FALSE = Não contem a media
dados$HasTheMean = dados$upper >= 5 &  dados$lower <= 5

# grafico = ggplot(dados[!dados$HasTheMean,]) +
grafico = ggplot(dados) +
  geom_line(aes(x, y), data=data.Normal) +
  geom_vline(xintercept = pop.Media, linetype="dashed") +
  geom_errorbar(aes(y=runif(n=length(upper), min = -0.02, max=0.02),
                    xmin=lower, xmax=upper,
                    colour=HasTheMean),
                width=0.01, alpha=0.5) +
  scale_color_manual("Contem a média",
                     values = c("TRUE"="#336666", "FALSE"="#FF3300"),
                     breaks = c("TRUE", "FALSE"),
                     labels = c("Sim", "Não")) +
  labs(title = "Intervalo de confiança",
       subtitle = "Curva normal",
       x=NULL,
       y=NULL,
       caption = sprintf("População: média=%.1f\ns.d.=%.1f", pop.Media, pop.Sd)) + 
  guides(colour = guide_legend(override.aes = list(alpha = 1)))


print(grafico)
