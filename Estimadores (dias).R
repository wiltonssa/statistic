# clear all data
rm(list=ls())

# Libraries
library(ggplot2)
library(dplyr)
library(lubridate)

# load file
load(file = "./Aula07/Database/Hd_dataGruped_2.RData")

# mostra a estrutura atual
head(data)

# seleciona apenas drives com falha
data = data %>% filter(total_Failure>0)

# Determina life span
data$life_span_days = lubridate::time_length(lubridate::interval(start = data$birth, end = data$death ), unit = "day")
data$life_span_month = lubridate::time_length(lubridate::interval(start = data$birth, end = data$death ), unit = "month")
data$life_span_year = lubridate::time_length(lubridate::interval(start = data$birth, end = data$death ), unit = "year")

# plotagerm de BoxPlot
ggplot(data, aes(x = Manufacture, y = life_span_days)) + 
  geom_boxplot(colour="grey40", fill="blue", alpha=0.2)

# mostra um resumos dos dados por fabricante
data %>% group_by(Manufacture) %>%  summarise(count=n())

# seleciona apenas fabricantes com amostra suficiente
data_full = data
data = data %>% filter(Manufacture %in% c("Western Digital", "Seagate", "Toshiba"))

# Escolhe qual sera a variavel avaliada
data$x = data$life_span_days

# Plota o histograma dos dados das distribuicoes marginais
ggplot(data) + 
  geom_histogram(aes(x = x, y=..density..), colour="grey40", fill="blue", alpha=0.2, bins = 50) +
  facet_wrap(~Manufacture)

# Vamos nos concentrar em WD
data = data %>% filter(Manufacture %in% c("Western Digital"))

# Plota o histograma dos dados das distribuicoes marginais
grafico = ggplot(data) + 
  geom_histogram(aes(x = x, y=..density..), colour="grey40", fill="blue", alpha=0.2, bins = 50)

grafico

# Metodo dos momentos ----
media.MMo = mean(data$x)

# Metodo de maxima verosimilhanca ----

# log like da geometric
loglike.geomDist = function(p, x){
  return(-sum(dgeom(x, p, log = TRUE)))
}

# Chute inicial da Geometric
chute.geom=0.1

# Otimizacao da geometric
geom.opt = optim(chute.geom,
                 loglike.geomDist,
                 x=as.integer(data$x),
                 method = "Brent", lower = 0, upper = 1)


# plota histograma e curva exponencial
grafico = grafico +
  geom_line(aes(x = x, y=dgeom(x, geom.opt$par[1]), color="MLE"), size=1) 

grafico

# Calcula as medias das otimizacoes
media.mle = 1/geom.opt$par[1]


# Plotagem de medias
grafico +
  geom_vline(xintercept = media.mle, color="red", size=1) +
  geom_vline(xintercept = media.MMo, color="blue", size=1)


# Metodo de OLS ----
# Busca dados do histograma
ols.data = ggplot_build(grafico)$data[[1]]

# Verifica que os pontos sao o mesmo do histograma.
grafico +
  geom_point(aes(x=x, y=y), data=ols.data)

# replota o grafico sem as barras do histograma
ggplot(ols.data) + 
  geom_point(aes(x=x, y=y))

# passa o ln em y
ols.data$ln_y = log(ols.data$y)

# replota o grafico com ln
ggplot(ols.data) + 
  geom_point(aes(x=x, y=ln_y))

# Modelagem de death rate x
mdl.ols = lm(ln_y ~ x, data=ols.data)
summary(mdl.ols)
ols.data$pred = mdl.ols$fitted.values

# ploto os pontos vs reta do OLS
ggplot(ols.data) + 
  geom_point(aes(x=x, y=ln_y)) +
  geom_line(aes(x=x, y=pred), color="red", size = 1)

# Plota os dados originais com a curva estimada do OLS
grafico +
  geom_line(aes(x=x, y=exp(pred), color="OLS"), size=1, data = ols.data)  

media.ols = -1/mdl.ols$coefficients[2]

grafico +
  geom_line(aes(x=x, y=exp(pred), color="OLS"), size=1, data = ols.data) +
  geom_vline(xintercept = media.mle, color="red", size=1) +
  geom_vline(xintercept = media.MMo, color="blue", size=1) +
  geom_vline(xintercept = media.ols, color="black", size=1, show.legend = TRUE) 

cat(sprintf("\n===== Medias ===== \n%7s: %.2f dias\n%7s: %.2f dias\n%7s: %.2f dias",
            "Momento", media.MMo, "MLE", media.mle, "OLS", media.ols))




