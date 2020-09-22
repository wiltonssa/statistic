# Clear all
rm(list = ls())

# Biblioteca utilizada
library(ggplot2)

#  Amostra
daily.intake <- c(5260,5470,5640,6180,6390,6515,6805,7515,7515,8230,8770)
quantile(daily.intake)

# Boxplot dos dados
boxplot(daily.intake)

# Passo 1. 
# Definir H0 mu != 7725
# H1 mu != 7725
mu=7725

# Passo 4: Calcular os valores 
media = mean(daily.intake)
sd = sd(daily.intake)

# Passo 2: definir o tipo de teste e com isso a Estatistica
t <- (media - mu)/(sd/length(daily.intake)^0.5)
df <- length(daily.intake) -1
pvalor <- pt(t, df)*2


# Desenho da curva t-student
t.grid <- seq(from=-3, to=3, by=0.1)
pt <- dt(t.grid, df)

tbl <- data.frame(t.grid, pt)

ggplot(tbl) +
  geom_line(aes(x=t.grid, y=pt))

# Passo 3: Definir o nivel de confiança e com isso o Critical Value
critical <- qt(0.90, df = df)

# Passo 5: Avaliar os resultados
x.range <- seq(from=-3, to=3, by=0.1)

table.t_dist <- data.frame(x = x.range, p=0)
table.t_dist$p <- dt(table.t_dist$x, df=1)

print(sprintf("Estatistica = %f", t))


ggplot(table.t_dist) +
  geom_line(aes(x=x, y=p)) +
  geom_area(aes(x=x, y=p),
            data=table.t_dist[table.t_dist$x >= critical,],
            fill="red", alpha=0.2) +
  geom_area(aes(x=x, y=p),
            data=table.t_dist[table.t_dist$x <= -critical,],
            fill="red", alpha=0.2) +
  geom_vline(xintercept = 0, linetype="dashed") +
  geom_vline(xintercept = t, color="blue", size = 1)


