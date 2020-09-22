rm(list = ls())
library(plotly)
library(dplyr)

grid.scale = 50

grid = seq(from=0, to=1, length.out = grid.scale)

X = grid
Y = grid

MGrid= matrix(NA, ncol = grid.scale, nrow = grid.scale)

idx = 1:length(MGrid)
x_idx = rep(NA, length(MGrid))
y_idx = rep(NA, length(MGrid))

ncont = 1
for (i in 1:length(X)) {
  for (j in 1:length(Y)) {
    MGrid[i,j] = 6*X[i]*Y[j]^2
    x_idx[ncont] = X[i]
    y_idx[ncont] = Y[j]
    ncont = ncont+1
  }
}


plot_ly(
  x = x_idx, y = y_idx,
  z = as.vector(MGrid),
  size = 1,
  alpha = 0.2
)   %>%
  add_markers()



# Curvas de nivel da funcao de probabilidade
plot_ly(
  type = 'contour',
  z = MGrid,
  # colorscale = 'Jet',
  autocontour = T,
  # contours = list(
  #   start = 0,
  #   end = 6
  # )
)  



prb = MGrid[idx]
amostra = sample(x=idx, prob = prb/sum(prb), replace = TRUE, size = 1e3)

amostra_x = x_idx[amostra]
amostra_y = y_idx[amostra]

plot(amostra_x, amostra_y)

sum((amostra_x + amostra_y) <= 1) / length(amostra_x)

