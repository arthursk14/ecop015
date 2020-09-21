
## Testes com simulação

# Tamanho da amostra
n = 10e2

# Constante
c = 0

# Parâmetros
fi = 1
de = 2
b = 0
beta = 0.8

# Inovações
e = rnorm(n,0,de)
u = rnorm(n,0,de)
y = rep(0,n)
x = rep(0,n)

# Monta a série
for (i in 2:n){
  y[i] = c + b*i + fi*y[i-1] + e[i]
  x[i] = beta*y[i-1] + u[i]
  
}

# Plot da série
ts.plot(ts(x), y, gpars = list(col = c("red", "blue")))

# Série das diferenças
Dif_y_x = y - x

# Plot com a diferença
ts.plot(ts(x), y, Dif_y_x, gpars = list(col = c("black", "red", "blue")))

# Testa estacioneriedade da diferença entre as séries
adfTest(Dif_y_x)
unitrootTest(Dif_y_x)

# Plot da série de diferença
ts.plot(Dif_y_x)

# OLS para descobrir o parâmetro beta
OLS = lm(x~y)
plot(x~y)

# Série das diferenças
Dif_y_x_beta = y - OLS$coefficients[2]*x

# Plot com a diferença
ts.plot(ts(x), y, Dif_y_x_beta, gpars = list(col = c("black", "red", "blue")))

# Testa estacioneriedade da diferença entre as séries
adfTest(Dif_y_x_beta)
unitrootTest(Dif_y_x_beta)

# Plot da série de diferença
ts.plot(Dif_y_x_beta)

# Testa estacioneriedade das séries
adfTest(y)
unitrootTest(y)

adfTest(x)
unitrootTest(x)
