
## Testes com simula��o

# Tamanho da amostra
n = 10e2

# Constante
c = 0

# Par�metros
fi = 1
de = 2
b = 0
beta = 0.8

# Inova��es
e = rnorm(n,0,de)
u = rnorm(n,0,de)
y = rep(0,n)
x = rep(0,n)

# Monta a s�rie
for (i in 2:n){
  y[i] = c + b*i + fi*y[i-1] + e[i]
  x[i] = beta*y[i-1] + u[i]
  
}

# Plot da s�rie
ts.plot(ts(x), y, gpars = list(col = c("red", "blue")))

# S�rie das diferen�as
Dif_y_x = y - x

# Plot com a diferen�a
ts.plot(ts(x), y, Dif_y_x, gpars = list(col = c("black", "red", "blue")))

# Testa estacioneriedade da diferen�a entre as s�ries
adfTest(Dif_y_x)
unitrootTest(Dif_y_x)

# Plot da s�rie de diferen�a
ts.plot(Dif_y_x)

# OLS para descobrir o par�metro beta
OLS = lm(x~y)
plot(x~y)

# S�rie das diferen�as
Dif_y_x_beta = y - OLS$coefficients[2]*x

# Plot com a diferen�a
ts.plot(ts(x), y, Dif_y_x_beta, gpars = list(col = c("black", "red", "blue")))

# Testa estacioneriedade da diferen�a entre as s�ries
adfTest(Dif_y_x_beta)
unitrootTest(Dif_y_x_beta)

# Plot da s�rie de diferen�a
ts.plot(Dif_y_x_beta)

# Testa estacioneriedade das s�ries
adfTest(y)
unitrootTest(y)

adfTest(x)
unitrootTest(x)
