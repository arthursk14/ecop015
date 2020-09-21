# Plot série temporal
ts.plot(AirPassengers)

# Log da série para estabilizar a variância (log reduz a escala da variável)
ly = log(AirPassengers)
# Plot série do log
ts.plot(ly)

# Função de autocorrelação amostral
acf(ly,lag=48)

# Tira a diferença da série
dly = diff(ly)

# Plot série diferenciada
ts.plot(dly)

# Função de autocorrelação amostral da diferença
acf(dly,lag=48)

# Função de autocorrelação parcial
pacf(dly,lag=48)

# Observamos um corte depois do lag sazonal 1 (primeiros 12 meses)
# Com isso, vamos tirar a diferença sazonal - AR(1)
m1 = arima(ly, order = c(0,1,0), seasonal = list(order = c(1,0,0), period = 12))
# Resíduos do modelo com a diferença sazonal
r1 = residuals(m1)

# Plot da função de autocorrelação parcial dos residuos
pacf(r1,lag=48)

