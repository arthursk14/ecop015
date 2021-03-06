# Plot s�rie temporal
ts.plot(AirPassengers)

# Log da s�rie para estabilizar a vari�ncia (log reduz a escala da vari�vel)
ly = log(AirPassengers)
# Plot s�rie do log
ts.plot(ly)

# Fun��o de autocorrela��o amostral
acf(ly,lag=48)

# Tira a diferen�a da s�rie
dly = diff(ly)

# Plot s�rie diferenciada
ts.plot(dly)

# Fun��o de autocorrela��o amostral da diferen�a
acf(dly,lag=48)

# Fun��o de autocorrela��o parcial
pacf(dly,lag=48)

# Observamos um corte depois do lag sazonal 1 (primeiros 12 meses)
# Com isso, vamos tirar a diferen�a sazonal - AR(1)
m1 = arima(ly, order = c(0,1,0), seasonal = list(order = c(1,0,0), period = 12))
# Res�duos do modelo com a diferen�a sazonal
r1 = residuals(m1)

# Plot da fun��o de autocorrela��o parcial dos residuos
pacf(r1,lag=48)

