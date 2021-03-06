# Econometria II - Cointegra��o

# Montando o modelo para as duas s�ries
n = 1000
d1 = 2
d2 = 1
d3 = 1.5

e1 = rnorm(n,0,d1)
e2 = rnorm(n,0,d2)
e3 = rnorm(n,0,d3)

y1 = y2 = y3 = rep(0,n)

for (i in 2:n){
  y1[i] = y1[i-1] + e1[i]
  
  y2[i] = y2[i-1] + e2[i]
  
  y3[i] = 2*y1[i] + e3[i]
  
}

# Plot das s�ries criadas

ts.plot(ts(y1),ts(y2),ts(y3),col=c("blue","red","green"))

# Regress�es lineares
reg1 = lm(y1~y2)
summary(reg1)

reg2 = lm(y1~y3)
summary(reg2)

# An�lise dos res�duos
r1 = residuals(reg1)
r2 = residuals(reg2)

# Gr�fico dos res�duos
ts.plot(r1)
ts.plot(r2)

# Teste para estacionariedade dos res�duos
adf.test(r1)
