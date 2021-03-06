# Tamanho da amostra
n = 10e2

# Constante
c = 0

# Par�metros
fi = 0.9
de = 2
b = 0

# Set Seed
set.seed(98)

# Inova��es
e = rnorm(n,0,de)
y = rep(0,n)

# Monta a s�rie
for (i in 2:n){
  y[i] = c + b*i + fi*y[i-1] + e[i]
  
}

# Plot da s�rie
ts.plot(y)

# Cria s�rie da diferen�a de y
dy = diff(y)

# Plot da s�rie
ts.plot(dy)

# Fun��o de autocorrela��o
acf(y)
acf(dy)

# Carrega pacote necess�rio
require("fUnitRoots")

# Teste de raiz unit�ria n�o aumentado sem constante
resnc = adfTest(y,type='nc')
resnc

# Teste KPSS
reskpss = kpss.test(y, null = 'Trend')
reskpss

# Teste de raiz unit�ria n�o aumentado com constante
resc = adfTest(y,type='c')
resc

# Teste de raiz unit�ria n�o aumentado com constante e tend�ncia
resct = adfTest(y,type='ct')
resct 

t = seq(1,n)
mod1 = lm(y~t)
summary(mod1)
abline(mod1,col="blue")

res = residuals(mod1)
ts.plot(res)
adfTest(res,type="nc")
