# Tamanho da amostra
n = 10e2

# Constante
c = 0

# Parâmetros
fi = 0.9
de = 2
b = 0

# Set Seed
set.seed(98)

# Inovações
e = rnorm(n,0,de)
y = rep(0,n)

# Monta a série
for (i in 2:n){
  y[i] = c + b*i + fi*y[i-1] + e[i]
  
}

# Plot da série
ts.plot(y)

# Cria série da diferença de y
dy = diff(y)

# Plot da série
ts.plot(dy)

# Função de autocorrelação
acf(y)
acf(dy)

# Carrega pacote necessário
require("fUnitRoots")

# Teste de raiz unitária não aumentado sem constante
resnc = adfTest(y,type='nc')
resnc

# Teste KPSS
reskpss = kpss.test(y, null = 'Trend')
reskpss

# Teste de raiz unitária não aumentado com constante
resc = adfTest(y,type='c')
resc

# Teste de raiz unitária não aumentado com constante e tendência
resct = adfTest(y,type='ct')
resct 

t = seq(1,n)
mod1 = lm(y~t)
summary(mod1)
abline(mod1,col="blue")

res = residuals(mod1)
ts.plot(res)
adfTest(res,type="nc")
