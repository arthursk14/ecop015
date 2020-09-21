# Teste Diebold- Mariano
  
  # Modela para as primeiras observações da série
  f1 = ets(WWWusage[1:80])
  f2 = auto.arima(WWWusage[1:80])
  
  # Previsão para as demais observações (previsão dentro da amostra)
  f1.out = ets(WWWusage[81:100], model=f1)
  f2.out = Arima(WWWusage[81:100], model=f2)
  
  # Mede a acurácia da previsão feita
  accuracy(f1.out)
  accuracy(f2.out)
  
  # Teste
  dm.test(residuals(f1.out), residuals(f2.out), h=1)
  
# VAR e VEC
  
  
