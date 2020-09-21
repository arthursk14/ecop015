# Carrega os pacotes necessários
require("readxl")
require("fUnitRoots")
require("timetk")
require("tseries")
require("xts")
require("urca")
require("aTSA")
require("viridis")
require("RColorBrewer")

################

################
    
################
    
# Importa dados

    # Importa vértices da curva de juros brasileira (swap DI x PRE B3)

    PRE6M <- read_excel("PRE6M.xlsx")
    PRE6M <- tk_xts(PRE6M, date_var  = Data)
    plot.xts(PRE6M["2000-01-01/2020-04-02"])
    acf(ts(PRE6M["2000-01-01/2020-04-02"]))
    pp.test(ts(PRE6M["2000-01-01/2020-04-02"]))
    adf.test(ts(PRE6M["2000-01-01/2020-04-02"]))
    
    PRE1A <- read_excel("PRE1A.xlsx")
    PRE1A <- tk_xts(PRE1A, date_var  = Data)
    plot.xts(PRE1A["2000-01-01/2020-04-02"])
    acf(ts(PRE1A["2000-01-01/2020-04-02"]))
    pp.test(ts(PRE1A["2000-01-01/2020-04-02"]))
    adf.test(ts(PRE1A["2000-01-01/2020-04-02"]))
    
    PRE2A <- read_excel("PRE2A.xlsx")
    PRE2A <- tk_xts(PRE2A, date_var  = Data)
    plot.xts(PRE2A["2000-01-01/2020-04-02"])
    acf(ts(PRE2A["2000-01-01/2020-04-02"]))
    pp.test(ts(PRE2A["2000-01-01/2020-04-02"]))
    adf.test(ts(PRE2A["2000-01-01/2020-04-02"]))
    
    PRE3A <- read_excel("PRE3A.xlsx")
    PRE3A <- tk_xts(PRE3A, date_var  = Data)
    plot.xts(PRE3A["2000-01-01/2020-04-02"])
    acf(ts(PRE3A["2000-01-01/2020-04-02"]))
    pp.test(ts(PRE3A["2000-01-01/2020-04-02"]))
    adf.test(ts(PRE3A["2000-01-01/2020-04-02"]))
    
    PRE4A <- read_excel("PRE4A.xlsx")
    PRE4A <- tk_xts(PRE4A, date_var  = Data)
    plot.xts(PRE4A["2000-01-01/2020-04-02"])
    acf(ts(PRE4A["2000-01-01/2020-04-02"]))
    pp.test(ts(PRE4A["2000-01-01/2020-04-02"]))
    adf.test(ts(PRE4A["2000-01-01/2020-04-02"]))
    
    PRE5A <- read_excel("PRE5A.xlsx")
    PRE5A <- tk_xts(PRE5A, date_var  = Data)
    plot.xts(PRE5A["2000-01-01/2020-04-02"])
    acf(ts(PRE5A["2000-01-01/2020-04-02"]))
    pp.test(ts(PRE5A["2000-01-01/2020-04-02"]))
    adf.test(ts(PRE5A["2000-01-01/2020-04-02"]))
    
    PRE6A <- read_excel("PRE6A.xlsx")
    PRE6A <- tk_xts(PRE6A, date_var  = Data)
    plot.xts(PRE6A["2000-01-01/2020-04-02"])
    acf(ts(PRE6A["2000-01-01/2020-04-02"]))
    pp.test(ts(PRE6A["2000-01-01/2020-04-02"]))
    adf.test(ts(PRE6A["2000-01-01/2020-04-02"]))
    
    PRE7A <- read_excel("PRE7A.xlsx")
    PRE7A <- tk_xts(PRE7A, date_var  = Data)
    plot.xts(PRE7A["2000-01-01/2020-04-02"])
    acf(ts(PRE7A["2000-01-01/2020-04-02"]))
    pp.test(ts(PRE7A["2000-01-01/2020-04-02"]))
    adf.test(ts(PRE7A["2000-01-01/2020-04-02"]))
    
    PRE10A <- read_excel("PRE10A.xlsx")
    PRE10A <- tk_xts(PRE10A, date_var  = Data)
    plot.xts(PRE10A["2000-01-01/2020-04-02"])
    acf(ts(PRE10A["2000-01-01/2020-04-02"]))
    pp.test(ts(PRE10A["2000-01-01/2020-04-02"]))
    adf.test(ts(PRE10A["2000-01-01/2020-04-02"]))
    
    display.brewer.all()
    colors = brewer.pal(n=9, name = "Paired")
    
    plot.xts(merge(PRE6M["2000-01-01/2020-04-02"],PRE1A["2000-01-01/2020-04-02"]
                   ,PRE2A["2000-01-01/2020-04-02"],PRE3A["2000-01-01/2020-04-02"]
                   ,PRE4A["2000-01-01/2020-04-02"],PRE5A["2000-01-01/2020-04-02"]
                   ,PRE6A["2000-01-01/2020-04-02"],PRE7A["2000-01-01/2020-04-02"]
                   ,PRE10A["2000-01-01/2020-04-02"]), 
             main = "Swap DI x PRE (BM&FBOVESPA)", col = colors)
    
    addLegend("topright", 
              legend.names=c("R(0.5,t)", "R(1,t)", 
                             "R(2,t)", "R(3,t)", 
                             "R(4,t)", "R(5,t)", "R(6,t)",
                             "R(7,t)", "R(10,t)"),
              col=colors,
              lty=c(1,1,1,1,1,1,1,1,1),
              lwd=c(2,2,2,2,2,2,2,2,2),
              ncol=2,
              bg="white",
              bty="o")
    
    plot.xts(merge(diff(PRE6M["2000-01-01/2020-04-02"]),diff(PRE1A["2000-01-01/2020-04-02"])
                   ,diff(PRE2A["2000-01-01/2020-04-02"]),diff(PRE3A["2000-01-01/2020-04-02"])
                   ,diff(PRE4A["2000-01-01/2020-04-02"]),diff(PRE5A["2000-01-01/2020-04-02"])
                   ,diff(PRE6A["2000-01-01/2020-04-02"]),diff(PRE7A["2000-01-01/2020-04-02"])
                   ,diff(PRE10A["2000-01-01/2020-04-02"])), 
             main = "Primeira diferença: Swap DI x PRE (BM&FBOVESPA)", col = colors)
    
    addLegend("topright", 
              legend.names=c("R(0.5,t)", "R(1,t)", 
                             "R(2,t)", "R(3,t)", 
                             "R(4,t)", "R(5,t)", "R(6,t)",
                             "R(7,t)", "R(10,t)"),
              col=colors,
              lty=c(1,1,1,1,1,1,1,1,1),
              lwd=c(2,2,2,2,2,2,2,2,2),
              ncol=2,
              bg="white",
              bty="o")
    
    # Importa as séries dos vértices das curvas de crédito
    
    AAA <- read_excel("AAA.xlsx")
    AAA = tk_xts(AAA, date_var  = Data)
    plot.xts(AAA)
    
    AA <- read_excel("AA.xlsx")
    AA = tk_xts(AA, date_var  = Data)
    plot.xts(AA)
    
    A <- read_excel("A.xlsx")
    A = tk_xts(A, date_var  = Data)
    plot.xts(A)
    
################

# Teste de Johansen para os vértices da curva PRE (replicando hall1992)
    
    jotest = ca.jo(merge(PRE6M["2000-01-01/2020-04-02"],PRE1A["2000-01-01/2020-04-02"]
                         ,PRE2A["2000-01-01/2020-04-02"],PRE3A["2000-01-01/2020-04-02"]
                         ,PRE4A["2000-01-01/2020-04-02"],PRE5A["2000-01-01/2020-04-02"]
                         ,PRE6A["2000-01-01/2020-04-02"],PRE7A["2000-01-01/2020-04-02"]
                         ,PRE10A["2000-01-01/2020-04-02"]))
    summary(jotest)
    
# Teste de Engle-Granger para os vértices da curva PRE (replicando hall1992)
    
    # PRE1A x PRE6M
    
      # Tempo
      t = seq(1,length(PRE6M["2000-01-01/2020-04-02"]))
      
      # OLS
      OLS = lm(PRE1A["2000-01-01/2020-04-02"]~t+PRE6M["2000-01-01/2020-04-02"])  
      summary(OLS)
      
      # Cria série dos resíduos
      res = tk_xts(OLS$residuals)
      plot.xts(res, main = "Resíduos da regressão")
      
      # Testa estacionariedade dos resíduos
      acf(ts(res))
      pp.test(ts(res))
      adf.test(ts(res))
      
    # PRE2A x PRE1A
      
      # Tempo
      t = seq(1,length(PRE1A["2000-01-01/2020-04-02"]))
      
      # OLS
      OLS = lm(PRE2A["2000-01-01/2020-04-02"]~t+PRE1A["2000-01-01/2020-04-02"])  
      summary(OLS)
      
      # Cria série dos resíduos
      res = tk_xts(OLS$residuals)
      plot.xts(res, main = "Resíduos da regressão")
      
      # Testa estacionariedade dos resíduos
      acf(ts(res))
      pp.test(ts(res))
      adf.test(ts(res))
      
    # PRE3A x PRE1A
      
      # Tempo
      t = seq(1,length(PRE1A["2000-01-01/2020-04-02"]))
      
      # OLS
      OLS = lm(PRE3A["2000-01-01/2020-04-02"]~t+PRE1A["2000-01-01/2020-04-02"])  
      summary(OLS)
      
      # Cria série dos resíduos
      res = tk_xts(OLS$residuals)
      plot.xts(res, main = "Resíduos da regressão")
      
      # Testa estacionariedade dos resíduos
      acf(ts(res))
      pp.test(ts(res))
      adf.test(ts(res))

    # PRE4A x PRE1A
      
      # Tempo
      t = seq(1,length(PRE1A["2000-01-01/2020-04-02"]))
      
      # OLS
      OLS = lm(PRE4A["2000-01-01/2020-04-02"]~t+PRE1A["2000-01-01/2020-04-02"])  
      summary(OLS)
      
      # Cria série dos resíduos
      res = tk_xts(OLS$residuals)
      plot.xts(res, main = "Resíduos da regressão")
      
      # Testa estacionariedade dos resíduos
      acf(ts(res))
      pp.test(ts(res))
      adf.test(ts(res))  
      
################
        
        # Monta séries de crédito
      
        plot.xts(merge(PRE6M["2013-11-01/2019-11-01"],PRE1A["2013-11-01/2019-11-01"]
                       ,PRE2A["2013-11-01/2019-11-01"],PRE3A["2013-11-01/2019-11-01"]
                       ,PRE4A["2013-11-01/2019-11-01"],PRE5A["2013-11-01/2019-11-01"]
                       ,PRE6A["2013-11-01/2019-11-01"],PRE7A["2013-11-01/2019-11-01"]
                       ,PRE10A["2013-11-01/2019-11-01"]), 
                 main = "Swap DI x PRE (BM&FBOVESPA)", col = colors)
        
        addLegend("topright", 
                  legend.names=c("R(0.5,t)", "R(1,t)", 
                                 "R(2,t)", "R(3,t)", 
                                 "R(4,t)", "R(5,t)", "R(6,t)",
                                 "R(7,t)", "R(10,t)"),
                  col=colors,
                  lty=c(1,1,1,1,1,1,1,1,1),
                  lwd=c(2,2,2,2,2,2,2,2,2),
                  ncol=2,
                  bg="white",
                  bty="o")
        
        # AAA
          AAA6m = ((1+PRE6M)*(1+AAA$`6m`/100)) - 1
          AAA1a = ((1+PRE1A)*(1+AAA$`12m`/100)) - 1
          AAA2a = ((1+PRE2A)*(1+AAA$`24m`/100)) - 1
          AAA3a = ((1+PRE3A)*(1+AAA$`36m`/100)) - 1
          AAA4a = ((1+PRE4A)*(1+AAA$`48m`/100)) - 1
          AAA5a = ((1+PRE5A)*(1+AAA$`60m`/100)) - 1
          AAA6a = ((1+PRE6A)*(1+AAA$`72m`/100)) - 1
          AAA7a = ((1+PRE7A)*(1+AAA$`84m`/100)) - 1
          AAA10a = ((1+PRE10A)*(1+AAA$`120m`/100)) - 1
          
          plot.xts(merge(AAA6m["2013-11-01/2019-11-01"], AAA1a["2013-11-01/2019-11-01"]
                         , AAA2a["2013-11-01/2019-11-01"], AAA3a["2013-11-01/2019-11-01"]
                         , AAA4a["2013-11-01/2019-11-01"], AAA5a["2013-11-01/2019-11-01"]
                         , AAA6a["2013-11-01/2019-11-01"], AAA7a["2013-11-01/2019-11-01"]
                         , AAA10a["2013-11-01/2019-11-01"])
                   , main = "Taxas praticadas: Debêntures AAA", col = colors)
          
          addLegend("topright", 
                    legend.names=c("D(0.5,t)", "D(1,t)", 
                                   "D(2,t)", "D(3,t)", 
                                   "D(4,t)", "D(5,t)", "D(6,t)",
                                   "D(7,t)", "D(10,t)"),
                    col=colors,
                    lty=c(1,1,1,1,1,1,1,1,1),
                    lwd=c(2,2,2,2,2,2,2,2,2),
                    ncol=2,
                    bg="white",
                    bty="o")
        
        # AA
          AA6m = ((1+PRE6M)*(1+AA$`6m`/100)) - 1
          AA1a = ((1+PRE1A)*(1+AA$`12m`/100)) - 1
          AA2a = ((1+PRE2A)*(1+AA$`24m`/100)) - 1
          AA3a = ((1+PRE3A)*(1+AA$`36m`/100)) - 1
          AA4a = ((1+PRE4A)*(1+AA$`48m`/100)) - 1
          AA5a = ((1+PRE5A)*(1+AA$`60m`/100)) - 1
          AA6a = ((1+PRE6A)*(1+AA$`72m`/100)) - 1
          AA7a = ((1+PRE7A)*(1+AA$`84m`/100)) - 1
          AA10a = ((1+PRE10A)*(1+AA$`120m`/100)) - 1
          
          plot.xts(merge(AA6m["2013-11-01/2019-11-01"], AA1a["2013-11-01/2019-11-01"]
                         , AA2a["2013-11-01/2019-11-01"], AA3a["2013-11-01/2019-11-01"]
                         , AA4a["2013-11-01/2019-11-01"], AA5a["2013-11-01/2019-11-01"]
                         , AA6a["2013-11-01/2019-11-01"], AA7a["2013-11-01/2019-11-01"]
                         , AA10a["2013-11-01/2019-11-01"])
                   , main = "Taxas praticadas: Debêntures AA", col = colors)
          
          addLegend("topright", 
                    legend.names=c("D(0.5,t)", "D(1,t)", 
                                   "D(2,t)", "D(3,t)", 
                                   "D(4,t)", "D(5,t)", "D(6,t)",
                                   "D(7,t)", "D(10,t)"),
                    col=colors,
                    lty=c(1,1,1,1,1,1,1,1,1),
                    lwd=c(2,2,2,2,2,2,2,2,2),
                    ncol=2,
                    bg="white",
                    bty="o")
          
        # A
          A6m = ((1+PRE6M)*(1+A$`6m`/100)) - 1
          A1a = ((1+PRE1A)*(1+A$`12m`/100)) - 1
          A2a = ((1+PRE2A)*(1+A$`24m`/100)) - 1
          A3a = ((1+PRE3A)*(1+A$`36m`/100)) - 1
          A4a = ((1+PRE4A)*(1+A$`48m`/100)) - 1
          A5a = ((1+PRE5A)*(1+A$`60m`/100)) - 1
          A6a = ((1+PRE6A)*(1+A$`72m`/100)) - 1
          A7a = ((1+PRE7A)*(1+A$`84m`/100)) - 1
          A10a = ((1+PRE10A)*(1+A$`120m`/100)) - 1
          
          plot.xts(merge(A6m["2013-11-01/2019-11-01"], A1a["2013-11-01/2019-11-01"]
                         , A2a["2013-11-01/2019-11-01"], A3a["2013-11-01/2019-11-01"]
                         , A4a["2013-11-01/2019-11-01"], A5a["2013-11-01/2019-11-01"]
                         , A6a["2013-11-01/2019-11-01"], A7a["2013-11-01/2019-11-01"]
                         , A10a["2013-11-01/2019-11-01"])
                   , main = "Taxas praticadas: Debêntures A", col = colors)
          
          addLegend("topright", 
                    legend.names=c("D(0.5,t)", "D(1,t)", 
                                   "D(2,t)", "D(3,t)", 
                                   "D(4,t)", "D(5,t)", "D(6,t)",
                                   "D(7,t)", "D(10,t)"),
                    col=colors,
                    lty=c(1,1,1,1,1,1,1,1,1),
                    lwd=c(2,2,2,2,2,2,2,2,2),
                    ncol=2,
                    bg="white",
                    bty="o")
          
        # Testa estacioneriedade das séries
          
          pp.test(ts(AAA6m))
          pp.test(ts(AAA1a))
          pp.test(ts(AAA2a))
          pp.test(ts(AAA3a))
          pp.test(ts(AAA4a))
          pp.test(ts(AAA5a))
          pp.test(ts(AAA6a))
          pp.test(ts(AAA7a))
          pp.test(ts(AAA10a))
          
          pp.test(ts(AA6m))
          pp.test(ts(AA1a))
          pp.test(ts(AA2a))
          pp.test(ts(AA3a))
          pp.test(ts(AA4a))
          pp.test(ts(AA5a))
          pp.test(ts(AA6a))
          pp.test(ts(AA7a))
          pp.test(ts(AA10a))
          
          pp.test(ts(A6m))
          pp.test(ts(A1a))
          pp.test(ts(A2a))
          pp.test(ts(A3a))
          pp.test(ts(A4a))
          pp.test(ts(A5a))
          pp.test(ts(A6a))
          pp.test(ts(A7a))
          pp.test(ts(A10a))

      # Testa cointegração: até 2019
          
        # 6 meses
          
          # Tempo
          t = seq(1,length(PRE6M["2013-11-01/2019-11-01"]))
          
          # OLS
          OLS = lm(PRE6M["2013-11-01/2019-11-01"]~t+AAA6m["2013-11-01/2019-11-01"])  
          summary(OLS)
          
          # Cria série dos resíduos
          res = tk_xts(OLS$residuals)
          plot.xts(res, main = "Resíduos da regressão")
          
          # Testa estacionariedade dos resíduos
          acf(ts(res))
          pp.test(ts(res))
          adf.test(ts(res))  
          
          # Teste de Johansen
          jotest = ca.jo(merge(PRE6M["2013-11-01/2019-11-01"],AAA6m["2013-11-01/2019-11-01"]))
          summary(jotest)
          
        # 1 ano
          
          # Tempo
          t = seq(1,length(PRE1A["2013-11-01/2019-11-01"]))
          
          # OLS
          OLS = lm(PRE1A["2013-11-01/2019-11-01"]~t+AAA1a["2013-11-01/2019-11-01"])  
          summary(OLS)
          
          # Cria série dos resíduos
          res = tk_xts(OLS$residuals)
          plot.xts(res, main = "Resíduos da regressão")
          
          # Testa estacionariedade dos resíduos
          acf(ts(res))
          pp.test(ts(res))
          adf.test(ts(res))  
          
          # Teste de Johansen
          jotest = ca.jo(merge(PRE1A["2013-11-01/2019-11-01"],AAA1a["2013-11-01/2019-11-01"]))
          summary(jotest)
          
        # 10 anos
          
          # Tempo
          t = seq(1,length(PRE10A["2013-11-01/2019-11-01"]))
          
          # OLS
          OLS = lm(PRE10A["2013-11-01/2019-11-01"]~t+AAA10a["2013-11-01/2019-11-01"])  
          summary(OLS)
          
          # Cria série dos resíduos
          res = tk_xts(OLS$residuals)
          plot.xts(res, main = "Resíduos da regressão")
          
          # Testa estacionariedade dos resíduos
          acf(ts(res))
          pp.test(ts(res))
          adf.test(ts(res))  
          
          # Teste de Johansen
          jotest = ca.jo(merge(PRE10A["2013-11-01/2019-11-01"],AAA10a["2013-11-01/2019-11-01"]))
          summary(jotest)
          
        # 6 meses
          
          # Tempo
          t = seq(1,length(PRE6M["2013-11-01/2019-11-01"]))
          
          # OLS
          OLS = lm(PRE6M["2013-11-01/2019-11-01"]~t+AA6m["2013-11-01/2019-11-01"])  
          summary(OLS)
          
          # Cria série dos resíduos
          res = tk_xts(OLS$residuals)
          plot.xts(res, main = "Resíduos da regressão")
          
          # Testa estacionariedade dos resíduos
          acf(ts(res))
          pp.test(ts(res))
          adf.test(ts(res)) 
          
          # Teste de Johansen
          jotest = ca.jo(merge(PRE6M["2013-11-01/2019-11-01"],AA6m["2013-11-01/2019-11-01"]))
          summary(jotest)
          
        # 1 ano
          
          # Tempo
          t = seq(1,length(PRE1A["2013-11-01/2019-11-01"]))
          
          # OLS
          OLS = lm(PRE1A["2013-11-01/2019-11-01"]~t+AA1a["2013-11-01/2019-11-01"])  
          summary(OLS)
          
          # Cria série dos resíduos
          res = tk_xts(OLS$residuals)
          plot.xts(res, main = "Resíduos da regressão")
          
          # Testa estacionariedade dos resíduos
          acf(ts(res))
          pp.test(ts(res))
          adf.test(ts(res)) 
          
          # Teste de Johansen
          jotest = ca.jo(merge(PRE1A["2013-11-01/2019-11-01"],AA1a["2013-11-01/2019-11-01"]))
          summary(jotest)
          
        # 10 anos
          
          # Tempo
          t = seq(1,length(PRE10A["2013-11-01/2019-11-01"]))
          
          # OLS
          OLS = lm(PRE10A["2013-11-01/2019-11-01"]~t+AA10a["2013-11-01/2019-11-01"])  
          summary(OLS)
          
          # Cria série dos resíduos
          res = tk_xts(OLS$residuals)
          plot.xts(res, main = "Resíduos da regressão")
          
          # Testa estacionariedade dos resíduos
          acf(ts(res))
          pp.test(ts(res))
          adf.test(ts(res))
          
          # Teste de Johansen
          jotest = ca.jo(merge(PRE10A["2013-11-01/2019-11-01"],AA10a["2013-11-01/2019-11-01"]))
          summary(jotest)
          
        # 6 meses
          
          # Tempo
          t = seq(1,length(PRE6M["2013-11-01/2019-11-01"]))
          
          # OLS
          OLS = lm(PRE6M["2013-11-01/2019-11-01"]~t+A6m["2013-11-01/2019-11-01"])  
          summary(OLS)
          
          # Cria série dos resíduos
          res = tk_xts(OLS$residuals)
          plot.xts(res, main = "Resíduos da regressão")
          
          # Testa estacionariedade dos resíduos
          acf(ts(res))
          pp.test(ts(res))
          adf.test(ts(res)) 
          
          # Teste de Johansen
          jotest = ca.jo(merge(PRE6M["2013-11-01/2019-11-01"],A6m["2013-11-01/2019-11-01"]))
          summary(jotest)
          
        # 1 ano
          
          # Tempo
          t = seq(1,length(PRE1A["2013-11-01/2019-11-01"]))
          
          # OLS
          OLS = lm(PRE1A["2013-11-01/2019-11-01"]~t+A1a["2013-11-01/2019-11-01"])  
          summary(OLS)
          
          # Cria série dos resíduos
          res = tk_xts(OLS$residuals)
          plot.xts(res, main = "Resíduos da regressão")
          
          # Testa estacionariedade dos resíduos
          acf(ts(res))
          pp.test(ts(res))
          adf.test(ts(res)) 
          
          # Teste de Johansen
          jotest = ca.jo(merge(PRE1A["2013-11-01/2019-11-01"],A1a["2013-11-01/2019-11-01"]))
          summary(jotest)
          
        # 10 anos
          
          # Tempo
          t = seq(1,length(PRE10A["2013-11-01/2019-11-01"]))
          
          # OLS
          OLS = lm(PRE10A["2013-11-01/2019-11-01"]~t+A10a["2013-11-01/2019-11-01"])  
          summary(OLS)
          
          # Cria série dos resíduos
          res = tk_xts(OLS$residuals)
          plot.xts(res, main = "Resíduos da regressão")
          
          # Testa estacionariedade dos resíduos
          acf(ts(res))
          pp.test(ts(res))
          adf.test(ts(res))
          
          # Teste de Johansen
          jotest = ca.jo(merge(PRE10A["2013-11-01/2019-11-01"],A10a["2013-11-01/2019-11-01"]))
          summary(jotest)
          
      # Testa cointegração: até 2020
          
        # 6 meses
          
          # Tempo
          t = seq(1,length(PRE6M["2013-11-01/2020-04-01"]))
          
          # OLS
          OLS = lm(AAA6m["2013-11-01/2020-04-01"]~t+PRE6M["2013-11-01/2020-04-01"])  
          summary(OLS)
          
          # Cria série dos resíduos
          res = tk_xts(OLS$residuals)
          plot.xts(res, main = "Resíduos da regressão D(1,t) ~ R(1,t)")
          
          # Testa estacionariedade dos resíduos
          acf(ts(res))
          pp.test(ts(res))
          adf.test(ts(res))  
          
          # Teste de Johansen
          jotest = ca.jo(merge(PRE6M["2013-11-01/2020-04-01"],AAA6m["2013-11-01/2020-04-01"]))
          summary(jotest)
          
        # 1 ano
          
          # Tempo
          t = seq(1,length(PRE1A["2013-11-01/2020-04-01"]))
          
          # OLS
          OLS = lm(PRE1A["2013-11-01/2020-04-01"]~t+AAA1a["2013-11-01/2020-04-01"])  
          summary(OLS)
          
          # Cria série dos resíduos
          res = tk_xts(OLS$residuals)
          plot.xts(res, main = "Resíduos da regressão")
          
          # Testa estacionariedade dos resíduos
          acf(ts(res))
          pp.test(ts(res))
          adf.test(ts(res))  
          
          # Teste de Johansen
          jotest = ca.jo(merge(PRE1A["2013-11-01/2020-04-01"],AAA1a["2013-11-01/2020-04-01"]))
          summary(jotest)
          
        # 10 anos
          
          # Tempo
          t = seq(1,length(PRE10A["2013-11-01/2020-04-01"]))
          
          # OLS
          OLS = lm(PRE10A["2013-11-01/2020-04-01"]~t+AAA10a["2013-11-01/2020-04-01"])  
          summary(OLS)
          
          # Cria série dos resíduos
          res = tk_xts(OLS$residuals)
          plot.xts(res, main = "Resíduos da regressão")
          
          # Testa estacionariedade dos resíduos
          acf(ts(res))
          pp.test(ts(res))
          adf.test(ts(res))  
          
          # Teste de Johansen
          jotest = ca.jo(merge(PRE10A["2013-11-01/2020-04-01"],AAA10a["2013-11-01/2020-04-01"]))
          summary(jotest)
          
        # 6 meses
          
          # Tempo
          t = seq(1,length(PRE6M["2013-11-01/2020-04-01"]))
          
          # OLS
          OLS = lm(PRE6M["2013-11-01/2020-04-01"]~t+AA6m["2013-11-01/2020-04-01"])  
          summary(OLS)
          
          # Cria série dos resíduos
          res = tk_xts(OLS$residuals)
          plot.xts(res, main = "Resíduos da regressão")
          
          # Testa estacionariedade dos resíduos
          acf(ts(res))
          pp.test(ts(res))
          adf.test(ts(res)) 
          
          # Teste de Johansen
          jotest = ca.jo(merge(PRE6M["2013-11-01/2020-04-01"],AA6m["2013-11-01/2020-04-01"]))
          summary(jotest)
          
        # 1 ano
          
          # Tempo
          t = seq(1,length(PRE1A["2013-11-01/2020-04-01"]))
          
          # OLS
          OLS = lm(PRE1A["2013-11-01/2020-04-01"]~t+AA1a["2013-11-01/2020-04-01"])  
          summary(OLS)
          
          # Cria série dos resíduos
          res = tk_xts(OLS$residuals)
          plot.xts(res, main = "Resíduos da regressão")
          
          # Testa estacionariedade dos resíduos
          acf(ts(res))
          pp.test(ts(res))
          adf.test(ts(res)) 
          
          # Teste de Johansen
          jotest = ca.jo(merge(PRE1A["2013-11-01/2020-04-01"],AA1a["2013-11-01/2020-04-01"]))
          summary(jotest)
          
        # 10 anos
          
          # Tempo
          t = seq(1,length(PRE10A["2013-11-01/2020-04-01"]))
          
          # OLS
          OLS = lm(PRE10A["2013-11-01/2020-04-01"]~t+AA10a["2013-11-01/2020-04-01"])  
          summary(OLS)
          
          # Cria série dos resíduos
          res = tk_xts(OLS$residuals)
          plot.xts(res, main = "Resíduos da regressão")
          
          # Testa estacionariedade dos resíduos
          acf(ts(res))
          pp.test(ts(res))
          adf.test(ts(res))
          
          # Teste de Johansen
          jotest = ca.jo(merge(PRE10A["2013-11-01/2020-04-01"],AA10a["2013-11-01/2020-04-01"]))
          summary(jotest)
          
        # 6 meses
          
          # Tempo
          t = seq(1,length(PRE6M["2013-11-01/2020-04-01"]))
          
          # OLS
          OLS = lm(PRE6M["2013-11-01/2020-04-01"]~t+A6m["2013-11-01/2020-04-01"])  
          summary(OLS)
          
          # Cria série dos resíduos
          res = tk_xts(OLS$residuals)
          plot.xts(res, main = "Resíduos da regressão")
          
          # Testa estacionariedade dos resíduos
          acf(ts(res))
          pp.test(ts(res))
          adf.test(ts(res)) 
          
          # Teste de Johansen
          jotest = ca.jo(merge(PRE6M["2013-11-01/2020-04-01"],A6m["2013-11-01/2020-04-01"]))
          summary(jotest)
          
        # 1 ano
          
          # Tempo
          t = seq(1,length(PRE1A["2013-11-01/2020-04-01"]))
          
          # OLS
          OLS = lm(PRE1A["2013-11-01/2020-04-01"]~t+A1a["2013-11-01/2020-04-01"])  
          summary(OLS)
          
          # Cria série dos resíduos
          res = tk_xts(OLS$residuals)
          plot.xts(res, main = "Resíduos da regressão")
          
          # Testa estacionariedade dos resíduos
          acf(ts(res))
          pp.test(ts(res))
          adf.test(ts(res)) 
          
          # Teste de Johansen
          jotest = ca.jo(merge(PRE1A["2013-11-01/2020-04-01"],A1a["2013-11-01/2020-04-01"]))
          summary(jotest)
          
        # 10 anos
          
          # Tempo
          t = seq(1,length(PRE10A["2013-11-01/2020-04-01"]))
          
          # OLS
          OLS = lm(PRE10A["2013-11-01/2020-04-01"]~t+A10a["2013-11-01/2020-04-01"])  
          summary(OLS)
          
          # Cria série dos resíduos
          res = tk_xts(OLS$residuals)
          plot.xts(res, main = "Resíduos da regressão")
          
          # Testa estacionariedade dos resíduos
          acf(ts(res))
          pp.test(ts(res))
          adf.test(ts(res))
          
          # Teste de Johansen
          jotest = ca.jo(merge(PRE10A["2013-11-01/2020-04-01"],A10a["2013-11-01/2020-04-01"]))
          summary(jotest)
          