require("quantmod")
require("rugarch")

# Declara as datas
startDate = as.Date("2007-01-03")
endDate = as.Date("2019-10-22")

# Pega dados
getSymbols("AAPL", from=startDate, to=endDate)

head(AAPL)
AAPL[length(AAPL[,1]),]

# Monta a série de preços de fechamento
y = log(AAPL[3123:3223,4])

# Gráfico da série
ts.plot(y)
chart_Series(y)
chartSeries(y)

# Monta a série de retornos
r = diff(log(y))

# Gráfico da série de retornos
ts.plot(r)
chart_Series(r)
chartSeries(r)
