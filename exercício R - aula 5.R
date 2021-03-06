require("quantmod")
require("rugarch")

# Declara as datas
startDate = as.Date("2007-01-03")
endDate = as.Date("2019-10-22")

# Pega dados
getSymbols("AAPL", from=startDate, to=endDate)

head(AAPL)
AAPL[length(AAPL[,1]),]

# Monta a s�rie de pre�os de fechamento
y = log(AAPL[3123:3223,4])

# Gr�fico da s�rie
ts.plot(y)
chart_Series(y)
chartSeries(y)

# Monta a s�rie de retornos
r = diff(log(y))

# Gr�fico da s�rie de retornos
ts.plot(r)
chart_Series(r)
chartSeries(r)
