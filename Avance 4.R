#Leer

library(forecast)
library(Mcomp)
Recaudos <- read_excel(("Recaudos.xlsx"))
Recaudosf <- read_excel(("Recaudos futuro.xlsx"))
#View(Recaudosf)
#View(Recaudos)
#head(Recaudos)
#head(Recaudosf)
yri = ts(Rec,frequency=12,star=c(2017,01),end=c(2021,12))
yrf = ts(Recf,frequency=12,star=c(2022,01),end=c(2022,09))
#length(yri)
#length(yrf)
m = 9
#View(yrf)
#View(yri)
#win.graph()
#autoplot(yi, xlab = "Año", ylab = "Recaudos en MM")
win.graph()
ts.plot(yri, main="Serie temporal de recaudos", 
        ylab= "Recaudos en MM", xlab="Año")
frequency(yri)


head(yri)
cycle(yri)

win.graph()
boxplot(yri~cycle(yri), xlab="Año", ylab="Recaudos mensuales en MM", main="Recaudos mensuales en MM - ciclo:Meses",
        names = month.abb, col="lightblue")

#Descomponer

win.graph()
plot(stl(yri,"per"))
win.graph()
decompose(yri)
plot(decompose(yri), xlab="Años", ylab="Recaudos en MM", names = month.name, warning())
np = length(yri)
warnings(yri)

# grafica con fechas en el eje x

np = length(yri)

fecha = seq(as.Date("2017/01/01"), as.Date("2021/12/31"), by="months")

ejex.mes = seq(fecha[1],fecha[np], "months")
ejex.año = seq(fecha[1],fecha[np],"years")

win.graph()
plot(fecha,yri, xaxt="n", panel.first = grid(),type='l',
     main="Recaudos mensuales en MM",
     ylab='Recaudos en MM', lwd = 2,col='black')
axis.Date(1, at=ejex.mes, format="%m/%y")
axis.Date(1, at=ejex.año, labels = FALSE, tcl = -0.2)

#----- Otros valores



lyi = log(yri)
It.trig = fourier(yri,4)
It = seasonaldummy(yri)
ti = seq(1,length(yri))

#--------------------descomposición con stl

y.stl <- stl(yri,"per")

win.graph()
  plot(y.stl, main="Descomposición serie STL")

St.stl = y.stl$time.series[,1]
Tt.stl = y.stl$time.series[,2]

ti = seq(1,length(yri))

yhat.stl = Tt.stl+St.stl

fecha = seq(as.Date("2017/01/01"), as.Date("2021/12/31"), by="months")

ejex.mes = seq(fecha[1],fecha[np], "months")
ejex.año = seq(fecha[1],fecha[np],"years")

win.graph()
plot(fecha[1:(np)],yri,type='o', xlab ="meses", xaxt="n", ylab="Recaudos en MM", main="Valores ajustados con stl vs observados", col='darkgray')
lines(fecha[1:(np)],yhat.stl, col='blue')
axis.Date(1, at=ejex.mes, format="%m/%y")
axis.Date(1, at=ejex.año, labels = FALSE, tcl = -0.2)
legend("topleft", 
       c("Obs","stl"), 
       pch = c(1, 3),
       col = c("black","blue"))


medidas.yest = function(y,yest,k){
  # y = serie, m = modelo, k = numero parametros
  T = length(y)
  sse = sum((yest-y)^2)
  ssr = sum((y-mean(y))^2) 
  mse = sse/(T-k)
  R2 = 1 - sse/ssr
  Ra2 = 1 - (T-1)*(1-R2)/(T-k)
  aic = log((T-k)*exp(2*k/T)*mse/T)
  bic = log(T^(k/T)*(T-k)*mse/T)
  
  M = c(sqrt(mse),Ra2,  aic, bic)
  names(M) = c("rmse","R2-ad","log.aic","log.bic")
  return(M)
}

A=rbind(medidas.yest(yri,yhat.stl,3))
rownames(A) = c("descomp.stl")
(A)

# MOdelo 2: modelo local de tendencia amortiguado


#---------------------------forecast Modelo HW Amor
library(forecast)

mod7.m <- hw(yri, seasonal="add",damped=TRUE)
summary(mod7.m)

yhat.7 = mod7.m$model$fitted

(M = mod7.m$model$par[1:4])

str(mod7.m)

#--------------ajuste

win.graph()
plot(fecha[1:(np)],yri, panel.first = grid(),type='l', xaxt="n",
     ylab='Recaudo en MM', xlab="Año", lwd = 2,col='gray', main="HW Amortig. vs Observados")
axis.Date(1, at=ejex.mes, format="%m/%y")
axis.Date(1, at=ejex.año, labels = FALSE, tcl = -0.2)
lines(fecha[1:(np)],yhat.7,col='blue')
legend("topleft", 
       c("Obs","HW-Amort"), 
       pch = c(1, 3),
       col = c("black","red"))


medidas.yest = function(y,yest,k){
  # y = serie, m = modelo, k = numero parametros
  T = length(y)
  sse = sum((yest-y)^2)
  ssr = sum((y-mean(y))^2) 
  mse = sse/(T-k)
  R2 = 1 - sse/ssr
  Ra2 = 1 - (T-1)*(1-R2)/(T-k)
  aic = log((T-k)*exp(2*k/T)*mse/T)
  bic = log(T^(k/T)*(T-k)*mse/T)
  
  M = c(sqrt(mse),Ra2,  aic, bic)
  names(M) = c("rmse","R2-ad","log.aic","log.bic")
  return(M)
}

(A=medidas.yest(yri,yhat.7,5))

#--------------------pronosticos con stl

library(forecast)

pron.stl = forecast(y.stl,method='ets', h=9)$mean
pron.stl2=forecast(y.stl)

m = 9
T = length(yri)
Itf = seasonaldummy(yri,m)
tf = seq(T+1,T+m,1)
np = length(yri)
np2=length(yrf)
n=length(yri)+length(yrf)
length(yri)
length(yrf)

seq(T+1,T+9,1)
View(yrf)

fecha = seq(as.Date("2022/01/01"), as.Date("2022/09/01"), by="months")
ejex.mes = seq(fecha[1],fecha[np2], "months")
ejex.año = seq(fecha[1],fecha[np2],"years")

win.graph()
par(mfrow=c(1,1))
plot(tf,yrf,type = 'o', main="Pronóstico")
lines(tf,pron.stl, type = 'b', pch = 3,col='blue' )
axis.Date(1, at=seq(as.Date("2022/01/01"), as.Date("2022/09/01"), by="months"), format="%m-%Y")
axis.Date(1, at=seq(as.Date("2022/01/01"), as.Date("2022/09/01"), by="months"), labels = FALSE, tcl = -0.2)

legend("bottomleft", 
       c("Obs", "pron.stl"), 
       pch = c(1,  3),
       col = c("black","blue"))

#-------medidas de calidad de pronosticos

accuracy(pron.stl,yrf)

#-----------pronosticos HW

ypron7 = forecast(mod7.m,h=m)$mean

win.graph()
par(mfrow=c(1,1))
plot(tf,yrf, type = 'b', main="Pronóstico Obs vs STL y HWA")
lines(tf,pron.stl, type = 'b', pch = 3,col='blue' )
lines(tf,ypron7, type = 'b', pch = 3,col='red' )
legend("bottomleft", 
       c("Obs", "Des.stl", "HW Amort"), 
       pch = c(1,  3, 5),
       col = c("black","blue", "red"))



#-------medidas de calidad de pronosticos




accuracy(ypron7,yrf)


