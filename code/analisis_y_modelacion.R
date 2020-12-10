library(data.table)

dt <- fread("./data/incidencia_violencia_familiar_estatal.csv")

# Filter national data and relevant columns
dt <- dt[entidad == "República Mexicana", .(fecha, tasa_incidencia)]

# Filter out november and december 2020 NA records
dt <- dt[complete.cases(dt), ]

# chart1: Tasa de incidencia de violencia familiar en México
ggplot(dt, aes(x=fecha, y=tasa_incidencia)) +
  geom_line() + 
  scale_x_date(date_labels = "%m-%Y") +
  xlab("Mes") + 
  ylab("Tasa de incidencia") +
  theme_light()

# chart2: Tasa de incidencia de violencia familiar en México por año
ggseasonplot(ts(dt$tasa_incidencia, c(2015, 1), frequency=12), year.labels=T) +
  xlab("Mes") + 
  ylab("Tasa de incidencia") +
  theme_light()

# FIlter our 2020 data and convert to ts object
data <- ts(dt[fecha <= "2020-04-01", tasa_incidencia], c(2015, 1), frequency=12) 

# Mean by year
dt[, fecha := ymd(fecha)]
dt[, anio := year(fecha)]
mean_by_year <- dt[, .(media=mean(tasa_incidencia)), by=anio]

# chart3: Descomposición de la serie de tiempo por medias móvi;es
library(forecast)
# autoplot(stl(data, s.window="periodic"))
autoplot(decompose(data, "additive")) +
  xlab("Mes") + 
  ylab("Tasa de incidencia") +
  theme_light()

# chart4: ACF y PACF de la serie de tiempo
acf(data)
pacf(data)

# Unit root tests for original time series
library(tseries)
adf.test(data) 
pp.test(data, alternative="stationary")
kpss.test(data)

# Transformation #1
log_data <- log(data)

# chart5: Serie de tiempo logarítmica
autoplot(log_data) +
  scale_x_date(date_labels = "%m-%Y") +
  xlab("Mes") + 
  ylab("Tasa de incidencia") +
  theme_light()

# Transformation #2
diff_data <- diff(data)

# chart6: Serie de tiempo diferenciada
autoplot(diff_data) +
  scale_x_date(date_labels = "%m-%Y") +
  xlab("Mes") + 
  ylab("Tasa de incidencia") +
  theme_light()

# chart6: ACF y PACF de la serie de tiempo logarítmica
acf(log_data)
pacf(log_data)

# Unit root tests for transformation #1
adf.test(log_data) 
pp.test(log_data, alternative="stationary")
kpss.test(log_data)

# chart7: ACF y PACF de la serie de tiempo diferenciada
acf(diff_data)
pacf(diff_data)

# Unit root tests for transformation #2
adf.test(diff_data) 
pp.test(diff_data, alternative="stationary")
kpss.test(diff_data)

# Modelling
model <- arima(diff_data, c(0,0,1), c(1,1,0))
model

# Normality testa for model residuals
Box.test(model$residuals, type="Ljung-Box")
jarque.bera.test(model$residuals)
shapiro.test(model$residuals)

# chart9: Ajuste del modelo
autoplot(model) +
  scale_x_date(date_labels = "%m-%Y") +
  xlab("Mes") + 
  ylab("Tasa de incidencia") +
  theme_light()

# chart10: Evaluación de los residuales del modelo
checkresiduals(model) 

# Forecasting
forecast_7m <- forecast(model, h=7)

# chart11: Pronóstico de los siguientes 7 meses para la serie de tiempo diferenciada 
autoplot(forecast_7m) + 
  xlab("Mes") + 
  ylab("Tasa de incidencia") +
  theme_light()

# Transformation 
diff_data_forecast <- c(diff_data, c(forecast_7m$mean))
data_forecast <- diffinv(diff_data_forecast, xi = 6.362703) 
data_forecast <- ts(data_forecast, c(2015, 1), frequency=12)

# chart12
autoplot(data_forecast) +
  scale_x_date(date_labels = "%m-%Y") +
  xlab("Mes") + 
  ylab("Tasa de incidencia") +
  theme_light()