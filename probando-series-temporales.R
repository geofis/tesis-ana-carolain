# Paquetes
library(zoo)
library(lubridate)
# Datos
df_selec <- distances[distances$transect==6, c('date', 'distance_sign')]
todas_las_fechas <- data.frame(date = seq(min(df_selec$date), max(df_selec$date), by = "day"))
df_completo <- merge(todas_las_fechas, df_selec, all.x = T)
serie_regular <- zoo(df_completo$distance_sign, df_completo$date)

# Rellenado de la serie usando medias móviles
serie_rellena <- na.approx(serie_regular, na.rm = F)

# Serie clase ts
serie_rellena_ts <- ts(data = as.vector(serie_rellena), frequency = 365.25,
                       start = c(year(min(df_selec$date)), yday(min(df_selec$date))))

# Obtención de la tendencia por descomposición de la serie mediante medias móviles, y obtención tasa de cambio  (-erosión, +acreción)
descomposicion <- decompose(serie_rellena_ts)
plot(descomposicion)
tasa_de_cambio_por_dia <- diff(descomposicion$trend)
tendencia_por_dia <- zoo(descomposicion$trend, todas_las_fechas[-1, ])
tendencia_por_mes <- aggregate(tendencia_por_dia, as.yearmon, tail, 1)
tasa_de_cambio_por_mes <- diff(tendencia_por_mes)
tasa_de_cambio_por_mes
tendencia_por_ano <- aggregate(tendencia_por_dia, function(x) year(floor(as.yearmon(x))), tail, 1)
tasa_de_cambio_por_ano <- diff(tendencia_por_ano)
tasa_de_cambio_por_ano
tasa_de_cambio_anual <- mean(tasa_de_cambio_por_ano, na.rm = T)
tasa_de_cambio_anual

# Obtención de la tendencia por descomposición de la serie mediante LOESS, y obtención tasa de cambio  (-erosión, +acreción)
descomposicion <- stl(serie_rellena_ts, 'periodic', na.action = na.trim)
plot(descomposicion)
tendencia_por_dia <- zoo(as.matrix(descomposicion$time.series[,'trend']), todas_las_fechas$date)
tendencia_por_mes <- aggregate(tendencia_por_dia, as.yearmon, tail, 1)
tasa_de_cambio_por_mes <- diff(tendencia_por_mes)
tasa_de_cambio_por_mes
tendencia_por_ano <- aggregate(tendencia_por_dia, function(x) year(floor(as.yearmon(x))), tail, 1)
tasa_de_cambio_por_ano <- diff(tendencia_por_ano)
tasa_de_cambio_por_ano
tasa_de_cambio_anual <- mean(tasa_de_cambio_por_ano, na.rm = T)
tasa_de_cambio_anual




# Tasa de cambio de periodo completo y anual por medio del método Theil-Sen (-erosión, +acreción)
serie_rellena_mensual_ts <- as.ts(aggregate(as.zoo(serie_rellena_ts), as.yearmon, mean, na.rm=T))
tasa_cambio_todo <- sens.slope(serie_rellena_mensual_ts)$estimate[[1]]
tasa_cambio_ano <- sapply(2013:2021,
                            function(x)
                              sens.slope(
                                window(serie_rellena_ts,
                                       start=c(x, 1),
                                       end=c(x, 365)))$estimate[[1]]*365.25,
                            simplify = F)

# Algunas pruebas con la prueba Mann-Kendall
sapply(2013:2021, function(x) mk.test(window(serie_rellena_ts, start=c(x, 1), end=c(x, 365))), simplify = F)
sapply(2013:2021, function(x)
  mk.test(
    window(
      as.ts(
        # aggregate(as.zoo(serie_rellena_ts), as.yearmon, tail, 1)
        aggregate(as.zoo(serie_rellena_ts), week, mean, na.rm=T)
        ), start=c(x,1), end=c(x,12))),
  simplify = F) %>% setNames(2013:2021)
